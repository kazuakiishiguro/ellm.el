;;; ellm.el --- Minimal interactive LLM chat in Emacs (ChatGPT)  -*- lexical-binding: t; -*-

;;; Commentary:
;; ellm.el provides an interactive shell for chatting with an LLM (e.g., OpenAI's ChatGPT)
;; directly from Emacs.It uses comint-mode for an interactive experience (prompt, history),
;; and communicates with the OpenAI API using Emacs's built-in URL library.
;;
;; Usage:
;;  M-x ellm  -- to start a chat session in an Emacs buffer.
;;  In the *ElLM* buffer, type your question at the prompt and press Enter.
;;  Type "clear" to reset the conversation.
;;  M-x ellm-explain-code -- After selecting code, explains it in a dedicated buffer with markdown view.
;;
;; Customize `ellm-api-key` with your OpenAI API key (required).

;;; Code:

(require 'comint)
(require 'json)
(require 'markdown-mode nil t) ;; Optional dependency for code explanations

(defgroup ellm nil
  "Interactive chat with an LLM (Large Language Model) in Emacs."
  :group 'applications)

(defcustom ellm-api-key nil
  "OpenAI API key used for authentication.
This can be a string containing the API key, or a function that returns the key string."
  :type '(choice (const :tag "Not set" nil) string function)
  :group 'ellm)

(defcustom ellm-model "chatgpt-4o-latest"
  "Which OpenAI model to use for chat completions."
  :type 'string
  :group 'ellm)

(defcustom ellm-system-message "You use markdown liberally to structure responses. Always show code snippets in markdown blocks with language labels."
   "Initial system prompt message sent to the LLM at the start of conversation.
If nil or empty, no system message is sent."
   :type 'string
   :group 'ellm)

(defcustom ellm-welcome-message "💬 ElLM is ready! Type your prompt and press Enter. (Type 'clear' to reset.)"
  "Optional welcome text shown in the chat buffer on startup. Set to nil for no welcome message."
  :type '(choice string (const nil))
  :group 'ellm)

(defconst ellm-endpoint "https://api.openai.com/v1/chat/completions"
  "URL endpoint for OpenAI Chat Completion API.")

(defvar-local ellm--conversation nil
  "Conversation history as a list of messages.
Each entry is an alist like ((\"role\" . \"user\") (\"content\" . \"...\")).")

(defvar-local ellm--busy nil
  "Non-nil if a response is currently being fetched (i.e., awaiting API reply).")

;; New customization options for code explanation feature
(defcustom ellm-explain-code-prompt "Explain the following code:\n%s"
  "Template prompt for explaining code.
%s will be replaced by the selected code region."
  :type 'string
  :group 'ellm)

(defcustom ellm-explain-buffer-name "*ElLM Code Explanation*"
  "Buffer name for code explanations."
  :type 'string
  :group 'ellm)

(defcustom ellm-prompt-face '(:foreground "blue" :weight bold)
  "Face used to display user prompts in explanation buffer."
  :type 'face
  :group 'ellm)

(defcustom ellm-code-face '(:background "gray90" :extend t)
  "Face used to display code in explanation buffer."
  :type 'face
  :group 'ellm)

(defun ellm--get-api-key ()
  "Return the OpenAI API key as a string, or signal an error if not set."
  (cond
   ((and ellm-api-key (stringp ellm-api-key))
    ellm-api-key)
   ((and ellm-api-key (functionp ellm-api-key))
    (funcall ellm-api-key))
   (t
    (error "Please set `ellm-api-key` to your OpenAI API key (string or function)"))))

(defun ellm--insert-prompt ()
  "Insert the ElLM prompt at point (usually end of buffer)."
  (let ((prompt "ElLM> ")
        (inhibit-read-only t))
    (unless (bolp) (insert "\n"))
    (let ((start (point)))
      (insert prompt)
      (when comint-prompt-read-only
        (add-text-properties start (point)
                             '(read-only t front-sticky (read-only) rear-nonsticky (read-only)))))))

;; Ensure a dummy process exists in the buffer so that comint-mode doesn't complain.
(defun ellm--ensure-process ()
  "Ensure the current ElLM buffer has a dummy process."
  (unless (get-buffer-process (current-buffer))
    (let ((proc (start-process "ellm-dummy" (current-buffer) "cat")))
      (set-process-query-on-exit-flag proc nil))))

;;;###autoload
(define-derived-mode ellm-mode comint-mode "ElLM"
  "Major mode for interacting with an LLM via ElLM."
  :group 'ellm
  (setq comint-prompt-regexp "^ElLM> ")
  (setq comint-prompt-read-only t)
  (setq comint-input-sender #'ellm--send)
  (define-key ellm-mode-map (kbd "C-d") #'ellm-exit-session)
  (visual-line-mode 1))

;; Exit session
(defun ellm-exit-session ()
  "Gracefully exit LLM session"
  (interactive)
  (kill-buffer-and-window))

;;;###autoload
(defun ellm (&optional reset)
  "Launch a ElLM chat session. With prefix argument RESET, start a fresh session."
  (interactive "P")
  (let ((buf-name "*ElLM*"))
    (when reset
      (when (get-buffer buf-name)
        (kill-buffer buf-name)))
    (pop-to-buffer-same-window (get-buffer-create buf-name))
    (unless (derived-mode-p 'ellm-mode)
      (ellm-mode))
    (ellm--ensure-process)
    ;; Reset conversation and busy flag.
    (setq ellm--conversation nil
          ellm--busy nil)
    ;; Only add the system prompt message to the payload.
    (when (and ellm-system-message (not (string-empty-p ellm-system-message)))
      (push `(("role" . "system") ("content" . ,ellm-system-message))
            ellm--conversation))
    (let ((inhibit-read-only t))
      (erase-buffer))
    ;; Display the welcome message in the buffer output, but do not add it to the payload.
    (when ellm-welcome-message
      (comint-output-filter (get-buffer-process (current-buffer))
                            (concat ellm-welcome-message "\n")))
    (ellm--insert-prompt)))

(defun ellm--send (_proc input)
  "Handle user INPUT in the ElLM comint buffer.
_PROC is ignored; input is processed and sent to the LLM API."
  (let ((command (string-trim input))
        (inhibit-read-only t))
    (if (string-empty-p command)
        (progn
          (goto-char (point-max))
          (insert "[No input]\n")
          (ellm--insert-prompt))
      (if ellm--busy
          (progn
            (message "ElLM is still processing. Please wait.")
            (goto-char (point-max))
            (ellm--insert-prompt))
        (cond
         ((string-equal command "clear")
          (setq ellm--conversation nil)
          (when (and ellm-system-message
                     (not (string-empty-p ellm-system-message)))
            (push `(("role" . "system") ("content" . ,ellm-system-message))
                  ellm--conversation))
          (erase-buffer)
          (when ellm-welcome-message
            (comint-output-filter (get-buffer-process (current-buffer))
                                 (concat ellm-welcome-message "\n")))
          (ellm--insert-prompt))
         (t
          (setq ellm--conversation
                (append ellm--conversation
                        (list `(("role" . "user") ("content" . ,command)))))
          (setq ellm--busy t)
          (ellm--api-request (ellm--get-api-key)
                               ellm-model
                               ellm--conversation
                               (current-buffer))))))))

(defun ellm--api-request (api-key model messages orig-buffer)
  "Send an HTTP request to OpenAI with API-KEY, MODEL, and MESSAGES.
ORIG-BUFFER is the ellm chat buffer to receive the response."
  (let* ((payload `(("model" . ,model)
                    ("messages" . ,messages)))
         (json-data (json-encode payload)))
    (let ((url-request-method "POST")
          (url-request-extra-headers
           `(("Content-Type" . "application/json; charset=utf-8")
             ("Authorization" . ,(concat "Bearer " api-key))))
          ;;; Encode JSON payload to a unibyte string using UTF-8.
          (url-request-data (encode-coding-string json-data 'utf-8)))
      (url-retrieve ellm-endpoint
                    #'ellm--handle-response
                    (list orig-buffer)
                    t))))

(defun ellm--handle-response (status orig-buffer)
  "Callback to handle the HTTP response from OpenAI.
STATUS is the response status; ORIG-BUFFER is where to insert the reply."
  (if (plist-get status :error)
      ;; Error case: API request failed
      (progn
        (message "ElLM: API request failed: %s" (plist-get status :error))
        (when (buffer-live-p orig-buffer)
          (with-current-buffer orig-buffer
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert "Error: Failed to retrieve response from the LLM.\n")
              (setq ellm--busy nil)
              (ellm--insert-prompt)))))
    ;; Success case: parse response
    (goto-char (point-min))
    (when (search-forward "\n\n" nil t)  ;; find the JSON body
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (response (if (fboundp 'json-parse-buffer)
                           (json-parse-buffer :object-type 'alist :array-type 'list)
                         (json-read)))
             ;; Extract fields safely
             (choices   (alist-get 'choices response))
             (first     (when (and choices (listp choices)) (car choices)))
             (msg       (when (and first (listp first)) (alist-get 'message first)))
             (content   (when (and msg (listp msg)) (alist-get 'content msg)))
             (answer    (and (stringp content) content)))  ; ensure answer is a string or nil
        (when (buffer-live-p orig-buffer)
          (with-current-buffer orig-buffer
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              ;; Insert the answer or a fallback message
              (insert (if answer answer "[No response]") "\n")
              ;; Update conversation history if answer exists
              (when answer
                (setq ellm--conversation
                      (append ellm--conversation
                              (list `(("role" . "assistant") ("content" . ,answer))))))
              ;; If no answer, we do not append a nil content message.
              (setq ellm--busy nil)
              (ellm--insert-prompt))))))
    ;; Clean up: kill the temporary response buffer
    (kill-buffer (current-buffer))))

;; Define a special mode for code explanation buffers
(define-derived-mode ellm-explanation-mode special-mode "ElLM-Explanation"
  "Major mode for displaying code explanations in ElLM."
  (setq buffer-read-only t)
  (visual-line-mode 1))

;; New functions for code explanation with direct HTTP request

(defun ellm--prepare-explanation-buffer (code-text prompt)
  "Create and prepare a buffer for code explanation.
CODE-TEXT is the code to be explained.
PROMPT is the prompt to be sent to the LLM."
  (let ((buf (get-buffer-create ellm-explain-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (ellm-explanation-mode)

        ;; Insert header
        (insert "# ElLM Code Explanation\n\n")

        ;; Insert user prompt with special face
        (let ((start (point)))
          (insert "## Prompt\n\n" prompt "\n\n")
          (add-face-text-property start (point) ellm-prompt-face))

        ;; Insert original code with special face
        (let ((start (point)))
          (insert "## Original Code\n\n```\n" code-text "\n```\n\n")
          (add-face-text-property start (point) ellm-code-face))

        ;; Insert placeholder for explanation
        (insert "## Explanation\n\n")
        (insert "Fetching explanation...\n")))

    ;; Display the buffer in another window
    (pop-to-buffer buf)
    buf))

(defun ellm--curl-sync-request (api-key model prompt)
  "Make a synchronous request to the OpenAI API using curl.
Returns the response text or nil if the request failed."
  (let* ((messages `((("role" . "system")
                      ("content" . "You are a helpful assistant specializing in explaining code.
                                  Use markdown formatting to structure your response with headings,
                                  code blocks, and bullet points as appropriate."))
                     (("role" . "user") ("content" . ,prompt))))
         (payload `(("model" . ,model)
                    ("messages" . ,messages)))
         (json-data (json-encode payload))
         (temp-file (make-temp-file "ellm-request-"))
         (payload-file (make-temp-file "ellm-payload-"))
         (response nil))

    (unwind-protect
        (progn
          ;; Write the JSON payload to a file to avoid shell escaping issues
          (with-temp-file payload-file
            (insert json-data))

          ;; Execute curl command using the payload file
          (call-process-shell-command
           (format "curl -s -X POST %s -H \"Content-Type: application/json\" -H \"Authorization: Bearer %s\" --data @%s > %s"
                   ellm-endpoint
                   api-key
                   payload-file
                   temp-file)
           nil nil)

          ;; Read response from file
          (when (file-exists-p temp-file)
            (with-temp-buffer
              (insert-file-contents temp-file)
              (goto-char (point-min))

              ;; Check if we have a valid JSON response before parsing
              (condition-case err
                  (let* ((json-object-type 'alist)
                         (json-array-type 'list)
                         (content-empty (= (buffer-size) 0))
                         (json-response (unless content-empty
                                          (if (fboundp 'json-parse-buffer)
                                              (json-parse-buffer :object-type 'alist :array-type 'list)
                                            (json-read))))
                         ;; Extract fields
                         (choices (and json-response (alist-get 'choices json-response)))
                         (first (when (and choices (listp choices)) (car choices)))
                         (msg (when (and first (listp first)) (alist-get 'message first)))
                         (content (when (and msg (listp msg)) (alist-get 'content msg))))

                    (setq response (and (stringp content) content)))
                (error
                 (setq response (format "Error parsing API response: %s\n\nCheck your API key and network connection."
                                        (error-message-string err))))))))

      ;; Cleanup temp files
      (when (file-exists-p temp-file)
        (delete-file temp-file))
      (when (file-exists-p payload-file)
        (delete-file payload-file)))

    ;; Return a default message if response is still nil
    (or response "Failed to get a response from the API. Please check your network connection and API key.")))

;;;###autoload
(defun ellm-explain-code (beg end)
  "Explain the code in the selected region from BEG to END using OpenAI.
Creates a dedicated buffer that remains open with:
1. The prompt with a special face
2. The original code with syntax highlighting
3. The explanation in markdown-view-mode"
  (interactive "r")
  (unless (use-region-p)
    (error "No region selected. Please select a code region to explain."))

  (let* ((code-text (buffer-substring-no-properties beg end))
         (prompt (format ellm-explain-code-prompt code-text))
         (explanation-buffer (ellm--prepare-explanation-buffer code-text prompt)))

    ;; Start a background process to make the API request
    (message "Fetching code explanation... Please wait.")

    ;; Create a background process to handle the API request
    (let ((process-buffer (generate-new-buffer " *ellm-explain-process*"))
          (api-key (ellm--get-api-key))
          (model ellm-model))

      (set-process-sentinel
       (start-process "ellm-explain" process-buffer "sh" "-c" "sleep 1")
       (lambda (proc _event)
         (when (eq (process-status proc) 'exit)
           ;; Process completed, make the API request
           (let ((response (ellm--curl-sync-request api-key model prompt)))
             (when (buffer-live-p explanation-buffer)
               (with-current-buffer explanation-buffer
                 (let ((inhibit-read-only t))
                   ;; Find and replace the placeholder
                   (goto-char (point-min))
                   (when (search-forward "Fetching explanation..." nil t)
                     (beginning-of-line)
                     (delete-region (point) (line-end-position))
                     (insert (if response
                                response
                              "[No explanation provided]")))

                   ;; Convert to markdown-view-mode if available
                   (when (fboundp 'markdown-view-mode)
                     (markdown-view-mode))))

               ;; Make sure the buffer is visible
               (pop-to-buffer explanation-buffer))

             ;; Cleanup
             (when (buffer-live-p process-buffer)
               (kill-buffer process-buffer))
             (message "Code explanation complete."))))))))

(provide 'ellm)
;;; ellm.el ends here
