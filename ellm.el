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
;;
;; Customize `ellm-api-key` with your OpenAI API key (required).

;;; Code:

(require 'comint)
(require 'json)

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
  (visual-line-mode 1))

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
	    (comint-output-fiter (get-buffer-process (current-buffer))
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
      (progn
        (message "ElLM: API request failed: %s" (plist-get status :error))
        (when (buffer-live-p orig-buffer)
          (with-current-buffer orig-buffer
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert "Error: Failed to retrieve response from the LLM.\n")
              (setq ellm--busy nil)
              (ellm--insert-prompt)))))
    (goto-char (point-min))
    (when (search-forward "\n\n" nil t)
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (response (if (fboundp 'json-parse-buffer)
                           (json-parse-buffer :object-type 'alist :array-type 'list)
                         (json-read)))
	     (choices (alist-get 'choices response))
             (first-choice (car choices))
             (msg (alist-get 'message first-choice))
             (answer (alist-get 'content msg))))
        (when (buffer-live-p orig-buffer)
          (with-current-buffer orig-buffer
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert (or answer "[No response]") "\n")
              (setq ellm--conversation
                    (append ellm--conversation
                            (list `(("role" . "assistant") ("content" . ,answer)))))
              (setq ellm--busy nil)
              (ellm--insert-prompt)))))))
  (kill-buffer (current-buffer)))

(provide 'ellm)
;;; ellm.el ends here
