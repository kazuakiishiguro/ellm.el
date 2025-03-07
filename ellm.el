;;; ellm.el --- Interactive coding assistant with local LLMs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Kazuaki Ishiguro

;; Author: Kazuaki Ishiguro
;; Maintainer: Kazuaki Ishiguro
;; Created: 2025
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, tools, processes
;; URL: https://github.com/kazuakiishiguro/ellm.el

;;; Commentary:
;; ellm.el provides an interactive shell for working with Large Language Models
;; directly from Emacs.  It's designed to be similar to claude-code but for local LLMs
;; like DeepSeek or Qwen models.
;;
;; It uses comint-mode for an interactive experience (prompt, history),
;; and communicates with local LLM servers using Emacs's built-in URL library.
;;
;; Usage:
;;  M-x ellm  -- to start a chat session in an Emacs buffer.
;;  M-x ellm-describe-code -- to explain selected code regions.
;;  M-x ellm-git-commit -- to generate commit messages for code changes.
;;
;;  In the *ElLM* buffer, type your question at the prompt and press Enter.
;;  Type "clear" to reset the conversation.
;;  Type "/help" to see available commands.
;;
;; Customize `ellm-server-type` and `ellm-endpoint` for your local LLM server.
;; API key is optional depending on your local server setup.

;;; Code:

(require 'comint)
(require 'json)
(require 'url)

(defgroup ellm nil
  "Interactive coding assistant with an LLM (Large Language Model) in Emacs."
  :group 'applications)

(defcustom ellm-api-key nil
  "API key used for authentication with the LLM server.
This can be a string containing the API key, or a function that returns
the key string. For most local LLM servers, this is optional."
  :type '(choice (const :tag "Not set" nil) string function)
  :group 'ellm)

(defcustom ellm-server-type "local-llama-cpp"
  "Type of LLM server being used.
Options: local-llama-cpp, local-deepseek, local-ollama, local-llama-server"
  :type 'string
  :group 'ellm)

(defcustom ellm-endpoint-config
  '(("local-ollama" . "http://localhost:11434/api/chat")
    ("local-llama-cpp" . "http://localhost:1234/v1/chat/completions")
    ("local-deepseek" . "http://localhost:8080/v1/chat/completions")
    ("local-llama-server" . "http://localhost:1234/v1/chat/completions"))
  "Mapping of server types to API endpoints for different local LLMs."
  :type '(alist :key-type string :value-type string)
  :group 'ellm)

(defcustom ellm-local-server-config nil
  "Configuration for running a local LLM server.
When set, provides the command-line configuration for starting a local server.
Format is an alist with these keys:
- server-bin: Path to server binary (e.g., './build/bin/llama-server')
- model: Path to model file (e.g., './models/model.gguf')
- args: Additional arguments as a list (e.g., ('--n-gpu-layers' '59'
  '--ctx-size' '2048'))"
  :type '(choice
          (const :tag "Not configured" nil)
          (list :tag "Server configuration"
                (cons :tag "Server binary path"
                      (const server-bin) (string :tag "Path"))
                (cons :tag "Model path"
                      (const model) (string :tag "Path"))
                (cons :tag "Extra arguments"
                      (const args) (repeat :tag "Arguments" string))))
  :group 'ellm)

(defcustom ellm-model "deepseek-coder"
  "Which LLM model to use for chat completions.
Common values:
- For DeepSeek: deepseek-coder, deepseek-chat
- For Qwen: qwen1.5-7b, qwen1.5-14b
- For Ollama: llama3, mistral, codellama"
  :type 'string
  :group 'ellm)

(defcustom ellm-model-parameters
  '(("deepseek-coder" . ((temperature . 0.2) (max_tokens . 4096)))
    ("qwen1.5-7b" . ((temperature . 0.7) (max_tokens . 2048))))
  "Model-specific parameters to use with different LLMs."
  :type '(alist :key-type string :value-type (alist :key-type symbol :value-type sexp))
  :group 'ellm)

(defcustom ellm-system-message 
  "You are a helpful coding assistant. You use markdown liberally to structure
responses with headings, lists, and code blocks. Always show code snippets in
markdown blocks with language labels. When asked to modify files, show exact
changes needed with file paths."
  "Initial system prompt message sent to the LLM at the start of conversation.
If nil or empty, no system message is sent."
  :type 'string
  :group 'ellm)

(defcustom ellm-welcome-message 
  "💬 ElLM coding assistant is ready! Type your prompt and press Enter.
Type '/help' to see available commands. Type 'clear' to reset conversation."
  "Optional welcome text shown in the chat buffer on startup.
Set to nil for no welcome message."
  :type '(choice string (const nil))
  :group 'ellm)

(defcustom ellm-context-dirs nil
  "List of directories to include in context for code-related queries."
  :type '(repeat directory)
  :group 'ellm)

(defun ellm--get-endpoint ()
  "Get the endpoint URL for the configured server type."
  (or (alist-get ellm-server-type ellm-endpoint-config nil nil #'string-equal)
      "http://localhost:1234/v1/chat/completions"))

(defvar-local ellm--conversation nil
  "Conversation history as a list of messages.
Each entry is an alist like ((\"role\" . \"user\") (\"content\" . \"...\")).")

(defvar-local ellm--busy nil
  "Non-nil if a response is currently being fetched (i.e., awaiting API reply).")

(defcustom ellm-special-commands
  '(("help" . ellm--cmd-help)
    ("files" . ellm--cmd-list-files)
    ("search" . ellm--cmd-search-files)
    ("read" . ellm--cmd-read-file)
    ("context" . ellm--cmd-add-context))
  "Mapping of special commands to their handler functions."
  :type '(alist :key-type string :value-type function)
  :group 'ellm)

(defun ellm--cmd-help (_args)
  "Show help message for available commands. _ARGS are ignored."
  (with-current-buffer (current-buffer)
    (goto-char (point-max))
    (insert "
Available commands:
/help - Show this help message
/files [dir] [pattern] - List files in directory
/search [pattern] [file-pattern] - Search for pattern in files
/read [filename] - Read and display file content
/context [dir] - Set context directory for code-related queries
clear - Reset the conversation

Server management (used outside the chat buffer):
M-x ellm-configure-server - Set up local LLM server configuration
M-x ellm-start-server - Start the local LLM server
M-x ellm-stop-server - Stop the local LLM server
M-x ellm-server-status - Check if the local server is running

Example usage:
/files . *.el
/search defun *.el
/read /path/to/file.txt
/context ~/projects/myproject
")
    t))

(defun ellm--cmd-list-files (args)
  "List files matching the pattern in ARGS."
  (let* ((parts (split-string args))
         (dir (or (car parts) default-directory))
         (pattern (or (cadr parts) "*"))
         (files (directory-files-recursively dir pattern)))
    (with-current-buffer (current-buffer)
      (goto-char (point-max))
      (insert "Files matching '" pattern "' in " dir ":\n\n")
      (dolist (file files)
        (insert "  " file "\n")))
    t))

(defun ellm--cmd-search-files (args)
  "Search for pattern in files. ARGS should be `pattern [file-pattern]`."
  (let* ((parts (split-string args))
         (pattern (car parts))
         (file-pattern (or (cadr parts) "*"))
         (default-directory (or (car ellm-context-dirs) default-directory))
         (cmd (format "grep -n '%s' -r --include='%s' '%s' 2>/dev/null" 
                     pattern file-pattern default-directory))
         (output (shell-command-to-string cmd)))
    (with-current-buffer (current-buffer)
      (goto-char (point-max))
      (insert "Search results for '" pattern "' in files matching '" file-pattern "':\n\n")
      (insert output))
    t))

(defun ellm--cmd-read-file (args)
  "Read and display the content of file specified in ARGS."
  (let ((filename (expand-file-name args)))
    (if (file-exists-p filename)
        (with-current-buffer (current-buffer)
          (goto-char (point-max))
          (insert "Contents of " filename ":\n\n")
          (insert-file-contents filename)
          t)
      (with-current-buffer (current-buffer)
        (goto-char (point-max))
        (insert "File not found: " filename "\n")
        t))))

(defun ellm--cmd-add-context (args)
  "Set the context directory to ARGS for code-related queries."
  (let ((dir (expand-file-name args)))
    (if (file-directory-p dir)
        (progn
          (setq ellm-context-dirs (list dir))
          (with-current-buffer (current-buffer)
            (goto-char (point-max))
            (insert "Context directory set to: " dir "\n")
            t))
      (with-current-buffer (current-buffer)
        (goto-char (point-max))
        (insert "Invalid directory: " dir "\n")
        t))))

(defun ellm--parse-command (input)
  "Parse INPUT to check if it's a special command."
  (if (string-match "^/\\([a-z]+\\)\\s-*\\(.*\\)" input)
      (let* ((cmd (match-string 1 input))
             (args (match-string 2 input))
             (handler (alist-get cmd ellm-special-commands nil nil #'string-equal)))
        (if handler
            (cons handler args)  ; Return handler and args
          (cons nil input)))     ; Not a recognized command
    (cons nil input)))           ; Not a command at all

(defun ellm--get-api-key ()
  "Return the API key as a string, or nil if not set or not needed."
  (cond
   ((and ellm-api-key (stringp ellm-api-key))
    ellm-api-key)
   ((and ellm-api-key (functionp ellm-api-key))
    (funcall ellm-api-key))
   (t nil)))

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
  "Launch a ElLM chat session.
With prefix argument RESET, start a fresh session."
  (interactive "P")
  (ellm--prepare-buffer nil reset)
  ;; Display the welcome message in the buffer output, but do not add it to the payload.
  (when ellm-welcome-message
    (comint-output-filter (get-buffer-process (current-buffer))
                          (concat ellm-welcome-message "\n")))
  (ellm--insert-prompt))

(defun ellm--prepare-buffer (input &optional reset)
  "Launch a ElLM chat session.
INPUT is to display prompt.
With prefix argument RESET, start a fresh session.
This is primary used for preset functions."
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
      (erase-buffer)))
  (if input (insert input)))

(defun ellm--send (_proc input)
  "Handle user INPUT in the ElLM comint buffer.
_PROC is ignored; input is processed and sent to the LLM API."
  (let* ((command (string-trim input))
         ;; Remove the "ElLM> " prompt prefix if present
         (clean-command (if (string-prefix-p "ElLM> " command)
                            (substring command 6)  ; Length of "ElLM> "
                          command))
         (cmd-result (ellm--parse-command clean-command))
         (handler (car cmd-result))
         (args (cdr cmd-result))
         (inhibit-read-only t))
    (if (string-empty-p clean-command)
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
         ;; Handle special built-in commands
         ((string-equal clean-command "clear")
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
         
         ;; Handle slash commands
         (handler
          (when (funcall handler args)
            (ellm--insert-prompt)))
         
         ;; Default: send to LLM
         (t
          (setq ellm--conversation
                (append ellm--conversation
                        (list `(("role" . "user") ("content" . ,clean-command)))))
          (setq ellm--busy t)
          (ellm--api-request (ellm--get-api-key)
                             ellm-model
                             ellm--conversation
                             (current-buffer))))))))

(defun ellm--get-payload-for-model (model messages)
  "Create the appropriate payload for MODEL with MESSAGES based on server type."
  (let* ((base-payload `(("model" . ,model)
                         ("messages" . ,messages)))
         (model-params (alist-get model ellm-model-parameters nil nil #'string-equal)))
    ;; Add model-specific parameters if available
    (when model-params
      (dolist (param model-params)
        (setq base-payload (append base-payload (list param)))))
    base-payload))

(defun ellm--api-request (api-key model messages orig-buffer)
  "Send an HTTP request to LLM server with API-KEY, MODEL, and MESSAGES.
ORIG-BUFFER is the ellm chat buffer to receive the response."
  (let* ((endpoint (ellm--get-endpoint))
         (headers '(("Content-Type" . "application/json; charset=utf-8")))
         (payload (ellm--get-payload-for-model model messages))
         (json-data (json-encode payload)))
    ;; Add Authorization header if API key is provided
    (when api-key
      (push (cons "Authorization" (concat "Bearer " api-key)) headers))
    (setq url-request-method "POST"
          url-request-extra-headers headers
          url-request-data (encode-coding-string json-data 'utf-8))
    (url-retrieve endpoint
                  #'ellm--handle-response
                  (list orig-buffer)
                  t)))

(defun ellm--extract-answer (response)
  "Extract answer content from RESPONSE based on server type."
  (pcase ellm-server-type
    ("local-ollama" 
     (let ((message (alist-get 'message response)))
       (when message
         (alist-get 'content message))))
    ("local-deepseek"
     (let* ((choices (alist-get 'choices response))
            (first (when choices (car choices)))
            (message (when first (alist-get 'message first))))
       (when message
         (alist-get 'content message))))
    (_ 
     ;; Default OpenAI-compatible format (works with llama.cpp)
     (let* ((choices (alist-get 'choices response))
            (first (when choices (car choices)))
            (message (when first (alist-get 'message first))))
       (when message
         (alist-get 'content message))))))

(defun ellm--check-tool-calls (response _orig-buffer)
  "Check if RESPONSE contains tool calls and handle them in _ORIG-BUFFER."
  (let* ((choices (alist-get 'choices response))
         (first (when choices (car choices)))
         (message (when first (alist-get 'message first))))
    ;; Just check if tool calls exist for now, will be implemented later
    (and message (alist-get 'tool_calls message nil))))

(defun ellm--handle-response (status orig-buffer)
  "Callback to handle the HTTP response from the LLM server.
STATUS is the response status; ORIG-BUFFER is where to insert the reply."
  (if (plist-get status :error)
      ;; Error case: API request failed
      (progn
        (message "ElLM: API request failed: %s" (plist-get status :error))
        (when (buffer-live-p orig-buffer)
          (with-current-buffer orig-buffer
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert (format "Error: Failed to retrieve response from the LLM server at %s.\n"
                             (ellm--get-endpoint)))
              (setq ellm--busy nil)
              (ellm--insert-prompt)))))
    ;; Success case: parse response
    (goto-char (point-min))
    (when (search-forward "\n\n" nil t)  ;; find the JSON body
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (response (if (fboundp 'json-parse-buffer)
                           (json-parse-buffer :object-type 'alist :array-type 'list)
                         (json-read))))
        ;; Check for tool calls first
        (ellm--check-tool-calls response orig-buffer)
        
        ;; Extract the answer text
        (let ((answer (ellm--extract-answer response)))
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
                ;; Reset busy state and insert prompt
                (setq ellm--busy nil)
                (ellm--insert-prompt)))))))
    ;; Clean up: kill the temporary response buffer
    (kill-buffer (current-buffer))))

;;; Tool functions for code-related tasks

(defun ellm-add-file-to-context (file)
  "Add FILE contents to conversation context."
  (interactive "fAdd file to context: ")
  (when (file-exists-p file)
    (let ((content (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string))))
      (with-current-buffer (get-buffer "*ElLM*")
        (goto-char (point-max))
        (insert (format "Added '%s' to context\n" file))
        (ellm--insert-prompt))
      (setq ellm--conversation
            (append ellm--conversation
                    (list `(("role" . "user") 
                            ("content" . ,(format "Here's the content of %s for context:\n\n```\n%s\n```" 
                                                 file content))))))
      (message "Added %s to conversation context" file))))

(defun ellm-set-context-dir (dir)
  "Set current context directory to DIR for code-related queries."
  (interactive "DSet context directory: ")
  (setq ellm-context-dirs (list dir))
  (with-current-buffer (get-buffer-create "*ElLM*")
    (goto-char (point-max))
    (insert (format "Context directory set to: %s\n" dir))
    (ellm--insert-prompt))
  (message "Context directory set to %s" dir))

;;; Local server management

(defvar ellm--local-server-process nil
  "Process object for the local LLM server if started by Emacs.")

(defun ellm--build-server-command ()
  "Build command list for starting the local LLM server."
  (unless ellm-local-server-config
    (user-error "Local server configuration not set. Customize `ellm-local-server-config`"))
  
  (let ((server-bin (alist-get 'server-bin ellm-local-server-config))
        (model-path (alist-get 'model ellm-local-server-config))
        (args (alist-get 'args ellm-local-server-config)))
    
    (unless (and server-bin model-path)
      (user-error "Server binary and model path must be configured"))
    
    (append (list server-bin "--model" model-path) args)))

;;;###autoload
(defun ellm-start-server ()
  "Start a local LLM server using the configured settings."
  (interactive)
  (if ellm--local-server-process
      (message "Local LLM server is already running")
    (if ellm-local-server-config
        (let ((cmd (ellm--build-server-command))
              (buf (generate-new-buffer "*ellm-server*")))
          (message "Starting local LLM server with: %s" 
                   (mapconcat #'identity cmd " "))
          (setq ellm--local-server-process 
                (make-process
                 :name "ellm-llm-server"
                 :buffer buf
                 :command cmd
                 :sentinel #'ellm--server-sentinel))
          (message "Local LLM server started. Server logs in buffer: %s" 
                   (buffer-name buf)))
      (user-error "Local server configuration not set. Customize `ellm-local-server-config`"))))

(defun ellm--server-sentinel (process event)
  "Handle PROCESS EVENT for the local LLM server."
  (when (memq (process-status process) '(exit signal))
    (message "Local LLM server stopped: %s" (string-trim event))
    (setq ellm--local-server-process nil)))

;;;###autoload
(defun ellm-stop-server ()
  "Stop the running local LLM server."
  (interactive)
  (if ellm--local-server-process
      (progn
        (message "Stopping local LLM server...")
        (interrupt-process ellm--local-server-process)
        (delete-process ellm--local-server-process)
        (setq ellm--local-server-process nil)
        (message "Local LLM server stopped"))
    (message "No local LLM server running")))

;;;###autoload
(defun ellm-server-status ()
  "Show status of the local LLM server."
  (interactive)
  (if ellm--local-server-process
      (if (process-live-p ellm--local-server-process)
          (message "Local LLM server is running (PID: %d)"
                   (process-id ellm--local-server-process))
        (message "Local LLM server process exists but is not running"))
    (message "No local LLM server running")))

;;;###autoload
(defun ellm-configure-server ()
  "Configure the local LLM server interactively."
  (interactive)
  (let* ((server-bin (read-string "Server binary path: " 
                                 (or (alist-get 'server-bin ellm-local-server-config) 
                                     "./build/bin/llama-server")))
         (model-path (read-file-name "Model file path: " nil
                                    (alist-get 'model ellm-local-server-config)
                                    t nil))
         (args-str (read-string "Additional arguments (space separated): " 
                               (mapconcat #'identity 
                                         (or (alist-get 'args ellm-local-server-config) 
                                             '("--n-gpu-layers" "59" "--ctx-size" "2048" "--port" "1234"))
                                         " ")))
         (args (split-string args-str " " t)))
    
    (setq ellm-local-server-config
          `((server-bin . ,server-bin)
            (model . ,model-path)
            (args . ,args)))
    
    (customize-save-variable 'ellm-local-server-config ellm-local-server-config)
    (message "Server configuration saved. You can now use `ellm-start-server`")))

;;; Additional preset commands:
(defcustom ellm-describe-code-prompt "Describe the following code:\n%s"
  "Template prompt for describing code.
%s will be replaced by the selected code region."
  :type 'string
  :group 'ellm)

(defcustom ellm-git-commit-prompt "Generate a concise and descriptive git commit message for the following code changes:\n%s"
  "Template prompt for generating git commit messages.
%s will be replaced by the selected code region."
  :type 'string
  :group 'ellm)

;;;###autoload
(defun ellm-describe-code ()
  "Explain the code in the selected region using the LLM."
  (interactive)
  (unless (use-region-p)
    (user-error "No region selected.  Please select a code region to explain.  "))
  (let* ((region-text (buffer-substring-no-properties (region-beginning) (region-end)))
         (prompt (format ellm-describe-code-prompt region-text)))
    (ellm--prepare-buffer region-text)
    (ellm--send nil prompt)))

;;;###autoload
(defun ellm-git-commit ()
  "Generate a git commit message for the selected code changes using the LLM."
  (interactive)
  (unless (use-region-p)
    (user-error "No region selected.  Please select code changes to generate a commit message"))
  (let* ((region-text (buffer-substring-no-properties (region-beginning) (region-end)))
         (prompt (format ellm-git-commit-prompt region-text)))
    (ellm--prepare-buffer region-text)
    (ellm--send nil prompt)))

(provide 'ellm)
;;; ellm.el ends here
