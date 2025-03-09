;;; ellm.el --- Interactive coding assistant with local LLMs  -*- lexical-binding: t; -*-

;;; Commentary:

;; An Emacs package to interact with a local Ollama LLM server.
;; Heavily inspired by chatgpt-shell (https://github.com/xenodium/chatgpt-shell).

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'dired)
(require 'esh-mode)
(require 'em-prompt)
(require 'eshell)
(require 'find-func)
(require 'flymake)
(require 'ielm)
(require 'shell-maker)
(require 'smerge-mode)
(require 'ob-core)
(require 'color)

(require 'ellm-ollama)
(require 'ellm-prompt-compose)

(defcustom ellm-request-timeout 600
  "How long to wait for a request to time out in seconds."
  :type 'integer
  :group 'ellm)

(defcustom ellm-default-prompts
  '("Write a unit test for the following code:"
    "Refactor the following code so that "
    "Summarize the output of the following command:"
    "What's wrong with this command?"
    "Explain what the following code does:")
  "List of default prompts to choose from."
  :type '(repeat string)
  :group 'ellm)

(defcustom ellm-prompt-header-describe-code
  "What does the following code do?"
  "Prompt header of `describe-code`."
  :type 'string
  :group 'ellm)

(defcustom ellm-prompt-header-write-git-commit
  "Please help me write a git commit message for the following commit:"
  "Prompt header of `git-commit`."
  :type 'string
  :group 'ellm)

(defcustom ellm-prompt-header-refactor-code
  "Please help me refactor the following code.
   Please reply with the refactoring explanation in English, refactored code, and diff between two versions.
   Please ignore the comments and strings in the code during the refactoring.
   If the code remains unchanged after refactoring, please say 'No need to refactor'."
  "Prompt header of `refactor-code`."
  :type 'string
  :group 'ellm)

(defcustom ellm-prompt-header-generate-unit-test
  "Please help me generate unit-test following function:"
  "Prompt header of `generate-unit-test`."
  :type 'string
  :group 'ellm)

(defcustom ellm-prompt-header-proofread-region
  "Please help me proofread the following English text and only reply with fixed text.
Output just the proofread text without any intro, comments, or explanations.
If the original text was indented on the left, preserve the same amount of spacing in your response:

"
  "Prompt header used by `ellm-proofread-region`."
  :type 'string
  :group 'ellm)

(defcustom ellm-prompt-header-whats-wrong-with-last-command
  "What's wrong with this command execution?"
  "Prompt header of `whats-wrong-with-last-command`."
  :type 'string
  :group 'ellm)

(defcustom ellm-prompt-header-eshell-summarize-last-command-output
  "Summarize the output of the following command:"
  "Prompt header of `eshell-summarize-last-command-output`."
  :type 'string
  :group 'ellm)

(defcustom ellm-prompt-query-response-style 'other-buffer
  "Determines the prompt style when invoking from other buffers.

`'inline' inserts responses into current buffer.
`'other-buffer' inserts responses into a transient buffer.
`'shell' inserts responses and focuses the shell

Note: in all cases responses are written to the shell to keep context."
  :type '(choice (const :tag "Inline" inline)
                 (const :tag "Other Buffer" other-buffer)
                 (const :tag "Shell" shell))
  :group 'chatgpt)

(defcustom ellm-after-command-functions nil
  "Abnormal hook (i.e. with parameters) invoked after each command.

This is useful if you'd like to automatically handle or suggest things
post execution.

For example:

\(add-hook `ellm-after-command-functions'
   (lambda (command output success)
     (message \"Command: %s\" command)
     (message \"Output: %s\" output)))"
  :type 'hook
  :group 'shell-maker)

(defvaralias 'ellm-swap-model-version 'ellm-swap-model)

(defvaralias 'ellm-display-function 'shell-maker-display-function)

(defvaralias 'ellm-read-string-function 'shell-maker-read-string-function)

(defvaralias 'ellm-logging 'shell-maker-logging)

(defvaralias 'ellm-root-path 'shell-maker-root-path)

(defalias 'ellm-save-session-transcript #'shell-maker-save-session-transcript)

(defvar ellm--prompt-history nil)

(defcustom ellm-language-mapping '(("elisp" . "emacs-lisp")
                                            ("objective-c" . "objc")
                                            ("objectivec" . "objc")
                                            ("cpp" . "c++"))
  "Maps external language names to Emacs names.

Use only lower-case names.

For example:

                  lowercase      Emacs mode (without -mode)
Objective-C -> (\"objective-c\" . \"objc\")"
  :type '(alist :key-type (string :tag "Language Name/Alias")
                :value-type (string :tag "Mode Name (without -mode)"))
  :group 'ellm)

(defcustom ellm-babel-headers '(("dot" . ((:file . "<temp-file>.png")))
                                         ("plantuml" . ((:file . "<temp-file>.png")))
                                         ("ditaa" . ((:file . "<temp-file>.png")))
                                         ("objc" . ((:results . "output")))
                                         ("python" . ((:python . "python3")))
                                         ("c++" . ((:results . "raw")))
                                         ("c" . ((:results . "raw"))))
  "Additional headers to make babel blocks work.

Entries are of the form (language . headers).  Headers should
conform to the types of `org-babel-default-header-args', which
see.

Please submit contributions so more things work out of the box."
  :type '(alist :key-type (string :tag "Language")
                :value-type (alist :key-type (restricted-sexp :match-alternatives (keywordp) :tag "Argument Name")
                                   :value-type (string :tag "Value")))
  :group 'ellm)

(defcustom ellm-source-block-actions
  nil
  "Block actions for known languages.

Can be used compile or run source block at point."
  :type '(alist :key-type (string :tag "Language")
                :value-type (list (cons (const primary-action-confirmation) (string :tag "Confirmation Prompt:"))
                                  (cons (const primary-action) (function :tag "Action:"))))
  :group 'ellm)

(defun ellm--make-default-models ()
  "Create a list of default models by combining models from different providers.
This function aggregates models from OpenAI, Anthropic, Google, and Ollama.
It returns a list containing all available models from these providers."
  (append (ellm-ollama-models)))

(defcustom ellm-models
  (ellm--make-default-models)
  "The list of supported models to swap from.
See `ellm-ollama-models' for details."
  :type '(repeat (alist :key-type symbol :value-type sexp))
  :group 'ellm)

(defcustom ellm-model-version nil
  "The active model version as either a string.
See `ellm-models' for available model versions.
Swap using `ellm-swap-model'."
  :type '(choice (string :tag "String")
                 (integer :tag "Integer")
                 (const :tag "Nil" nil))
  :group 'ellm)

(defcustom ellm-model-temperature nil
  "What sampling temperature to use, between 0 and 2, or nil.
Higher values like 0.8 will make the output more random, while
lower values like 0.2 will make it more focused and
deterministic.  Value of nil will not pass this configuration to
the model.

See
https://platform.openai.com/docs/api-reference/completions\
/create#completions/create-temperature
for details."
  :type '(choice (float :tag "Float")
                 (const :tag "Nil" nil))
  :group 'ellm)

(defun ellm--append-system-info (text)
  "Append system info to TEXT."
  (cl-labels ((ellm--get-system-info-command
               ()
               (cond ((eq system-type 'darwin) "sw_vers")
                     ((or (eq system-type 'gnu/linux)
                          (eq system-type 'gnu/kfreebsd)) "uname -a")
                     ((eq system-type 'windows-nt) "ver")
                     (t (format "%s" system-type)))))
    (let ((system-info (string-trim
                        (shell-command-to-string
                         (ellm--get-system-info-command)))))
      (concat text
              "\n# System info\n"
              "\n## OS details\n"
              system-info
              "\n## Editor\n"
              (emacs-version)))))

(defcustom ellm-system-prompts
  `(("tl;dr" . "Be as succint but informative as possible and respond in tl;dr form to my queries")
    ("General" . "You use markdown liberally to structure responses. Always show code snippets in markdown blocks with language labels.")
    ;; Based on https://github.com/benjamin-asdf/dotfiles/blob/8fd18ff6bd2a1ed2379e53e26282f01dcc397e44/mememacs/.emacs-mememacs.d/init.el#L768
    ("Programming" . ,(ellm--append-system-info
                       "The user is a programmer with very limited time.
                        You treat their time as precious. You do not repeat obvious things, including their query.
                        You are as concise as possible in responses.
                        You never apologize for confusions because it would waste their time.
                        You use markdown liberally to structure responses.
                        Always show code snippets in markdown blocks with language labels.
                        Don't explain code snippets.
                        Whenever you output updated code for the user, only show diffs, instead of entire snippets."))
    ("Mathematics" . "The user is a mathematician with very limited time.
                      You treat their time as precious. You do not repeat obvious things, including their query.
                      You are as concise as possible in responses.
                      You never apologize for confusions because it would waste their time.
                      All mathematical outputs must be in proper LaTeX format.
                      Use \\( and \\( for in-line equations, and \\[ and \\] for equation environments.
                      All mathematical delimiters must be on their own line.
                      All mathematical outputs are formally expressed in formal mathematical notation.
                      Don't give approximations or pronunciations for constants or variables.
                      You give formal definitions for all variables.
                      You keep your answers succinct and to-the-point.")
    ("Positive Programming" . ,(ellm--append-system-info
                                "Your goal is to help the user become an amazing computer programmer.
                                 You are positive and encouraging.
                                 You love see them learn.
                                 You do not repeat obvious things, including their query.
                                 You are as concise in responses. You always guide the user go one level deeper and help them see patterns.
                                 You never apologize for confusions because it would waste their time.
                                 You use markdown liberally to structure responses. Always show code snippets in markdown blocks with language labels.
                                 Don't explain code snippets. Whenever you output updated code for the user, only show diffs, instead of entire snippets.")))

  "List of system prompts to choose from.

If prompt is a cons, its car will be used as a title to display.

For example:

\(\"Translating\" . \"You are a helpful English to Spanish assistant.\")\"
\(\"Programming\" . \"The user is a programmer with very limited time...\")"
  :type '(alist :key-type (string :tag "Title")
                :value-type (string :tag "Prompt value"))
  :group 'ellm)

(defcustom ellm-system-prompt 1 ;; Concise
  "The system prompt `ellm-system-prompts' index.

Or nil if none."
  :type '(choice (string :tag "String")
                 (integer :tag "Integer")
                 (const :tag "No Prompt" nil))
  :group 'ellm)

(defun ellm-model-version ()
  "Return active model version."
  (when (boundp 'ellm-model-versions)
    (error (concat "\"ellm-model-versions\" no longer supported. "
                   "Please unset or migrate to \"ellm-models\".")))
  (cond ((not ellm-model-version)
         ;; No default model set, find one that's cleared for sending commands.
         (if-let* ((cleared-model (seq-find (lambda (model)
                                              (cond ((not (map-elt model :validate-command))
                                                     t)
                                                    ((and (map-elt model :validate-command)
                                                          (not (funcall (map-elt model :validate-command)
                                                                        "hello" model nil)))
                                                     t)
                                                    (t
                                                     nil)))
                                            ellm-models))
                   (model-version (map-elt cleared-model :version)))
             model-version
           (if (map-elt (seq-first ellm-models) :version)
               (map-elt (seq-first ellm-models) :version)
             (error "Could not find a model.  Missing model setup?"))))
        ((stringp ellm-model-version)
         ellm-model-version)
        ((integerp ellm-model-version)
         (let ((model (nth ellm-model-version
                           ellm-models)))
           (cond ((stringp model)
                  model)
                 ((stringp (map-elt model :version))
                  (map-elt model :version))
                 (t
                  (error "Don't know how to resolve model to version %s"
                         ellm-model-version)))))
        (t
         (error "Could not find a model.  Missing model setup?"))))

(defun ellm-system-prompt ()
  "Return active system prompt."
  (cond ((stringp ellm-system-prompt)
         ellm-system-prompt)
        ((integerp ellm-system-prompt)
         (let ((prompt (nth ellm-system-prompt
                            ellm-system-prompts)))
           (if (consp prompt)
               (cdr prompt)
             prompt)))
        (t
         nil)))

(defun ellm-duplicate-map-keys (map)
  "Return duplicate keys in MAP."
  (let ((keys (map-keys map))
        (seen '())
        (duplicates '()))
    (dolist (key keys)
      (if (member key seen)
          (push key duplicates)
        (push key seen)))
    duplicates))

(defun ellm-validate-no-system-prompt (command model settings)
  "Perform validation for COMMAND with MODEL and SETTINGS.
Then enforce that there is no system prompt.  This is useful for models like
OpenAI's o1 that do not allow one."
    (or (ellm-openai--validate-command command model settings)
        (when (map-elt settings :system-prompt)
          (format "Model \"%s\" does not support system prompts. Please unset via \"M-x ellm-swap-system-prompt\" by selecting None."
                  (map-elt model :version)))))

(defun ellm-reload-default-models ()
  "Reload all available models."
  (interactive)
  (setq ellm-models (ellm--make-default-models))
  (message "Reloaded %d models" (length ellm-models)))

(defcustom ellm-model-filter nil
  "Filter models to swap from using this function as a filter.

See `ellm-allow-model-versions' and
`ellm-ignore-model-versions' as examples."
  :type 'function
  :group 'ellm)

(defun ellm-allow-model-versions (versions)
  "Return a filter function to keep known model VERSIONS only.

Use with `ellm-model-filter'."
  (lambda (models)
    (seq-filter (lambda (model)
                  (member (map-elt model :version) versions))
                models)))

(defun ellm-ignore-model-versions (versions)
  "Return a filter function to drop model VERSIONS.

Use with `ellm-model-filter'."
  (lambda (models)
    (seq-filter (lambda (model)
                  (not (member (map-elt model :version) versions)))
                models)))

(defun ellm-swap-model ()
  "Swap model version from `ellm-models'."
  (interactive)
  (if-let* ((last-label (ellm--model-label))
            (width (let ((width))
                     (mapc (lambda (model)
                             (unless width
                               (setq width 0))
                             (when-let ((provider (map-elt model :provider))
                                        (provider-width (length (map-elt model :provider)))
                                        (longer (> provider-width width)))
                               (setq width provider-width)))
                           ellm-models)
                     width))
            (models (seq-map (lambda (model)
                               (format (format "%%-%ds   %%s" width)
                                       (map-elt model :provider)
                                       (map-elt model :version)))
                             (if ellm-model-filter
                                 (funcall ellm-model-filter ellm-models)
                               ellm-models)))
            (selection (nth 1 (split-string (completing-read "Model version: "
                                                             models nil t)))))
      (progn
        (when (derived-mode-p 'ellm-mode)
          (setq-local ellm-model-version selection)
          (ellm--update-prompt t)
          (ellm-interrupt nil)
          (unless (equal last-label (ellm--model-label))
            (ellm-clear-buffer)))
        (setq-default ellm-model-version selection))
    (error "No other providers found")))

(defcustom ellm-streaming t
  "Whether or not to stream ElLM responses (show chunks as they arrive)."
  :type 'boolean
  :group 'ellm)

(defcustom ellm-proxy nil
  "When non-nil, use as a proxy (for example http or socks5)."
  :type 'string
  :group 'ellm)

(defun ellm--model-settings ()
  "Variable model settings.

See `ellm-streaming'
    `ellm-model-temperature'
    variable `ellm-system-prompt'."
  (list (cons :streaming ellm-streaming)
        (cons :temperature ellm-model-temperature)
        (cons :system-prompt (ellm-system-prompt))))

(defcustom ellm-highlight-blocks t
  "Whether or not to highlight source blocks."
  :type 'boolean
  :group 'ellm)

(defcustom ellm-render-latex nil
  "Whether or not to render LaTeX blocks (experimental).

Experimental.  Please report issues."
  :type 'boolean
  :group 'ellm)

(defcustom ellm-insert-dividers nil
  "Whether or not to display a divider between requests and responses."
  :type 'boolean
  :group 'ellm)

(defcustom ellm-transmitted-context-length
  #'ellm--approximate-context-length
  "Controls the amount of context provided to chatGPT.

This context needs to be transmitted to the API on every request.
ChatGPT reads the provided context on every request, which will
consume more and more prompt tokens as your conversation grows.
Models do have a maximum token limit, however.

A value of nil will send full chat history (the full contents of
the comint buffer), to ChatGPT.

A value of 0 will not provide any context.  This is the cheapest
option, but ChatGPT can't look back on your conversation.

A value of 1 will send only the latest prompt-completion pair as
context.

A Value > 1 will send that amount of prompt-completion pairs to
ChatGPT.

A function `(lambda (tokens-per-message tokens-per-name messages))'
returning length.  Can use custom logic to enable a shifting context
window."
  :type '(choice (integer :tag "Integer")
                 (const :tag "Not set" nil)
                 (function :tag "Function"))
  :group 'ellm)

(defcustom ellm-welcome-function #'shell-maker-welcome-message
  "Function returning welcome message or nil for no message.

See `shell-maker-welcome-message' as an example."
  :type 'function
  :group 'ellm)

(defvar ellm--config
  (make-shell-maker-config
   :name "ElLM"
   :validate-command
   (lambda (command)
     (when-let* ((model (ellm--resolved-model))
                 (settings (ellm--model-settings))
                 (validate-command (map-elt model :validate-command)))
       (funcall validate-command command model settings)))
   :execute-command
   (lambda (command shell)
     (if-let* ((model (ellm--resolved-model))
               (handler (map-elt model :handler)))
         (funcall handler
                  :model model
                  :command command
                  :context (ellm-crop-context
                            :model model
                            :command command
                            :context (map-elt shell :history))
                  :shell shell
                  :settings (ellm--model-settings))
       (error "%s not found" (ellm-model-version))))
   :on-command-finished
   (lambda (command output success)
     (ellm--put-source-block-overlays)
     (run-hook-with-args 'ellm-after-command-functions
                         command output success))
   :redact-log-output
   (lambda (output)
     (if-let ((key (map-elt (ellm--resolved-model) :key)))
         (replace-regexp-in-string (regexp-quote (funcall key))
                                   "SK-REDACTED-PROVIDER-KEY"
                                   output)
       output))))

(defalias 'ellm-explain-code #'ellm-describe-code)

;; Aliasing enables editing as text in babel.
(defalias 'ellm-mode #'text-mode)

;; Define the ellm major mode
(shell-maker-define-major-mode ellm--config)

;; Implementation generated by shell-maker
(declare-function ellm-clear-buffer "chatgpt-shell")

(cl-defun ellm--resolved-model (&key versioned)
  "Resolve model VERSIONED name."
  (seq-find (lambda (model)
              (equal (map-elt model :version)
                     (or versioned (ellm-model-version))))
            ellm-models))

(cl-defun ellm--make-payload (&key version context streaming temperature system-prompt)
  "Create a payload for model with VERSION.
Set CONTEXT, STREAMING, TEMPERATURE, and SYSTEM-PROMPT as usual."
  (let* ((model (ellm--resolved-model :versioned version))
         (settings (list (cons :streaming streaming)
                         (cons :temperature temperature)
                         (cons :system-prompt system-prompt)))
         (payload (or (map-elt model :payload)
                      (error "Model :payload not found"))))
    (funcall payload
             :model model
             :context context
             :settings settings)))

;;;###autoload
(defun ellm (&optional new-session)
  "Start a ElLM shell interactive command.
With NEW-SESSION, start a new session."
  (interactive "P")
  (let ((buffer 
         ;; Check for existing ellm buffers to avoid duplicates
         (if (and (not new-session) (ellm--shell-buffers))
             ;; Use existing buffer
             (car (ellm--shell-buffers))
           ;; Create new session if explicitly requested or no buffers exist
           (ellm-start nil new-session))))
    ;; Always ensure we switch to the buffer and return it
    (switch-to-buffer buffer)
    buffer))

(defvar ellm-mode-map (make-sparse-keymap)
  "Keymap for `ellm-mode'.")

(defun ellm-start (&optional no-focus new-session ignore-as-primary model-version system-prompt)
  "Start a ElLM shell programmatically.
Set NO-FOCUS to start in background.
Set NEW-SESSION to start a separate new session.
Set IGNORE-AS-PRIMARY to avoid making new buffer the primary one.
Set MODEL-VERSION to override variable `ellm-model-version'.
Set SYSTEM-PROMPT to override variable `ellm-system-prompt'"
  (let* ((existing-primary (ellm--primary-buffer))
         (ellm--config
          (let ((config (copy-sequence ellm--config))
                (ellm-model-version (or model-version ellm-model-version))
                (ellm-system-prompt (or system-prompt ellm-system-prompt)))
            (setf (shell-maker-config-prompt config)
                  (car (ellm--prompt-pair)))
            (setf (shell-maker-config-prompt-regexp config)
                  (cdr (ellm--prompt-pair)))
            config))
         ;; Determine the buffer name
         (buffer-name (cond
                       ;; If explicitly requesting new session, generate a new name
                       (new-session
                        (ellm--make-buffer-name))
                       ;; If we have a primary buffer and not ignoring it, use its name
                       ((and existing-primary (not ignore-as-primary))
                        (buffer-name existing-primary))
                       ;; Otherwise generate a new name
                       (t
                        (ellm--make-buffer-name))))
         ;; First check if buffer already exists with this name
         (existing-buffer (get-buffer buffer-name))
         ;; Reuse existing buffer unless explicitly requesting new session
         (shell-buffer
          (if (and existing-buffer (not new-session))
              (progn
                ;; Switch to or display the buffer based on no-focus
                (if no-focus
                    (display-buffer existing-buffer)
                  (switch-to-buffer existing-buffer))
                existing-buffer)
            ;; Create a new buffer only if explicitly requested or needed
            (let ((new-buffer (shell-maker-start ellm--config
                                                no-focus
                                                ellm-welcome-function
                                                new-session
                                                buffer-name
                                                "LLM")))
              ;; Switch to the new buffer if we're not in no-focus mode
              (unless no-focus
                (switch-to-buffer new-buffer))
              new-buffer))))
    (when (and (not ignore-as-primary)
               (not (ellm--primary-buffer)))
      (ellm--set-primary-buffer shell-buffer))
    (unless model-version
      (setq model-version ellm-model-version))
    (unless system-prompt
      (setq system-prompt ellm-system-prompt))
    (with-current-buffer shell-buffer
      ;; Make sure we're in ellm-mode before continuing
      (when (eq major-mode 'ellm-mode)
        (setq-local ellm-model-version model-version)
        (setq-local ellm-system-prompt system-prompt)
        (ellm--update-prompt t)
        (ellm--add-menus)))
    ;; Configure keymaps only once
    (unless (commandp (lookup-key ellm-mode-map (kbd "C-M-h")))
      (define-key ellm-mode-map (kbd "C-M-h")
        #'ellm-mark-at-point-dwim)
      (define-key ellm-mode-map (kbd "C-c C-c")
        #'ellm-ctrl-c-ctrl-c)
      (define-key ellm-mode-map (kbd "C-c C-v")
        #'ellm-swap-model)
      (define-key ellm-mode-map (kbd "C-c C-s")
        #'ellm-swap-system-prompt)
      (define-key ellm-mode-map (kbd "C-c C-p")
        #'ellm-previous-item)
      (define-key ellm-mode-map (kbd "<backtab>")
        #'ellm-previous-item)
      (define-key ellm-mode-map (kbd "C-c C-n")
        #'ellm-next-item)
      (define-key ellm-mode-map (kbd "<tab>")
        #'ellm-next-item)
      (define-key ellm-mode-map (kbd "C-c C-e")
        #'ellm-prompt-compose))
    shell-buffer))

(defun ellm--shrink-system-prompt (prompt)
  "Shrink PROMPT."
  (if (consp prompt)
      (ellm--shrink-system-prompt (car prompt))
    (if (> (length (string-trim prompt)) 15)
        (format "%s..."
                (substring (string-trim prompt) 0 12))
      (string-trim prompt))))

(defun ellm--shell-info ()
  "Generate shell info for display."
  (concat
   (ellm--model-short-version)
   (cond ((and (integerp ellm-system-prompt)
               (nth ellm-system-prompt
                    ellm-system-prompts))
          (concat "/" (ellm--shrink-system-prompt (nth ellm-system-prompt
                                                                ellm-system-prompts))))
         ((stringp ellm-system-prompt)
          (concat "/" (ellm--shrink-system-prompt ellm-system-prompt)))
         (t
          ""))))

(defun ellm--model-label ()
  "Return the current model label."
  (or (map-elt (ellm--resolved-model) :label)
      "Unknown"))

(defun ellm--model-short-version ()
  "Return the current model short version."
  (or (map-elt (ellm--resolved-model) :short-version)
      (map-elt (ellm--resolved-model) :version)
      "unknown"))

(defun ellm--prompt-pair ()
  "Return a pair with prompt and prompt-regexp."
  (let* ((label (ellm--model-label)))
    (cons
     (format "%s(%s)> " label (ellm--shell-info))
     (ellm--prompt-regexp))))

(defun ellm--prompt-regexp ()
  "Return a regexp to match any model prompt."
  (rx bol
      (one-or-more alphanumeric)
      "("
      (minimal-match (one-or-more not-newline))
      ")> "))

(defun ellm--shell-buffers ()
  "Return a list of all shell buffers."
  (seq-filter
   (lambda (buffer)
     (eq (buffer-local-value 'major-mode buffer)
         'ellm-mode))
   (buffer-list)))

(defun ellm-set-as-primary-shell ()
  "Set as primary shell when there are multiple sessions."
  (interactive)
  (unless (derived-mode-p 'ellm-mode)
    (user-error "Not in a shell"))
  (ellm--set-primary-buffer (current-buffer)))

(defvar-local ellm--is-primary-p nil
  "Non-nil if shell buffer is considered primary.")

(defun ellm--set-primary-buffer (primary-shell-buffer)
  "Set PRIMARY-SHELL-BUFFER as primary buffer."
  (unless primary-shell-buffer
    (error "No primary shell available"))
  (mapc (lambda (shell-buffer)
          (with-current-buffer shell-buffer
            (setq ellm--is-primary-p nil)))
        (ellm--shell-buffers))
  (with-current-buffer primary-shell-buffer
    (setq ellm--is-primary-p t)))

(defun ellm--primary-buffer ()
  "Return the primary shell buffer.

This is used for sending a prompt to in the background."
  (let* ((shell-buffers (ellm--shell-buffers))
         (primary-shell-buffer (seq-find
                                (lambda (shell-buffer)
                                  (with-current-buffer shell-buffer
                                    ellm--is-primary-p))
                                shell-buffers)))
    (unless primary-shell-buffer
      (if shell-buffers
          ;; If we have shell buffers but none is primary, designate the first one
          (setq primary-shell-buffer (seq-first shell-buffers))
        ;; Only if we have no shell buffers at all, create a new one
        (let ((buffer-name (ellm--make-buffer-name)))
          (setq primary-shell-buffer
                (shell-maker-start ellm--config
                                   t
                                   ellm-welcome-function
                                   nil  ;; Don't force new session
                                   buffer-name
                                   "LLM"))))
      (ellm--set-primary-buffer primary-shell-buffer))
    primary-shell-buffer))

(defun ellm--make-buffer-name ()
  "Generate a buffer name using current shell config info."
  (format "*%s llm (%s)*"
          (downcase (ellm--model-label))
          (ellm--shell-info)))

(defun ellm--add-menus ()
  "Add ChatGPT shell menu items."
  (when (derived-mode-p 'ellm-mode)
    ;; Only continue if we're in a shell buffer
    (when-let ((duplicates (ellm-duplicate-map-keys ellm-system-prompts)))
      (message "Warning: Duplicate prompt names found %s. Please remove." duplicates))
    (easy-menu-define ellm-system-prompts-menu (current-local-map) "ChatGPT"
      `("ChatGPT"
        ("Versions"
         ,@(mapcar (lambda (version)
                     `[,version
                       (lambda ()
                         (interactive)
                         (setq-local ellm-model-version
                                     (seq-position ellm-models ,version))
                         (ellm--update-prompt t)
                         (ellm-interrupt nil))])
                   ellm-models))
        ("Prompts"
         ,@(mapcar (lambda (prompt)
                     `[,(car prompt)
                       (lambda ()
                         (interactive)
                         (setq-local ellm-system-prompt
                                     (seq-position (map-keys ellm-system-prompts) ,(car prompt)))
                         (ellm--save-variables)
                         (ellm--update-prompt t)
                         (ellm-interrupt nil))])
                   ellm-system-prompts))))
    (easy-menu-add ellm-system-prompts-menu)))

(defun ellm--update-prompt (rename-buffer)
  "Update prompt and prompt regexp from `ellm-models'.

Set RENAME-BUFFER to also rename the buffer accordingly."
  (when (derived-mode-p 'ellm-mode)
    ;; Only perform these operations if we're in a shell buffer
    (shell-maker-set-prompt
     (car (ellm--prompt-pair))
     (cdr (ellm--prompt-pair)))
    (when rename-buffer
      (shell-maker-set-buffer-name
       (current-buffer)
       (ellm--make-buffer-name)))))

(defun ellm-interrupt (ignore-item)
  "Interrupt `chatgpt-shell' from any buffer.

With prefix IGNORE-ITEM, do not mark as failed."
  (interactive "P")
  (with-current-buffer
      (cond
       ((derived-mode-p 'ellm-mode)
        (current-buffer))
       (t
        (shell-maker-buffer-name ellm--config)))
    (shell-maker-interrupt ignore-item)))

(defun ellm-ctrl-c-ctrl-c (ignore-item)
  "If point in source block, execute it.  Otherwise interrupt.

With prefix IGNORE-ITEM, do not use interrupted item in context."
  (interactive "P")
  (cond ((ellm-block-action-at-point)
         (ellm-execute-block-action-at-point))
        ((ellm-markdown-block-at-point)
         (user-error "No action available"))
        ((and shell-maker--busy
              (eq (line-number-at-pos (point-max))
                  (line-number-at-pos (point))))
         (shell-maker-interrupt ignore-item))
        (t
         (shell-maker-interrupt ignore-item))))

(defun ellm-mark-at-point-dwim ()
  "Mark source block if at point.  Mark all output otherwise."
  (interactive)
  (if-let ((block (ellm-markdown-block-at-point)))
      (progn
        (set-mark (map-elt block 'end))
        (goto-char (map-elt block 'start)))
    (shell-maker-mark-output)))

(defun ellm-markdown-block-language (text)
  "Get the language label of a Markdown TEXT code block."
  (when (string-match (rx bol "```" (0+ space) (group (+ (not (any "\n"))))) text)
    (match-string 1 text)))

(defun ellm-markdown-block-at-point ()
  "Markdown start/end cons if point at block.  nil otherwise."
  (save-excursion
    (save-restriction
      (when (derived-mode-p 'ellm-mode)
        (shell-maker-narrow-to-prompt))
      ;; Ensure point is within block if at bol in header.
      (move-end-of-line 1)
      (let* ((language)
             (language-start)
             (language-end)
             (start (save-excursion
                      (when (re-search-backward "^[ \t]*```" nil t)
                        (setq language (ellm-markdown-block-language (thing-at-point 'line)))
                        (save-excursion
                          (forward-char 3) ; ```
                          (setq language-start (point))
                          (end-of-line)
                          (setq language-end (point)))
                        language-end)))
             (end (save-excursion
                    (when (re-search-forward "^[ \t]*```" nil t)
                      (forward-line 0)
                      (point)))))
        (when (and start end
                   (>= (point) start)
                   (< (point) end))
          (list (cons 'language language)
                (cons 'language-start language-start)
                (cons 'language-end language-end)
                (cons 'start start)
                (cons 'end end)))))))

;; TODO: Move to shell-maker.
(defun ellm--markdown-headers (&optional avoid-ranges)
  "Extract markdown headers with AVOID-RANGES."
  (let ((headers '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (rx bol (group (one-or-more "#"))
                  (one-or-more space)
                  (group (one-or-more (not (any "\n")))) eol)
              nil t)
        (when-let ((begin (match-beginning 0))
                   (end (match-end 0)))
          (unless (seq-find (lambda (avoided)
                              (and (>= begin (car avoided))
                                   (<= end (cdr avoided))))
                            avoid-ranges)
            (push
             (list
              'start begin
              'end end
              'level (cons (match-beginning 1) (match-end 1))
              'title (cons (match-beginning 2) (match-end 2)))
             headers)))))
    (nreverse headers)))

;; TODO: Move to shell-maker.
(defun ellm--markdown-links (&optional avoid-ranges)
  "Extract markdown links with AVOID-RANGES."
  (let ((links '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (rx (seq "["
                       (group (one-or-more (not (any "]"))))
                       "]"
                       "("
                       (group (one-or-more (not (any ")"))))
                       ")"))
              nil t)
        (when-let ((begin (match-beginning 0))
                   (end (match-end 0)))
          (unless (seq-find (lambda (avoided)
                              (and (>= begin (car avoided))
                                   (<= end (cdr avoided))))
                            avoid-ranges)
            (push
             (list
              'start begin
              'end end
              'title (cons (match-beginning 1) (match-end 1))
              'url (cons (match-beginning 2) (match-end 2)))
             links)))))
    (nreverse links)))

;; TODO: Move to shell-maker.
(defun ellm--markdown-bolds (&optional avoid-ranges)
  "Extract markdown bolds with AVOID-RANGES."
  (let ((bolds '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (rx (or (group "**" (group (one-or-more (not (any "\n*")))) "**")
                      (group "__" (group (one-or-more (not (any "\n_")))) "__")))
              nil t)
        (when-let ((begin (match-beginning 0))
                   (end (match-end 0)))
          (unless (seq-find (lambda (avoided)
                              (and (>= begin (car avoided))
                                   (<= end (cdr avoided))))
                            avoid-ranges)
            (push
             (list
              'start begin
              'end end
              'text (cons (or (match-beginning 2)
                              (match-beginning 4))
                          (or (match-end 2)
                              (match-end 4))))
             bolds)))))
    (nreverse bolds)))

;; TODO: Move to shell-maker.
(defun ellm--markdown-strikethroughs (&optional avoid-ranges)
  "Extract markdown strikethroughs with AVOID-RANGES."
  (let ((strikethroughs '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (rx "~~" (group (one-or-more (not (any "\n~")))) "~~")
              nil t)
        (when-let ((begin (match-beginning 0))
                   (end (match-end 0)))
          (unless (seq-find (lambda (avoided)
                              (and (>= begin (car avoided))
                                   (<= end (cdr avoided))))
                            avoid-ranges)
            (push
             (list
              'start begin
              'end end
              'text (cons (match-beginning 1)
                          (match-end 1)))
             strikethroughs)))))
    (nreverse strikethroughs)))

;; TODO: Move to shell-maker.
(defun ellm--markdown-italics (&optional avoid-ranges)
  "Extract markdown italics with AVOID-RANGES."
  (let ((italics '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (rx (or (group (or bol (one-or-more (any "\n \t")))
                             (group "*")
                             (group (one-or-more (not (any "\n*")))) "*")
                      (group (or bol (one-or-more (any "\n \t")))
                             (group "_")
                             (group (one-or-more (not (any "\n_")))) "_")))
              nil t)
        (when-let ((begin (match-beginning 0))
                   (end (match-end 0)))
          (unless (seq-find (lambda (avoided)
                              (and (>= begin (car avoided))
                                   (<= end (cdr avoided))))
                            avoid-ranges)
            (push
             (list
              'start (or (match-beginning 2)
                         (match-beginning 5))
              'end end
              'text (cons (or (match-beginning 3)
                              (match-beginning 6))
                          (or (match-end 3)
                              (match-end 6))))
             italics)))))
    (nreverse italics)))

;; TODO: Move to shell-maker.
(defun ellm--markdown-inline-codes (&optional avoid-ranges)
  "Get a list of all inline markdown code in buffer with AVOID-RANGES."
  (let ((codes '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "`\\([^`\n]+\\)`"
              nil t)
        (when-let ((begin (match-beginning 0))
                   (end (match-end 0)))
          (unless (seq-find (lambda (avoided)
                              (and (>= begin (car avoided))
                                   (<= end (cdr avoided))))
                            avoid-ranges)
            (push
             (list
              'body (cons (match-beginning 1) (match-end 1))) codes)))))
    (nreverse codes)))

;; TODO: Move to shell-maker.
(defvar ellm--source-block-regexp
  (rx  bol (zero-or-more whitespace) (group "```") (zero-or-more whitespace) ;; ```
       (group (zero-or-more (or alphanumeric "-" "+" "#"))) ;; languages like: emacs-lisp C++ C#
       (zero-or-more whitespace)
       (one-or-more "\n")
       (group (*? anychar)) ;; body
       (one-or-more "\n")
       (zero-or-more whitespace)
       (group "```") (or "\n" eol)))

(defun ellm-next-source-block ()
  "Move point to the next source block's body."
  (interactive)
  (let ((blocks (ellm--source-blocks))
        (pos (point)))
    (when-let ((next-block (seq-find (lambda (block)
                                       (> (car (map-elt block 'start)) pos))
                                     blocks)))
      (goto-char (car (map-elt next-block 'start)))
      (point))))

(defun ellm-next-link ()
  "Move point to the next link."
  (interactive)
  (let ((links (ellm--markdown-links))
        (pos (point)))
    (when-let ((next-link (seq-find (lambda (link)
                                       (> (map-elt link 'start) pos))
                                     links)))
      (goto-char (map-elt next-link 'start))
      (point))))

(defun ellm-previous-item ()
  "Go to previous item.

Could be a prompt or a source block."
  (interactive)
  (unless (derived-mode-p 'ellm-mode)
    (user-error "Not in a shell"))
  (let* ((prompt-pos (save-excursion
                       (when (comint-next-prompt (- 1))
                         (point))))
         (block-pos (save-excursion
                      (ellm-previous-source-block)))
         (link-pos (save-excursion
                     (ellm-previous-link)))
         (positions (delq nil (list prompt-pos
                                    block-pos
                                    link-pos)))
         (next-pos (when positions
                     (apply 'max positions))))
    (when next-pos
      (cond ((eq next-pos prompt-pos)
             (deactivate-mark)
             (goto-char prompt-pos))
            ((eq next-pos block-pos)
             (deactivate-mark)
             (goto-char block-pos)
             (call-interactively #'ellm-mark-block))
            ((eq next-pos link-pos)
             (deactivate-mark)
             (goto-char link-pos))))))

(defun ellm-next-item ()
  "Go to next item.

Could be a prompt or a source block."
  (interactive)
  (unless (derived-mode-p 'ellm-mode)
    (user-error "Not in a shell"))
  (let* ((prompt-pos (save-excursion
                       (when (comint-next-prompt 1)
                         (point))))
         (block-pos (save-excursion
                      (ellm-next-source-block)))
         (link-pos (save-excursion
                     (ellm-next-link)))
         (next-pos (apply 'min (delq nil (list prompt-pos
                                               block-pos
                                               link-pos)))))
    (when next-pos
      (cond ((eq next-pos prompt-pos)
             (deactivate-mark)
             (goto-char prompt-pos))
            ((eq next-pos block-pos)
             (deactivate-mark)
             (goto-char block-pos)
             (call-interactively #'ellm-mark-block))
            ((eq next-pos link-pos)
             (deactivate-mark)
             (goto-char link-pos))))))

(defun ellm-previous-source-block ()
  "Move point to the previous source block's body."
  (interactive)
  (let ((blocks (ellm--source-blocks))
        (pos (point)))
    (when-let ((next-block (seq-find (lambda (block)
                                       (< (car (map-elt block 'end)) pos))
                                     (reverse blocks))))
      (goto-char (car (map-elt next-block 'start)))
      (point))))

(defun ellm-previous-link ()
  "Move point to the previous link."
  (interactive)
  (let ((links (ellm--markdown-links))
        (pos (point)))
    (when-let ((previous-link (seq-find (lambda (link)
                                          (< (map-elt link 'end) pos))
                                        (reverse links))))
      ;; May not be on actual URL text because of overlay (not too sure).
      ;; So, pressing RET does not open link.
      ;; Work around by moving forward 1 char (not visible to user).
      (goto-char (map-elt previous-link 'start))
      (forward-char)
      (point))))

;; TODO: Move to shell-maker.
(defun ellm--match-source-block ()
  "Return a matched source block by the previous search/regexp operation."
  (list
   'start (cons (match-beginning 1)
                (match-end 1))
   'end (cons (match-beginning 4)
              (match-end 4))
   'language (when (and (match-beginning 2)
                        (match-end 2))
               (cons (match-beginning 2)
                     (match-end 2)))
   'body (cons (match-beginning 3) (match-end 3))))

;; TODO: Move to shell-maker.
(defun ellm--source-blocks ()
  "Get a list of all source blocks in buffer."
  (let ((markdown-blocks '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              ellm--source-block-regexp
              nil t)
        (when-let ((begin (match-beginning 0))
                   (end (match-end 0)))
          (push (ellm--match-source-block)
                markdown-blocks))))
    (nreverse markdown-blocks)))

(defun ellm--minibuffer-prompt ()
  "Construct a prompt for the minibuffer."
  (if (ellm--primary-buffer)
      (concat (string-trim
               (replace-regexp-in-string
                "\\*" ""
                (buffer-name (ellm--primary-buffer)))) "> ")
    (shell-maker-prompt
     ellm--config)))

;;;###autoload
(defun ellm-prompt ()
  "Make a ChatGPT request from the minibuffer.

If region is active, append to prompt."
  (interactive)
  (unless ellm--prompt-history
    (setq ellm--prompt-history
          ellm-default-prompts))
  (let ((overlay-blocks (derived-mode-p 'prog-mode))
        (prompt (funcall shell-maker-read-string-function
                         (concat
                          (if (region-active-p)
                              "[appending region] "
                            "")
                          (ellm--minibuffer-prompt))
                         'ellm--prompt-history)))
    (when (string-empty-p (string-trim prompt))
      (user-error "Nothing to send"))
    (when (region-active-p)
      (setq prompt (concat prompt "\n\n"
                           (if overlay-blocks
                               (format "``` %s\n"
                                       (string-remove-suffix "-mode" (format "%s" major-mode)))
                             "")
                           (buffer-substring (region-beginning) (region-end))
                           (if overlay-blocks
                               "\n```"
                             ""))))
    (ellm-send-to-buffer prompt nil)))

;;;###autoload
(defun ellm-prompt-appending-kill-ring ()
  "Make a ChatGPT request from the minibuffer appending kill ring."
  (interactive)
  (unless ellm--prompt-history
    (setq ellm--prompt-history
          ellm-default-prompts))
  (let ((prompt (funcall shell-maker-read-string-function
                         (concat
                          "[appending kill ring] "
                          (ellm--minibuffer-prompt))
                         'ellm--prompt-history)))
    (ellm-send-to-buffer
     (concat prompt "\n\n"
             (current-kill 0)) nil)))

;;;###autoload
(defun ellm-describe-code ()
  "Describe code from region using ChatGPT."
  (interactive)
  (unless (region-active-p)
    (user-error "No region active"))
  (let ((overlay-blocks (derived-mode-p 'prog-mode)))
    (ellm-send-to-buffer
     (concat ellm-prompt-header-describe-code
             "\n\n"
             (if overlay-blocks
                 (format "``` %s\n"
                         (string-remove-suffix "-mode" (format "%s" major-mode)))
               "")
             (buffer-substring (region-beginning) (region-end))
             (if overlay-blocks
                 "\n```"
               "")) nil)
    (when overlay-blocks
      (with-current-buffer
          (ellm--primary-buffer)
        (ellm--put-source-block-overlays)))))

(defun ellm-send-region-with-header (header)
  "Send text with HEADER from region using ChatGPT."
  (unless (region-active-p)
    (user-error "No region active"))
  (let ((question (concat header "\n\n" (buffer-substring (region-beginning) (region-end)))))
    (ellm-send-to-buffer question nil)))

;;;###autoload
(defun ellm-refactor-code ()
  "Refactor code from region using ChatGPT."
  (interactive)
  (ellm-send-region-with-header ellm-prompt-header-refactor-code))

;;;###autoload
(defun ellm-write-git-commit ()
  "Write commit from region using ChatGPT."
  (interactive)
  (ellm-send-region-with-header ellm-prompt-header-write-git-commit))

;;;###autoload
(defun ellm-generate-unit-test ()
  "Generate unit-test for the code from region using ChatGPT."
  (interactive)
  (ellm-send-region-with-header ellm-prompt-header-generate-unit-test))

;;;###autoload
(defun ellm-proofread-region ()
  "Proofread text from region using ChatGPT.

See `ellm-prompt-header-proofread-region' to change prompt or language."
  (interactive)
  (let* ((region (ellm--region))
         (query (map-elt region :text))
         (context nil))
    (ellm-request-and-insert-merged-response
     :system-prompt ellm-prompt-header-proofread-region
     :query query
     :context context
     :remove-block-markers t
     :region region
     :on-iterate (lambda (output)
                   (set-mark (map-elt region :end))
                   (goto-char (map-elt region :start))
                   (ellm-quick-insert (append context
                                                       (list (cons query output))))))))

;;;###autoload
(defun ellm-send-region (review)
  "Send region to ChatGPT.
With prefix REVIEW prompt before sending to ChatGPT."
  (interactive "P")
  (unless (region-active-p)
    (user-error "No region active"))
  (let ((ellm-prompt-query-response-style 'shell)
        (region-text (buffer-substring (region-beginning) (region-end))))
    (ellm-send-to-buffer
     (if review
         (concat "\n\n" region-text)
       region-text) review)))

;;;###autoload
(defun ellm-send-and-review-region ()
  "Send region to ChatGPT, review before submitting."
  (interactive)
  (ellm-send-region t))

(defun ellm-command-line-from-prompt-file (file-path)
  "Send prompt in FILE-PATH and output to standard output."
  (let ((prompt (with-temp-buffer
                  (insert-file-contents file-path)
                  (buffer-string))))
    (if (string-empty-p (string-trim prompt))
        (princ (format "Could not read prompt from %s" file-path)
               #'external-debugging-output)
      (ellm-command-line prompt))))

(defun ellm-command-line (prompt)
  "Send PROMPT and output to standard output."
  (let ((ellm-prompt-query-response-style 'shell)
        (worker-done nil)
        (buffered ""))
    (ellm-post :context (list (cons prompt nil))
                        :streaming t
                        :on-output (lambda (output)
                                     (setq buffered (concat buffered output)))
                        :on-success (lambda (_output)
                                      (setq worker-done t))
                        :on-failure (lambda (_output)
                                      (setq worker-done t)))
    (while buffered
      (unless (string-empty-p buffered)
        (princ buffered #'external-debugging-output))
      (setq buffered "")
      (when worker-done
        (setq buffered nil))
      (sleep-for 0.1))
    (princ "\n")))

(defun ellm--eshell-last-last-command ()
  "Get second to last eshell command."
  (save-excursion
    (if (derived-mode-p 'eshell-mode)
        (let ((cmd-start)
              (cmd-end))
          ;; Find command start and end positions
          (goto-char eshell-last-output-start)
          (re-search-backward eshell-prompt-regexp nil t)
          (setq cmd-start (point))
          (goto-char eshell-last-output-start)
          (setq cmd-end (point))

          ;; Find output start and end positions
          (goto-char eshell-last-output-start)
          (forward-line 1)
          (re-search-forward eshell-prompt-regexp nil t)
          (forward-line -1)
          (buffer-substring-no-properties cmd-start cmd-end))
      (message "Current buffer is not an eshell buffer."))))

;; Based on https://emacs.stackexchange.com/a/48215
(defun ellm--source-eshell-string (string)
  "Execute eshell command in STRING."
  (let ((orig (point))
        (here (point-max))
        (inhibit-point-motion-hooks t))
    (goto-char (point-max))
    (with-silent-modifications
      ;; FIXME: Use temporary buffer and avoid insert/delete.
      (insert string)
      (goto-char (point-max))
      (throw 'eshell-replace-command
             (prog1
                 (list 'let
                       (list (list 'eshell-command-name (list 'quote "source-string"))
                             (list 'eshell-command-arguments '()))
                       (eshell-parse-command (cons here (point))))
               (delete-region here (point))
               (goto-char orig))))))

;;;###autoload
(defun ellm-add-??-command-to-eshell ()
  "Add `??' command to `eshell'."

  ;; Note: Must have "eshell/" prefix to be recognized as an eshell command.
  (defun eshell/?? (&rest _args)
    "Implements `??' eshell command."
    (interactive)
    (let ((prompt (concat
                   "What's wrong with the following command execution? Be succinct.\n\n"
                   (ellm--eshell-last-last-command)))
          (prompt-file (concat temporary-file-directory
                               "ellm-command-line-prompt")))
      (when (file-exists-p prompt-file)
        (delete-file prompt-file))
      (with-temp-file prompt-file nil nil t
                      (insert prompt))
      (ellm--source-eshell-string
       (concat
        (file-truename (expand-file-name invocation-name invocation-directory)) " "
        "--quick --batch --eval "
        "'"
        (prin1-to-string
         `(progn
            (interactive)
            (load ,(find-library-name "chatgpt-shell") nil t)
            (load ,(find-library-name "ellm-anthropic") nil t)
            (load ,(find-library-name "ellm-deepseek") nil t)
            (load ,(find-library-name "ellm-google") nil t)
            (load ,(find-library-name "ellm-kagi") nil t)
            (load ,(find-library-name "ellm-ollama") nil t)
            (load ,(find-library-name "ellm-openai") nil t)
            (load ,(find-library-name "ellm-openrouter") nil t)
            (load ,(find-library-name "ellm-perplexity") nil t)
            (load ,(find-library-name "ellm-prompt-compose") nil t)
            (load ,(find-library-name "shell-maker") nil t)
            (setq ellm-model-temperature 0)
            (setq ellm-openai-key ,(ellm-openai-key))
            (ellm-command-line-from-prompt-file ,prompt-file)))
        "'"))))

  (add-hook 'eshell-post-command-hook
            (defun ellm--eshell-post-??-execution ()
              (when (string-match "ellm-command-line-from-prompt-file"
                                  (string-join eshell-last-arguments " "))
                (save-excursion
                  (save-restriction
                    (narrow-to-region (eshell-beginning-of-output)
                                      (eshell-end-of-output))
                    (ellm--put-source-block-overlays))))))

  (require 'esh-cmd)

  (add-to-list 'eshell-complex-commands "??"))

(cl-defun ellm-request-and-insert-response (&key query
                                                          (buffer (current-buffer))
                                                          model-version
                                                          system-prompt
                                                          streaming
                                                          start
                                                          end)
  "Send a contextless request (no history) with:

QUERY: Request query text.
BUFFER (optional): Buffer to insert to or omit to insert to current buffer.
MODEL-VERSION (optional): Index from `ellm-models' or string.
SYSTEM-PROMPT (optional): As string.
STREAMING (optional): Non-nil to stream insertion.
START (optional): Beginning of region to replace (overrides active region).
END (optional): End of region to replace (overrides active region)."
  (let* ((point (point))
         (delete-text (or
                       (and start end)
                       (region-active-p)))
         (delete-from (when delete-text
                        (or start (region-beginning))))
         (delete-to (when delete-text
                      (or end (region-end))))
         (marker (if delete-text
                     (copy-marker (max delete-from delete-to))
                   (copy-marker (point))))
         (progress-reporter (unless streaming
                              (make-progress-reporter
                               (format "%s " (ellm--model-label))))))
    (ellm-send-contextless-request
     :model-version model-version
     :system-prompt system-prompt
     :query query
     :streaming t
     :on-output (lambda (output)
                    (if streaming
                        (progn
                          (with-current-buffer buffer
                            (when delete-text
                              (deactivate-mark)
                              (delete-region delete-from delete-to)
                              (setq delete-text nil))
                            (save-excursion
                              (goto-char marker)
                              (insert output)
                              (set-marker marker (+ (length output)
                                                    (marker-position marker))))))
                      (progn
                        (progress-reporter-update progress-reporter))))
     :on-success (lambda (output)
                   (if streaming
                        (with-current-buffer buffer
                          (goto-char point))
                      (progress-reporter-done progress-reporter)
                      (with-current-buffer buffer
                        (when delete-text
                          (deactivate-mark)
                          (delete-region delete-from delete-to)
                          (setq delete-text nil))
                        (save-excursion
                          (goto-char marker)
                          (insert output))
                        (goto-char point))))
     :on-failure (lambda (output)
                   (if streaming
                       (with-current-buffer buffer
                         (goto-char point))
                     (progress-reporter-done progress-reporter))
                   (when (not (string-empty-p (string-trim
                                               (or output ""))))
                     (message (or output "failed")))))))

;; TODO: Review. Can it become service agnostic?
(cl-defun ellm-send-contextless-request
    (&key model-version
          system-prompt
          query
          streaming
          on-output
          on-success
          on-failure)
  "Send a request with:

QUERY: Request query text.
ON-OUTPUT: Of the form (lambda (output))
ON-FINISHED: Of the form (lambda (success))
MODEL-VERSION (optional): Index from `ellm-models' or string.
SYSTEM-PROMPT (optional): As string.
STREAMING (optional): non-nil to received streamed ON-OUTPUT events."
  (unless query
    (error "Missing mandatory \"query\" param"))
  (ellm-post :context (list (cons query nil))
                      :system-prompt system-prompt
                      :version model-version
                      :streaming streaming
                      :on-output on-output
                      :on-success on-success
                      :on-failure on-failure))

;;;###autoload
(defun ellm-prompt-compose (prefix)
  "Compose and send prompt from a dedicated buffer.

With PREFIX, clear existing history (wipe asociated shell history).

Whenever `ellm-prompt-compose' is invoked, appends any active
region (or flymake issue at point) to compose buffer.

Additionally, if point is at an error/warning raised by flymake,
automatically add context (error/warning + code) to expedite ChatGPT
for help to fix the issue.

The compose buffer always shows the latest interaction, but it's
backed by the shell history.  You can always switch to the shell buffer
to view the history.

Editing: While compose buffer is in in edit mode, it offers a couple
of magit-like commit buffer bindings.

 `\\[ellm-prompt-compose-send-buffer]` to send the buffer query.
 `\\[ellm-prompt-compose-cancel]` to cancel compose buffer.
 `\\[ellm-prompt-compose-search-history]` search through history.
 `\\[ellm-prompt-compose-previous-history]` cycle through previous
item in history.
 `\\[ellm-prompt-compose-next-history]` cycle through next item in
history.

Read-only: After sending a query, the buffer becomes read-only and
enables additional key bindings.

 `\\[ellm-prompt-compose-send-buffer]` After sending offers to abort
query in-progress.
 `\\[View-quit]` Exits the read-only buffer.
 `\\[ellm-prompt-compose-retry]` Refresh (re-send the query).  Useful
to retry on disconnects.
 `\\[ellm-prompt-compose-next-item]` Jump to next source block.
 `\\[ellm-prompt-compose-previous-item]` Jump to next previous block.
 `\\[ellm-prompt-compose-reply]` Reply to follow-up with additional questions.
 `\\[ellm-prompt-compose-request-entire-snippet]` Send \"Show entire snippet\" query.
 `\\[ellm-prompt-compose-insert-block-at-point]` Insert block at point at last known location.
 `\\[ellm-prompt-compose-request-more]` Send \"Show me more\" query.
 `\\[ellm-prompt-compose-other-buffer]` Jump to other buffer (ie. the shell itself).
 `\\[ellm-mark-block]` Mark block at point."
  (interactive "P")
  (ellm-prompt-compose-show-buffer :clear-history prefix))

;; TODO: move to cl-defun and consider removing `ellm-prompt-query-response-style'.
(defun ellm-send-to-buffer (input &optional review handler on-finished response-style)
  "Send INPUT to *chatgpt* shell buffer.

Set REVIEW to make changes before submitting to ChatGPT.

If HANDLER function is set, ignore RESPONSE-STYLE.

RESPONSE-STYLE defaults to `ellm-prompt-query-response-style',

and is of the form:

  (lambda (input output error finished))

ON-FINISHED is invoked when the entire interaction is finished and of the form:

  (lambda (input output success))."
  (unless response-style
    (setq response-style ellm-prompt-query-response-style))
  (if (eq response-style 'other-buffer)
      (let ((buffer (ellm-prompt-compose-show-buffer :content input)))
        (unless review
          (with-current-buffer buffer
            (ellm-prompt-compose-send-buffer))))
    (let* ((buffer (cond (handler
                          nil)
                         ((eq response-style 'inline)
                          (current-buffer))
                         (t
                          nil)))
           (marker (copy-marker (point)))
           (orig-region-active (region-active-p))
           (region-beginning (when orig-region-active
                               (region-beginning)))
           (region-end (when orig-region-active
                         (region-end)))
           (no-focus (or (eq response-style 'inline)
                         handler)))
      (when (region-active-p)
        (setq marker (copy-marker (max (region-beginning)
                                       (region-end)))))
      ;; Check if we have a primary buffer
      (let ((primary-buffer (ellm--primary-buffer)))
        ;; If we have a primary buffer, use it; otherwise create a new one
        (if primary-buffer
            ;; Display or switch to the existing buffer based on no-focus
            (if no-focus
                (display-buffer primary-buffer)
              (switch-to-buffer primary-buffer))
          ;; Only create a new buffer if there isn't one already
          (ellm-start no-focus t)))
      (let ((primary-buffer (ellm--primary-buffer))) ;; Get the primary buffer once
        (cl-flet ((send ()
                    (with-current-buffer primary-buffer ;; Always use the primary buffer
                      (when shell-maker--busy
                        (shell-maker-interrupt nil))
                      (if review
                          (save-excursion
                            (goto-char (point-max))
                            (insert input))
                        (shell-maker-submit
                         :input input
                         :on-output (lambda (output)
                                      (setq output (or output ""))
                                      (when (buffer-live-p buffer)
                                        (with-current-buffer buffer
                                          (let ((inhibit-read-only t))
                                            (save-excursion
                                              (when orig-region-active
                                                (delete-region region-beginning region-end)
                                                (setq orig-region-active nil))
                                              (goto-char marker)
                                              (insert output)
                                              (set-marker marker (+ (length output)
                                                                    (marker-position marker))))))))
                         :on-finished (lambda (input output success)
                                        (when on-finished
                                          (funcall on-finished input output success))
                                        (with-current-buffer primary-buffer
                                          (ellm--put-source-block-overlays))))))))
          (if (or (eq response-style 'inline)
                  handler)
              (progn
                (with-current-buffer primary-buffer
                  (goto-char (point-max)))
                (send))
            (with-selected-window (get-buffer-window primary-buffer)
              (send))))))))

(defun ellm-send-to-ielm-buffer (text &optional execute save-excursion)
  "Send TEXT to *ielm* buffer.
Set EXECUTE to automatically execute.
Set SAVE-EXCURSION to prevent point from moving."
  (ielm)
  (with-current-buffer (get-buffer-create "*ielm*")
    (goto-char (point-max))
    (if save-excursion
        (save-excursion
          (insert text))
      (insert text))
    (when execute
      (ielm-return))))

(defun ellm-parse-elisp-code (code)
  "Parse emacs-lisp CODE and return a list of expressions."
  (with-temp-buffer
    (insert code)
    (goto-char (point-min))
    (let (sexps)
      (while (not (eobp))
        (condition-case nil
            (push (read (current-buffer)) sexps)
          (error nil)))
      (reverse sexps))))

(defun ellm-split-elisp-expressions (code)
  "Split emacs-lisp CODE into a list of stringified expressions."
  (mapcar
   (lambda (form)
     (prin1-to-string form))
   (ellm-parse-elisp-code code)))

;;;###autoload
(defun ellm-japanese-ocr-lookup ()
  "Select a region of the screen to OCR and look up in Japanese."
  (interactive)
  (let* ((term)
         (process (start-process "macosrec-ocr" nil "macosrec" "--ocr")))
    (if (memq window-system '(mac ns))
        (unless (executable-find "macosrec")
          (user-error "You need \"macosrec\" installed: brew install xenodium/macosrec/macosrec"))
      (user-error "Not yet supported on %s (please send a pull request)" window-system))
    (set-process-filter process (lambda (_proc text)
                                  (setq term (concat term text))))
    (set-process-sentinel process (lambda (_proc event)
                                    (when (string= event "finished\n")
                                      (ellm-japanese-lookup term))))))

;;;###autoload
(defun ellm-japanese-audio-lookup ()
  "Transcribe audio at current file (buffer or `dired') and look up in Japanese."
  (interactive)
  (let* ((term)
         (file (ellm--current-file))
         (extension (downcase (file-name-extension file)))
         (process (start-process "macosrec-speechrec" nil "macosrec"
                                 "--speech-to-text" "--locale" "ja-JP" "--input" file)))
    (if (memq window-system '(mac ns))
        (unless (executable-find "macosrec")
          (user-error "You need \"macosrec\" installed: brew install xenodium/macosrec/macosrec"))
      (user-error "Not yet supported on %s (please send a pull request)" window-system))
    (unless (seq-contains-p '("mp3" "wav" "m4a" "caf") extension)
      (user-error "Must be using either .mp3, .m4a, .caf or .wav"))
    (set-process-filter process (lambda (_proc text)
                                  (setq term (concat term text))))
    (set-process-sentinel process (lambda (_proc event)
                                    (when (string= event "finished\n")
                                      (ellm-japanese-lookup term))))))

(defun ellm-japanese-lookup (&optional capture)
  "Look Japanese term up.

Ask user for term.  Alternatively, if point is on an image, extract Japanese
data from the image.

If command invoked with prefix, CAPTURE a screenshot.

Display result in org table of the form:

|----------+----------+-------+--------+---------+-------|
| Hiragana | Katakana | Kanji | Romaji | English | #Tags |
|----------+----------+-------+--------+---------+-------|
|          |          |       |        |         |       |
|----------+----------+-------+--------+---------+-------|"
  (interactive "P")
  (let* ((file (ellm--current-image-file capture))
         (term (unless file
                 (ellm--read-string :prompt "Japanese lookup: ")))
         (buffer "*Japanese lookup*"))
    (ellm-lookup :buffer buffer
                          :prompt (if file
                                      "Use for this image to extract each row data."
                                    (format "Fill out a row for \"%s\"" term))
                          :on-success (lambda (_output)
                                        (with-current-buffer buffer
                                          (goto-char (point-min))
                                          (org-mode)
                                          (let ((inhibit-read-only t))
                                            (goto-char (point-min))
                                            (org-table-align)
                                            (when (fboundp 'org-modern-mode)
                                              (org-modern-mode +1))
                                            ;; (when (fboundp 'valign-mode)
                                            ;;   (valign-mode +1))
                                            (visual-line-mode -1))))
                          :prompt-url file
                          :streaming t
                          :system-prompt "
1. Fill out an org mode table using this format as an example:

|---------------+----------+-------+-------------------+---------+-------------------|
| Hiragana      | Katakana | Kanji | Romaji            | English | Tags              |
|---------------+----------+-------+-------------------+---------+-------------------|
| おなかすきました |          | 空腹   | onaka sukimashita | hungry  | #describe #myself |
|---------------+----------+-------+-------------------+---------+-------------------|
2. ALWAYS Fill out Hiragana when appropriate.
3. ALWAYS Fill out Katakana when appropriate.
4. ALWAYS Fill out Kanji when appropriate.
5. Show long romaji vowels (i.e ō).
6. Ensure columns align.
7. Do NOT wrap anything in Markdown source blocks.
8. Do NOT add any text or explanations outside the org table.")))

;; TODO: Make service agnostic.
(cl-defun ellm-lookup (&key buffer model-version system-prompt prompt prompt-url streaming
                                     temperature on-success on-failure)
  "Look something up as a one-off (no shell history) and output to BUFFER.

Inputs:

Either (or all) of MODEL-VERSION, SYSTEM-PROMPT, PROMPT,
and PROMPT-URL (image file).

Optionally:

STREAMING: Non-nil streams output to BUFFER.

TEMPERATURE: Defaults to 1 otherwise.

ON-SUCCESS: (lambda (output)) for completion event.

ON-FAILURE: (lambda (output)) for completion event."
  (setq buffer (get-buffer-create buffer))
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer))
    (use-local-map (let ((map (make-sparse-keymap)))
                     (define-key map (kbd "q") 'kill-buffer-and-window)
                     map))
    (setq buffer-read-only t))
  (let* ((model (or (ellm--resolved-model :versioned model-version)
                    (error "Model \"%s\" not found" model-version)))
         (settings (list (cons :streaming streaming)
                         (cons :temperature temperature)
                         (cons :system-prompt system-prompt)))
         (url (funcall (map-elt model :url)
                       :model model
                       :settings settings
                       :command prompt))
         (headers (when (map-elt model :headers)
                    (funcall (map-elt model :headers)
                             :model model
                             :settings settings))))
    (when streaming
      (display-buffer buffer))
    (unless (equal (map-elt model :provider) "OpenAI")
      (error "%s's %s not yet supported (please sponsor development)"
             (map-elt model :provider) (map-elt model :version)))
    (shell-maker-make-http-request
     :async t
     :url url
     :proxy ellm-proxy
     :data (ellm-openai-make-chatgpt-request-data
            :prompt prompt
            :prompt-url prompt-url
            :system-prompt system-prompt
            :version (map-elt model :version)
            :temperature (or temperature 1)
            :streaming streaming
            :other-params (map-elt model :other-params))
     :headers headers
     :filter #'ellm-openai--filter-output
     :on-output
     (lambda (output)
       (with-current-buffer buffer
         (let ((inhibit-read-only t))
           (goto-char (point-max))
           (insert output))))
     :on-finished
     (lambda (result)
       (unless streaming
         (display-buffer buffer))
       (if (equal 0 (map-elt result :exit-status))
           (when on-success
             (funcall on-success (map-elt result :output)))
         (when on-failure
           (funcall on-failure (map-elt result :output))))))))

(cl-defun ellm--read-string (&key prompt default-value)
  "Like `read-string' but disallowing empty input (unless DEFAULT-VALUE given).

Specify PROMPT to signal the user."
  (let ((input ""))
    (while (string-empty-p (string-trim input))
      (setq input (read-string (or prompt "")
                               (when (use-region-p)
                                 (buffer-substring-no-properties
                                  (region-beginning)
                                  (region-end)))
                               nil nil default-value))
      (when (string-empty-p (string-trim input))
        (setq input default-value)))
    input))

(cl-defun ellm-post (&key context
                                   version
                                   system-prompt
                                   on-output
                                   on-success on-failure
                                   temperature streaming)
  "Make a single ChatGPT request with MESSAGES and FILTER.

`ellm-openai--filter-output' typically used as extractor.

Optionally pass model VERSION, TEMPERATURE and OTHER-PARAMS.

OTHER-PARAMS are appended to the json object at the top level.

CONTEXT: A list of cons of the form: (command . response).

ON-OUTPUT: (lambda (output))

ON-SUCCESS: (lambda (output))

ON-FAILURE: (lambda (output))

If ON-FINISHED, ON-SUCCESS, and ON-FINISHED are missing, execute synchronously."
  (unless context
    (error "Missing mandatory \"context\" param"))
  (let* ((model (ellm--resolved-model :versioned version))
         (settings (list (cons :streaming streaming)
                         (cons :temperature temperature)
                         (cons :system-prompt system-prompt)))
         (url (funcall (map-elt model :url)
                       :model model
                       :settings settings
                       :command (car (car (last context)))))
         (payload (map-elt model :payload))
         (filter (or (map-elt model :filter)
                     (error "Model :filter not found")))
         (headers (map-elt model :headers)))
    (if (or on-output on-success on-failure)
        (progn
          (unless (boundp 'shell-maker--current-request-id)
            (setq-local shell-maker--current-request-id 0))
          ;; TODO: Remove the need to create a temporary
          ;; shell configuration when invoking `shell-maker-make-http-request'.
          (with-temp-buffer
            (setq-local shell-maker--config
                        ellm--config)
            ;; Async exec
            (shell-maker-make-http-request
             :async t
             :url url
             :proxy ellm-proxy
             :data (when payload
                     (funcall payload
                              :model model
                              :context context
                              :settings settings))
             :headers (when headers
                        (funcall headers
                                 :model model
                                 :settings settings))
             :filter filter
             :on-output on-output
             :on-finished (lambda (result)
                            (if (equal 0 (map-elt result :exit-status))
                                (when on-success
                                  (funcall on-success (map-elt result :output)))
                              (when on-failure
                                (funcall on-failure (map-elt result :output))))))))
      ;; Sync exec
      (let ((result
             (shell-maker-make-http-request
              :async nil ;; Block to return result
              :url url
              :proxy ellm-proxy
              :data (when payload
                      (funcall payload
                               :model model
                               :context context
                               :settings settings))
              :headers (when headers
                         (funcall headers
                                  :model model
                                  :settings settings))
              :filter filter)))
        (map-elt result :output)))))

;;;###autoload
(defun ellm-describe-image (&optional capture)
  "Request OpenAI to describe image.

When visiting a buffer with an image, send that.

If command invoked with prefix, CAPTURE a screenshot.

If in a `dired' buffer, use selection (single image only for now)."
  (interactive "P")
  (let ((file (ellm--current-image-file capture))
        (description-buffer (get-buffer-create "*chatgpt image description*"))
        (prompt (ellm--read-string
                 :prompt "Describe image (default \"What's in this image?\"): "
                 :default-value "What's in this image?")))
    (unless file
      (error "No image found"))
    (message "Requesting...")
    (ellm-lookup :buffer description-buffer
                          :prompt prompt
                          :on-success (lambda (output)
                                        (with-current-buffer description-buffer
                                          (let ((inhibit-read-only t))
                                            (erase-buffer)
                                            (insert output)
                                            (use-local-map (let ((map (make-sparse-keymap)))
                                                             (define-key map (kbd "q") 'kill-buffer-and-window)
                                                             map)))
                                          (message "Image description ready")
                                          (read-only-mode +1))
                                        (display-buffer description-buffer))
                          :prompt-url file
                          :streaming nil)))

(defun ellm--current-image-file (&optional capture)
  "Return buffer image file, Dired selected file, or image at point.

If optional CAPTURE is non-nil, cature a screenshot."
  (when (or (and (use-region-p) (derived-mode-p 'image-mode))
            (and (use-region-p) (derived-mode-p 'dired-mode)))
    (user-error "No region selection supported"))
  (cond (capture
         (redisplay) ;; Call process will block. Give redisplay a chance.
         (when-let ((file (make-temp-file "screenshot" nil ".png"))
                    ;; TODO: Make screenshot utility configurable.
                    (success (eq 0 (call-process "/usr/sbin/screencapture" nil nil nil "-i" file)))
                    (found (file-exists-p file))
                    (written (not (zerop (nth 7 (file-attributes file))))))
           file))
        ((derived-mode-p 'image-mode)
         (buffer-file-name))
        ((derived-mode-p 'dired-mode)
         (let* ((dired-files (dired-get-marked-files))
                (file (seq-first dired-files)))
           (when (> (length dired-files) 1)
             (user-error "Only one file selection supported"))
           file))
        (t
         (when-let* ((image (cdr (get-text-property (point) 'display)))
                     (image-file (cond ((plist-get image :file)
                                        (plist-get image :file))
                                       ((plist-get image :data)
                                        (ignore-errors
                                          (delete-file (ellm--image-request-file)))
                                        (with-temp-file (ellm--image-request-file)
                                          (set-buffer-multibyte nil)
                                          (insert (plist-get image :data)))
                                        (ellm--image-request-file)))))
           image-file))))

(defun ellm--current-file ()
  "Return buffer file, Dired selected file, or image at point."
  (when (use-region-p)
    (user-error "No region selection supported"))
  (cond ((buffer-file-name)
         (buffer-file-name))
        ((derived-mode-p 'dired-mode)
         (let* ((dired-files (dired-get-marked-files))
                (file (seq-first dired-files)))
           (unless dired-files
             (user-error "No file selected"))
           (when (> (length dired-files) 1)
             (user-error "Only one file selection supported"))
           file))
        (t
         (user-error "Nothing found to work on"))))

(defun ellm--make-chatgpt-url (url)
  "Create ChatGPT message content for URL."
  (unless url
    (error "URL missing"))
  (let* ((extension (downcase (file-name-extension url)))
         (name (file-name-nondirectory url)))
    (unless (string-prefix-p "http" url)
      (unless (file-exists-p url)
        (error "File not found"))
      (unless (or (seq-contains-p '("jpg" "jpeg" "png" "webp" "gif") extension)
                  (equal name "image.request"))
        (user-error "Image must be either .jpg, .jpeg, .png, .webp or .gif"))
      (setq url (concat "data:image/jpeg;base64,"
                        (with-temp-buffer
                          (insert-file-contents-literally url)
                          (base64-encode-region (point-min) (point-max) t)
                          (buffer-string)))))
    url))

(defun ellm--temp-dir ()
  "Get chatgpt-shell's temp directory."
  (let ((temp-dir (file-name-concat temporary-file-directory "chatgpt-shell")))
    (make-directory temp-dir t)
    temp-dir))

(defun ellm--json-request-file ()
  "JSON request written to this file prior to sending."
  (file-name-concat (ellm--temp-dir) "request.json"))

(defun ellm--image-request-file ()
  "Image written to this file prior to sending."
  (file-name-concat (ellm--temp-dir) "image.request"))

(cl-defun ellm--write-json-request-file (&key data)
  "Encode and write DATA as json to request file."
  (unless data
    (error "Missing mandatory \"data\" param"))
  (with-temp-file (ellm--json-request-file)
    (setq-local coding-system-for-write 'utf-8)
    (insert (shell-maker--json-encode data))))

;; TODO: replace all ellm-crop-context
(cl-defun ellm-crop-context (&key context model command)
  "Crop CONTEXT to fit MODEL limits appending COMMAND."
  (unless model
    (error "Missing mandatory \"model\" param"))
  (if (map-elt model :ignore-context)
      (list (cons command nil))
    ;; Temporarily appending command for context
    ;; calculation and removing with butlast.
    (let ((complete-context (append context
                                    (list (cons command nil)))))
      (butlast
       (last complete-context
             (ellm--unpaired-length
              (if (functionp ellm-transmitted-context-length)
                  (funcall ellm-transmitted-context-length
                           model complete-context)
                ellm-transmitted-context-length)))
       1))))

(defun ellm--approximate-context-length (model context)
  "Approximate the CONTEXT length using MODEL."
  (let* ((approx-chars-per-token)
         (context-window)
         (original-length (floor (/ (length context) 2)))
         (context-length original-length))
    (if (map-elt model :token-width)
        (setq approx-chars-per-token
              (map-elt model :token-width))
      (error "Don't know %s's approximate token width" model))
    (if (map-elt model :context-window)
        (setq context-window
              (map-elt model :context-window))
      (error "Don't know %s's max tokens context limit" model))
    (while (> (ellm--num-tokens-from-context
               approx-chars-per-token context)
              context-window)
      ;; Keep dropping history until under context-window.
      (setq context (cdr context)))
    (setq context-length (floor (/ (length context) 2)))
    (unless (eq original-length context-length)
      (message "Warning: chatgpt-shell context clipped"))
    context-length))

;; Very rough token approximation loosely based on num_tokens_from_messages from:
;; https://github.com/openai/openai-cookbook/blob/main/examples/How_to_count_tokens_with_tiktoken.ipynb
(defun ellm--num-tokens-from-context (chars-per-token context)
  "Approximate number of tokens in CONTEXT using approximate CHARS-PER-TOKEN."
  (let ((num-tokens 0))
    (dolist (message context)
      (setq num-tokens (+ num-tokens chars-per-token))
      (setq num-tokens (+ num-tokens (/ (length (cdr message)) chars-per-token))))
    ;; Every reply is primed with <|start|>assistant<|message|>
    (setq num-tokens (+ num-tokens 3))
    num-tokens))

;; TODO: Move to shell-maker.
(defun ellm--fontify-source-block (quotes1-start quotes1-end lang
lang-start lang-end body-start body-end quotes2-start quotes2-end)
  "Fontify a source block.
Use QUOTES1-START QUOTES1-END LANG LANG-START LANG-END BODY-START
 BODY-END QUOTES2-START and QUOTES2-END."
  ;; Overlay beginning "```" with a copy block button.
  (ellm--overlay-put-all
   (make-overlay quotes1-start quotes1-end)
   'evaporate t
   'display
   (propertize "📋 "
               'pointer 'hand
               'keymap (shell-maker--make-ret-binding-map
                        (lambda ()
                          (interactive)
                          (kill-ring-save body-start body-end)
                          (message "Copied")))))
  ;; Hide end "```" altogether.
  (ellm--overlay-put-all
   (make-overlay quotes2-start quotes2-end)
   'evaporate t
   'invisible 'ellm)
  (unless (eq lang-start lang-end)
    (ellm--overlay-put-all
     (make-overlay lang-start lang-end)
     'evaporate t
     'face '(:box t))
    (ellm--overlay-put-all
     (make-overlay lang-end (1+ lang-end))
     'evaporate t
     'display "\n\n"))
  (let ((lang-mode (intern (concat (or
                                    (ellm--resolve-internal-language lang)
                                    (downcase (string-trim lang)))
                                   "-mode")))
        (string (buffer-substring-no-properties body-start body-end))
        (buf (if (and (boundp 'shell-maker--config)
                      shell-maker--config)
                 (shell-maker-buffer shell-maker--config)
               (current-buffer)))
        (pos 0)
        (props)
        (overlay)
        (propertized-text))
    (if (and (fboundp lang-mode)
             (provided-mode-derived-p lang-mode 'prog-mode))
        (progn
          (setq propertized-text
                (with-current-buffer
                    (get-buffer-create
                     (format " *ellm-fontification:%s*" lang-mode))
                  (let ((inhibit-modification-hooks nil)
                        (inhibit-message t))
                    (erase-buffer)
                    ;; Additional space ensures property change.
                    (insert string " ")
                    (funcall lang-mode)
                    (font-lock-ensure))
                  (buffer-string)))
          (while (< pos (length propertized-text))
            (setq props (text-properties-at pos propertized-text))
            (setq overlay (make-overlay (+ body-start pos)
                                        (+ body-start (1+ pos))
                                        buf))
            (ellm--overlay-put-all
             overlay
             'evaporate t
             'face (plist-get props 'face))
            (setq pos (1+ pos))))
      (ellm--overlay-put-all
       (make-overlay body-start body-end buf)
       'evaporate t
       'face 'font-lock-doc-markup-face))))

(defun ellm--overlay-put-all (overlay &rest props)
  "Set multiple properties on OVERLAY via PROPS."
  (unless (= (mod (length props) 2) 0)
    (error "Props missing a property or value"))
  (while props
    (overlay-put overlay (pop props) (pop props))))

(defun ellm--fontify-divider (start end)
  "Display text between START and END as a divider."
  (ellm--overlay-put-all
   (make-overlay start end
                 (if (and (boundp 'shell-maker--config)
                          shell-maker--config)
                     (shell-maker-buffer shell-maker--config)
                   (current-buffer)))
   'evaporate t
   'display
   (concat (propertize (concat (make-string (window-body-width) ? ) "")
                       'face '(:underline t)) "\n")))

;; TODO: Move to shell-maker.
(defun ellm--fontify-link (start end title-start title-end url-start url-end)
  "Fontify a markdown link.
Use START END TITLE-START TITLE-END URL-START URL-END."
  ;; Hide markup before
  (ellm--overlay-put-all
   (make-overlay start title-start)
   'evaporate t
   'invisible 'ellm)
  ;; Show title as link
  (ellm--overlay-put-all
   (make-overlay title-start title-end)
   'evaporate t
   'face 'link)
  ;; Make RET open the URL
  (define-key (let ((map (make-sparse-keymap)))
                (define-key map [mouse-1]
                            (lambda () (interactive)
                              (browse-url (buffer-substring-no-properties url-start url-end))))
                (define-key map (kbd "RET")
                            (lambda () (interactive)
                              (browse-url (buffer-substring-no-properties url-start url-end))))
                (ellm--overlay-put-all
                 (make-overlay title-start title-end)
                 'evaporate t
                 'keymap map)
                map)
              [remap self-insert-command] 'ignore)
  ;; Hide markup after
  (ellm--overlay-put-all
   (make-overlay title-end end)
   'evaporate t
   'invisible 'ellm))

;; TODO: Move to shell-maker.
(defun ellm--fontify-bold (start end text-start text-end)
  "Fontify a markdown bold.
Use START END TEXT-START TEXT-END."
  ;; Hide markup before
  (ellm--overlay-put-all
   (make-overlay start text-start)
   'evaporate t
   'invisible 'ellm)
  ;; Show title as bold
  (ellm--overlay-put-all
   (make-overlay text-start text-end)
   'evaporate t
   'face 'bold)
  ;; Hide markup after
  (ellm--overlay-put-all
   (make-overlay text-end end)
   'evaporate t
   'invisible 'ellm))

;; TODO: Move to shell-maker.
(defun ellm--fontify-header (start _end level-start level-end title-start title-end)
  "Fontify a markdown header.
Use START END LEVEL-START LEVEL-END TITLE-START TITLE-END."
  ;; Hide markup before
  (ellm--overlay-put-all
   (make-overlay start title-start)
   'evaporate t
   'invisible 'ellm)
  ;; Show title as header
  (ellm--overlay-put-all
   (make-overlay title-start title-end)
   'evaporate t
   'face
   (cond ((eq (- level-end level-start) 1)
          'org-level-1)
         ((eq (- level-end level-start) 2)
          'org-level-2)
         ((eq (- level-end level-start) 3)
          'org-level-3)
         ((eq (- level-end level-start) 4)
          'org-level-4)
         ((eq (- level-end level-start) 5)
          'org-level-5)
         ((eq (- level-end level-start) 6)
          'org-level-6)
         ((eq (- level-end level-start) 7)
          'org-level-7)
         ((eq (- level-end level-start) 8)
          'org-level-8)
         (t
          'org-level-1))))

;; TODO: Move to shell-maker.
(defun ellm--fontify-italic (start end text-start text-end)
  "Fontify a markdown italic.
Use START END TEXT-START TEXT-END."
  ;; Hide markup before
  (ellm--overlay-put-all
   (make-overlay start text-start)
   'evaporate t
   'invisible 'ellm)
  ;; Show title as italic
  (ellm--overlay-put-all
   (make-overlay text-start text-end)
   'evaporate t
   'face 'italic)
  ;; Hide markup after
  (ellm--overlay-put-all
   (make-overlay text-end end)
   'evaporate t
   'invisible 'ellm))

;; TODO: Move to shell-maker.
(defun ellm--fontify-strikethrough (start end text-start text-end)
  "Fontify a markdown strikethrough.
Use START END TEXT-START TEXT-END."
  ;; Hide markup before
  (ellm--overlay-put-all
   (make-overlay start text-start)
   'evaporate t
   'invisible 'ellm)
  ;; Show title as strikethrough
  (ellm--overlay-put-all
   (make-overlay text-start text-end)
   'evaporate t
   'face '(:strike-through t))
  ;; Hide markup after
  (ellm--overlay-put-all
   (make-overlay text-end end)
   'evaporate t
   'invisible 'ellm))

;; TODO: Move to shell-maker.
(defun ellm--fontify-inline-code (body-start body-end)
  "Fontify a source block.
Use QUOTES1-START QUOTES1-END LANG LANG-START LANG-END BODY-START
 BODY-END QUOTES2-START and QUOTES2-END."
  ;; Hide ```
  (ellm--overlay-put-all
   (make-overlay (1- body-start)
                 body-start)
   'evaporate t
   'invisible 'ellm)
  (ellm--overlay-put-all
   (make-overlay body-end
                 (1+ body-end))
   'evaporate t
   'invisible 'ellm)
  (ellm--overlay-put-all
   (make-overlay body-start body-end
                 (if (and (boundp 'shell-maker--config)
                          shell-maker--config)
                     (shell-maker-buffer shell-maker--config)
                   (current-buffer)))
   'evaporate t

   'face 'font-lock-doc-markup-face))

(defun ellm-rename-block-at-point ()
  "Rename block at point (perhaps a different language)."
  (interactive)
  (save-excursion
    (if-let ((block (ellm-markdown-block-at-point)))
        (if (map-elt block 'language)
            (perform-replace (map-elt block 'language)
                             (read-string "Name: " nil nil "") nil nil nil nil nil
                             (map-elt block 'language-start) (map-elt block 'language-end))
          (let ((new-name (read-string "Name: " nil nil "")))
            (goto-char (map-elt block 'language-start))
            (insert new-name)
            (ellm--put-source-block-overlays)))
      (user-error "No block at point"))))

(defun ellm-remove-block-overlays ()
  "Remove block overlays.  Handy for renaming blocks."
  (interactive)
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (delete-overlay overlay)))

(defun ellm-refresh-rendering ()
  "Refresh markdown rendering by re-applying to entire buffer."
  (interactive)
  (ellm--put-source-block-overlays))

;; TODO: Move to shell-maker.
(defun ellm--put-source-block-overlays ()
  "Put overlays for all source blocks."
  (let* ((source-blocks (ellm--source-blocks))
         (avoid-ranges (seq-map (lambda (block)
                                  (map-elt block 'body))
                                source-blocks)))
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (delete-overlay overlay))
    (when ellm-highlight-blocks
      (dolist (block source-blocks)
        (ellm--fontify-source-block
         (car (map-elt block 'start))
         (cdr (map-elt block 'start))
         (buffer-substring-no-properties (car (map-elt block 'language))
                                         (cdr (map-elt block 'language)))
         (car (map-elt block 'language))
         (cdr (map-elt block 'language))
         (car (map-elt block 'body))
         (cdr (map-elt block 'body))
         (car (map-elt block 'end))
         (cdr (map-elt block 'end)))))
    (when ellm-insert-dividers
      (dolist (divider (shell-maker--prompt-end-markers))
        (ellm--fontify-divider (car divider) (cdr divider))))
    (dolist (link (ellm--markdown-links avoid-ranges))
      (ellm--fontify-link
       (map-elt link 'start)
       (map-elt link 'end)
       (car (map-elt link 'title))
       (cdr (map-elt link 'title))
       (car (map-elt link 'url))
       (cdr (map-elt link 'url))))
    (dolist (header (ellm--markdown-headers avoid-ranges))
      (ellm--fontify-header
       (map-elt header 'start)
       (map-elt header 'end)
       (car (map-elt header 'level))
       (cdr (map-elt header 'level))
       (car (map-elt header 'title))
       (cdr (map-elt header 'title))))
    (dolist (bold (ellm--markdown-bolds avoid-ranges))
      (ellm--fontify-bold
       (map-elt bold 'start)
       (map-elt bold 'end)
       (car (map-elt bold 'text))
       (cdr (map-elt bold 'text))))
    (dolist (italic (ellm--markdown-italics avoid-ranges))
      (ellm--fontify-italic
       (map-elt italic 'start)
       (map-elt italic 'end)
       (car (map-elt italic 'text))
       (cdr (map-elt italic 'text))))
    (dolist (strikethrough (ellm--markdown-strikethroughs avoid-ranges))
      (ellm--fontify-strikethrough
       (map-elt strikethrough 'start)
       (map-elt strikethrough 'end)
       (car (map-elt strikethrough 'text))
       (cdr (map-elt strikethrough 'text))))
    (dolist (inline-code (ellm--markdown-inline-codes avoid-ranges))
      (ellm--fontify-inline-code
       (car (map-elt inline-code 'body))
       (cdr (map-elt inline-code 'body))))
    (when ellm-render-latex
      (require 'org)
      ;; Silence org-element warnings.
      (let ((major-mode 'org-mode))
        (save-excursion
          (dolist (range (ellm--invert-ranges
                          avoid-ranges
                          (point-min)
                          (point-max)))
            (org-format-latex
             (concat org-preview-latex-image-directory "chatgpt-shell")
             (car range) (cdr range)
             temporary-file-directory
             'overlays nil 'forbuffer org-preview-latex-default-process)))))))

(defun ellm--invert-ranges (ranges min max)
  "Invert a list of RANGES within the interval [MIN, MAX].
Each range is a cons of start and end integers."
  (let ((result nil)
        (start min))
    (dolist (range ranges)
      (when (< start (car range))
        (push (cons start (car range)) result))
      (setq start (cdr range)))
    (when (< start max)
      (push (cons start max) result))
    result))

;; TODO: Move to shell-maker.
(defun ellm--unpaired-length (length)
  "Expand LENGTH to include paired responses.

Each request has a response, so double LENGTH if set.

Add one for current request (without response).

If no LENGTH set, use 2048."
  (if length
      (1+ (* 2 length))
    2048))

(defun ellm-view-at-point ()
  "View prompt and output at point in a separate buffer."
  (interactive)
  (unless (derived-mode-p 'ellm-mode)
    (user-error "Not in a shell"))
  (let ((prompt-pos (save-excursion
                      (goto-char (process-mark
                                  (get-buffer-process (current-buffer))))
                      (point)))
        (buf))
    (save-excursion
      (when (>= (point) prompt-pos)
        (goto-char prompt-pos)
        (forward-line -1)
        (end-of-line))
      (let* ((items (ellm-openai--user-assistant-messages
                     (list (shell-maker--command-and-response-at-point))))
             (command (string-trim (or (map-elt (seq-first items) 'content) "")))
             (response (string-trim (or (map-elt (car (last items)) 'content) ""))))
        (setq buf (generate-new-buffer (if command
                                           (concat
                                            (buffer-name (current-buffer)) "> "
                                            ;; Only the first line of prompt.
                                            (seq-first (split-string command "\n")))
                                         (concat (buffer-name (current-buffer)) "> "
                                                 "(no prompt)"))))
        (when (seq-empty-p items)
          (user-error "Nothing to view"))
        (with-current-buffer buf
          (save-excursion
            (insert (propertize (or command "") 'face font-lock-doc-face))
            (when (and command response)
              (insert "\n\n"))
            (insert (or response "")))
          (ellm--put-source-block-overlays)
          (view-mode +1)
          (setq view-exit-action 'kill-buffer))))
    (switch-to-buffer buf)
    buf))

(defun ellm--extract-history (text prompt-regexp)
  "Extract all command and responses in TEXT with PROMPT-REGEXP."
  (ellm-openai--user-assistant-messages
   (shell-maker--extract-history text prompt-regexp)))

(defun ellm-run-command (command callback)
  "Run COMMAND list asynchronously and call CALLBACK function.

CALLBACK can be like:

\(lambda (success output)
  (message \"%s\" output))"
  (let* ((buffer (generate-new-buffer "*run command*"))
         (proc (apply #'start-process
                      (append `("exec" ,buffer) command))))
    (set-process-sentinel
     proc
     (lambda (proc _)
       (with-current-buffer buffer
         (funcall callback
                  (equal (process-exit-status proc) 0)
                  (buffer-string))
         (kill-buffer buffer))))))

;; TODO: Move to shell-maker.
(defun ellm--resolve-internal-language (language)
  "Resolve external LANGUAGE to internal.

For example \"elisp\" -> \"emacs-lisp\"."
  (when language
    (or (map-elt ellm-language-mapping
                 (downcase (string-trim language)))
        (when (intern (concat (downcase (string-trim language))
                              "-mode"))
          (downcase (string-trim language))))))

(defun ellm-block-action-at-point ()
  "Return t if block at point has an action.  nil otherwise."
  (let* ((source-block (ellm-markdown-block-at-point))
         (language (ellm--resolve-internal-language
                    (map-elt source-block 'language)))
         (actions (ellm--get-block-actions language)))
    actions
    (if actions
        actions
      (ellm--org-babel-command language))))

(defun ellm--get-block-actions (language)
  "Get block actions for LANGUAGE."
  (map-elt ellm-source-block-actions
           (ellm--resolve-internal-language
            language)))

(defun ellm--org-babel-command (language)
  "Resolve LANGUAGE to org babel command."
  (require 'ob)
  (when language
    (ignore-errors
      (or (require (intern (concat "ob-" (capitalize language))) nil t)
          (require (intern (concat "ob-" (downcase language))) nil t)))
    (let ((f (intern (concat "org-babel-execute:" language)))
          (f-cap (intern (concat "org-babel-execute:" (capitalize language)))))
      (if (fboundp f)
          f
        (if (fboundp f-cap)
            f-cap)))))

(defun ellm-execute-block-action-at-point ()
  "Execute block at point."
  (interactive)
  (if-let ((block (ellm-markdown-block-at-point)))
      (if-let ((actions (ellm--get-block-actions (map-elt block 'language)))
               (action (map-elt actions 'primary-action))
               (confirmation (map-elt actions 'primary-action-confirmation))
               (default-directory "/tmp"))
          (when (y-or-n-p confirmation)
            (funcall action (buffer-substring-no-properties
                             (map-elt block 'start)
                             (map-elt block 'end))))
        (if (and (map-elt block 'language)
                 (ellm--org-babel-command
                  (ellm--resolve-internal-language
                   (map-elt block 'language))))
            (ellm-execute-babel-block-action-at-point)
          (user-error "No primary action for %s blocks" (map-elt block 'language))))
    (user-error "No block at point")))

(defun ellm-edit-block-at-point ()
  "Execute block at point."
  (interactive)
  (error "Not yet supported")
  (if-let ((block (ellm-markdown-block-at-point)))
      (ellm--view-code :language (map-elt block 'language)
                                :code (buffer-substring-no-properties
                                       (map-elt block 'start)
                                       (map-elt block 'end))
                                :on-finished (lambda (code)
                                               (when-let ((inhibit-read-only t)
                                                          (success code))
                                                 (deactivate-mark)
                                                 (delete-region (map-elt block 'start)
                                                                (map-elt block 'end))
                                                 (insert "\n"
                                                         (string-trim code)
                                                         "\n")
                                                 (ellm--put-source-block-overlays))))
    (user-error "No block at point")))

(defun ellm-view-block-at-point ()
  "View code block at point (using language's major mode)."
  (interactive)
  (if-let ((block (ellm-markdown-block-at-point)))
      (ellm--view-code :language (map-elt block 'language)
                                :code (buffer-substring-no-properties
                                       (map-elt block 'start)
                                       (map-elt block 'end))
                                :on-finished #'identity)
    (user-error "No block at point")))

(cl-defun ellm--view-code (&key edit language code on-finished)
  "Open a temporary buffer for editing CODE in LANGUAGE major mode.
When done, invoke the FINISHED function with the resulting code.

ARGS:
- EDIT: To enable editing CODE in own buffer.
- LANGUAGE: A string with the language name.
- CODE: A string with the initial content of the buffer.
- ON-FINISHED: Invoked with saved changes, nil if cancelled."
  (let* ((block-buffer (current-buffer))
         (edit-buffer (generate-new-buffer (format "*%s code block*"
                                                   (if edit "edit" "view"))))
         (buffer-name (buffer-name edit-buffer))
         (language-mode (intern (concat (or
                                         (ellm--resolve-internal-language language)
                                         (downcase (string-trim language)))
                                        "-mode"))))
    (switch-to-buffer edit-buffer)
    (set-visited-file-name (make-temp-file "ellm-edit-block"))
    (rename-buffer buffer-name)
    (add-hook 'after-change-functions
              (lambda (_beg _end _len)
                (set-buffer-modified-p nil)) nil t)
    (funcall language-mode)
    (insert (or code ""))
    (set-buffer-modified-p nil)
    (if edit
        (progn
          (setq header-line-format
                (concat
                 " "
                 (propertize "C-c '" 'face 'help-key-binding)
                 " to Save. "
                 (propertize "C-c C-k" 'face 'help-key-binding)
                 " to Cancel and Discard."))
          (let ((local-map (make-sparse-keymap)))
            (define-key local-map (kbd "C-c '") (lambda ()
                                                  (interactive)
                                                  (let ((result (buffer-string)))
                                                    (with-current-buffer block-buffer
                                                      (funcall on-finished result))
                                                    (set-buffer-modified-p nil)
                                                    (kill-buffer edit-buffer))))
            (define-key local-map (kbd "C-c C-k") (lambda ()
                                                    (interactive)
                                                    (with-current-buffer block-buffer
                                                      (funcall on-finished nil))
                                                    (set-buffer-modified-p nil)
                                                    (kill-buffer edit-buffer)))
            (use-local-map local-map)))
      (setq header-line-format
            (concat
             " Press "
             (propertize "q" 'face 'help-key-binding)
             " to exit"))
      (let ((local-map (make-sparse-keymap)))
        (define-key local-map (kbd "q") (lambda ()
                                          (interactive)
                                          (quit-restore-window
                                           (get-buffer-window edit-buffer) 'kill)))
        (use-local-map local-map))
      (setq buffer-read-only t))
    (goto-char (point-min))))

(defun ellm--override-language-params (language params)
  "Override PARAMS for LANGUAGE if found in `ellm-babel-headers'."
  (if-let* ((overrides (map-elt ellm-babel-headers
                                language))
            (temp-dir (file-name-as-directory
                       (make-temp-file "ellm-" t)))
            (temp-file (concat temp-dir "source-block-" language)))
      (if (cdr (assq :file overrides))
          (append (list
                   (cons :file
                         (replace-regexp-in-string (regexp-quote "<temp-file>")
                                                   temp-file
                                                   (cdr (assq :file overrides)))))
                  (assq-delete-all :file overrides)
                  params)
        (append
         overrides
         params))
    params))

(defun ellm-execute-babel-block-action-at-point ()
  "Execute block as org babel."
  (interactive)
  (require 'ob)
  (if-let ((block (ellm-markdown-block-at-point)))
      (if-let* ((language (ellm--resolve-internal-language
                           (map-elt block 'language)))
                (babel-command (ellm--org-babel-command language))
                (lang-headers (intern
                               (concat "org-babel-default-header-args:" language)))
                (bound (fboundp babel-command))
                (default-directory "/tmp"))
          (when (y-or-n-p (format "Execute %s ob block?" (capitalize language)))
            (message "Executing %s block..." (capitalize language))
            (let* ((params (org-babel-process-params
                            (ellm--override-language-params
                             language
                             (org-babel-merge-params
                              org-babel-default-header-args
                              (and (boundp
                                    (intern
                                     (concat "org-babel-default-header-args:" language)))
                                   (eval (intern
                                          (concat "org-babel-default-header-args:" language)) t))))))
                   (output (progn
                             (when (get-buffer org-babel-error-buffer-name)
                               (kill-buffer (get-buffer org-babel-error-buffer-name)))
                             (funcall babel-command
                                      (buffer-substring-no-properties
                                       (map-elt block 'start)
                                       (map-elt block 'end)) params)))
                   (buffer))
              (if (and output (not (stringp output)))
                  (setq output (format "%s" output))
                (when (and (cdr (assq :file params))
                           (file-exists-p (cdr (assq :file params))))
                  (setq output (cdr (assq :file params)))))
              (if (and output (not (string-empty-p output)))
                  (progn
                    (setq buffer (get-buffer-create (format "*%s block output*" (capitalize language))))
                    (with-current-buffer buffer
                      (save-excursion
                        (let ((inhibit-read-only t))
                          (erase-buffer)
                          (setq output (when output (string-trim output)))
                          (if (file-exists-p output) ;; Output was a file.
                              ;; Image? insert image.
                              (if (member (downcase (file-name-extension output))
                                          '("jpg" "jpeg" "png" "gif" "bmp" "webp"))
                                  (progn
                                    (insert "\n")
                                    (insert-image (create-image output)))
                                ;; Insert content of all other file types.
                                (insert-file-contents output))
                            ;; Just text output, insert that.
                            (insert output))))
                      (view-mode +1)
                      (setq view-exit-action 'kill-buffer))
                    (message "")
                    (select-window (display-buffer buffer)))
                (if (get-buffer org-babel-error-buffer-name)
                    (select-window (display-buffer org-babel-error-buffer-name))
                  (setq buffer (get-buffer-create (format "*%s block output*" (capitalize language))))
                  (message "No output. Check %s blocks work in your .org files." language)))))
        (user-error "No primary action for %s blocks" (map-elt block 'language)))
    (user-error "No block at point")))

(defun ellm-eval-elisp-block-in-ielm (text)
  "Run elisp source in TEXT."
  (ellm-send-to-ielm-buffer text t))


(defun ellm-write-temp-file (content extension)
  "Create a temporary file with EXTENSION and write CONTENT to it.

Return the file path."
  (let* ((temp-dir (file-name-as-directory
                    (make-temp-file "ellm-" t)))
         (temp-file (concat temp-dir "source-block" extension)))
    (with-temp-file temp-file
      (insert content)
      (let ((inhibit-message t))
        (write-file temp-file)))
    temp-file))

(defun ellm--remove-compiled-file-names (filename text)
  "Remove lines starting with FILENAME in TEXT.

Useful to remove temp file names from compilation output when
compiling source blocks."
  (replace-regexp-in-string
   (rx-to-string `(: bol ,filename (one-or-more (not (any " "))) " ") " ")
   "" text))

(defun ellm--save-variables ()
  "Save variables across Emacs sessions."
  (setq-default ellm-system-prompt
                ellm-system-prompt)
  (with-temp-file (expand-file-name ".chatgpt-shell.el" ellm-root-path)
    (prin1 (list
            (cons 'ellm-system-prompt ellm-system-prompt)
            (cons 'ellm-system-prompt-resolved
                  (when (integerp ellm-system-prompt)
                    (nth ellm-system-prompt
                         ellm-system-prompts)))) (current-buffer))))

(with-eval-after-load 'ellm
  (ellm--load-variables))

(defun ellm--load-variables ()
  "Load variables across Emacs sessions."
  (with-temp-buffer
    (condition-case nil
      ;; Try to insert the contents of .chatgpt-shell.el
      (insert-file-contents (expand-file-name ".chatgpt-shell.el" ellm-root-path))
      (error
        ;; If an error happens, execute ellm--save-variables
        (ellm--save-variables)))
    (goto-char (point-min))
    (let ((vars (read (current-buffer))))
      (when (and (map-elt vars 'ellm-system-prompt)
                 (map-elt vars 'ellm-system-prompt-resolved)
                 (equal (map-elt vars 'ellm-system-prompt-resolved)
                        (nth (map-elt vars 'ellm-system-prompt)
                             ellm-system-prompts)))
        (setq ellm-system-prompt (map-elt vars 'ellm-system-prompt))))))

(defun ellm--flymake-context ()
  "Return flymake diagnostic context if available.  Nil otherwise."
  (when-let* ((point (point))
              (diagnostic (flymake-diagnostics (point)))
              (line-start (line-beginning-position))
              (line-end (line-end-position))
              (top-context-start (max (line-beginning-position -5) (point-min)))
              (top-context-end (max (line-beginning-position 1) (point-min)))
              (bottom-context-start (min (line-beginning-position 2) (point-max)))
              (bottom-context-end (min (line-beginning-position 7) (point-max)))
              (current-line (buffer-substring line-start line-end)))
    (list
     (cons :point (point))
     (cons :start top-context-start)
     (cons :end bottom-context-end)
     (cons :diagnostic (mapconcat #'flymake-diagnostic-text diagnostic "\n"))
     (cons :content (concat
                     (buffer-substring-no-properties top-context-start top-context-end)
                     (buffer-substring-no-properties line-start line-end)
                     " <--- issue is here\n"
                     (buffer-substring-no-properties bottom-context-start bottom-context-end))))))

(when-let ((flymake-context (ellm--flymake-context)))
  (set-mark (map-elt flymake-context :start))
  (goto-char (map-elt flymake-context :end)))

;;;###autoload
(defun ellm-fix-error-at-point ()
  "Fixes flymake error at point."
  (interactive)
  (if-let ((flymake-context (ellm--flymake-context))
           (progress-reporter (make-progress-reporter
                               (format "%s " (ellm--model-label))))
           (buffer (current-buffer))
           (prog-mode-p (derived-mode-p 'prog-mode)))
      (progn
        (ellm--fader-start-fading-region
         (save-excursion
           (goto-char (map-elt flymake-context :point))
           (line-beginning-position))
         (save-excursion
           (goto-char (map-elt flymake-context :point))
           (line-end-position)))
        (progress-reporter-update progress-reporter)
        (ellm-send-contextless-request
         :system-prompt "Fix the error highlighted in code and show the entire snippet rewritten with the fix.
Do not give explanations. Do not add comments.
Do not balance unbalanced brackets or parenthesis at beginning or end of text.
Do not wrap snippets in markdown blocks.\n\n"
         :query (concat (map-elt flymake-context :diagnostic) "\n\n"
                        "Code: \n\n"
                        (map-elt flymake-context :content))
         :streaming t
         :on-output (lambda (_chunk)
                      (progress-reporter-update progress-reporter))
         :on-success (lambda (output)
                       (with-current-buffer buffer
                         ;; In prog mode, remove unnecessary
                         ;; markdown blocks prior to insertion.
                         (when prog-mode-p
                           (setq output
                                 (ellm--remove-source-block-markers output)))
                         (ellm--fader-stop-fading)
                         (progress-reporter-done progress-reporter)
                         (ellm--pretty-smerge-insert
                          :text output
                          :start (map-elt flymake-context :start)
                          :end (map-elt flymake-context :end)
                          :buffer buffer)))
         :on-failure (lambda (output)
                       (with-current-buffer buffer
                         (ellm--fader-stop-fading)
                         (progress-reporter-done progress-reporter)
                         (when (not (string-empty-p (string-trim
                                                     (or output ""))))
                           (message (or output "failed")))))))
    (error "Nothing to fix")))

(defun ellm--region ()
  "Return region info (ensuring start is always at bol).

Of the form

\((:buffer . buffer)
 (:start . start)
 (:end . end)
 (:text . text))"
  (when (region-active-p)
    (let ((start (save-excursion
                   ;; Always select from beginning of line.
                   (goto-char (region-beginning))
                   (line-beginning-position)))
          (end (region-end)))
      ;; Barf trailing space from selection.
      (let ((text (buffer-substring-no-properties
                   start
                   end)))
        (when (string-match "[ \n\t]+$"
                            text)
          (setq end (- end (length (match-string 0 text))))))
      (list (cons :start start)
            (cons :end end)
            (cons :buffer (current-buffer))
            (cons :text (buffer-substring start end))))))

(cl-defun ellm-request-and-insert-merged-response (&key query
                                                                 context
                                                                 (buffer (current-buffer))
                                                                 (region (ellm--region))
                                                                 model-version
                                                                 system-prompt
                                                                 remove-block-markers
                                                                 on-iterate)
  "Send a contextless request (no history) and merge into BUFFER:

QUERY: Request query text.
CONTEXT: Any history context to include.
BUFFER (optional): Buffer to insert to or omit to insert to current buffer.
MODEL-VERSION (optional): Index from `ellm-models' or string.
SYSTEM-PROMPT (optional): As string."
  (unless query
    (error "Missing mandatory \"query\" param"))
  (unless region
    (error "No region selected"))
  (let ((progress-reporter (make-progress-reporter
                            (format "%s " (ellm--model-label)))))
    (ellm--fader-start-fading-region (map-elt region :start)
                                              (map-elt region :end))
    (progress-reporter-update progress-reporter)
    (ellm-post :context (append
                                  context
                                  (list (cons query nil)))
                        :system-prompt system-prompt
                        :version model-version
                        :streaming t
                        :on-output (lambda (_chunk)
                                     (progress-reporter-update progress-reporter))
                        :on-success (lambda (output)
                                      (with-current-buffer buffer
                                        (when remove-block-markers
                                          (setq output (ellm--remove-source-block-markers output)))
                                        (ellm--fader-stop-fading)
                                        (progress-reporter-done progress-reporter)
                                        (if (equal (string-trim output)
                                                   (string-trim (map-elt region :text)))
                                            (message "No change suggested")
                                          (let ((choice (ellm--pretty-smerge-insert
                                                         :text output
                                                         :start (map-elt region :start)
                                                         :end (map-elt region :end)
                                                         :buffer buffer
                                                         :iterate on-iterate)))
                                            (cond ((and on-iterate
                                                        (eq choice ?i))
                                                   (funcall on-iterate output))
                                                  ((eq choice ?n)
                                                   (set-mark (map-elt region :end))
                                                   (goto-char (map-elt region :start))))))))
                        :on-failure (lambda (output)
                                      (with-current-buffer buffer
                                        (ellm--fader-stop-fading)
                                        (progress-reporter-done progress-reporter)
                                        (when (not (string-empty-p (string-trim
                                                                    (or output ""))))
                                          (message (or output "failed"))))))))

(defun ellm--remove-source-block-markers (text)
  "Remove markdown code block markers TEXT."
  (replace-regexp-in-string
   (rx (optional "\n") bol "```" (zero-or-more (or alphanumeric "-" "+"))
       (zero-or-more space) eol (optional "\n"))
   "" text t))

;;;###autoload
(defun ellm-quick-insert(&optional context)
  "Request from minibuffer and insert response into current buffer.

Optionally include any CONTEXT to consider."
  (interactive)
  (let ((system-prompt "Follow my instruction and only my instruction.
Do not explain nor wrap in a markdown block.
Do not balance unbalanced brackets or parenthesis at beginning or end of text.
Write solutions in their entirety.")
        (query (read-string (format "%s (%s) insert: "
                                    (ellm--model-label)
                                    (ellm--model-short-version)))))
    (when (derived-mode-p 'prog-mode)
      (setq system-prompt (format "%s\nUse `%s` programming language."
                                  system-prompt
                                  (string-trim-right (symbol-name major-mode) "-mode"))))
    (if-let ((region (ellm--region)))
        (progn
          (setq query (concat query
                              "\n\n"
                              "Apply my instruction to:"
                              "\n\n"
                              (map-elt region :text)))
          (ellm-request-and-insert-merged-response
           :system-prompt system-prompt
           :query query
           :context context
           :remove-block-markers t
           :region region
           :on-iterate (lambda (output)
                         (set-mark (map-elt region :end))
                         (goto-char (map-elt region :start))
                         (ellm-quick-insert (append context
                                                             (list (cons query output)))))))
      (ellm-request-and-insert-response
       :streaming t
       :system-prompt system-prompt
       :query query))))

(defun ellm-mark-block ()
  "Mark current block in compose buffer."
  (interactive)
  (when-let ((block (ellm-markdown-block-at-point)))
    (set-mark (map-elt block 'end))
    (goto-char (map-elt block 'start))))

;; pretty smerge start

(cl-defun ellm--pretty-smerge-insert (&key text start end buffer iterate)
  "Insert TEXT, replacing content of START and END at BUFFER.

With non-nil ITERATE, ask user if they'd like to iterate further on change.

Return non-nil if either inserted or cancelled (for manual merge)."
  (unless (and text (stringp text))
    (error ":text is missing or not a string"))
  (unless (and buffer (bufferp buffer))
    (error ":buffer is missing or not a buffer"))
  (unless (and start (integerp start))
    (error ":start is missing or not an integer"))
  (unless (and end (integerp end))
    (error ":end is missing or not an integer"))
  (with-current-buffer buffer
    (let* ((needs-wrapping (not visual-line-mode))
           (orig-start (copy-marker start))
           (orig-end (copy-marker end))
           (orig-text (buffer-substring-no-properties orig-start
                                                      orig-end))
           (diff (ellm--pretty-smerge--make-merge-patch
                  :old-label "Before" :old orig-text
                  :new-label "After" :new text))
           (outcome))
      (delete-region orig-start orig-end)
      (when (looking-at-p "\n")
        (delete-char 1))
      (goto-char orig-start)
      (insert diff)
      (when needs-wrapping
        (visual-line-mode +1))
      (smerge-mode +1)
      (ignore-errors
        (smerge-prev))
      (ellm--pretty-smerge-mode +1)
      (condition-case nil
          (unwind-protect
              (progn
                (setq outcome
                      (if iterate
                          (read-char-choice (substitute-command-keys
                                             "Apply change? (\\`y' or \\`n') / Iterate? (\\`i'): ")
                                            '(?y ?n ?i))
                        (read-char-choice (substitute-command-keys
                                           "Apply change? (\\`y' or \\`n'): ")
                                          '(?y ?n))))
                (cond
                 ((eq outcome ?y)
                  (smerge-keep-lower)
                  (message "Applied"))
                 ((eq outcome ?n)
                  (smerge-keep-upper)
                  (message "Skipped"))
                 ((eq outcome ?i)
                  (smerge-keep-upper))
                 (t
                  (smerge-keep-upper)))
                (smerge-mode -1)
                outcome)
            (ellm--pretty-smerge-mode -1)
            (when needs-wrapping
              (visual-line-mode -1))
            outcome)
        (quit
         (ellm--pretty-smerge-mode -1)
         (when needs-wrapping
           (visual-line-mode -1))
         t)
        (error nil)))))

(cl-defun ellm--pretty-smerge--make-merge-patch (&key old new old-label new-label)
  "Write OLD and NEW to temporary files, run diff3, and return merge patch.
OLD-LABEL (optional): To display for old text.
NEW-LABEL (optional): To display for new text."
  (let ((base-file (make-temp-file "base"))
        (old-file (make-temp-file "old"))
        (new-file (make-temp-file "new")))
    (with-temp-file old-file
      (insert old)
      (unless (string-suffix-p "\n" old)
        (insert "\n")))
    (with-temp-file new-file
      (insert new)
      (unless (string-suffix-p "\n" new)
        (insert "\n")))
    (with-temp-buffer
      (let ((retval (call-process "diff3" nil t nil "-m" old-file base-file new-file)))
        (delete-file base-file)
        (delete-file old-file)
        (delete-file new-file)
        ;; 0: No differences or no conflicts.
        ;; 1: Merge conflicts.
        ;; 2: Error occurred.
        (when (= retval 2)
          (error (buffer-substring-no-properties (point-min)
                                                 (point-max))))
        (goto-char (point-min))
        (while (search-forward old-file nil t)
          (replace-match (or old-label "old")))
        (goto-char (point-min))
        (while (search-forward new-file nil t)
          (replace-match (or new-label "new")))
        (goto-char (point-min))
        (flush-lines "^|||||||")
        (buffer-substring-no-properties (point-min)
                                        (point-max))))))

(define-minor-mode ellm--pretty-smerge-mode
  "Minor mode to display overlays for conflict markers."
  :lighter " PrettySmerge"
  (if ellm--pretty-smerge-mode
      (progn
        (ellm--pretty-smerge--refresh)
        (add-hook 'after-change-functions
                  #'ellm--pretty-smerge--autodisable
                  nil t))
    (ellm--pretty-smerge-mode-remove--overlays)
    (remove-hook 'after-change-functions
                 #'ellm--pretty-smerge--autodisable
                 t)))

(defun ellm--pretty-smerge--autodisable (_beg _end _len)
  "Disable `ellm--pretty-smerge-mode' on edit."
  (ellm--pretty-smerge-mode -1)
  (remove-hook 'after-change-functions
               #'ellm--pretty-smerge--autodisable
               t))

(defun ellm--pretty-smerge--refresh ()
  "Apply overlays to conflict markers."
  (ellm--pretty-smerge-mode-remove--overlays)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (concat
             "^\\(<<<<<<<[ \t]*\\)" ;; begin marker
             "\\(.*\\)\n"           ;; begin label
             "\\(\\(?:.*\n\\)*?\\)"     ;; upper content
             "\\(=======\n\\)"      ;; maker
             "\\(\\(?:.*\n\\)*?\\)"     ;; lower content
             "\\(>>>>>>>[ \t]*\\)"  ;; end marker
             "\\(.*\\)\n")          ;; end label
            nil t)
      (let ((overlay (make-overlay (match-beginning 1)
                                   (match-end 2))))
        (overlay-put overlay 'category 'conflict-marker)
        (overlay-put overlay 'display
                     (concat (propertize (concat " " (match-string 2) " ")
                                         'face '(:inherit default :box t))
                             "\n"))
        (overlay-put overlay 'evaporate t))
      (let ((overlay (make-overlay (match-beginning 4)
                                   (match-end 4))))
        (overlay-put overlay 'category 'conflict-marker)
        (overlay-put overlay 'display
                     (concat "\n" (propertize (concat " " (match-string 7) " ")
                                              'face '(:inherit default :box t)) "\n\n"))
        (overlay-put overlay 'evaporate t))
      (let ((overlay (make-overlay (match-beginning 6)
                                   (match-end 7))))
        (overlay-put overlay 'category 'conflict-marker)
        (overlay-put overlay 'display "")
        (overlay-put overlay 'face 'warning)
        (overlay-put overlay 'evaporate t)))))

(defun ellm--pretty-smerge-mode-remove--overlays ()
  "Remove all conflict marker overlays."
  (remove-overlays (point-min) (point-max) 'category 'conflict-marker))

;; pretty smerge end

;; fader start

(defvar-local ellm--fader-timer nil
  "Timer object for animating the region.")

(defvar-local ellm--fader-overlays nil
  "List of overlays for the animated regions.")

(defun ellm--fader-start-fading-region (start end)
  "Animate the background color of the region between START and END."
  (deactivate-mark)
  (ellm--fader-stop-fading)
  (let ((colors (append (ellm--fader-palette)
                        (reverse (ellm--fader-palette)))))
    (dolist (ov ellm--fader-overlays) (delete-overlay ov))
    (setq ellm--fader-overlays (list (make-overlay start end)))
    (setq ellm--fader-timer
          (run-with-timer 0 0.01
                          (lambda ()
                            (let* ((color (pop colors)))
                              (if (and color
                                       ellm--fader-overlays)
                                  (progn
                                    (overlay-put (car ellm--fader-overlays) 'face `(:background ,color :extend t))
                                    (setq colors (append colors (list color))))
                                (ellm--fader-stop-fading))))))))

(defun ellm--fader-palette ()
  "Generate a gradient palette from the `region' face to the `default' face."
  (let* ((start-color (face-background 'region))
         (end-color (face-background 'default))
         (start-rgb (color-name-to-rgb start-color))
         (end-rgb (color-name-to-rgb end-color))
         (steps 50))
    (mapcar (lambda (step)
              (apply #'color-rgb-to-hex
                     (cl-mapcar (lambda (start end)
                                  (+ start (* step (/ (- end start) (1- steps)))))
                                start-rgb end-rgb)))
            (number-sequence 0 (1- steps)))))

(defun ellm--fader-start ()
  "Start animating the currently active region."
  (if (use-region-p)
      (progn
        (deactivate-mark)
        (ellm--fader-start-fading-region (region-beginning) (region-end)))
    (message "No active region")))

(defun ellm--fader-stop-fading ()
  "Stop animating and remove all overlays."
  (when ellm--fader-timer
    (cancel-timer ellm--fader-timer)
    (setq ellm--fader-timer nil))
  (dolist (ov ellm--fader-overlays)
    (delete-overlay ov))
  (setq ellm--fader-overlays nil))

;; fader end

(provide 'ellm)
;;; ellm.el ends here
