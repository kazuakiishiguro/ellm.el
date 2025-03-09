;;; ellm-ollama.el --- Ollama-specific logic  -*- lexical-binding: t -*-

;;; Commentary:

;; Adds Ollama specifics for `ellm'.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'let-alist)
(require 'shell-maker)
(require 'map)
(require 'seq)
(require 'subr-x)

;; Muffle warning about free variable.
(defvar ellm-models)
(defvar ellm-request-timeout)
(declare-function ellm-crop-context "ellm")
(declare-function ellm--make-chatgpt-url "ellm")

(cl-defun ellm-ollama-make-model (&key version
                                       short-version
                                       token-width
                                       context-window)
  "Create an Ollama model.
Set VERSION, SHORT-VERSION, TOKEN-WIDTH, CONTEXT-WINDOW and
VALIDATE-COMMAND handler."
  (unless version
    (error "Missing mandatory :version param"))
  (unless token-width
    (error "Missing mandatory :token-width param for %s" version))
  (unless context-window
    (error "Missing mandatory :context-window param for %s" version))
  (unless (integerp token-width)
    (error ":token-width must be an integer"))
  (unless (integerp context-window)
    (error ":context-window must be an integer"))
  `((:provider . "Ollama")
    (:label . "Ollama")
    (:version . ,version)
    (:short-version . ,short-version)
    (:token-width . ,token-width)
    (:context-window . ,context-window)
    (:handler . ellm-ollama--handle-ollama-command)
    (:filter . ellm-ollama--extract-ollama-response)
    (:payload . ellm-ollama-make-payload)
    (:url . ellm-ollama--make-url)
    (:validate-command . ellm-ollama--validate-command)))

(defcustom ellm-base-url "http://localhost:11434"
  "Base URL for Ollama API."
  :type 'string
  :safe #'stringp
  :group 'ellm)

(defun ellm-ollama-models ()
  "Build a list of Ollama LLM models available."
  ;; Context windows have been verified via ollama show <model> as of
  ;; 11/26/2024.
  (list (ellm-ollama-make-model
	 :version "qwq"
	 :token-width 4
	 :context-window 131072)))

(defun ellm-ollama--fetch-model-versions ()
  (mapcar (lambda (model)
            (string-remove-suffix ":latest" (map-elt model 'name)))
          (map-elt (shell-maker--json-parse-string
                    (map-elt (shell-maker-make-http-request
                              :async nil
                              :url (concat ellm-base-url "/api/tags"))
                             :output))
                   'models)))

(defun ellm-ollama--parse-token-width (quantization)
  (when (string-match "^[FQ]\\([1-9][0-9]*\\)" quantization)
    (string-to-number (match-string 1 quantization))))

(defun ellm-ollama--fetch-model (version)
  (let* ((data (shell-maker--json-parse-string
                (map-elt (shell-maker-make-http-request
                          :async nil
                          :url (concat ellm-base-url "/api/show")
                          :data `((model . ,version)))
                         :output)))
         (token-width (let-alist data
                        (ellm-ollama--parse-token-width
                         .details.quantization_level)))
         ;; The context length key depends on the name of the model. For qwen2,
         ;; it's at: model_info -> qwen2.context_length.
         (context-window (cdr (cl-find-if (lambda (cell)
                                            (string-suffix-p "context_length" (symbol-name (car cell))))
                                          (map-elt data 'model_info)))))
    (ellm-ollama-make-model
     :version version
     :token-width token-width
     :context-window context-window)))

(cl-defun ellm-ollama-load-models (&key override)
  "Query ollama for the locally installed models and add them to
`ellm-models' unless a model with the same name is
already present. By default, replace the ollama models in
`ellm-models' locally installed ollama models. When
OVERRIDE is non-nil (interactively with a prefix argument),
replace all models with locally installed ollama models."
  (interactive (list :override current-prefix-arg))
  (let* ((ollama-predicate (lambda (model)
                             (string= (map-elt model :provider) "Ollama")))
         ;; Find the index of the first ollama model so that the new ones will
         ;; be placed in the same part of the list.
         (ollama-index (or (cl-position-if ollama-predicate ellm-models)
                           (length ellm-models))))
    (setq ellm-models (and (not override)
                                    (cl-remove-if ollama-predicate ellm-models)))
    (let* ((existing-ollama-versions (mapcar (lambda (model)
                                               (map-elt model :version))
                                             (cl-remove-if-not ollama-predicate
                                                               ellm-models)))
           (new-ollama-versions (cl-remove-if (lambda (version)
                                                (member version existing-ollama-versions))
                                              (ellm-ollama--fetch-model-versions)))
           (new-ollama-models (mapcar #'ellm-ollama--fetch-model new-ollama-versions)))
      (setq ellm-models
            (append (seq-take ellm-models ollama-index)
                    new-ollama-models
                    (seq-drop ellm-models ollama-index)))
      (message "Added %d ollama model(s); kept %d existing ollama model(s)"
               (length new-ollama-models)
               (length existing-ollama-versions)))))

(cl-defun ellm-ollama--handle-ollama-command (&key model command context shell settings)
  "Handle Ollama shell COMMAND (prompt) using MODEL, CONTEXT, SHELL, and SETTINGS."
  (shell-maker-make-http-request
   :async t
   :url (ellm-ollama--make-url :model model
                                           :settings settings)
   :data (ellm-ollama-make-payload :model model
                                                 :context
                                                 (append
                                                  context
                                                  (list (cons command nil)))
                                                :settings settings)
   :filter #'ellm-ollama--extract-ollama-response
   :timeout ellm-request-timeout
   :shell shell))

(cl-defun ellm-ollama--make-url (&key _command _model _settings)
  "Create the API URL using MODEL and SETTINGS."
  (concat ellm-base-url
          "/api/chat"))

(defun ellm-ollama--extract-ollama-response (raw-response)
  "Extract Claude response from RAW-RESPONSE."
  (if-let* ((whole (shell-maker--json-parse-string raw-response))
            (response (let-alist whole
                        .response)))
      response
    (if-let ((chunks (string-split raw-response "\n")))
        (let ((response))
          (mapc (lambda (chunk)
                  (let-alist (shell-maker--json-parse-string chunk)
                    (unless (string-empty-p .message.content)
                      (setq response (concat response .message.content)))))
                chunks)
          (or response raw-response))
      raw-response)))

(cl-defun ellm-ollama-make-payload (&key model context settings)
  "Create the API payload using MODEL CONTEXT and SETTINGS."
  (unless (map-elt model :version)
    (error "Missing mandatory :version param"))
  (append
   `((model . ,(map-elt model :version))
     (messages . ,(vconcat (ellm-ollama--make-messages
                            :system-prompt (map-elt settings :system-prompt)
                            :context context))))
   (when (map-elt settings :temperature)
     `((temperature . ,(map-elt settings :temperature))))
   (when (map-elt settings :streaming)
     `((stream . t)))))

(cl-defun ellm-ollama--make-messages (&key model system-prompt prompt prompt-url context)
  "Create messages using MODEL.

SYSTEM-PROMPT: string.

PROMPT: string.

PROMPT-URL: string.

CONTEXT: Excludes PROMPT."
  (when prompt-url
    (setq prompt-url (ellm--make-chatgpt-url prompt-url)))
  (vconcat
   (when system-prompt
     `(((role . "system")
        (content . ,system-prompt))))
   (when context
     (ellm-openai--user-assistant-messages
      (if model
          (ellm-crop-context
           :model model
           :command prompt
           :context context)
        context)))
   (when (or prompt
             prompt-url)
     `(((role . "user")
        (content . ,(vconcat
                     (append
                      (when prompt
                        `(((type . "text")
                           (text . ,prompt))))
                      (when prompt-url
                        `(((type . "image_url")
                           (image_url . ,prompt-url))))))))))))

(defun ellm-ollama--validate-command (_command model _settings)
  "Return error string if command/setup isn't valid."
  (unless (seq-contains-p (ellm-ollama--fetch-model-versions)
                          (map-elt model :version))
    (format "  Local model \"%s\" not found.

  Try installing from the command line via:

    %% ollama pull %s

  Check out the [Ollama CLI reference](https://github.com/ollama/ollama?tab=readme-ov-file#cli-reference)

  Alternatively, fetch available models from Emacs via:

    M-x ellm-ollama-load-models

  Then swap active model with:

    M-x ellm-swap-system-prompt"
            (map-elt model :version)
            (map-elt model :version))))


(defun ellm-openai--user-assistant-messages (history)
  "Convert HISTORY to ChatGPT format.

Sequence must be a vector for json serialization.

For example:

 [
   ((role . \"user\") (content . \"hello\"))
   ((role . \"assistant\") (content . \"world\"))
 ]"
  (let ((result))
    (mapc
     (lambda (item)
       (when (car item)
         (push (list (cons 'role "user")
                     (cons 'content (car item))) result))
       (when (cdr item)
         (push (list (cons 'role "assistant")
                     (cons 'content (cdr item))) result)))
     history)
    (nreverse result)))


(provide 'ellm-ollama)

;;; ellm-ollama.el ends here
