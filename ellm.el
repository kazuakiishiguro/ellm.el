;;; ellm.el --- Interact with local Ollama LLM server -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: 
;; URL: https://github.com/yourusername/ellm.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (markdown-mode "2.3"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; An Emacs package to interact with a local Ollama LLM server.
;; Inspired by chatgpt-shell (https://github.com/xenodium/chatgpt-shell).

;;; Code:

(require 'json)
(require 'markdown-mode)
(require 'url)
(require 'url-util)
(require 'seq)
(require 'subr-x)

(defgroup ellm nil
  "Interact with local Ollama LLM server."
  :group 'communication)

(defcustom ellm-model "llama3"
  "Default Ollama model to use."
  :type 'string
  :group 'ellm)

(defcustom ellm-available-models nil
  "List of available Ollama models.
If nil, will be populated by calling the API."
  :type '(repeat string)
  :group 'ellm)

(defcustom ellm-base-url "http://localhost:11434"
  "Base URL for Ollama API."
  :type 'string
  :group 'ellm)

(defcustom ellm-history-max-entries 50
  "Maximum number of entries to keep in history."
  :type 'integer
  :group 'ellm)

(defcustom ellm-display-function #'switch-to-buffer-other-window
  "Function used to display the Ollama buffer."
  :type 'function
  :group 'ellm)

(defcustom ellm-markdown-rendering t
  "Whether to render markdown in responses."
  :type 'boolean
  :group 'ellm)

(defcustom ellm-spinner-type 'progress-bar
  "Spinner type to use while waiting for responses."
  :type 'symbol
  :group 'ellm)

(defvar ellm-buffer-name "*ellm*"
  "Name of the buffer used for Ollama interactions.")

(defvar ellm-current-request nil
  "Current active request to the Ollama API.")

(defvar ellm-request-headers
  '(("Content-Type" . "application/json"))
  "Headers sent with each Ollama API request.")

(defvar ellm-history '()
  "History of prompts and responses.")

(defvar ellm-spinner nil
  "Spinner for indicating activity.")

(defvar ellm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'ellm-send)
    (define-key map (kbd "C-c C-c") #'ellm-send)
    (define-key map (kbd "C-c C-k") #'ellm-cancel)
    (define-key map (kbd "C-c C-q") #'ellm-quit)
    (define-key map (kbd "C-c C-n") #'ellm-next-prompt)
    (define-key map (kbd "C-c C-p") #'ellm-previous-prompt)
    (define-key map (kbd "C-c C-m") #'ellm-select-model)
    (define-key map (kbd "C-c C-r") #'ellm-clear-buffer)
    map)
  "Keymap for `ellm-mode'.")

;;;###autoload
(defun ellm ()
  "Start an interactive session with Ollama LLM."
  (interactive)
  (let ((buffer (get-buffer-create ellm-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'ellm-mode)
        (ellm-mode))
      (ellm-ensure-models)
      (goto-char (point-max)))
    (funcall ellm-display-function buffer)
    (ellm-insert-prompt)))

;;;###autoload
(defun ellm-region (begin end)
  "Send the region from BEGIN to END to Ollama and display the response."
  (interactive "r")
  (let ((text (buffer-substring-no-properties begin end)))
    (ellm)
    (with-current-buffer (get-buffer ellm-buffer-name)
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert text)
        (ellm-send)))))

;;;###autoload
(defun ellm-buffer ()
  "Send the current buffer content to Ollama and display the response."
  (interactive)
  (ellm-region (point-min) (point-max)))

;;;###autoload
(defun ellm-git-commit ()
  "Generate a git commit message from staged changes."
  (interactive)
  (let ((diff (shell-command-to-string "git diff --cached")))
    (when (string-empty-p diff)
      (user-error "No staged changes found"))
    (ellm)
    (with-current-buffer (get-buffer ellm-buffer-name)
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert "Generate a concise, meaningful git commit message for the following changes:\n\n")
        (insert diff)
        (ellm-send)))))

(defun ellm-ensure-models ()
  "Make sure we have a list of available models."
  (when (null ellm-available-models)
    (ellm-fetch-models)))

(defun ellm-fetch-models ()
  "Fetch the list of available models from the Ollama API."
  (let ((url-request-method "GET")
        (url-request-extra-headers ellm-request-headers))
    (url-retrieve
     (concat ellm-base-url "/api/tags")
     (lambda (status)
       (if-let ((error-status (plist-get status :error)))
           (message "Failed to fetch models: %s" error-status)
         (let* ((json-object-type 'hash-table)
                (json-array-type 'list)
                (json-key-type 'symbol)
                (json-data (progn
                             (goto-char url-http-end-of-headers)
                             (json-read)))
                (models (mapcar (lambda (model)
                                  (gethash 'name model))
                                (gethash 'models json-data))))
           (setq ellm-available-models models)))))))

(defun ellm-select-model ()
  "Select an Ollama model to use."
  (interactive)
  (ellm-ensure-models)
  (if (null ellm-available-models)
      (message "No models available. Is Ollama running?")
    (let ((model (completing-read "Select model: " ellm-available-models nil t)))
      (setq ellm-model model)
      (message "Selected model: %s" model))))

(defun ellm-insert-prompt ()
  "Insert a prompt at the end of the buffer."
  (with-current-buffer (get-buffer-create ellm-buffer-name)
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (unless (bolp) (insert "\n"))
      (insert "You: ")
      (ellm-mark-prompt-start)
      (insert "\n")
      ;; Make the prompt area writable
      (let ((input-start (point)))
        (add-text-properties input-start (1+ input-start)
                             '(read-only nil rear-nonsticky t))))))

(defun ellm-mark-prompt-start ()
  "Mark the start of the prompt with a text property."
  (put-text-property (point) (1+ (point)) 'ellm-prompt-start t))

(defun ellm-find-prompt-start ()
  "Find the start of the current prompt."
  (save-excursion
    (goto-char (point-max))
    (while (and (not (bobp))
                (not (get-text-property (point) 'ellm-prompt-start)))
      (backward-char 1))
    (if (get-text-property (point) 'ellm-prompt-start)
        (point)
      (point-min))))

(defun ellm-send ()
  "Send the current prompt to Ollama."
  (interactive)
  (let* ((prompt-start (ellm-find-prompt-start))
         (content (buffer-substring-no-properties 
                  (+ prompt-start 5) ; Skip "You: "
                  (point-max)))
         (inhibit-read-only t))
    (when (string-empty-p (string-trim content))
      (user-error "Cannot send empty prompt"))
    (with-current-buffer (get-buffer-create ellm-buffer-name)
      (goto-char (point-max))
      (insert "\n\nOllama: ")
      (when (require 'spinner nil t)
        (setq ellm-spinner (spinner-create ellm-spinner-type))
        (spinner-start ellm-spinner))
      (ellm-send-request content)
      (insert "\n")
      ;; Insert a new prompt for the next message
      (ellm-insert-prompt))))

(defun ellm-cancel ()
  "Cancel the current request."
  (interactive)
  (when (and ellm-current-request
             (process-live-p ellm-current-request))
    (delete-process ellm-current-request)
    (setq ellm-current-request nil)
    (when (and (boundp 'ellm-spinner)
               ellm-spinner)
      (spinner-stop ellm-spinner)
      (setq ellm-spinner nil))
    (message "Request canceled")))

(defun ellm-send-request (content)
  "Send CONTENT as a request to the Ollama API."
  (let* ((url-request-method "POST")
         (url-request-extra-headers ellm-request-headers)
         (url-request-data
          (json-encode
           `((model . ,ellm-model)
             (prompt . ,content)
             (stream . t))))
         (buf (current-buffer))
         (response-start (point)))
    (push `(prompt . ,content) ellm-history)
    (when (> (length ellm-history) ellm-history-max-entries)
      (setq ellm-history (seq-take ellm-history ellm-history-max-entries)))
    (setq ellm-current-request
          (url-retrieve
           (concat ellm-base-url "/api/generate")
           (lambda (status)
             (ellm-handle-response status buf response-start))
           nil t t))))

(defun ellm-handle-response (status buffer response-start)
  "Handle response from Ollama API with STATUS in BUFFER starting at RESPONSE-START."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((json-object-type 'hash-table)
            (json-array-type 'list)
            (json-key-type 'symbol)
            (inhibit-read-only t)
            (response-text ""))
        (goto-char url-http-end-of-headers)
        (while (not (eobp))
          (let* ((line (buffer-substring-no-properties (point) (line-end-position)))
                 (json-data (condition-case nil
                                (json-read-from-string line)
                              (error nil))))
            (when json-data
              (let ((response (gethash 'response json-data "")))
                (setq response-text (concat response-text response))
                (with-current-buffer buffer
                  (save-excursion
                    (goto-char (point-max))
                    (insert response))))))
          (forward-line 1))
        (when (and (boundp 'ellm-spinner)
                   ellm-spinner)
          (spinner-stop ellm-spinner)
          (setq ellm-spinner nil))
        (when (and ellm-markdown-rendering
                   (not (string-empty-p response-text)))
          (with-current-buffer buffer
            (save-excursion
              (let ((response-end (point-max)))
                (when (> response-end response-start)
                  (ellm-render-markdown response-start response-end))))))
        (push `(response . ,response-text) ellm-history)))))

(defun ellm-render-markdown (begin end)
  "Render markdown in region from BEGIN to END."
  (let ((overlay (make-overlay begin end)))
    (overlay-put overlay 'ellm-markdown t)
    (font-lock-ensure begin end)
    (markdown-fontify-region begin end)))

(defun ellm-next-prompt ()
  "Move to the next prompt."
  (interactive)
  (let ((pos (point)))
    (goto-char (point-min))
    (while (and (not (eobp))
                (or (not (get-text-property (point) 'ellm-prompt-start))
                    (<= (point) pos)))
      (forward-char 1))
    (if (get-text-property (point) 'ellm-prompt-start)
        (forward-char 5) ; Move past "You: "
      (goto-char pos)
      (message "No next prompt"))))

(defun ellm-previous-prompt ()
  "Move to the previous prompt."
  (interactive)
  (let ((pos (point)))
    (goto-char pos)
    (while (and (not (bobp))
                (not (get-text-property (point) 'ellm-prompt-start)))
      (backward-char 1))
    (if (get-text-property (point) 'ellm-prompt-start)
        (progn
          (backward-char 1) ; Move back from the current marker
          (while (and (not (bobp))
                      (not (get-text-property (point) 'ellm-prompt-start)))
            (backward-char 1))
          (if (get-text-property (point) 'ellm-prompt-start)
              (forward-char 5) ; Move past "You: "
            (goto-char pos)
            (message "No previous prompt")))
      (goto-char pos)
      (message "No previous prompt"))))

(defun ellm-clear-buffer ()
  "Clear the ellm buffer and start fresh."
  (interactive)
  (with-current-buffer (get-buffer-create ellm-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (ellm-insert-prompt))))

(defun ellm-protect-prompt (_begin _end _length)
  "Prevent modification of the buffer before the prompt.
Arguments _BEGIN _END _LENGTH are ignored."
  (let ((prompt-start (ellm-find-prompt-start)))
    (when prompt-start
      (put-text-property (point-min) 
                         (+ prompt-start 5) ; "You: " length
                         'read-only t)
      (put-text-property (+ prompt-start 5) 
                         (+ prompt-start 6) 
                         'rear-nonsticky t))))

(defun ellm-quit ()
  "Quit the Ollama interaction buffer."
  (interactive)
  (ellm-cancel)
  (quit-window))

(define-derived-mode ellm-mode fundamental-mode "ELLM"
  "Major mode for interacting with Ollama LLM."
  (setq buffer-read-only t)
  (when (require 'markdown-mode nil t)
    (setq-local markdown-fontify-code-blocks-natively t))
  ;; Allow typing after the prompt
  (add-hook 'after-change-functions #'ellm-protect-prompt nil t))

(provide 'ellm)
;;; ellm.el ends here