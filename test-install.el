;;; test-install.el --- Test installation of ellm  -*- lexical-binding: t; -*-

;; Test installation of ellm.el

;;; Commentary:
;; This file is used to test that ellm can be properly installed.

;;; Code:

(require 'package)
(package-initialize)

;; Test the main file header
(message "Testing package header in ellm.el...")
(with-temp-buffer
  (insert-file-contents-literally "ellm.el")
  (condition-case err
      (progn
        (package-buffer-info)
        (message "✓ Package header correctly identified in ellm.el"))
    (error
     (message "✗ Package header validation failed: %s" (error-message-string err))
     (kill-emacs 1))))

;; Test package directory installation simulation
(message "Testing package directory installation simulation...")
(let ((pkg-dir "ellm-0.1.0"))
  (when (file-exists-p pkg-dir)
    (condition-case err
        (progn
          (message "✓ Package directory structure exists")
          ;; Check if the main file header is correctly detected by package.el
          (with-temp-buffer
            (insert-file-contents-literally (expand-file-name "ellm.el" pkg-dir))
            (package-buffer-info)
            (message "✓ Package header correctly identified in directory")))
      (error
       (message "✗ Package directory validation failed: %s" (error-message-string err))
       (kill-emacs 1)))))

(message "✓ All package installation tests passed")
(kill-emacs 0)

;;; test-install.el ends here