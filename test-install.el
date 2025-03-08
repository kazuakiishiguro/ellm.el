;;; test-install.el --- Test installation of ellm.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple script to verify that ellm can be required correctly

;;; Code:

(require 'ellm)
(message "Successfully loaded ellm.el")

(provide 'test-install)
;;; test-install.el ends here