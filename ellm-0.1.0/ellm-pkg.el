;;; ellm-pkg.el --- Package descriptor for ellm  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Kazuaki Ishiguro

;; Author: Kazuaki Ishiguro
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, tools, processes
;; URL: https://github.com/kazuakiishiguro/ellm.el

;;; Commentary:
;; Package descriptor for ellm.el

;;; Code:
(define-package "ellm" "0.1.0"
  "Interactive coding assistant with local LLMs"
  '((emacs "27.1"))
  :authors '(("Kazuaki Ishiguro"))
  :maintainer '("Kazuaki Ishiguro")
  :keywords '("convenience" "tools" "processes")
  :url "https://github.com/kazuakiishiguro/ellm.el")

(provide 'ellm-pkg)
;;; ellm-pkg.el ends here