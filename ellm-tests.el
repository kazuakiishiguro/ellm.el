;;; ellm-tests.el --- Tests for ellm.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: 
;; URL: https://github.com/yourusername/ellm.el

;;; Commentary:

;; Tests for ellm.el

;;; Code:

(require 'ert)
(require 'ellm)
(require 'cl-lib)

(ert-deftest ellm-test-buffer-creation ()
  "Test that ellm creates a buffer with the correct name."
  (when (get-buffer ellm-buffer-name)
    (kill-buffer ellm-buffer-name))
  (should (not (get-buffer ellm-buffer-name)))
  (cl-letf (((symbol-function 'switch-to-buffer-other-window) #'ignore)
            ((symbol-function 'ellm-fetch-models) #'ignore))
    (ellm)
    (should (get-buffer ellm-buffer-name))
    (should (with-current-buffer ellm-buffer-name
              (eq major-mode 'ellm-mode)))))

(ert-deftest ellm-test-prompt-insertion ()
  "Test that ellm-insert-prompt correctly inserts a prompt."
  (when (get-buffer ellm-buffer-name)
    (kill-buffer ellm-buffer-name))
  (cl-letf (((symbol-function 'switch-to-buffer-other-window) #'ignore)
            ((symbol-function 'ellm-fetch-models) #'ignore))
    (ellm)
    (with-current-buffer ellm-buffer-name
      (goto-char (point-min))
      (should (search-forward "You: " nil t))
      (should (get-text-property (1- (point)) 'ellm-prompt-start)))))

(ert-deftest ellm-test-find-prompt-start ()
  "Test that ellm-find-prompt-start finds the correct position."
  (when (get-buffer ellm-buffer-name)
    (kill-buffer ellm-buffer-name))
  (cl-letf (((symbol-function 'switch-to-buffer-other-window) #'ignore)
            ((symbol-function 'ellm-fetch-models) #'ignore))
    (ellm)
    (with-current-buffer ellm-buffer-name
      (let ((start-pos (ellm-find-prompt-start)))
        (should start-pos)
        (should (get-text-property start-pos 'ellm-prompt-start))))))

(ert-deftest ellm-test-clear-buffer ()
  "Test that ellm-clear-buffer clears the buffer and adds a new prompt."
  (when (get-buffer ellm-buffer-name)
    (kill-buffer ellm-buffer-name))
  (cl-letf (((symbol-function 'switch-to-buffer-other-window) #'ignore)
            ((symbol-function 'ellm-fetch-models) #'ignore))
    (ellm)
    (with-current-buffer ellm-buffer-name
      (let ((inhibit-read-only t))
        (insert "Test content that should be cleared"))
      (ellm-clear-buffer)
      (should (= (point-min) 1))
      (should (search-forward "You: " nil t))
      (should (get-text-property (1- (point)) 'ellm-prompt-start)))))

(ert-deftest ellm-test-model-selection ()
  "Test model selection functionality."
  (let ((ellm-available-models '("model1" "model2" "model3")))
    (cl-letf (((symbol-function 'completing-read) 
               (lambda (prompt collection &rest _) 
                 (should (string= prompt "Select model: "))
                 (should (equal collection '("model1" "model2" "model3")))
                 "model2")))
      (ellm-select-model)
      (should (string= ellm-model "model2")))))

(ert-deftest ellm-test-history-management ()
  "Test that history is properly maintained."
  (let ((ellm-history nil)
        (ellm-history-max-entries 3)
        (ellm-current-request nil))
    ;; Mock the url-retrieve function to avoid actual network calls
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback &rest _) 
                 (should (string-match-p "/api/generate" url))
                 nil)))
      
      ;; Add three prompts
      (ellm-send-request "prompt1")
      (ellm-send-request "prompt2")
      (ellm-send-request "prompt3")
      
      ;; Check history length and order
      (should (= (length ellm-history) 3))
      (should (string= (cdr (nth 0 ellm-history)) "prompt3"))
      (should (string= (cdr (nth 1 ellm-history)) "prompt2"))
      (should (string= (cdr (nth 2 ellm-history)) "prompt1"))
      
      ;; Add one more prompt to test max entries
      (ellm-send-request "prompt4")
      
      ;; Check that oldest entry was removed
      (should (= (length ellm-history) 3))
      (should (string= (cdr (nth 0 ellm-history)) "prompt4"))
      (should (string= (cdr (nth 1 ellm-history)) "prompt3"))
      (should (string= (cdr (nth 2 ellm-history)) "prompt2"))
      (should (not (cl-find "prompt1" ellm-history 
                            :test (lambda (p e) (string= p (cdr e)))))))))

(provide 'ellm-tests)
;;; ellm-tests.el ends here