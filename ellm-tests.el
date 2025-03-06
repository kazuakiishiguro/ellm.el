;;; ellm-tests.el --- Tests for ellm.el  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for ellm.el using ERT framework

;;; Code:

(require 'ert)
(require 'ellm)

;; Basic test
(ert-deftest ellm-test-simple ()
  "A simple test to verify ERT is working."
  (should (equal 1 1)))

;; Test ellm--get-endpoint
(ert-deftest ellm-test-get-endpoint ()
  "Test that ellm--get-endpoint returns the correct endpoint."
  (let ((ellm-server-type "local-llama-cpp")
        (ellm-endpoint-config '(("local-llama-cpp" . "http://localhost:1234/v1/chat/completions"))))
    (should (string= (ellm--get-endpoint) "http://localhost:1234/v1/chat/completions"))))

;; Test ellm--get-api-key
(ert-deftest ellm-test-get-api-key ()
  "Test that ellm--get-api-key handles different types of API key configurations."
  (let ((ellm-api-key "test-key"))
    (should (string= (ellm--get-api-key) "test-key")))
  
  (let ((ellm-api-key (lambda () "lambda-key")))
    (should (string= (ellm--get-api-key) "lambda-key")))
  
  (let ((ellm-api-key nil))
    (should (null (ellm--get-api-key)))))

;; Test ellm--get-payload-for-model
(ert-deftest ellm-test-get-payload-for-model ()
  "Test that ellm--get-payload-for-model correctly constructs the API payload."
  (let ((ellm-model-parameters '(("test-model" . ((temperature . 0.5) (max_tokens . 1000))))))
    (let ((payload (ellm--get-payload-for-model "test-model" '((("role" . "user") ("content" . "hello"))))))
      (should (equal (alist-get "model" payload nil nil #'string-equal) "test-model"))
      (should (equal (alist-get 'temperature payload) 0.5))
      (should (equal (alist-get 'max_tokens payload) 1000)))))

;; Test ellm--build-server-command
(ert-deftest ellm-test-build-server-command ()
  "Test that ellm--build-server-command builds correct command line arguments."
  (let ((ellm-local-server-config 
         '((server-bin . "/path/to/server")
           (model . "/path/to/model.gguf")
           (args . ("--n-gpu-layers" "59" "--ctx-size" "2048")))))
    (should (equal (ellm--build-server-command)
                   '("/path/to/server" "--model" "/path/to/model.gguf" 
                     "--n-gpu-layers" "59" "--ctx-size" "2048")))))

;; Test ellm--build-server-command with missing configuration
(ert-deftest ellm-test-build-server-command-missing-config ()
  "Test that ellm--build-server-command handles missing configuration."
  (let ((ellm-local-server-config nil))
    (should-error (ellm--build-server-command)))
  
  (let ((ellm-local-server-config '((server-bin . "/path/to/server"))))
    (should-error (ellm--build-server-command)))
  
  (let ((ellm-local-server-config '((model . "/path/to/model.gguf"))))
    (should-error (ellm--build-server-command))))

;; Test ellm--parse-command
(ert-deftest ellm-test-parse-command ()
  "Test that ellm--parse-command correctly parses special commands."
  (let ((ellm-special-commands '(("help" . ellm--cmd-help)
                               ("files" . ellm--cmd-list-files))))
    ;; Test valid command
    (let ((result (ellm--parse-command "/help")))
      (should (equal (car result) 'ellm--cmd-help))
      (should (string= (cdr result) "")))
    
    ;; Test valid command with arguments
    (let ((result (ellm--parse-command "/files /path/to/dir")))
      (should (equal (car result) 'ellm--cmd-list-files))
      (should (string= (cdr result) "/path/to/dir")))
    
    ;; Test unknown command
    (let ((result (ellm--parse-command "/unknown command")))
      (should (null (car result)))
      (should (string= (cdr result) "/unknown command")))
    
    ;; Test regular input (not a command)
    (let ((result (ellm--parse-command "regular input")))
      (should (null (car result)))
      (should (string= (cdr result) "regular input")))))

;; Test extract answer - simplified version
(ert-deftest ellm-test-extract-answer-simple ()
  "Basic test for ellm--extract-answer."
  (let ((ellm-server-type "local-llama-cpp"))
    (let ((resp (list (cons 'choices (list (list (cons 'message (list (cons 'content "hello")))))))))
      (should (equal (ellm--extract-answer resp) "hello")))))

(provide 'ellm-tests)
;;; ellm-tests.el ends here