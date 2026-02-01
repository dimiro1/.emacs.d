;;; d1-ai-apropos.el --- AI-powered function lookup using Ollama  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Provides AI-powered function lookup similar to apropos, using Ollama.
;; Detects the current programming language from major-mode and asks
;; the AI to suggest relevant functions for the user's question.
;;
;; Usage: M-x d1-ai-apropos or C-c a a
;;

;;; Code:

;;; Configuration

(defgroup d1-ai-apropos nil
  "AI-powered function lookup using Ollama."
  :group 'tools
  :prefix "d1-ai-apropos-")

(defcustom d1-ai-apropos-model "codellama"
  "Ollama model to use for AI apropos queries."
  :type 'string
  :group 'd1-ai-apropos)

(defcustom d1-ai-apropos-url "http://localhost:11434"
  "Ollama API base URL."
  :type 'string
  :group 'd1-ai-apropos)

(defcustom d1-ai-apropos-debug nil
  "When non-nil, log raw AI responses to debug buffer."
  :type 'boolean
  :group 'd1-ai-apropos)

(defvar d1--ai-apropos-buffer-name "*AI Apropos*"
  "Name of the buffer for AI apropos results.")

(defvar d1--ai-apropos-debug-buffer-name "*AI Apropos Debug*"
  "Name of the buffer for debug output.")


;;; Helper Functions

(defun d1--ai-apropos-http-get (endpoint)
  "Make a synchronous GET request to ENDPOINT.
Returns parsed JSON as alist, or nil on error."
  (condition-case nil
      (let ((buffer (url-retrieve-synchronously
                     (concat d1-ai-apropos-url endpoint)
                     t t 5)))
        (when buffer
          (unwind-protect
              (with-current-buffer buffer
                (goto-char url-http-end-of-headers)
                (json-parse-string
                 (buffer-substring-no-properties (point) (point-max))
                 :object-type 'alist))
            (kill-buffer buffer))))
    (error nil)))

(defun d1--ai-apropos-http-post (endpoint data callback)
  "Make an async POST request to ENDPOINT with DATA.
CALLBACK is called with parsed JSON response or nil on error."
  (let* ((url (concat d1-ai-apropos-url endpoint))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string (json-encode data) 'utf-8)))
    (url-retrieve
     url
     (lambda (status)
       (let ((response nil))
         (unwind-protect
             (progn
               (unless (plist-get status :error)
                 (goto-char url-http-end-of-headers)
                 (condition-case nil
                     (setq response (json-parse-string
                                     (buffer-substring-no-properties (point) (point-max))
                                     :object-type 'alist))
                   (error nil)))
               (funcall callback response))
           (kill-buffer (current-buffer)))))
     nil t)))

(defun d1--ai-apropos-ollama-running-p ()
  "Check if Ollama server is running."
  (not (null (d1--ai-apropos-http-get "/api/tags"))))

(defun d1--ai-apropos-get-installed-models ()
  "Get list of installed Ollama models."
  (let ((response (d1--ai-apropos-http-get "/api/tags")))
    (when response
      (mapcar (lambda (m) (cdr (assoc 'name m)))
              (cdr (assoc 'models response))))))

(defun d1--ai-apropos-model-installed-p (model)
  "Check if MODEL is installed in Ollama."
  (let ((installed (d1--ai-apropos-get-installed-models)))
    (or (member model installed)
        (member (concat model ":latest") installed))))

(defun d1--ai-apropos-ensure-model ()
  "Ensure Ollama is running and the configured model is available.
Returns t if ready, nil otherwise."
  (cond
   ;; Check if Ollama is running
   ((not (d1--ai-apropos-ollama-running-p))
    (message "Ollama is not running. Start it with: ollama serve")
    nil)
   ;; Check if model is installed
   ((not (d1--ai-apropos-model-installed-p d1-ai-apropos-model))
    (message "Model '%s' not found. Install it with: ollama pull %s"
             d1-ai-apropos-model d1-ai-apropos-model)
    nil)
   ;; All good
   (t t)))

(defun d1--ai-apropos-mode-name ()
  "Get a clean mode name from current major-mode for the AI.
Strips -mode and -ts-mode suffixes and any text properties."
  (let ((name (substring-no-properties (symbol-name major-mode))))
    (thread-last name
                 (replace-regexp-in-string "-ts-mode$" "")
                 (replace-regexp-in-string "-mode$" ""))))

(defun d1--ai-apropos-request (prompt callback)
  "Send PROMPT to Ollama asynchronously.
CALLBACK is called with the response text when complete, or nil on error."
  (d1--ai-apropos-http-post
   "/api/generate"
   `((model . ,d1-ai-apropos-model)
     (prompt . ,prompt)
     (stream . :json-false)
     (context . []))
   (lambda (response)
     (let ((result (cdr (assoc 'response response)))
           (error-msg (cdr (assoc 'error response))))
       (cond
        (error-msg (message "Ollama error: %s" error-msg) (funcall callback nil))
        (result (funcall callback result))
        (t (message "Error: No response from Ollama") (funcall callback nil)))))))

(defun d1--ai-apropos-display (question response)
  "Display RESPONSE for QUESTION in a dedicated buffer."
  (when d1-ai-apropos-debug
    (with-current-buffer (get-buffer-create d1--ai-apropos-debug-buffer-name)
      (erase-buffer)
      (insert response)
      (goto-char (point-min))))
  (let ((buf (get-buffer-create d1--ai-apropos-buffer-name)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "# %s\n\n" (upcase-initials question)))
      (insert (format "*Model: %s*\n\n---\n\n" d1-ai-apropos-model))
      (insert response)
      (insert "\n")
      (whitespace-cleanup)
      (markdown-mode)
      (setq-local markdown-header-scaling t)
      (markdown-update-header-faces t)
      (ignore-errors (markdown-table-align))
      (visual-line-mode 1)
      (goto-char (point-min))
      (read-only-mode 1)
      (local-set-key (kbd "q") 'quit-window))
    (display-buffer buf '(display-buffer-pop-up-window))))

;;; Public Commands

(defun d1-ai-apropos (question)
  "Ask AI for functions related to QUESTION in the current language.
Provides detailed descriptions with usage examples."
  (interactive "sAI Apropos (describe what you need): ")
  (when (d1--ai-apropos-ensure-model)
    (let* ((mode-name (d1--ai-apropos-mode-name))
           (prompt (format "You are a programming assistant. The user is working in %s.

Task: %s

Interpret the question - if vague, infer intent (e.g., \"define a function\" means \"how to define functions\").

Respond using this template:
## Explanation
Brief explanation of the concept.

## Example 1
Description of first example.
```%s
code
```

## Example 2
Description of second example.
```%s
code
```

Add more examples if helpful. Use markdown headers for sections. Always include the language tag in code blocks. NEVER use markdown tables, use lists instead."
                          mode-name question mode-name mode-name
                          mode-name question)))
      (message "Asking %s..." d1-ai-apropos-model)
      (d1--ai-apropos-request prompt
                              (lambda (response)
                                (if response
                                    (d1--ai-apropos-display question response)
                                  (message "No response received")))))))

(provide 'd1-ai-apropos)
;;; d1-ai-apropos.el ends here
