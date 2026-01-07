;;; d1-ai-apropos.el --- AI-powered function lookup using Ollama  -*- lexical-binding: t; no-byte-compile: t; -*-

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

(defun d1--ai-apropos-ollama-running-p ()
  "Check if Ollama server is running."
  (let ((response (shell-command-to-string
                   (format "curl -s -o /dev/null -w '%%{http_code}' %s/api/tags 2>/dev/null"
                          d1-ai-apropos-url))))
    (string= (string-trim response) "200")))

(defun d1--ai-apropos-get-installed-models ()
  "Get list of installed Ollama models."
  (let* ((response (shell-command-to-string
                    (format "curl -s %s/api/tags 2>/dev/null" d1-ai-apropos-url)))
         (json (condition-case nil
                   (json-parse-string response :object-type 'alist)
                 (error nil)))
         (models (cdr (assoc 'models json))))
    (when models
      (mapcar (lambda (m) (cdr (assoc 'name m))) models))))

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
CALLBACK is called with the response text when complete, or nil on error.
Each request is independent - no conversation context is maintained."
  (let* ((url (concat d1-ai-apropos-url "/api/generate"))
         (payload (json-encode `((model . ,d1-ai-apropos-model)
                                 (prompt . ,prompt)
                                 (stream . :json-false)
                                 (context . [])))) ;; Empty context = fresh conversation
         (output-buffer (generate-new-buffer " *ollama-output*"))
         (process-name (format "ollama-request-%s" (random 10000))))
    (make-process
     :name process-name
     :buffer output-buffer
     :command (list "curl" "-s" url "-d" payload)
     :sentinel
     (lambda (process _event)
       (when (eq (process-status process) 'exit)
         (let ((response nil))
           (with-current-buffer (process-buffer process)
             (condition-case err
                 (let* ((json-response (json-parse-string (buffer-string) :object-type 'alist))
                        (result (cdr (assoc 'response json-response)))
                        (error-msg (cdr (assoc 'error json-response))))
                   (cond
                    (error-msg (message "Ollama error: %s" error-msg))
                    (result (setq response result))
                    (t (message "Error: No response from Ollama"))))
               (error (message "Error: %s" (error-message-string err)))))
           (kill-buffer output-buffer)
           (funcall callback response)))))))

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
      (insert (format "# %s\n\n" question))
      (insert (format "*Model: %s*\n\n---\n\n" d1-ai-apropos-model))
      (insert response)
      (insert "\n")
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
  (interactive "sWhat do you want to do? ")
  (when (d1--ai-apropos-ensure-model)
    (let* ((mode-name (d1--ai-apropos-mode-name))
           (prompt (format "You are a programming assistant. The user is working in %s.

Task: %s

Interpret the question - if vague, infer intent (e.g., \"define a function\" means \"how to define functions\").

Respond with:
1. Brief explanation
2. Code example in a markdown code block

Be precise. Only show what's relevant.
Use markdown headers (## Header) for sections, not **bold**."
                          mode-name question)))
      (message "Asking %s..." d1-ai-apropos-model)
      (d1--ai-apropos-request prompt
                              (lambda (response)
                                (if response
                                    (d1--ai-apropos-display question response)
                                  (message "No response received")))))))

(provide 'd1-ai-apropos)
;;; d1-ai-apropos.el ends here
