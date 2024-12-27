;;; Copilot Configuration
;; Copilot for Emacs integrates GitHub Copilot, an AI-powered code completion tool.
;; It provides context-aware suggestions as you type, helping you write code faster.
(use-package copilot
  :straight (:host github
             :repo "copilot-emacs/copilot.el"
             :files ("*.el")) ;; Install from the official GitHub repository
  :init (copilot-mode)
  :bind
  (:map copilot-completion-map
        ("<tab>"   . copilot-accept-completion)           ;; Accept the current suggestion
        ("TAB"     . copilot-accept-completion)           ;; Accept the current suggestion (alternative binding)
        ("C-TAB"   . copilot-accept-completion-by-word)   ;; Accept the suggestion word by word
        ("C-<tab>" . copilot-accept-completion-by-word))) ;; Same as above, alternate binding

(use-package gptel
  :config
  (setopt gptel-model 'claude-3-5-sonnet-20241022)
  (setopt gptel-backend
	  (gptel-make-anthropic "Claude"
	    :stream t
	    :key
	    (with-temp-buffer
	      (insert-file-contents
	       (expand-file-name "llm-token" user-emacs-directory)
	       (buffer-string))))))


(use-package gptel
  :config
  (setopt gptel-model 'claude-3-5-sonnet-20241022)
  (when-let* ((token-file (expand-file-name "llm-token" user-emacs-directory))
              ((file-exists-p token-file)))
    (setopt gptel-backend
            (gptel-make-anthropic "Claude"
              :stream t
              :key (with-temp-buffer
                     (insert-file-contents token-file)
                     (string-trim (buffer-string)))))))


(provide 'my-llm)
