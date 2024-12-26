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

(provide 'my-copilot)
