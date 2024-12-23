;; Company (Complete Anything) provides in-buffer, context-aware completion.
(use-package company
  :straight t
  :ensure t
  :defer t
  :hook ((go-ts-mode-hook . company-mode)
	 (emacs-lisp-mode-hook . company-mode)))

(provide 'my-company)
