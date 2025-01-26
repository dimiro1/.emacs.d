;;; -*- lexical-binding: t; -*-

;; Company (Complete Anything) provides in-buffer, context-aware completion.
(use-package company
  :ensure t
  :defer t
  :hook ((go-ts-mode . company-mode)
	 (emacs-lisp-mode . company-mode)))

(provide 'my-company)
