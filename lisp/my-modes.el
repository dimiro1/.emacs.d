;;; -*- lexical-binding: t; -*-

;; Install Markdown mode
(use-package markdown-mode
  :ensure t)

;; Ensure Org Babel recognizes Go code
(use-package emacs
  :config
  (with-eval-after-load 'org
    (add-to-list 'org-src-lang-modes '("go" . go-ts))))

(provide 'my-modes)
