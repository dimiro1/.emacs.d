;;; -*- lexical-binding: t; -*-

;; Install Markdown mode
(use-package markdown-mode)

;; Ensure Org Babel recognizes Go code
(use-package emacs
  :config
  (with-eval-after-load 'org
    (add-to-list 'org-src-lang-modes '("go" . go-ts))))


(provide 'my-modes)
