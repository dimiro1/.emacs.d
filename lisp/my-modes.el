;; Install Markdown mode
(use-package markdown-mode)


(use-package emacs
  :config
  ;; Ensure Org Babel recognizes Go code
  (with-eval-after-load 'org
  (add-to-list 'org-src-lang-modes '("go" . go-ts))))


(provide 'my-modes)
