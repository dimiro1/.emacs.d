;;; -*- lexical-binding: t; -*-

(use-package neotree
  :ensure t
  :bind
  ("s-1" . neotree-toggle)
  :custom
  (neo-reset-size-on-open t)
  (neo-theme 'nerd)
  (neo-window-width 35))

(provide 'my-neotree)
