;;; cafn-neotree.el --- File tree browser  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; File tree browser with NeoTree.
;;
;;; Code:

(use-package neotree
  :ensure t
  :bind
  ("s-1" . neotree-toggle)
  :custom
  (neo-reset-size-on-open t)
  (neo-theme 'nerd)
  (neo-window-width 35))

(provide 'cafn-neotree)
;;; cafn-neotree.el ends here
