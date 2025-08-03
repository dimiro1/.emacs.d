;;; d1-keybindings.el --- Custom keybindings and shortcuts  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Custom keybindings and shortcuts.
;;
;;; Code:

;;; Window Management
(use-package emacs
  :bind
  (("C-M-<left>"  . shrink-window-horizontally)  ;; Shrink window horizontally
   ("C-M-<right>" . enlarge-window-horizontally) ;; Enlarge window horizontally
   ("C-M-<down>"  . shrink-window)               ;; Shrink window vertically
   ("C-M-<up>"    . enlarge-window)))            ;; Enlarge window vertically

;;; Move up and down whole lines.
(use-package move-text
  :bind
  (("s-<down>" . move-text-down)
   ("s-<up>" . move-text-up)))
(provide 'd1-keybindings)
;;; d1-keybindings.el ends here
