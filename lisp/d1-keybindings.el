;;; d1-keybindings.el --- Custom keybindings and shortcuts  -*- lexical-binding: t; no-byte-compile: t; -*-

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

;;; NPM Script Runner
(global-set-key (kbd "C-c p n") 'd1-npm-run)

;;; programming notes
(defun d1-open-programming-notes ()
  "Open my programming notes."
  (interactive)
  (find-file "~/Documents/programming-notes.md"))

(global-set-key (kbd "C-c n") 'd1-open-programming-notes)

(provide 'd1-keybindings)
;;; d1-keybindings.el ends here
