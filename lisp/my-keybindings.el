;;; Window Management
(use-package emacs
  :bind (("C-M-<left>"  . shrink-window-horizontally) ;; Shrink window horizontally
         ("C-M-<right>" . enlarge-window-horizontally) ;; Enlarge window horizontally
         ("C-M-<down>"  . shrink-window) ;; Shrink window vertically
         ("C-M-<up>"    . enlarge-window))) ;; Enlarge window vertically

;;; Window Movement
(use-package emacs
  :bind ([s-left]  . windmove-left)  ;; Move to the window on the left
         ([s-right] . windmove-right) ;; Move to the window on the right
         ([s-up]    . windmove-up)    ;; Move to the window above
         ([s-down]  . windmove-down)) ;; Move to the window below

(provide 'my-keybindings)
