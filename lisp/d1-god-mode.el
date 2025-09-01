;;; d1-god-mode.el --- God Mode configuration  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;;
;; God Mode - Minimalist modal editing configuration.
;; Provides ergonomic command input without modifier keys.
;;

;;; Code:

;; God Mode - Minimalist modal editing
;; Provides: God-mode modal editing without complex keybindings
(use-package god-mode
  :demand t
  :custom
  (god-exempt-major-modes nil)
  (god-exempt-predicates nil)
  :config
  (god-mode)
  (with-eval-after-load 'which-key
    (which-key-enable-god-mode-support))
  :bind (("<escape>" . god-local-mode)
         ("ESC ESC" . god-local-mode)
         :map god-local-mode-map
         ("i" . god-local-mode)))

(provide 'd1-god-mode)
;;; d1-god-mode.el ends here
