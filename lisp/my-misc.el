;;; -*- lexical-binding: t; -*-

;;; XID Configuration
;; Generates globally unique IDs (xid github.com/rs/xid)
;; Usage: C-c x i d → cto6h99m65j2ggqeebc0
(use-package xid
  :ensure t
  :demand t
  :vc (:url "http://github.com/dimiro1/xid.el" :rev :newest)
  :bind ( "C-c x i d" . xid-insert ))

;;; Carbon.now
;; Integrates with carbon.now service to simplify taking coding "screeshots".
(use-package carbonnow
  :ensure t
  :vc (:url "http://github.com/dimiro1/carbonnow" :rev :newest))

;; Vterm terminal
;; Install a more capable terminal emulator
(use-package vterm
  :ensure t)

(provide 'my-misc)
