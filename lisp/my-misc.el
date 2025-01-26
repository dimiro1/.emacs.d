;;; -*- lexical-binding: t; -*-

;;; XID Configuration
;; Generates globally unique IDs (xid github.com/rs/xid)
;; Usage: C-c x i d → cto6h99m65j2ggqeebc0
(use-package xid
  :ensure t
  :demand t
  :vc (:url "http://github.com/dimiro1/xid.el")
  :bind ( "C-c x i d" . xid-insert ))

(use-package carbonnow
  :ensure t
  :vc (:url "http://github.com/dimiro1/carbonnow"))

(provide 'my-misc)
