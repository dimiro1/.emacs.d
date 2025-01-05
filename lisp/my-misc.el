;;; -*- lexical-binding: t; -*-

;;; XID Configuration
;; Generates globally unique IDs (xid github.com/rs/xid)
;; Usage: C-c x i d → cto6h99m65j2ggqeebc0
(use-package xid
  :demand t
  :straight ( :type git :host github :repo "dimiro1/xid.el" )
  :bind ( "C-c x i d" . xid-insert ))

(provide 'my-misc)
