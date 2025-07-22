;;; cafn-org.el --- Org mode configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Org mode configuration.
;;
;;; Code:

(use-package org
  :custom
  ;; Automatically find all .org files in ~/org directory for agenda
  ;; This recursively searches subdirectories for better organization
  (org-agenda-files (when (file-directory-p "~/org")
                      (directory-files-recursively "~/org" "\\.org$")))

  :config
  ;; Ensure org directory exists
  (unless (file-directory-p "~/org")
    (make-directory "~/org" t)
    (message "Created ~/org directory for org files"))

  ;; Additional org configuration can go here
  ;; For example: custom keywords, capture templates, etc.
  )

(provide 'cafn-org)
;;; cafn-org.el ends here