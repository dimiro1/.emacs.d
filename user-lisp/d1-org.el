;;; d1-org.el --- Org-mode configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Configuration for org-mode.
;;
;;; Code:

(use-package org
  :ensure nil
  :demand t
  :custom
  (org-directory "~/org")
  (org-default-notes-file (expand-file-name "notes.org" "~/org"))
  (org-agenda-files '("~/org")))

;; Global keybindings recommended by the Org manual
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(provide 'd1-org)
;;; d1-org.el ends here
