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
  (org-agenda-files '("~/org"))
  ;; Appearance (from Doom Emacs)
  (org-hide-leading-stars nil)
  (org-startup-indented nil)
  (org-startup-folded nil)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-image-actual-width nil)
  (org-tags-column 0)
  (org-use-sub-superscripts '{})
  (org-ellipsis " ...")
  (org-imenu-depth 6)
  ;; Refile
  (org-enforce-todo-dependencies t)
  (org-refile-targets '((nil :maxlevel . 3)
                        (org-agenda-files :maxlevel . 3)))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  ;; Priority faces
  (org-priority-faces '((?A . error)
                        (?B . warning)
                        (?C . shadow)))
  :config
  ;; Scale LaTeX previews
  (plist-put org-format-latex-options :scale 1.5))

;; Global keybindings recommended by the Org manual
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(provide 'd1-org)
;;; d1-org.el ends here
