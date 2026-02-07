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
  ;; TODO workflow
  (org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-log-done 'time)
  (org-log-into-drawer t)
  ;; Archive
  (org-archive-location "~/org/archive.org::datetree/")
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
  (org-startup-with-inline-images t)
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
  (org-capture-templates
   '(("t" "Task" entry
      (file+headline "~/org/notes.org" "Tasks")
      "* TODO %?\n  %u\n  %a")
     ("i" "Idea" entry
      (file+headline "~/org/notes.org" "Ideas")
      "* %?\n  %u\n")
     ("b" "Bookmark" entry
      (file+headline "~/org/notes.org" "Bookmarks")
      "* [[%^{URL}][%^{Title}]]\n  %u\n  %?\n")
     ("j" "Journal" entry
      (file+datetree "~/org/journal.org")
      "* %U %?\n")
     ("p" "Person" plain
      (file (lambda ()
              (let ((name (read-string "Person name: ")))
                (expand-file-name
                 (concat (downcase (replace-regexp-in-string " " "-" name)) ".org")
                 "~/org/"))))
      (file "~/org/templates/person.org"))))
  :config
  ;; Scale LaTeX previews
  (plist-put org-format-latex-options :scale 1.5))

(use-package org-download
  :after org
  :custom
  (org-download-image-dir "~/org/images")
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d-%H%M%S_")
  :bind (:map org-mode-map
         ("C-c i s" . org-download-screenshot)
         ("C-c i y" . org-download-yank)
         ("C-c i c" . org-download-clipboard)))

;; Global keybindings recommended by the Org manual
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(provide 'd1-org)
;;; d1-org.el ends here
