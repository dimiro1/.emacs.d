;;; -*- lexical-binding: t; -*-

;;; Magit Configuration
;; Magit is a powerful Git interface for Emacs, providing an intuitive way to manage repositories.
;; Example: Use `M-x magit-status` to view the Git status of a project, stage/unstage changes, or commit.
(use-package magit
  :ensure t
  :bind
  (("C-x g" . magit-status)))

(provide 'my-magit)
