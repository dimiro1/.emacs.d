;;; Magit Configuration
;; Magit is a powerful Git interface for Emacs, providing an intuitive way to manage repositories.
;; Example: Use `M-x magit-status` to view the Git status of a project, stage/unstage changes, or commit.
;; :straight t and :ensure t ensure installation via straight.el or the default package manager.
(use-package magit
  :straight t
  :defer t
  :ensure t)

(provide 'my-magit)
