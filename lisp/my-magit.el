;;; -*- lexical-binding: t; -*-

;;; Magit Configuration
;; Magit is a powerful Git interface for Emacs, providing an intuitive way to manage repositories.
;; Example: Use `M-x magit-status` to view the Git status of a project, stage/unstage changes, or commit.
(use-package magit
  :straight t
  :defer t)

;;; Git gutter config
;; Show git status in the gutter (margin)
(use-package git-gutter
  :ensure t
  ;; Enable git-gutter globally for all modes
  :hook (prog-mode . git-gutter-mode)
  ;; Update git gutter on save and when window changes focus
  :config
  (add-hook 'focus-in-hook #'git-gutter:update-all-windows)
  (add-hook 'after-save-hook #'git-gutter:update-all-windows)
  ;; Set fringe style and disable when in terminal
  :custom
  (git-gutter:update-interval 2)
  (git-gutter:window-width 1)
  (git-gutter:modified-sign "~")
  (git-gutter:added-sign "+")
  (git-gutter:deleted-sign "-"))

(provide 'my-magit)
