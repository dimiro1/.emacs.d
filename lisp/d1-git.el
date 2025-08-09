;;; d1-git.el --- Git integration with Magit  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;;
;; Git integration using Magit, a powerful Git interface for Emacs.
;; Provides comprehensive git operations with an intuitive interface.
;;

;;; Code:

;;; Magit Configuration
(use-package magit
  :ensure t
  :custom
  (magit-diff-refine-hunk t)
  (magit-save-repository-buffers 'dontask)
  (magit-auto-revert-mode t)
  (magit-no-confirm '(stage-all-changes unstage-all-changes))
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  :bind
  ("C-x g" . magit-status))

(provide 'd1-git)
;;; d1-git.el ends here
