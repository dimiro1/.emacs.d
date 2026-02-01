;;; d1-system.el --- System integration (environment + file management)  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; System integration combining environment configuration
;; and file management (backups, auto-save, recent files).
;;

;;; Code:

;;;; ============================================================
;;;; Environment
;;;; ============================================================

;;; Environment Variables & GNU Tools
(use-package emacs
  :config
  ;; Set default editor for git commits and other tools
  (setenv "EDITOR" "emacsclient")
  (setenv "VISUAL" "emacsclient")

  ;; Use GNU ls if available (from coreutils)
  ;; macOS ships with BSD ls which lacks --group-directories-first
  ;; Install with: brew install coreutils
  (when (executable-find "gls")
    (setopt insert-directory-program "gls")))

;;;; ============================================================
;;;; File Management
;;;; ============================================================

;;; Backups & Auto-save
(use-package emacs
  :custom
  ;; Backup settings (stored in ~/.emacs.d/var/backup/ via no-littering)
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t)
  ;; Auto-save settings (stored in ~/.emacs.d/var/auto-save/ via no-littering)
  (auto-save-interval 300)
  (auto-save-timeout 30))

;;; Custom File
;; Store customize-generated settings separately to keep init.el clean
(use-package emacs
  :custom
  (custom-file (no-littering-expand-etc-file-name "custom.el"))
  :config
  (when (and custom-file (file-exists-p custom-file))
    (load custom-file)))

;;; Auto-revert
;; Automatically reload files when they change on disk
(use-package autorevert
  :config
  (global-auto-revert-mode 1)
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil))

;;; Recent Files
(use-package recentf
  :custom
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 100)
  (recentf-exclude '("/tmp/" "/ssh:" "\\.git/" "COMMIT_EDITMSG" "node_modules"))
  :config
  (recentf-mode 1)
  ;; Exclude no-littering directories
  (add-to-list 'recentf-exclude (regexp-quote (expand-file-name "var/" user-emacs-directory)))
  (add-to-list 'recentf-exclude (regexp-quote (expand-file-name "etc/" user-emacs-directory)))
  :bind
  ("C-c f r" . recentf-open-files))

(provide 'd1-system)
;;; d1-system.el ends here
