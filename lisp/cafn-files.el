;;; cafn-files.el --- File management, backups, and persistence  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; File management, backups, and persistence.
;;
;;; Code:

;;; Backup Configuration
;; Centralize backups to keep project directories clean
(use-package emacs
  :custom
  ;; Store all backups in a central directory
  ;; This prevents ~ files from cluttering your projects
  (backup-directory-alist `(("." . "~/.emacs-saves")))

  ;; Use copying for backups to preserve file permissions and symlinks
  ;; Safer than moving the original file
  (backup-by-copying t)

  ;; Automatically delete old backup versions
  ;; Prevents the backup directory from growing indefinitely
  (delete-old-versions t)

  ;; Keep 6 recent versions of each file
  ;; Provides good history without excessive disk usage
  (kept-new-versions 6)

  ;; Keep 2 old versions for long-term reference
  (kept-old-versions 2)

  ;; Enable version numbers for backup files
  ;; Creates backups like file.~1~, file.~2~, etc.
  (version-control t)

  :init
  ;; Ensure backup directory exists
  (let ((backup-dir "~/.emacs-saves"))
    (unless (file-exists-p backup-dir)
      (make-directory backup-dir t))))

;;; Custom File Management
;; Keep Emacs-generated customizations separate from hand-written config
(use-package emacs
  :custom
  ;; Store customize-generated settings in a separate file
  ;; This keeps init.el clean and makes version control easier
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :config
  ;; Load custom file if it exists, but don't error if it doesn't
  ;; The :no-error-if-file-is-missing flag prevents startup errors
  (load custom-file :no-error-if-file-is-missing))

;;; Auto-save Configuration
;; Configure auto-save behavior for better crash recovery
(use-package emacs
  :custom
  ;; Auto-save every 300 characters typed
  ;; Lower values save more frequently but may impact performance
  (auto-save-interval 300)

  ;; Auto-save after 30 seconds of idle time
  ;; Ensures work is saved during thinking pauses
  (auto-save-timeout 30)

  :init
  ;; Store auto-save files in temporary directory
  ;; Keeps them separate from project files
  (setopt auto-save-file-name-transforms
          `((".*" ,(expand-file-name "auto-saves/" user-emacs-directory) t)))

  ;; Ensure auto-save directory exists
  (let ((auto-save-dir (expand-file-name "auto-saves/" user-emacs-directory)))
    (unless (file-exists-p auto-save-dir)
      (make-directory auto-save-dir t))))

;;; Auto-revert Files
;; Automatically reload files when they change on disk
(use-package autorevert
  :init
  ;; Enable global auto-revert mode
  (global-auto-revert-mode 1)
  :custom
  ;; Also auto-revert non-file buffers like dired
  (global-auto-revert-non-file-buffers t)
  ;; Be quiet about reverting files
  (auto-revert-verbose nil))

;;; Recent Files
;; Track recently opened files for quick access
(use-package recentf
  :init
  ;; Enable recent files mode
  (recentf-mode 1)
  :custom
  ;; Keep track of last 100 files
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 100)
  ;; Exclude some directories from recent files
  (recentf-exclude '("/tmp/" "/ssh:" "\\.git/" "COMMIT_EDITMSG"))
  :bind
  ;; Quick access to recent files
  ("C-c f r" . recentf-open-files))

(provide 'cafn-files)
;;; cafn-files.el ends here
