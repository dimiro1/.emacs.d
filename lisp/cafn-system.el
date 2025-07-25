;;; cafn-system.el --- System integration (environment + file management)  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; System integration combining environment configuration (PATH, shell variables)
;; and file management (backups, auto-save, recent files). This module handles
;; all system-level integration and file persistence.
;;

;;; Code:

;;; Core System Configuration
;; PATH, environment variables, and system integration
(use-package emacs
  :custom
  ;; === Backup Configuration ===
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

  ;; === Custom File Management ===
  ;; Store customize-generated settings in a separate file
  ;; This keeps init.el clean and makes version control easier
  (custom-file (expand-file-name "custom.el" user-emacs-directory))

  ;; === Auto-save Configuration ===
  ;; Auto-save every 300 characters typed
  ;; Lower values save more frequently but may impact performance
  (auto-save-interval 300)
  ;; Auto-save after 30 seconds of idle time
  ;; Ensures work is saved during thinking pauses
  (auto-save-timeout 30)

  ;; Store auto-save files in temporary directory
  ;; Keeps them separate from project files
  (auto-save-file-name-transforms
   `((".*" ,(expand-file-name "auto-saves/" user-emacs-directory) t)))

  :init
  ;; === PATH Configuration ===
  ;; Add Go binaries to PATH
  ;; This allows Emacs to find Go tools like gopls, gofmt, etc.
  ;; These tools are typically installed with 'go install'
  (add-to-list 'exec-path (expand-file-name "~/go/bin"))

  ;; Add Rust/Cargo binaries to PATH
  ;; This allows Emacs to find Rust tools like rust-analyzer, rustfmt, etc.
  ;; These tools are typically installed with 'cargo install'
  (add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))

  ;; Add local bin directory for user-installed tools
  ;; Many tools install here by default
  (add-to-list 'exec-path (expand-file-name "~/.local/bin"))

  ;; Ensure PATH environment variable matches exec-path
  ;; This is important for subprocesses spawned by Emacs
  (setenv "PATH" (string-join exec-path ":"))

  ;; === Development Environment Variables ===
  ;; Ensure GOPATH is set for Go development
  ;; This is less critical in modern Go with modules, but some tools still use it
  (unless (getenv "GOPATH")
    (setenv "GOPATH" (expand-file-name "~/go")))

  ;; Set default editor for git commits and other tools
  ;; This ensures external tools that need an editor will use Emacs
  (setenv "EDITOR" "emacsclient")
  (setenv "VISUAL" "emacsclient")

  ;; === macOS-specific Configuration ===
  ;; Use GNU ls if available (from coreutils)
  ;;
  ;; Why GNU ls (gls)?
  ;; - macOS ships with BSD ls, which lacks many GNU ls features
  ;; - Most importantly: BSD ls doesn't support --group-directories-first
  ;; - --group-directories-first shows directories before files in dired
  ;; - This makes file browsing much more organized and user-friendly
  ;;
  ;; Installation: brew install coreutils
  ;; This installs GNU versions with 'g' prefix: gls, gcp, gmv, etc.
  (when (executable-find "gls")
    (setopt insert-directory-program "gls")
    ;; Verify gls supports the options we want to use
    (when (eq 0 (call-process "gls" nil nil nil "--group-directories-first" "--version"))
      (message "Using GNU ls (gls) for dired with --group-directories-first")))

  ;; Fallback: if gls is not available, adjust dired settings for BSD ls
  (unless (executable-find "gls")
    (message "GNU ls (gls) not found. Install with: brew install coreutils")
    (message "Using BSD ls - directories and files will be mixed in dired")
    ;; BSD ls doesn't support --group-directories-first, so use simpler options
    ;; This setting will override the one in cafn-navigation.el
    (with-eval-after-load 'dired
      (setopt dired-listing-switches "-alh")))

  ;; Set up Homebrew paths if installed
  (when (file-directory-p "/opt/homebrew/bin")
    (add-to-list 'exec-path "/opt/homebrew/bin")
    (setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH"))))

  ;; === Directory Creation ===
  ;; Ensure backup directory exists
  (let ((backup-dir "~/.emacs-saves"))
    (unless (file-exists-p backup-dir)
      (make-directory backup-dir t)))

  ;; Ensure auto-save directory exists
  (let ((auto-save-dir (expand-file-name "auto-saves/" user-emacs-directory)))
    (unless (file-exists-p auto-save-dir)
      (make-directory auto-save-dir t)))

  :config
  ;; Load custom file if it exists, but don't error if it doesn't
  ;; The :no-error-if-file-is-missing flag prevents startup errors
  (load custom-file :no-error-if-file-is-missing))

;;; Shell Environment Integration
;; On macOS, GUI Emacs doesn't inherit shell environment variables
;; This package fixes that issue
(use-package exec-path-from-shell
  :init
  ;; Copy environment variables from shell
  ;; This ensures tools work the same in Emacs as in terminal
  (exec-path-from-shell-initialize)
  :custom
  ;; Copy these specific environment variables
  ;; Add more variables here if needed for your tools
  (exec-path-from-shell-variables
   '("PATH" "GOPATH" "GOROOT" "CARGO_HOME" "RUSTUP_HOME")))

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
  (recentf-exclude '("/tmp/" "/ssh:" "\\.git/" "COMMIT_EDITMSG" "node_modules"))
  :bind
  ;; Quick access to recent files
  ("C-c f r" . recentf-open-files))

;;; Helper Functions
(defun cafn-refresh-environment ()
  "Refresh environment variables from shell.
Useful when you've updated your shell configuration and want
Emacs to pick up the changes without restarting."
  (interactive)
  (when (fboundp 'exec-path-from-shell-initialize)
    (exec-path-from-shell-initialize)
    (message "Environment refreshed from shell")))

(provide 'cafn-system)
;;; cafn-system.el ends here
