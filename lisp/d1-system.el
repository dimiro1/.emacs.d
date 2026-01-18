;;; d1-system.el --- System integration (environment + file management)  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;;
;; System integration combining environment configuration (PATH, shell variables)
;; and file management (backups, auto-save, recent files).
;;
;; This module handles all system-level integration and file persistence.
;;

;;; Code:

;;; Core System Configuration
;; PATH, environment variables, and system integration
(use-package emacs
  :custom
  ;; no-littering package handles backup directory configuration
  ;; Backups are stored in ~/.emacs.d/var/backup/
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t)

  ;; Store customize-generated settings in a separate file
  ;; This keeps init.el clean and makes version control easier
  (custom-file (no-littering-expand-etc-file-name "custom.el"))

  ;; no-littering handles auto-save file locations
  ;; Auto-saves are stored in ~/.emacs.d/var/auto-save/
  (auto-save-interval 300)
  (auto-save-timeout 30)

  :config
  ;; Set default editor for git commits and other tools
  ;; This ensures external tools that need an editor will use Emacs
  (setenv "EDITOR" "emacsclient")
  (setenv "VISUAL" "emacsclient")

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
    ;; This setting will override the one in d1-navigation.el
    (with-eval-after-load 'dired
      (setopt dired-listing-switches "-alh")))

  :config
  ;; Load custom file if it exists
  ;; no-littering sets custom-file to ~/.emacs.d/etc/custom.el
  (when (and custom-file (file-exists-p custom-file))
    (load custom-file)))

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
  :config
  (global-auto-revert-mode 1)
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil))

;;; Recent Files
;; Track recently opened files for quick access
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
  ;; Quick access to recent files
  ("C-c f r" . recentf-open-files))

;;; Helper Functions
(defun d1-refresh-environment ()
  "Refresh environment variables from shell.
Useful when you've updated your shell configuration and want
Emacs to pick up the changes without restarting."
  (interactive)
  (when (fboundp 'exec-path-from-shell-initialize)
    (exec-path-from-shell-initialize)
    (message "Environment refreshed from shell")))

;;; Eshell Configuration
;; Minimalistic oh-my-zsh inspired prompt
(use-package eshell
  :config
  (defun d1-eshell-prompt ()
    "Minimalistic eshell prompt inspired by oh-my-zsh."
    (let* ((path (abbreviate-file-name (eshell/pwd)))
           (parts (split-string path "/" t))
           (last-3 (last parts 3)))
      (concat
       (propertize (string-join last-3 "/") 'face '(:foreground "cyan"))
       "\n"
       (propertize "❯" 'face '(:foreground "magenta"))
       " ")))

  (setopt eshell-prompt-function #'d1-eshell-prompt)
  (setopt eshell-prompt-regexp "^❯ ")

  ;; Zoxide integration - smart directory jumping
  (defun eshell/z (&rest args)
    "Jump to a directory using zoxide.
Usage: z [keywords...]"
    (if (null args)
        (eshell/cd "~")
      (let* ((query (string-join args " "))
             (result (string-trim
                      (shell-command-to-string
                       (concat "zoxide query -- " (shell-quote-argument query))))))
        (if (string-empty-p result)
            (eshell-error (format "zoxide: no match found for '%s'" query))
          (eshell/cd result)))))

  (defun eshell/zi (&rest args)
    "Interactively select a directory using zoxide and Emacs completion.
Usage: zi [keywords...]"
    (let* ((query (if args (string-join args " ") ""))
           (cmd (if (string-empty-p query)
                    "zoxide query --list"
                  (concat "zoxide query --list -- " (shell-quote-argument query))))
           (output (shell-command-to-string cmd))
           (dirs (split-string output "\n" t)))
      (if (null dirs)
          (eshell-error (format "zoxide: no matches found%s"
                                (if (string-empty-p query) "" (format " for '%s'" query))))
        (let ((selected (completing-read "Select directory: " dirs nil t)))
          (when selected
            (eshell/cd selected))))))

  (defun d1-eshell-zoxide-add ()
    "Add current directory to zoxide database."
    (let ((dir (eshell/pwd)))
      (call-process "zoxide" nil 0 nil "add" dir)))

  ;; Update zoxide when changing directories in eshell
  (add-hook 'eshell-directory-change-hook #'d1-eshell-zoxide-add)

  ;; Enhanced cat command with image rendering using advice
  ;; Inspired by https://xenodium.com/rinku-cli-link-previews
  (defun d1-eshell-cat-with-images (orig-fun &rest args)
    "Display images inline when using cat in eshell.
ORIG-FUN is the original cat function.
ARGS is a list of file paths to display."
    (if (seq-every-p (lambda (arg)
                       (and (stringp arg)
                            (file-exists-p arg)
                            (image-type-from-file-name arg)))
                     args)
        ;; All args are images - render them
        (with-temp-buffer
          (insert "\n")
          (dolist (path args)
            (let ((image (create-image (expand-file-name path)
                                       (image-type-from-file-name path)
                                       nil :max-width 800)))
              (insert-image image))
            (insert "\n"))
          (buffer-string))
      ;; Not all args are images - use original cat function
      (apply orig-fun args)))

  ;; Set the enhanced cat command.
  (advice-add 'eshell/cat :around #'d1-eshell-cat-with-images))

;;; AAX to M4B Conversion

(defun d1--aax-get-checksum (aax-file)
  "Get the checksum from AAX-FILE using ffprobe."
  (with-temp-buffer
    (call-process "ffprobe" nil '(t t) nil (expand-file-name aax-file))
    (goto-char (point-min))
    (when (re-search-forward "file checksum == \\([a-fA-F0-9]+\\)" nil t)
      (match-string 1))))

(defun d1--aax-get-activation-bytes (checksum callback)
  "Fetch activation bytes for CHECKSUM and call CALLBACK with result."
  (let ((url (format "https://aaxapiserverfunction20220831180001.azurewebsites.net/api/v2/Activation/%s?JonasIstCool=true" checksum)))
    (url-retrieve
     url
     (lambda (_status)
       (goto-char url-http-end-of-headers)
       (let* ((json-object-type 'alist)
              (response (json-read))
              (success (alist-get 'success response))
              (activation-bytes (alist-get 'activationBytes response)))
         (if success
             (funcall callback activation-bytes)
           (error "Failed to get activation bytes"))))
     nil t t)))

(defun d1--aax-convert (aax-file activation-bytes)
  "Convert AAX-FILE to M4B using ACTIVATION-BYTES."
  (let* ((output-file (concat (file-name-sans-extension aax-file) ".m4b"))
         (buf (get-buffer-create "*AAX Conversion*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "Converting: %s\n" (file-name-nondirectory aax-file)))
      (insert (format "Output: %s\n\n" (file-name-nondirectory output-file))))
    (display-buffer buf '(display-buffer-at-bottom (window-height . 10)))
    (make-process
     :name "aax-to-m4b"
     :buffer buf
     :command (list "ffmpeg" "-hide_banner" "-nostats" "-loglevel" "error"
                    "-y" "-activation_bytes" activation-bytes
                    "-i" (expand-file-name aax-file)
                    "-codec" "copy" (expand-file-name output-file))
     :sentinel (lambda (proc _event)
                 (when (memq (process-status proc) '(exit signal))
                   (with-current-buffer (process-buffer proc)
                     (goto-char (point-max))
                     (insert (if (= 0 (process-exit-status proc))
                                 (propertize "Conversion complete!\n" 'face 'success)
                               (propertize "Conversion failed!\n" 'face 'error)))))))))

;;;###autoload
(defun d1-convert-aax-to-m4b (aax-file)
  "Convert AAX-FILE to M4B format."
  (interactive "fAAX file: ")
  (unless (string-suffix-p ".aax" aax-file t)
    (user-error "File must have .aax extension"))
  (let ((checksum (d1--aax-get-checksum aax-file)))
    (unless checksum
      (user-error "Could not extract checksum from AAX file"))
    (message "Fetching activation bytes for checksum: %s" checksum)
    (d1--aax-get-activation-bytes
     checksum
     (lambda (activation-bytes)
       (d1--aax-convert aax-file activation-bytes)))))

;;;###autoload
(defun d1-dired-convert-aax-to-m4b ()
  "Convert marked AAX files or file at point to M4B in Dired."
  (interactive)
  (let ((files (dired-get-marked-files nil nil
                                       (lambda (f)
                                         (string-suffix-p ".aax" f t)))))
    (if (null files)
        (user-error "No AAX files selected")
      (dolist (file files)
        (d1-convert-aax-to-m4b file)))))

(provide 'd1-system)
;;; d1-system.el ends here
