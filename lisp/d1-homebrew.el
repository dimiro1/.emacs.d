;;; d1-homebrew.el --- Homebrew package manager for Emacs  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;;
;; Provides a tabulated list interface for managing Homebrew packages.
;; Supports formulas, casks, taps, and custom install commands.
;;
;; Usage:
;;   M-x d1-homebrew - Open the package manager
;;   RET - Install package at point
;;   i   - Install all pending packages
;;   I   - Show brew info for package at point
;;   r   - Refresh status check
;;   g   - Revert buffer
;;   q   - Quit
;;
;; Configuration:
;;   Customize `d1-homebrew-packages' to define your package list.
;;

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)

;;; Configuration Variables

;;;###autoload
(defvar d1-homebrew-packages
  '(;; Formulae
    (:name "Bun" :formula "bun")
    (:name "Deno" :formula "deno")
    (:name "Go" :formula "go")
    (:name "OpenCode" :formula "opencode")
    (:name "Zoxide" :formula "zoxide")
    (:name "Starship" :formula "starship")
    (:name "Ripgrep" :formula "ripgrep")
    (:name "Clojure" :formula "clojure")
    (:name "Chezmoi" :formula "chezmoi")
    (:name "Git" :formula "git")
    (:name "jq" :formula "jq")
    (:name "GNU Coreutils" :formula "coreutils")
    (:name "rbenv" :formula "rbenv")
    (:name "Babashka" :formula "babashka")
    (:name "GitHub CLI" :formula "gh")
    ;; Casks
    (:name "Claude Code" :formula "claude-code" :cask t)
    (:name "Colemak-DH" :formula "colemak-dh" :cask t)
    (:name "Ghostty" :formula "ghostty" :cask t)
    (:name "OrbStack" :formula "orbstack" :cask t)
    (:name "Temurin JDK" :formula "temurin" :cask t))
  "List of Homebrew packages to manage.
Each entry is a plist with:
  :name     - Display name for the UI
  :formula  - Homebrew formula or cask name
  :tap      - Optional tap to add before install (e.g., \"user/repo\")
  :cask     - If non-nil, install as cask instead of formula
  :command  - Optional custom install command (overrides default)")

;;; Internal Variables

(defvar d1--homebrew-buffer-name "*Homebrew Packages*"
  "Name of the Homebrew package management buffer.")

(defvar d1--homebrew-output-buffer-name "*Homebrew Install*"
  "Name of the buffer for Homebrew installation output.")

(defvar d1--homebrew-info-buffer-name "*Homebrew Info*"
  "Name of the buffer for displaying package info.")

(defvar d1--homebrew-package-states (make-hash-table :test 'equal)
  "Hash table tracking installation state of each package.
Keys are formula names, values are status symbols:
  pending, checking, installing, installed, failed")

(defvar d1--homebrew-install-queue nil
  "Queue of packages waiting to be installed.")

(defvar d1--homebrew-current-install nil
  "Currently installing package, or nil if none.")


;;; Faces

(defface d1-homebrew-pending
  '((t :inherit shadow))
  "Face for pending packages.")

(defface d1-homebrew-checking
  '((t :foreground "yellow"))
  "Face for packages being checked.")

(defface d1-homebrew-installing
  '((t :foreground "cyan" :weight bold))
  "Face for packages being installed.")

(defface d1-homebrew-installed
  '((t :foreground "green"))
  "Face for installed packages.")

(defface d1-homebrew-failed
  '((t :foreground "red" :weight bold))
  "Face for failed installations.")

;;; Status and State Management

(defun d1--homebrew-set-status (formula status)
  "Set the status of FORMULA to STATUS.
STATUS should be: pending, checking, installing, installed, or failed."
  (puthash formula status d1--homebrew-package-states))

(defun d1--homebrew-get-status (formula)
  "Get the current status of FORMULA."
  (gethash formula d1--homebrew-package-states 'pending))

(defun d1--homebrew-status-string (status)
  "Convert STATUS symbol to a display string with face."
  (pcase status
    ('pending    (propertize "pending"    'face 'd1-homebrew-pending))
    ('checking   (propertize "checking"   'face 'd1-homebrew-checking))
    ('installing (propertize "installing" 'face 'd1-homebrew-installing))
    ('installed  (propertize "installed"  'face 'd1-homebrew-installed))
    ('failed     (propertize "FAILED"     'face 'd1-homebrew-failed))
    (_           (propertize "unknown"    'face 'd1-homebrew-pending))))

;;; Validation

(defun d1--homebrew-valid-package-p (pkg)
  "Return non-nil if PKG is a valid package plist.
A valid package must have :name and :formula as non-empty strings."
  (and (listp pkg)
       (cl-typep (plist-get pkg :name) 'string)
       (cl-typep (plist-get pkg :formula) 'string)
       (not (string-empty-p (plist-get pkg :name)))
       (not (string-empty-p (plist-get pkg :formula)))))

;;; Homebrew Check Functions

(defun d1--homebrew-installed-p (formula &optional cask)
  "Check if FORMULA is installed.
If CASK is non-nil, check cask list instead of formula list."
  (let* ((quoted (shell-quote-argument formula))
         (cmd (if cask
                  (format "brew list --cask %s 2>/dev/null" quoted)
                (format "brew list --formula %s 2>/dev/null" quoted)))
         (result (shell-command-to-string cmd)))
    (not (string-empty-p (string-trim result)))))

(defun d1--homebrew-tap-exists-p (tap)
  "Check if TAP is already tapped."
  (let ((result (shell-command-to-string
                 (format "brew tap | grep -qF %s && echo yes"
                         (shell-quote-argument tap)))))
    (string= (string-trim result) "yes")))

(defun d1--homebrew-check-all-packages ()
  "Check installation status of all packages asynchronously.
Updates the status hash table and refreshes the buffer."
  ;; Set all to checking first
  (dolist (pkg d1-homebrew-packages)
    (let ((formula (plist-get pkg :formula)))
      (d1--homebrew-set-status formula 'checking)))
  (d1--homebrew-refresh-buffer)

  ;; Run checks asynchronously
  (dolist (pkg d1-homebrew-packages)
    (let* ((formula (plist-get pkg :formula))
           (cask (plist-get pkg :cask))
           (quoted (shell-quote-argument formula)))
      (make-process
       :name (format "brew-check-%s" formula)
       :command (list "sh" "-c"
                      (if cask
                          (format "brew list --cask %s >/dev/null 2>&1" quoted)
                        (format "brew list --formula %s >/dev/null 2>&1" quoted)))
       :sentinel (lambda (proc _event)
                   (when (memq (process-status proc) '(exit signal))
                     (let ((formula (replace-regexp-in-string
                                     "^brew-check-" ""
                                     (process-name proc))))
                       (d1--homebrew-set-status
                        formula
                        (if (= 0 (process-exit-status proc))
                            'installed
                          'pending))
                       (d1--homebrew-refresh-buffer))))))))

;;; Buffer Management

(defun d1--homebrew-refresh ()
  "Refresh the tabulated list entries."
  (setq tabulated-list-entries (d1--homebrew-build-entries)))

(defun d1--homebrew-refresh-buffer ()
  "Refresh the Homebrew buffer if it exists."
  (when-let* ((buf (get-buffer d1--homebrew-buffer-name)))
    (with-current-buffer buf
      (revert-buffer))))

(defun d1--homebrew-build-entries ()
  "Build the list of entries for `tabulated-list-mode`."
  (cl-loop for pkg in d1-homebrew-packages
           when (d1--homebrew-valid-package-p pkg)
           collect (let* ((name (plist-get pkg :name))
                          (formula (plist-get pkg :formula))
                          (cask (plist-get pkg :cask))
                          (tap (plist-get pkg :tap))
                          (status (d1--homebrew-get-status formula))
                          (formula-display (concat formula
                                                   (when cask " (cask)")
                                                   (when tap (format " [%s]" tap)))))
                     (list formula
                           (vector name
                                   (d1--homebrew-status-string status)
                                   formula-display)))))

;;; Installation Logic

(defun d1--homebrew-process-next-install ()
  "Process the next package in the install queue.
Called after each installation completes (success or failure)."
  (setq d1--homebrew-current-install nil)
  (if d1--homebrew-install-queue
      (let ((next-pkg (pop d1--homebrew-install-queue)))
        (d1--homebrew-install-package next-pkg))
    ;; Queue empty, refresh the display
    (d1--homebrew-refresh-buffer)
    (message "Homebrew installation queue complete")))

(defun d1--homebrew-install-package (pkg)
  "Install a single package PKG.
PKG is a plist from `d1-homebrew-packages'.
Handles taps if needed, then runs install command asynchronously."
  (let* ((formula (plist-get pkg :formula))
         (tap (plist-get pkg :tap))
         (cask (plist-get pkg :cask))
         (custom-cmd (plist-get pkg :command))
         (commands nil))

    (setq d1--homebrew-current-install pkg)
    (d1--homebrew-set-status formula 'installing)
    (d1--homebrew-refresh-buffer)

    ;; Build the command sequence
    (when (and tap (not (d1--homebrew-tap-exists-p tap)))
      (push (format "brew tap %s" (shell-quote-argument tap)) commands))

    (push (or custom-cmd
              (if cask
                  (format "brew install --cask %s" (shell-quote-argument formula))
                (format "brew install %s" (shell-quote-argument formula))))
          commands)

    ;; Execute asynchronously
    (d1--homebrew-run-command
     (string-join (nreverse commands) " && ")
     formula)))

(defun d1--homebrew-run-command (command formula)
  "Run COMMAND asynchronously and update status for FORMULA when done.
Output is redirected to a temp file to avoid ANSI escape code issues."
  (let* ((buf (get-buffer-create d1--homebrew-output-buffer-name))
         (log-file (make-temp-file "brew-install-" nil ".log"))
         (full-command (format "%s > %s 2>&1" command log-file)))
    (with-current-buffer buf
      (read-only-mode -1)
      (goto-char (point-max))
      (insert (propertize (format "\n=== Installing %s ===\n" formula)
                          'face '(:weight bold)))
      (insert (format "$ %s\n" command)))

    (display-buffer buf '(display-buffer-at-bottom (window-height . 15)))

    (make-process
     :name (format "brew-install-%s" formula)
     :buffer nil
     :command (list "sh" "-c" full-command)
     :sentinel (lambda (proc _event)
                 (when (memq (process-status proc) '(exit signal))
                   (let* ((exit-code (process-exit-status proc))
                          (success (= 0 exit-code))
                          (formula (replace-regexp-in-string
                                    "^brew-install-" ""
                                    (process-name proc)))
                          (output (with-temp-buffer
                                    (insert-file-contents log-file)
                                    (buffer-string))))
                     (delete-file log-file)
                     (with-current-buffer buf
                       (let ((inhibit-read-only t))
                         (goto-char (point-max))
                         (insert output)
                         (insert (if success
                                     (propertize "\n=== Done ===\n" 'face 'd1-homebrew-installed)
                                   (propertize (format "\n=== Failed (exit %d) ===\n" exit-code)
                                               'face 'd1-homebrew-failed)))))
                     (d1--homebrew-set-status formula (if success 'installed 'failed))
                     (d1--homebrew-process-next-install)))))))

;;; Tabulated List Mode

(defvar d1-homebrew-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'d1-homebrew-install-at-point)
    (define-key map (kbd "i") #'d1-homebrew-install-all-pending)
    (define-key map (kbd "I") #'d1-homebrew-info-at-point)
    (define-key map (kbd "r") #'d1-homebrew-refresh-status)
    (define-key map (kbd "g") #'revert-buffer)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `d1-homebrew-mode'.")

(define-derived-mode d1-homebrew-mode tabulated-list-mode "Homebrew"
  "Major mode for managing Homebrew packages.

\\{d1-homebrew-mode-map}"
  :keymap d1-homebrew-mode-map
  (setq tabulated-list-format
        [("Name"    25 t)
         ("Status"  12 t)
         ("Formula" 30 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Name" . nil))
  (add-hook 'tabulated-list-revert-hook #'d1--homebrew-refresh nil t)
  (tabulated-list-init-header))

;;; Public API

;;;###autoload
(defun d1-homebrew ()
  "Open the Homebrew package manager buffer.
Shows all configured packages with their installation status."
  (interactive)
  (let ((buf (get-buffer-create d1--homebrew-buffer-name)))
    (with-current-buffer buf
      (d1-homebrew-mode)
      (d1--homebrew-check-all-packages))
    (switch-to-buffer buf)))

(defun d1-homebrew-install-at-point ()
  "Install the package at point."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pkg (seq-find (lambda (p)
                               (string= (plist-get p :formula) id))
                             d1-homebrew-packages)))
    (let ((status (d1--homebrew-get-status id)))
      (if (memq status '(installed installing))
          (message "Package %s is already %s" id status)
        (if d1--homebrew-current-install
            ;; Add to queue if installation in progress
            (progn
              (add-to-list 'd1--homebrew-install-queue pkg t)
              (message "Added %s to installation queue" (plist-get pkg :name)))
          ;; Start installation immediately
          (d1--homebrew-install-package pkg))))))

(defun d1-homebrew-install-all-pending ()
  "Install all packages with pending status."
  (interactive)
  (let ((pending (seq-filter
                  (lambda (pkg)
                    (eq 'pending (d1--homebrew-get-status
                                  (plist-get pkg :formula))))
                  d1-homebrew-packages)))
    (if (null pending)
        (message "No pending packages to install")
      (setq d1--homebrew-install-queue (cdr pending))
      (d1--homebrew-install-package (car pending))
      (message "Starting installation of %d packages" (length pending)))))

(defun d1-homebrew-info-at-point ()
  "Display brew info for the package at point in a separate buffer."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (pkg (seq-find (lambda (p)
                               (string= (plist-get p :formula) id))
                             d1-homebrew-packages)))
    (let* ((formula (plist-get pkg :formula))
           (cask (plist-get pkg :cask))
           (quoted (shell-quote-argument formula))
           (cmd (if cask
                    (format "brew info --cask %s" quoted)
                  (format "brew info %s" quoted)))
           (output (shell-command-to-string cmd))
           (buf (get-buffer-create d1--homebrew-info-buffer-name)))
      (with-current-buffer buf
        (read-only-mode -1)
        (erase-buffer)
        (insert (propertize (format "Homebrew Info: %s\n" formula)
                            'face '(:weight bold :height 1.2)))
        (insert (make-string 50 ?-) "\n\n")
        (insert output)
        (goto-char (point-min))
        (special-mode))
      (display-buffer buf '(display-buffer-at-bottom
                            (window-height . 20))))))

(defun d1-homebrew-refresh-status ()
  "Re-check installation status of all packages."
  (interactive)
  (d1--homebrew-check-all-packages)
  (message "Checking package installation status..."))

(provide 'd1-homebrew)
;;; d1-homebrew.el ends here
