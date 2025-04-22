;;; -*- lexical-binding: t; -*-

;;; Core Encoding and Terminal Settings
(use-package emacs
  :init
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  :hook
  (term-mode . term-line-mode)) ;; Enable line mode in term-mode

;;; UI and Display Settings
(use-package emacs
  :init
  ;; General UI preferences
  (global-hl-line-mode t)
  (setopt fill-column 80)
  (auto-fill-mode t)
  (setopt show-trailing-whitespace t)
  (delete-selection-mode t)
  (setopt confirm-kill-emacs 'y-or-n-p)
  ;; Smooth scrolling
  (setopt scroll-step 1 scroll-conservatively 10000)
  (pixel-scroll-mode t)
  (pixel-scroll-precision-mode t)
  ;; Fullscreen and startup
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (setopt inhibit-startup-screen t)
  ;; Line numbers and columns
  (column-number-mode t)
  ;; Matching parentheses
  (show-paren-mode t))

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :custom
  (display-line-numbers-type 'relative)  ; for relative line numbers
  (display-line-numbers-width-start t))  ; auto-adjust width

;; Open init.el quickly
(defun open-init-file ()
  "Open the init.el file quickly."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(use-package emacs
  :bind (("C-c i" . open-init-file)))

;;; File and Backup Management
(use-package emacs
  :custom
  ;; Backup settings
  (backup-directory-alist `(("." . "~/.emacs-saves"))
          backup-by-copying t
          delete-old-versions t
          kept-new-versions 6
          kept-old-versions 2
          version-control t)
  ;; Custom file
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :config
  (load custom-file :no-error-if-file-is-missing))

;;; Spacing and Indentation
(use-package emacs
  :custom
  (line-spacing 2)
  (default-tab-width 2))

;;; CamelCase Navigation
(use-package emacs
  :init
  (subword-mode t)) ;; Enable CamelCase navigation

;;; Repeat Mode
(use-package emacs
  :hook
  (after-init . repeat-mode)) ;; Enable repeat mode

;;; Paths and Environment
(use-package emacs
  :init
  (add-to-list 'exec-path (concat (getenv "HOME") "/go/bin"))
  (add-to-list 'exec-path (concat (getenv "HOME") "/.cargo/bin")))

;;; Safe .dir-locals.el
(use-package emacs
  :config
  (customize-set-variable 'safe-local-variable-values
    '((go-test-go-command . "encore")
      (go-run-go-command . "encore"))))

;; Display project name and relative file path in mode-line
;; Format: "project-name:path/to/file.ext"
;; Falls back to abbreviated path for files outside projects
;; and regular buffer name for non-file buffers
(use-package emacs
  :custom
  (mode-line-buffer-identification
   '(:eval (if buffer-file-name
               (let* ((project (project-current))
                      (file-path (buffer-file-name))
                      (rel-path (if project
									(concat
                                     (project-name project)
                                     ":"
                                     (file-relative-name file-path
														 (project-root project)))
                                  (abbreviate-file-name file-path))))
                 (format "%s" rel-path))
             (format "%s" (buffer-name))))))

(provide 'my-common)
