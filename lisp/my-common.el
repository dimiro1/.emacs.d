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
  ;; Fullscreen and startup
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (setopt inhibit-startup-screen t)
  ;; Line numbers and columns
  (column-number-mode t)
  ;; Matching parentheses
  (show-paren-mode t))

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
  (load custom-file))

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

(provide 'my-common)
