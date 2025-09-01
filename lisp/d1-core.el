;;; d1-core.el --- Core Emacs configuration (packages + editor)  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;;
;; Core Emacs configuration combining package management and fundamental
;; editor settings.
;;
;; This module provides the foundation for all other configuration modules.
;;

;;; Code:

;;; Package Management
;;; Initialize package.el and use-package

;; Initialize package.el
(use-package package
  :custom
  ;; Configure package archives
  ;; Add MELPA and other repositories for more package options
  (package-archives '(("melpa" . "https://melpa.org/packages/")
					  ("gnu" . "https://elpa.gnu.org/packages/")
					  ("nongnu" . "https://elpa.nongnu.org/nongnu/"))))

;; Enable native compilation if available
(use-package emacs
  :if (native-comp-available-p)
  :custom
  (native-comp-deferred-compilation 1)
  (package-native-compile 1))

;; Configure use-package for better debugging and performance monitoring
(use-package use-package
  :custom
  ;; Always ensure packages are installed
  (use-package-always-ensure t)
  ;; Compute statistics for debugging startup time
  (use-package-compute-statistics t)
  ;; Expand use-package forms for debugging
  (use-package-expand-minimally nil)
  ;; Enable verbose loading for debugging (set to nil for normal use)
  (use-package-verbose nil))

;;; Flymake Configuration
;; Disable byte-compile checking for elisp files
(use-package flymake
  :hook (emacs-lisp-mode . (lambda ()
							 (remove-hook 'flymake-diagnostic-functions
										  'elisp-flymake-byte-compile t))))

;;; No-littering Configuration
;;; Keep ~/.emacs.d clean by putting files in appropriate directories
(use-package no-littering
  :demand t  ;; Load immediately before other packages
  :config
  ;; Use no-littering to keep ~/.emacs.d clean
  ;; Data files go to ~/.emacs.d/var/
  ;; Config files go to ~/.emacs.d/etc/

  ;; Store backups in subdirectories based on file name
  (no-littering-theme-backups))


;;; Core Editor Configuration
;;; Fundamental editing behavior, encoding, indentation, and UI settings

(use-package emacs
  :custom
  ;; Use 4 spaces as default tab width
  ;; This matches common conventions for many languages
  (tab-width 4)
  ;; Ensure consistent tab width in all modes
  (default-tab-width 4)
  ;; Enable multi-line comments
  (comment-multi-line t)

  ;; Require confirmation before killing Emacs
  ;; Prevents accidental closure with a simple y/n prompt
  (confirm-kill-emacs 'y-or-n-p)
  ;; Use short answers (y/n instead of yes/no)
  (use-short-answers t)
  ;; Disable bell sound - use visual indication instead
  (ring-bell-function 'ignore)

  ;; Set default fill column for auto-fill and visual indicators
  (fill-column 100)
  ;; Disable scroll bar for cleaner appearance
  (scroll-bar-mode nil)
  ;; Disable tool bar for more screen space
  (tool-bar-mode nil)

  ;; Scroll one line at a time (less "jumpy" than defaults)
  (scroll-step 1)
  ;; Don't recenter point when scrolling
  (scroll-conservatively 10000)
  ;; Keep 10 lines visible above and below cursor
  (scroll-margin 10)
  ;; Preserve cursor screen position when scrolling
  (scroll-preserve-screen-position t)

  :config
  ;; Ensure UTF-8 encoding everywhere for proper international text support
  ;; Set UTF-8 as the default encoding for all operations
  ;; This ensures proper handling of international characters
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  ;; Explicitly set language environment
  (set-language-environment "UTF-8")

  ;; Prefer spaces over tabs for indentation
  ;; This ensures consistent display across different editors
  (indent-tabs-mode nil)

  ;; Global subword mode for better navigation in compound words
  ;; M-f/M-b will stop at each part of CamelCase or snake_case words
  ;; Example: "CamelCase" -> stops at "Camel" and "Case"
  (global-subword-mode t)

  ;; Highlight current line for better visibility
  ;; This helps track cursor position, especially in large files
  (global-hl-line-mode t)

  ;; Show trailing whitespace to maintain clean code
  ;; Visible in all buffers - helps identify and remove unnecessary spaces
  (setopt show-trailing-whitespace nil)

  ;; Disable startup screen for faster access to work
  ;; Set to nil to see the startup screen with useful tips
  (setopt inhibit-startup-screen t)


  ;; Enable delete-selection-mode for more intuitive editing
  ;; When text is selected, typing replaces it (like most modern editors)
  (delete-selection-mode t)

  ;; Start Emacs maximized for maximum screen real estate
  ;; Alternative: use 'fullboth for true fullscreen
  (add-to-list 'default-frame-alist '(fullscreen . maximized))


  ;; Show column numbers in mode line
  ;; Useful for staying within line length limits
  (column-number-mode t)

  ;; Highlight matching parentheses
  ;; Essential for programming, especially in Lisp
  (show-paren-mode t)

  ;; Enable auto-fill mode for automatic line wrapping in text
  ;; This wraps lines at fill-column automatically while typing
  (auto-fill-mode t)

  ;; Enable pixel-level scrolling for smoother experience
  ;; Only available in Emacs 29+
  (when (fboundp 'pixel-scroll-mode)
	(pixel-scroll-mode t))
  (when (fboundp 'pixel-scroll-precision-mode)
	(pixel-scroll-precision-mode t))

  ;; Enable repeat mode
  ;; This allows repeating commands like C-x o with just 'o'
  ;; Works with many built-in commands for faster workflows
  (repeat-mode 1)
  ;; Set the default font
  ;; Google Sans Code is a high-quality monospace font designed for programming
  (when (find-font (font-spec :name "Google Sans Code"))
	(set-face-attribute 'default nil
						:family "Google Sans Code"
						:height 140  ; 14pt font size
						:weight 'regular))

  :hook
  ;; Remove trailing whitespace on save for all files
  ;; Keeps your files clean and prevents unnecessary diffs
  (before-save . delete-trailing-whitespace)

  :bind
  ;; C-c i for "init" - easy to remember
  (("C-c i" . d1-open-init-file)
   ;; Package management keybindings
   ("C-c P r" . d1-package-refresh)
   ("C-c P u" . d1-package-update-all)
   ("C-c P a" . d1-package-autoremove)
   ("C-c P l" . package-list-packages)))

;;; Line Numbers Configuration
;; Display line numbers in programming modes with relative numbering
(use-package display-line-numbers
  :hook prog-mode
  :custom
  ;; Use relative line numbers for easier navigation
  ;; Set to t for absolute line numbers
  ;; Set to 'visual for relative numbers that work with wrapped lines
  (display-line-numbers-type 'relative)
  ;; Dynamically adjust line number width to prevent layout shifts
  (display-line-numbers-width-start t))

;;; Keybinding Discovery
;; Which-key provides helpful keybinding hints
(use-package which-key
  :config
  (which-key-mode t)
  :custom
  (which-key-idle-delay 0.1))

;; Capture stats about key presses.
(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;;; Helper functions for package management

(defun d1-package-refresh ()
  "Refresh package contents from all configured archives."
  (interactive)
  (package-refresh-contents)
  (message "Package contents refreshed"))

(defun d1-package-update-all ()
  "Update all installed packages to their latest versions."
  (interactive)
  (package-refresh-contents)
  (save-window-excursion
	(package-list-packages t)
	(package-menu-mark-upgrades)
	(package-menu-execute t)))

(defun d1-package-autoremove ()
  "Remove packages that are no longer needed."
  (interactive)
  (package-autoremove))

;;; Quick Init File Access
;; Provide quick access to configuration files
(defun d1-open-init-file ()
  "Open the init.el file for quick configuration edits.
This function provides instant access to your Emacs configuration,
making it easy to tweak settings on the fly."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(provide 'd1-core)
;;; d1-core.el ends here
