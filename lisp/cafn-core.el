;;; cafn-core.el --- Core Emacs configuration (packages + editor)  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Core Emacs configuration combining package management and fundamental
;; editor settings. This module provides the foundation for all other
;; configuration modules.
;;

;;; Code:

;;; Package Management
;;; Initialize package.el and use-package

;; Initialize package.el
(require 'package)

;; Configure package archives
;; Add MELPA and other repositories for more package options
(setopt package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Initialize package system
(package-initialize)

;; Refresh package contents if needed
(unless package-archive-contents
  (package-refresh-contents))

;; Install and configure use-package
;; use-package provides a clean, declarative way to configure packages
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

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

;;; Core Editor Configuration
;;; Fundamental editing behavior, encoding, indentation, and UI settings

(use-package emacs
  :custom
  ;; === Indentation and Spacing ===
  ;; Use 4 spaces as default tab width
  ;; This matches common conventions for many languages
  (tab-width 4)
  ;; Ensure consistent tab width in all modes
  (default-tab-width 4)
  ;; Enable multi-line comments
  (comment-multi-line t)

  ;; === User Interface ===
  ;; Require confirmation before killing Emacs
  ;; Prevents accidental closure with a simple y/n prompt
  (confirm-kill-emacs 'y-or-n-p)
  ;; Use short answers (y/n instead of yes/no)
  (use-short-answers t)
  ;; Disable bell sound - use visual indication instead
  (ring-bell-function 'ignore)

  ;; === Text Display Settings ===
  ;; Set default fill column for auto-fill and visual indicators
  (fill-column 100)
  ;; Disable scroll bar for cleaner appearance
  (scroll-bar-mode nil)
  ;; Disable tool bar for more screen space
  (tool-bar-mode nil)

  ;; === Smooth Scrolling Configuration ===
  ;; Scroll one line at a time (less "jumpy" than defaults)
  (scroll-step 1)
  ;; Don't recenter point when scrolling
  (scroll-conservatively 10000)

  :init
  ;; === Encoding Configuration ===
  ;; Ensure UTF-8 encoding everywhere for proper international text support
  ;; Set UTF-8 as the default encoding for all operations
  ;; This ensures proper handling of international characters
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  ;; Explicitly set language environment
  (set-language-environment "UTF-8")

  ;; === Core Editor Behavior ===
  ;; Prefer spaces over tabs for indentation
  ;; This ensures consistent display across different editors
  (setopt indent-tabs-mode nil)

  ;; Global subword mode for better navigation in compound words
  ;; M-f/M-b will stop at each part of CamelCase or snake_case words
  ;; Example: "CamelCase" -> stops at "Camel" and "Case"
  (global-subword-mode t)

  ;; === UI Configuration ===
  ;; Highlight current line for better visibility
  ;; This helps track cursor position, especially in large files
  (global-hl-line-mode t)

  ;; Show trailing whitespace to maintain clean code
  ;; Visible in all buffers - helps identify and remove unnecessary spaces
  (setopt show-trailing-whitespace nil)

  ;; Enable delete-selection-mode for more intuitive editing
  ;; When text is selected, typing replaces it (like most modern editors)
  (delete-selection-mode t)

  ;; Start Emacs maximized for maximum screen real estate
  ;; Alternative: use 'fullboth for true fullscreen
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Disable startup screen for faster access to work
  ;; Set to nil to see the startup screen with useful tips
  (setopt inhibit-startup-screen t)

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

  :hook
  ;; Enable repeat mode after initialization
  ;; This allows repeating commands like C-x o with just 'o'
  ;; Works with many built-in commands for faster workflows
  (after-init . repeat-mode)

  ;; Remove trailing whitespace on save for all files
  ;; Keeps your files clean and prevents unnecessary diffs
  (before-save . delete-trailing-whitespace)

  :config
  ;; Set the default font
  ;; This is a high-quality monospace font designed for programming
  (when (find-font (font-spec :name "Comic Code"))
    (set-face-attribute 'default nil
                        :family "Comic Code"
                        :height 120  ; 12pt font size
                        :weight 'regular))

  :bind
  ;; C-c i for "init" - easy to remember
  (("C-c i" . cafn-open-init-file)
   ;; Package management keybindings
   ("C-c P r" . cafn-package-refresh)
   ("C-c P u" . cafn-package-update-all)
   ("C-c P a" . cafn-package-autoremove)
   ("C-c P l" . package-list-packages)))

;;; Line Numbers Configuration
;; Display line numbers in programming modes with relative numbering
(use-package display-line-numbers
  :hook
  ;; Enable line numbers only in programming modes
  ;; Add more hooks here for other modes if needed
  (prog-mode . display-line-numbers-mode)
  :custom
  ;; Use relative line numbers for easier navigation
  ;; Set to t for absolute line numbers
  ;; Set to 'visual for relative numbers that work with wrapped lines
  (display-line-numbers-type 'relative)
  ;; Dynamically adjust line number width to prevent layout shifts
  (display-line-numbers-width-start t))

;;; Keybinding Discovery
;; Which-key provides helpful keybinding hints
;; Conditional configuration based on Emacs version
(if (< emacs-major-version 30)
    ;; Emacs < 30: Install which-key via straight.el
    (use-package which-key
      :ensure t
      :hook (after-init . which-key-mode))
  ;; Emacs 30+: which-key is built-in
  (use-package emacs
    :hook (after-init . which-key-mode)))

;;; Helper functions for package management

(defun cafn-package-refresh ()
  "Refresh package contents from all configured archives."
  (interactive)
  (package-refresh-contents)
  (message "Package contents refreshed"))

(defun cafn-package-update-all ()
  "Update all installed packages to their latest versions."
  (interactive)
  (package-refresh-contents)
  (save-window-excursion
    (package-list-packages t)
    (package-menu-mark-upgrades)
    (package-menu-execute t)))

(defun cafn-package-autoremove ()
  "Remove packages that are no longer needed."
  (interactive)
  (package-autoremove))

;;; Quick Init File Access
;; Provide quick access to configuration files
(defun cafn-open-init-file ()
  "Open the init.el file for quick configuration edits.
This function provides instant access to your Emacs configuration,
making it easy to tweak settings on the fly."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(provide 'cafn-core)
;;; cafn-core.el ends here
