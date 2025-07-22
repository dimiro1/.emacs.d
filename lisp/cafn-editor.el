;;; cafn-editor.el --- Core editing behavior and user interface  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Core editing settings and UI configuration.
;;
;;; Code:

;;; Encoding Configuration
;; Ensure UTF-8 encoding everywhere for proper international text support
(use-package emacs
  :init
  ;; Set UTF-8 as the default encoding for all operations
  ;; This ensures proper handling of international characters
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  ;; Explicitly set language environment
  (set-language-environment "UTF-8"))

;;; Indentation and Spacing
;; Configure consistent indentation across all modes
(use-package emacs
  :custom
  ;; Use 4 spaces as default tab width
  ;; This matches common conventions for many languages
  (tab-width 4)
  ;; Ensure consistent tab width in all modes
  (default-tab-width 4)
  ;; Enable multi-line comments
  (comment-multi-line t)
  :init
  ;; Prefer spaces over tabs for indentation
  ;; This ensures consistent display across different editors
  (setopt indent-tabs-mode nil))

;;; CamelCase Navigation
;; Enable navigation by word parts in CamelCase and snake_case
(use-package emacs
  :init
  ;; Global subword mode for better navigation in compound words
  ;; M-f/M-b will stop at each part of CamelCase or snake_case words
  ;; Example: "CamelCase" -> stops at "Camel" and "Case"
  (global-subword-mode t))

;;; Repeat Mode
;; Enable efficient command repetition without chord keys
(use-package emacs
  :hook
  ;; Enable repeat mode after initialization
  ;; This allows repeating commands like C-x o with just 'o'
  ;; Works with many built-in commands for faster workflows
  (after-init . repeat-mode))

;;; Whitespace Management
;; Automatically clean up trailing whitespace
(use-package emacs
  :hook
  ;; Remove trailing whitespace on save for all files
  ;; Keeps your files clean and prevents unnecessary diffs
  (before-save . delete-trailing-whitespace))

;;; User Interface Configuration
;; Visual appearance and display settings

;; Core UI Settings
;; Configure fundamental UI behavior and appearance
(use-package emacs
  :init
  ;; Highlight current line for better visibility
  ;; This helps track cursor position, especially in large files
  (global-hl-line-mode t)

  ;; Show trailing whitespace to maintain clean code
  ;; Visible in all buffers - helps identify and remove unnecessary spaces
  (setopt show-trailing-whitespace nil)

  ;; Enable delete-selection-mode for more intuitive editing
  ;; When text is selected, typing replaces it (like most modern editors)
  (delete-selection-mode t)

  ;; Require confirmation before killing Emacs
  ;; Prevents accidental closure with a simple y/n prompt
  (setopt confirm-kill-emacs 'y-or-n-p)

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
  (show-paren-mode t))

;; Smooth Scrolling Configuration
;; Modern scrolling behavior for better reading experience
(use-package emacs
  :custom
  ;; Scroll one line at a time (less "jumpy" than defaults)
  (scroll-step 1)
  ;; Don't recenter point when scrolling
  (scroll-conservatively 10000)
  :init
  ;; Enable pixel-level scrolling for smoother experience
  ;; Only available in Emacs 29+
  (when (fboundp 'pixel-scroll-mode)
    (pixel-scroll-mode t))
  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode t)))

;; Line Numbers Configuration
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

;; Text Display Settings
;; Configure how text is displayed and formatted
(use-package emacs
  :custom
  ;; Set default fill column for auto-fill and visual indicators
  ;; 80 is traditional, but you might prefer 100 or 120 for modern displays
  (fill-column 80)
  ;; Disable scroll bar for cleaner appearance
  (scroll-bar-mode nil)
  ;; Disable tool bar for more screen space
  (tool-bar-mode nil)
  ;; Use short answers (y/n instead of yes/no)
  (use-short-answers t)
  ;; Disable bell sound - use visual indication instead
  (ring-bell-function 'ignore)
  :init
  ;; Enable auto-fill mode for automatic line wrapping in text
  ;; This wraps lines at fill-column automatically while typing
  (auto-fill-mode t))

;; Font Configuration
;; Set up the default font for better readability
(use-package emacs
  :config
  ;; Set the default font
  ;; This is a high-quality monospace font designed for programming
  (when (find-font (font-spec :name "Comic Code"))
    (set-face-attribute 'default nil
                        :family "Comic Code"
                        :height 120  ; 12pt font size
                        :weight 'regular)))

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

;;; Quick Init File Access
;; Provide quick access to configuration files
(defun cafn-open-init-file ()
  "Open the init.el file for quick configuration edits.
This function provides instant access to your Emacs configuration,
making it easy to tweak settings on the fly."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

;; Bind the function to a convenient key combination
(use-package emacs
  :bind
  ;; C-c i for "init" - easy to remember
  (("C-c i" . cafn-open-init-file)))

(provide 'cafn-editor)
;;; cafn-editor.el ends here
