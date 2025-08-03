;;; init.el --- Claudemiro Emacs Configuration Entry Point  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Claudemiro's modular Emacs configuration entry point.
;;
;;; Code:

;;; Add Custom `lisp` Directory to Load Path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;; Core Configuration Modules
;; Load configuration modules in dependency order with documentation

;; Core Configuration - Package management and fundamental editor settings
;; Provides: Package management (use-package), encoding, indentation, UI settings,
;; scrolling, line numbers, fonts, which-key, and core editing behavior
;; This must be loaded first as other modules depend on it
(require 'd1-core)

;; User Interface - Themes, modeline, and file tree
;; Provides: Multiple theme options, custom modeline with project info, NeoTree sidebar
;; Consolidated visual configuration for consistent appearance
(require 'd1-ui)

;; System Integration - Environment and file management
;; Provides: PATH configuration, shell integration, backup settings, auto-save,
;; authentication, and file persistence
(require 'd1-system)

;; Completion System - Complete completion framework
;; Provides: In-buffer completion (Corfu), minibuffer completion (Vertico + Marginalia),
;; fuzzy matching (Orderless), and enhanced completion experience
(require 'd1-completion)

;; Programming Languages - Language-specific configurations and LSP
;; Provides: Go, Rust, TypeScript, Markdown, Emacs Lisp support with eglot LSP integration
;; Includes: Tree-sitter parsers, Go testing, Paredit for Lisp editing, enhanced Markdown editing
;; Requires: External LSP servers to be installed
(require 'd1-languages)

;; Navigation - File and project navigation tools
;; Provides: Enhanced file finding, buffer switching, dired configuration, search tools
(require 'd1-navigation)

;; Version Control - Git integration and workflow tools
;; Provides: Magit configuration, Git gutter, time machine, and workflow enhancements
(require 'd1-git)

;;; Optional Features

;; Evil Mode - Vim-like editing experience
;; Provides: Modal editing, Vim keybindings, hybrid editing modes
;; Comment out this line if you prefer Emacs keybindings
;; (require 'd1-evil)

;; Custom Keybindings - Personal keybinding preferences
;; Provides: Custom shortcuts, workflow optimizations
;; Note: Loaded last to override any conflicting bindings
(require 'd1-keybindings)

;;; init.el ends here
