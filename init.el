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

;; Package Management - package.el setup for package management
;; Provides: Package installation from ELPA/MELPA archives
;; This must be loaded first as other modules depend on it
(require 'cafn-packages)

;; Core Editor - Fundamental editing behavior and user interface
;; Provides: Encoding, indentation, UI settings, scrolling, line numbers, fonts, which-key
;; Load early to establish core editing environment
(require 'cafn-editor)

;; Color Scheme - Theme configuration and management
;; Provides: Multiple theme options with easy switching
;; Currently using Catppuccin theme
(require 'cafn-colorscheme)

;; File Management - File handling, backups, and persistence
;; Provides: Backup configuration, custom file handling, auto-save
(require 'cafn-files)

;; Environment Setup - System integration and external tools
;; Provides: PATH configuration, shell integration, external tool setup
(require 'cafn-environment)

;; Mode Line - Custom mode-line with project information
;; Provides: Enhanced mode-line, project display, buffer identification
(require 'cafn-modeline)

;;; Enhanced Editing Features

;; Minibuffer Enhancement - Better minibuffer interaction
;; Provides: Enhanced minibuffer behavior and completion
(require 'cafn-minibuffer)

;; Completion Framework - Text completion and suggestions
;; Provides: Built-in completion configuration with flex matching and Corfu UI
;; Combines both the native completion system and modern in-buffer completion
(require 'cafn-completion)

;; Programming Languages - Language-specific configurations and LSP
;; Provides: Go, Rust, TypeScript, Markdown, Emacs Lisp support with eglot LSP integration
;; Includes: Go testing, Paredit for Lisp editing, Org Babel language support
;; Requires: External LSP servers to be installed
(require 'cafn-languages)

;; Navigation - File and project navigation tools
;; Provides: Enhanced file finding and buffer switching
(require 'cafn-navigation)

;; Version Control - Git integration and workflow tools
;; Provides: Magit configuration, Git workflow enhancements
(require 'cafn-magit)

;;; Specialized Tools

;; Tree-sitter - Modern syntax highlighting and code analysis
;; Provides: Enhanced syntax highlighting, code folding, structural editing
;; Note: Requires Emacs 29+ with tree-sitter support
(require 'cafn-treesit)

;; Code Snippets - Template system for common code patterns
;; Provides: YASnippet configuration, custom snippets
(require 'cafn-snippets)

;;; User Interface Enhancements

;; NEOTree - File explorer sidebar
;; Provides: Tree-based file navigation, project exploration
(require 'cafn-neotree)

;; Evil Mode - Vim-like editing experience
;; Provides: Modal editing, Vim keybindings, hybrid editing modes
;; Comment out this line if you prefer Emacs keybindings
(require 'cafn-evil)

;;; Additional Features

;; Markdown Support - Enhanced Markdown editing
;; Provides: Syntax highlighting, code block support, GitHub flavored markdown
(require 'cafn-markdown)

;; Org Mode - Productivity and note-taking
;; Provides: Agenda files, enhanced org mode experience
(require 'cafn-org)

;; Custom Keybindings - Personal keybinding preferences
;; Provides: Custom shortcuts, workflow optimizations
;; Note: Loaded last to override any conflicting bindings
(require 'cafn-keybindings)

;;; Post-initialization

;; Report startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract (current-time) cafn--startup-time)))
                     gcs-done)))

;;; Configuration Complete
(message "CAFN Emacs configuration loaded successfully!")

;;; init.el ends here
