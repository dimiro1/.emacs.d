;;; init.el --- Claudemiro Emacs Configuration Entry Point  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;;
;; Claudemiro's modular Emacs configuration entry point.
;;
;;; Code:

;; Prevent byte compilation
(setopt load-prefer-newer t)  ; Always load newer .el files over .elc

;;; Core Configuration Modules
;; Load configuration modules in dependency order with documentation

;; Define local modules directory
(defconst d1-modules-dir "lisp/"
  "Directory containing d1 configuration modules.")

;; Add the lisp dir to load path.
(add-to-list 'load-path (concat user-emacs-directory d1-modules-dir))

;; Core Configuration - Package management and fundamental editor settings
;; Provides: Package management (use-package), encoding, indentation, UI settings,
;; scrolling, line numbers, fonts, which-key, and core editing behavior
;; This must be loaded first as other modules depend on it
(use-package d1-core :load-path d1-modules-dir :demand t)

;; User Interface - Themes, modeline, and file tree
;; Provides: Multiple theme options, custom modeline with project info, NeoTree sidebar
;; Consolidated visual configuration for consistent appearance
(use-package d1-ui :load-path d1-modules-dir :demand t)

;; System Integration - Environment and file management
;; Provides: PATH configuration, shell integration, backup settings, auto-save,
;; authentication, and file persistence
(use-package d1-system :load-path d1-modules-dir :demand t)

;; Completion System - Complete completion framework
;; Provides: In-buffer completion (Corfu), minibuffer completion (Vertico + Marginalia),
;; fuzzy matching (Orderless), and enhanced completion experience
(use-package d1-completion :load-path d1-modules-dir :demand t)

;; Programming Languages - Language-specific configurations and LSP
;; Provides: Go, Rust, TypeScript, Markdown, Emacs Lisp support with eglot LSP integration
;; Includes: Tree-sitter parsers, Go testing, Paredit for Lisp editing, enhanced Markdown editing
;; Requires: External LSP servers to be installed
(use-package d1-languages :load-path d1-modules-dir :demand t)

;; Navigation - File and project navigation tools
;; Provides: Enhanced file finding, buffer switching, dired configuration, search tools
(use-package d1-navigation :load-path d1-modules-dir :demand t)

;; Version Control - Git integration and workflow tools
;; Provides: Magit configuration, Git gutter, time machine, and workflow enhancements
(use-package d1-git :load-path d1-modules-dir :demand t)

;;; Optional Features

;; Evil Mode - Vim-like editing experience
;; Provides: Modal editing, Vim keybindings, hybrid editing modes
;; Comment out this block if you prefer Emacs keybindings
(use-package d1-evil :load-path d1-modules-dir :demand t)

;; God Mode - Minimalist modal editing
;; Provides: God-mode modal editing without complex keybindings
;; (use-package d1-god-mode :load-path d1-modules-dir :demand t)

;; Custom Keybindings - Personal keybinding preferences
;; Provides: Custom shortcuts, workflow optimizations
;; Note: Loaded last to override any conflicting bindings
(use-package d1-keybindings :load-path d1-modules-dir :demand t)

;; Typing practice package.
(use-package d1-typing :load-path d1-modules-dir)

;; NPM Script Runner - Interactive npm script execution
;; Provides: Project-aware npm script discovery, interactive selection
;; Usage: M-x d1-npm-run or C-c n
(use-package d1-npm
  :load-path d1-modules-dir
  :demand t
  :bind
  ("C-c p n" . d1-npm-run)
  :config
  (with-eval-after-load 'marginalia
    (add-to-list 'marginalia-annotator-registry
                 '(npm-script d1-npm-annotate builtin none))
    (add-to-list 'marginalia-command-categories
                 '(d1-npm-run . npm-script))))

;;; init.el ends here
