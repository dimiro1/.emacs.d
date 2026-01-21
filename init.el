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
;; Includes: Tree-sitter parsers, Go testing, Parinfer for Lisp editing, enhanced Markdown editing
;; Requires: External LSP servers to be installed
(use-package d1-languages :load-path d1-modules-dir :demand t)

;; Navigation - File and project navigation tools
;; Provides: Enhanced file finding, buffer switching, dired configuration, search tools
(use-package d1-navigation :load-path d1-modules-dir :demand t)

;; Version Control - Git integration and workflow tools
;; Provides: Magit configuration, Git gutter, time machine, and workflow enhancements
(use-package d1-git :load-path d1-modules-dir :demand t)

;; Git Permalink - Generate permalinks to files on git hosting platforms
;; Provides: Browse/copy permalinks to GitHub, GitLab, Codeberg with line numbers
;; Usage: M-x d1-git-browse-permalink or C-c g o (browse)
;;        M-x d1-git-copy-permalink or C-u C-c g o (copy to kill ring)
(use-package d1-git-permalink
  :load-path d1-modules-dir
  :bind ("C-c g o" . d1-git-browse-permalink))

;;; Optional Features

;; Evil Mode - Vim-like editing experience
;; Provides: Modal editing, Vim keybindings, hybrid editing modes
;; Comment out this block if you prefer Emacs keybindings
;;(use-package d1-evil :load-path d1-modules-dir :demand t)

;; God Mode - Minimalist modal editing
;; Provides: God-mode modal editing without complex keybindings
;; (use-package d1-god-mode :load-path d1-modules-dir :demand t)

;; Custom Keybindings - Personal keybinding preferences
;; Provides: Custom shortcuts, workflow optimizations
;; Note: Loaded last to override any conflicting bindings
(use-package d1-keybindings :load-path d1-modules-dir :demand t)

;; Typing practice package.
(use-package d1-typing :load-path d1-modules-dir)

;; Atom and RSS feeds.
(use-package d1-rss :load-path d1-modules-dir)

;; Reading - EPUB and document reading
;; Provides: nov.el for EPUB files
(use-package d1-reading :load-path d1-modules-dir)

;; Utilities - General-purpose utility functions
;; Provides: Kill process by port, kill process with completion
;; Usage: M-x d1-kill-process-on-port or C-c x p
;;        M-x d1-kill-process or C-c x k
(use-package d1-utilities
  :load-path d1-modules-dir
  :bind (("C-c x p" . d1-kill-process-on-port)
         ("C-c x k" . d1-kill-process)))

;; AI Apropos - AI-powered function lookup using Ollama
;; Provides: Natural language function search based on current language
;; Usage: M-x d1-ai-apropos or C-c a a
(use-package d1-ai-apropos
  :load-path d1-modules-dir
  :bind ("C-c a a" . d1-ai-apropos))

;; German Language Utilities - German word gender and verb conjugation using Ollama
;; Provides: Gender lookup with article, plural, translations, explanations
;;           Verb conjugation with all major tenses (Präsens, Präteritum, Perfekt, Futur)
;; Usage: M-x d1-german-gender or C-c a g (for nouns)
;;        M-x d1-german-verb or C-c a v (for verbs)
(use-package d1-german
  :load-path d1-modules-dir
  :bind (("C-c a g" . d1-german-gender)
         ("C-c a G" . d1-german-gender-at-point)
         ("C-c a v" . d1-german-verb)
         ("C-c a V" . d1-german-verb-at-point)))

;; NPM Script Runner - Interactive npm script execution
;; Provides: Project-aware npm script discovery, interactive selection
;; Usage: M-x d1-npm-run or C-c n
(use-package d1-npm
  :load-path d1-modules-dir
  :demand t
  :bind
  ("C-c p n" . d1-npm-run))

;; Homebrew Package Manager - Visual package management for Homebrew
;; Provides: Tabulated list of packages, installation tracking, tap support
;; Usage: M-x d1-homebrew
(use-package d1-homebrew :load-path d1-modules-dir)

;; AI Coding Agents - Native agentic integrations for Claude Code, Gemini CLI, etc.
;; Provides: Interactive shell buffers for AI coding agents
;; Usage: M-x agent-shell, M-x agent-shell-new-shell, M-x agent-shell-toggle
(use-package d1-ai :load-path d1-modules-dir)

;; Audible AAX to M4B Conversion
;; Provides: Convert Audible AAX files to M4B format
;; Usage: M-x d1-convert-aax-to-m4b or M-x d1-dired-convert-aax-to-m4b in dired
(use-package d1-audible :load-path d1-modules-dir)

;;; init.el ends here
