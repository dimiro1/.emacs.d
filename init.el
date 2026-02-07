;;; init.el --- Claudemiro Emacs Configuration Entry Point  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Claudemiro's modular Emacs configuration entry point.
;;
;;; Code:

(setopt load-prefer-newer t)  ; Always load newer .el files over .elc

;;; Core Configuration Modules
;; Load configuration modules in dependency order with documentation

;; Core Configuration - Package management and fundamental editor settings
;; Provides: Package management (use-package), encoding, indentation, UI settings,
;; scrolling, line numbers, fonts, which-key, and core editing behavior
;; This must be loaded first as other modules depend on it
(use-package d1-core :ensure nil :demand t)

;; User Interface - Themes, modeline, and file tree
;; Provides: Multiple theme options, custom modeline with project info, NeoTree sidebar
;; Consolidated visual configuration for consistent appearance
(use-package d1-ui :ensure nil :demand t)

;; System Integration - Environment and file management
;; Provides: PATH configuration, shell integration, backup settings, auto-save,
;; authentication, and file persistence
(use-package d1-system :ensure nil :demand t)

;; Eshell - Shell configuration
;; Provides: Prompt customization, aliases, zoxide integration, enhanced commands
(use-package d1-eshell :ensure nil :demand t)

;; Completion System - Complete completion framework
;; Provides: In-buffer completion (Corfu), minibuffer completion (Vertico + Marginalia),
;; fuzzy matching (Orderless), and enhanced completion experience
(use-package d1-completion :ensure nil :demand t)

;; Programming Languages - Language-specific configurations and LSP
;; Provides: Go, Rust, TypeScript, Markdown, Emacs Lisp support with eglot LSP integration
;; Includes: Tree-sitter parsers, Go testing, Parinfer for Lisp editing, enhanced Markdown editing
;; Requires: External LSP servers to be installed
(use-package d1-languages :ensure nil :demand t)

;; Navigation - File and project navigation tools
;; Provides: Enhanced file finding, buffer switching, dired configuration, search tools
(use-package d1-navigation :ensure nil :demand t)

;; Version Control - Git integration and workflow tools
;; Provides: Magit configuration, Git gutter, time machine, and workflow enhancements
(use-package d1-git :ensure nil :demand t)

;; Git Permalink - Generate permalinks to files on git hosting platforms
;; Provides: Browse/copy permalinks to GitHub, GitLab, Codeberg with line numbers
;; Usage: M-x d1-git-browse-permalink or C-c g o (browse)
;;        M-x d1-git-copy-permalink or C-u C-c g o (copy to kill ring)
(use-package d1-git-permalink
  :ensure nil
  :bind ("C-c g o" . d1-git-browse-permalink))

;; Custom Keybindings - Personal keybinding preferences
;; Provides: Custom shortcuts, workflow optimizations
;; Note: Loaded last to override any conflicting bindings
(use-package d1-keybindings :ensure nil :demand t)

;; Typing practice package.
(use-package d1-typing :ensure nil)

;; Atom and RSS feeds.
(use-package d1-rss :ensure nil)

;; Reading - EPUB and document reading
;; Provides: nov.el for EPUB files
(use-package d1-reading :ensure nil)

;; Utilities - General-purpose utility functions
;; Provides: Kill process by port, kill process with completion
;; Usage: M-x d1-kill-process-on-port or C-c x p
;;        M-x d1-kill-process or C-c x k
(use-package d1-utilities
  :ensure nil
  :bind (("C-c x p" . d1-kill-process-on-port)
         ("C-c x k" . d1-kill-process)))

;; AI Apropos - AI-powered function lookup using Ollama
;; Provides: Natural language function search based on current language
;; Usage: M-x d1-ai-apropos or C-c a a
(use-package d1-ai-apropos
  :ensure nil
  :bind ("C-c a a" . d1-ai-apropos))

;; German Language Utilities - German word gender and verb conjugation using Ollama
;; Provides: Gender lookup with article, plural, translations, explanations
;;           Verb conjugation with all major tenses (Präsens, Präteritum, Perfekt, Futur)
;; Usage: M-x d1-german-gender or C-c a g (for nouns)
;;        M-x d1-german-verb or C-c a v (for verbs)
(use-package d1-german
  :ensure nil
  :bind (("C-c a g" . d1-german-gender)
         ("C-c a G" . d1-german-gender-at-point)
         ("C-c a v" . d1-german-verb)
         ("C-c a V" . d1-german-verb-at-point)))

;; NPM Script Runner - Interactive npm script execution
;; Provides: Project-aware npm script discovery, interactive selection
;; Usage: M-x d1-npm-run or C-c n
(use-package d1-npm
  :ensure nil
  :demand t
  :bind
  ("C-c p n" . d1-npm-run))

;; Homebrew Package Manager - Visual package management for Homebrew
;; Provides: Tabulated list of packages, installation tracking, tap support
;; Usage: M-x d1-homebrew
(use-package d1-homebrew :ensure nil)

;; AI Coding Agents - Native agentic integrations for Claude Code, Gemini CLI, etc.
;; Provides: Interactive shell buffers for AI coding agents
;; Usage: M-x agent-shell, M-x agent-shell-new-shell, M-x agent-shell-toggle
(use-package d1-ai :ensure nil)

;; Org-mode - Notes, agenda, and task management
;; Provides: org-directory, default notes file, agenda files
(use-package d1-org :ensure nil :demand t)

;; Audible AAX to M4B Conversion
;; Provides: Convert Audible AAX files to M4B format
;; Usage: M-x d1-convert-aax-to-m4b or M-x d1-dired-convert-aax-to-m4b in dired
(use-package d1-audible :ensure nil)

;;; init.el ends here
