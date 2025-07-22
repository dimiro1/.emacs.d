;;; cafn-productivity.el --- Productivity tools (Org + Snippets + Tree-sitter)  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Productivity enhancement tools combining Org mode for organization,
;; YASnippet for code templates, and Tree-sitter for advanced syntax analysis.
;; These tools enhance editing efficiency and content organization.
;;

;;; Code:

;;; YASnippet - Code Snippet System
;; Template system for common code patterns and boilerplate
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode t))

;; Add predefined snippet templates for various languages and frameworks
(use-package yasnippet-snippets
  :ensure t)

;;; Tree-sitter Configuration
;; Modern syntax highlighting and code analysis using Tree-sitter parsers
;; Note: Requires Emacs 29+ with tree-sitter support

;; Define language parsers and their source repositories for Tree-sitter
(setopt treesit-language-source-alist
        '((make       "https://github.com/alemuller/tree-sitter-make")
          (gomod      "https://github.com/camdencheek/tree-sitter-go-mod")
          (go         "https://github.com/tree-sitter/tree-sitter-go")
          (markdown   "https://github.com/ikatyang/tree-sitter-markdown")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (markdown   "https://github.com/tree-sitter-grammars/tree-sitter-markdown")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (yaml       "https://github.com/ikatyang/tree-sitter-yaml")))

(provide 'cafn-productivity)
;;; cafn-productivity.el ends here
