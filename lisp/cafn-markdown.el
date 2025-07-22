;;; cafn-markdown.el --- Markdown editing configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Markdown editing support.
;;
;;; Code:

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)) ; GitHub Flavored Markdown for README files
  :custom
  ;; Enable native syntax highlighting in code blocks
  ;; This makes code examples much more readable
  (markdown-fontify-code-blocks-natively t)

  ;; Configure language modes for code blocks
  ;; This maps code block language identifiers to their corresponding major modes
  (markdown-code-lang-modes
   '(("ocaml" . tuareg-mode)
     ("elisp" . emacs-lisp-mode)
     ("ditaa" . artist-mode)
     ("asymptote" . asy-mode)
     ("dot" . fundamental-mode)
     ("sqlite" . sql-mode)
     ("calc" . fundamental-mode)
     ("C" . c-mode)
     ("cpp" . c++-mode)
     ("C++" . c++-mode)
     ("screen" . shell-script-mode)
     ("shell" . sh-mode)
     ("bash" . sh-mode)
     ("go" . go-ts-mode)        ; Use tree-sitter mode for Go
     ("rust" . rust-ts-mode)))  ; Use tree-sitter mode for Rust

  :config
  ;; Additional markdown configuration can go here
  ;; For example, custom keybindings or hooks
  )

(provide 'cafn-markdown)
;;; cafn-markdown.el ends here