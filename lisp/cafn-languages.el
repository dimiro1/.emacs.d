;;; cafn-languages.el --- Programming language support with LSP  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Programming language support with LSP.
;;
;;; Code:

;;; Eglot Configuration (LSP Client)
(use-package emacs
  :hook (before-save . (lambda ()
                         (when (eglot-managed-p)
                           ;; Try to organize imports first, but don't stop execution if it fails.
                           ;; Some LSP servers may not support "source.organizeImports",
                           ;; causing an error. `ignore-errors` ensures that the process
                           ;; continues even if this action fails.
                           (when (fboundp 'eglot-code-action-organize-imports)
                             (ignore-errors
                               (call-interactively #'eglot-code-action-organize-imports)))

                           ;; Always format the buffer, even if organizing imports failed.
                           (eglot-format)))))

;; Enable flymake for on-the-fly syntax checking
(use-package flymake
  :hook ((prog-mode . flymake-mode))  ; Enable for all programming modes
  :custom
  (flymake-no-changes-timeout 0.5)    ; Faster feedback
  (flymake-start-on-save-buffer nil)  ; Don't wait for save
  (flymake-proc-ignored-file-name-regexps
   '("\\.#" "\\`#.*#\\'" "\\.git/" "\\.svn/" "_build/" "dist/" "node_modules/")))

;;; Go Programming Support
;; Use `go-ts-mode` for Go files and enable Eglot for LSP.
(use-package go-ts-mode
  :mode (("\\.go\\'" . go-ts-mode)
         ("go\\.mod\\'" . go-ts-mode)) ;; Add go.mod to the list
  :hook (go-ts-mode . eglot-ensure) ;; Enable Eglot in go-ts-mode
  :custom
  ;; Use 4-space indentation for Go
  (go-ts-mode-indent-offset 4))

;; Go Testing Support
;; Provides utilities for running Go tests from within Emacs
(use-package gotest
  :ensure t)

;;; Rust Programming Support
;; Configure `rust-ts-mode` for Rust files and enable Eglot for language server support.
(use-package rust-ts-mode
  :mode ("\\.rs\\'" . rust-ts-mode)
  :hook (rust-ts-mode . eglot-ensure))

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . typescript-ts-mode))
  :hook (typescript-ts-mode . eglot-ensure))

;;; Markdown Support
;; Enhanced Markdown editing with syntax highlighting
(use-package markdown-mode
  :ensure t)

;;; Emacs Lisp Support
;; Paredit for structured editing of Lisp code
;; Ensures parentheses are always balanced and provides structural editing commands
(use-package paredit
  :ensure t
  :hook (emacs-lisp-mode . paredit-mode))

;;; Org Babel Language Support
;; Configure Org mode to recognize additional languages in code blocks
(use-package emacs
  :config
  (with-eval-after-load 'org
    ;; Enable Go syntax highlighting in org-mode code blocks
    ;; This allows #+begin_src go blocks to use go-ts-mode
    (add-to-list 'org-src-lang-modes '("go" . go-ts))))

;;; Eglot-Specific Configuration
;; Configures Eglot to use `rust-analyzer` for Rust files, invoked via `rustup`.
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) . ("rustup" "run" "stable" "rust-analyzer"))))

(provide 'cafn-languages)
;;; cafn-languages.el ends here
