;;; -*- lexical-binding: t; -*-

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

;;; Go Programming Support
;; Use `go-ts-mode` for Go files and enable Eglot for LSP.
(use-package go-ts-mode
  :mode (("\\.go\\'" . go-ts-mode)
         ("go\\.mod\\'" . go-ts-mode)) ;; Add go.mod to the list
  :hook (go-ts-mode . eglot-ensure)) ;; Enable Eglot in go-ts-mode

;;; Rust Programming Support
;; Configure `rust-ts-mode` for Rust files and enable Eglot for language server support.
(use-package rust-ts-mode
  :mode ("\\.rs\\'" . rust-ts-mode)
  :hook (rust-ts-mode . eglot-ensure))

;;; Eglot-Specific Configuration
;; Configures Eglot to use `rust-analyzer` for Rust files, invoked via `rustup`.
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) . ("rustup" "run" "stable" "rust-analyzer"))))

(provide 'my-lsp)
