;;; Eglot Configuration (LSP Client)
(defun my-eglot-format-on-save ()
  "Format the buffer with Eglot before saving, if Eglot is managing it."
  (when (bound-and-true-p eglot--managed-mode)
    (eglot-format)))

(add-hook 'before-save-hook #'my-eglot-format-on-save)

;;; Go Programming Support
;; Use `go-ts-mode` for Go files and enable Eglot for LSP.
(use-package go-ts-mode
  :mode (("\\.go\\'" . go-ts-mode)
         ("go\\.mod\\'" . go-ts-mode)) ;; Add go.mod to the list
  :hook (go-ts-mode . eglot-ensure)) ;; Enable Eglot in go-ts-mode

(provide 'my-lsp)
