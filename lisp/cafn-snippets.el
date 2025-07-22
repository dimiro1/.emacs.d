;;; cafn-snippets.el --- Snippet management with yasnippet  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Snippet management with yasnippet.
;;
;;; Code:

;;; Enable yasnippet for global snippet expansion.
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode t))

;;; Add predefined snippet templates for various languages and frameworks.
(use-package yasnippet-snippets
  :ensure t)

(provide 'cafn-snippets)
;;; cafn-snippets.el ends here
