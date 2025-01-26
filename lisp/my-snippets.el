;;; -*- lexical-binding: t; -*-

;;; Enable yasnippet for global snippet expansion.
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode t))

;;; Add predefined snippet templates for various languages and frameworks.
(use-package yasnippet-snippets
  :ensure t)

(provide 'my-snippets)
