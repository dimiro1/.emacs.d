;;; d1-typing.el --- Configuration for typing enhancement packages -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures typing enhancement packages:
;; - speed-type: typing practice (https://codeberg.org/rafalraczka/speed-type/)
;; - yasnippet: template system for Emacs (https://github.com/joaotavora/yasnippet)
;; - yasnippet-snippets: collection of snippets (https://github.com/AndreaCrotti/yasnippet-snippets)

;;; Code:

(use-package speed-type
  :ensure t
  :custom
  ;; Optional customizations
  (speed-type-min-chars 200)
  (speed-type-max-chars 450))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode))
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(provide 'd1-typing)
;;; d1-typing.el ends here