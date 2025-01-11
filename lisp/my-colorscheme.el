;;; -*- lexical-binding: t; -*-

;;; Theme Configuration

;; "Standard Themes": https://protesilaos.com/codelog/2024-12-17-emacs-standard-themes-tinted/
(use-package standard-themes
  :straight t)

;;; Load Theme
(load-theme 'modus-operandi-tinted :no-confirm)

(provide 'my-colorscheme)
