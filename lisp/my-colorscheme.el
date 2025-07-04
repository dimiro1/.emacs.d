;;; -*- lexical-binding: t; -*-

;;; Theme Configuration

;; "Standard Themes": https://protesilaos.com/codelog/2024-12-17-emacs-standard-themes-tinted/
(use-package standard-themes
  :ensure t)

;; https://github.com/doomemacs/themes
(use-package doom-themes
  :ensure t)

(use-package catppuccin-theme
  :ensure t)

;;; Load Theme
(load-theme 'doom-ayu-dark :no-confirm)

(provide 'my-colorscheme)
