;;; -*- lexical-binding: t; -*-

;;; Theme Configuration

;; Gruvbox Theme: Retro theme with medium to high contrast (dark and light variants)
(use-package gruvbox-theme
  :straight t)

;; Catppuccin Theme: Warm pastel, low-contrast theme for long sessions
(use-package catppuccin-theme
  :straight t)

;; "Standard Themes": https://protesilaos.com/codelog/2024-12-17-emacs-standard-themes-tinted/
(use-package standard-themes
  :straight t)

;;; Load Theme
(load-theme 'modus-operandi-tinted :no-confirm)

(provide 'my-colorscheme)
