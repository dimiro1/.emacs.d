;;; Theme Configuration

;; Gotham Theme: Low-contrast dark theme
(use-package gotham-theme
  :straight t
  :ensure t)

;; Spacemacs Theme: Medium-contrast theme inspired by Spacemacs
(use-package spacemacs-theme
  :straight t
  :ensure t)

;; Gruvbox Theme: Retro theme with medium to high contrast (dark and light variants)
(use-package gruvbox-theme
  :straight t
  :ensure t)

;; Catppuccin Theme: Warm pastel, low-contrast theme for long sessions
(use-package catppuccin-theme
  :straight t
  :ensure t)

;;; Load Catppuccin Theme by Default
(load-theme 'modus-vivendi-deuteranopia :no-confirm)

(provide 'my-colorscheme)
