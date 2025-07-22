;;; cafn-colorscheme.el --- Color theme configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Color theme configuration and switching utilities.
;;
;;; Code:

;;; Theme Packages

;; Standard Themes - Accessible and well-designed themes
;; https://protesilaos.com/codelog/2024-12-17-emacs-standard-themes-tinted/
;; Provides both light and dark variants with careful color choices
(use-package standard-themes
  :ensure t)

;; Doom Themes - Popular themes from the Doom Emacs distribution
;; https://github.com/doomemacs/themes
;; Includes many popular themes like doom-one, doom-molokai, etc.
(use-package doom-themes
  :ensure t
  :config
  ;; Enable bold and italic fonts in themes
  (setopt doom-themes-enable-bold t
          doom-themes-enable-italic t))

;; Catppuccin Theme - Modern pastel theme
;; A soothing pastel theme with multiple flavor variants
(use-package catppuccin-theme
  :ensure t)

;;; Modus Themes Configuration
;; Configure Modus themes for easy toggling between light and dark
(use-package modus-themes
  :ensure nil
  :custom
  ;; Set themes to toggle between
  (modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
  :bind
  ;; Quick theme toggling
  ("<f5>" . modus-themes-toggle))

;;; Load Default Theme
;; Change this to load your preferred theme
;; Some alternatives:
;;   - (load-theme 'doom-one :no-confirm)
;;   - (load-theme 'standard-light :no-confirm)
;;   - (load-theme 'doom-molokai :no-confirm)
;;   - (load-theme 'modus-operandi-tinted :no-confirm)
(load-theme 'standard-light-tinted :no-confirm)

;;; Helper Functions

(defun cafn-switch-theme (theme)
  "Switch to THEME, disabling all other themes first.
This ensures only one theme is active at a time.

Unlike `load-theme', this function prevents theme stacking by disabling
all currently enabled themes before loading the new one. Without this,
multiple themes can be active simultaneously, causing visual conflicts
and unpredictable colors."
  (interactive
   (list (intern (completing-read "Load theme: "
                                (mapcar #'symbol-name
                                       (custom-available-themes))))))
  ;; Disable all current themes
  (mapc #'disable-theme custom-enabled-themes)
  ;; Load the new theme
  (load-theme theme :no-confirm))

(provide 'cafn-colorscheme)
;;; cafn-colorscheme.el ends here
