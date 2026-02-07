;;; d1-ui.el --- User interface configuration (themes + modeline + file tree)  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; User interface configuration combining color themes, custom modeline,
;; and file tree browser.
;;
;; This module handles all visual aspects of the Emacs interface.
;;

;;; Code:

;;; Theme Packages

;; Standard Themes - Accessible and well-designed themes by Protesilaos
;; https://protesilaos.com/codelog/2024-12-17-emacs-standard-themes-tinted/
;; Provides both light and dark variants with careful color choices
(use-package standard-themes)

;; Ef Themes - Colorful and legible themes by Protesilaos
;; https://protesilaos.com/emacs/ef-themes
;; Large collection of light/dark themes with varied color palettes
(use-package ef-themes)

;; Doric Themes - Minimalist themes by Protesilaos
;; https://github.com/protesilaos/doric-themes
;; Monochromatic themes with careful typography emphasis
(use-package doric-themes)

;; Doom emacs themes
(use-package doom-themes)

;;; Heading Sizes for Org-mode
;; Must be set before loading themes so they generate the correct faces.
(setopt modus-themes-headings
        '((1 . (bold 1.2))
          (2 . (bold 1.15))
          (3 . (bold 1.1))
          (4 . (bold 1.05))))

(setopt ef-themes-headings
        '((1 . (bold 1.2))
          (2 . (bold 1.15))
          (3 . (bold 1.1))
          (4 . (bold 1.05))))

;;; Load Default Theme
(load-theme 'ef-elea-dark :no-confirm)

;; Add extra padding around the frames and the status-bar.
(use-package spacious-padding
  ;; Use emacs-startup hook to ensure proper initialization order
  ;; Avoids conflicts with other UI packages like which-key
  :hook (emacs-startup . spacious-padding-mode))

;;; Terminal Mouse Support
;; Enable mouse support when running in terminal mode
(use-package xt-mouse
  :unless (display-graphic-p)
  :config
  (xterm-mouse-mode 1))

(provide 'd1-ui)
;;; d1-ui.el ends here
