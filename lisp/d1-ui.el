;;; d1-ui.el --- User interface configuration (themes + modeline + file tree)  -*- lexical-binding: t; no-byte-compile: t; -*-

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

;;; Load Default Theme
(load-theme 'modus-vivendi-deuteranopia :no-confirm)

;; Add extra padding around the frames and the status-bar.
(use-package spacious-padding
  ;; Use emacs-startup hook to ensure proper initialization order
  ;; Avoids conflicts with other UI packages like which-key
  :hook (emacs-startup . spacious-padding-mode))

(provide 'd1-ui)
;;; d1-ui.el ends here
