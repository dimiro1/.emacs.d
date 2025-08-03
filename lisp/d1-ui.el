;;; d1-ui.el --- User interface configuration (themes + modeline + file tree)  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; User interface configuration combining color themes, custom modeline,
;; and file tree browser. This module handles all visual aspects of
;; the Emacs interface.
;;

;;; Code:

;;; Theme Packages

;; Standard Themes - Accessible and well-designed themes
;; https://protesilaos.com/codelog/2024-12-17-emacs-standard-themes-tinted/
;; Provides both light and dark variants with careful color choices
(use-package standard-themes)

(use-package ef-themes)
(use-package doric-themes)

;;; Load Default Theme
;; Change this to load your preferred theme
(load-theme 'modus-vivendi :no-confirm)

;;; Custom Mode-line Configuration
;; Configure buffer identification to show project-relative paths
(use-package emacs
  :custom
  ;; Define custom buffer identification format
  ;; This replaces the default buffer name display in the mode-line
  (mode-line-buffer-identification
   '(:eval
     (if buffer-file-name
         ;; For file buffers, show project-aware path
         (let* ((project (project-current))
                (file-path (buffer-file-name))
                (rel-path (if project
                              ;; In a project: show "project:relative/path"
                              (concat
                               (project-name project)
                               ":"
                               (file-relative-name file-path
                                                   (project-root project)))
                            ;; Outside project: show abbreviated path
                            (abbreviate-file-name file-path))))
           ;; Format with appropriate face for visibility
           (format "%s" rel-path))
       ;; For non-file buffers, show buffer name as-is
       (format "%s" (buffer-name)))))

  :config
  ;; Ensure mode-line is always visible and readable
  (set-face-attribute 'mode-line nil
                      :box '(:line-width 1 :style released-button)))

(provide 'd1-ui)
;;; d1-ui.el ends here
