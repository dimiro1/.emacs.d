;;; cafn-modeline.el --- Custom mode-line configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Custom mode-line configuration.
;;
;;; Code:

;;; Custom Mode-line Format
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
  ;; Additional mode-line customizations can go here
  ;; For example, you might want to add git branch info, encoding, etc.

  ;; Ensure mode-line is always visible and readable
  (set-face-attribute 'mode-line nil
                      :box '(:line-width 1 :style released-button)))

;;; Helper Functions for Mode-line Information
(defun cafn-mode-line-file-encoding ()
  "Return a string describing the current buffer's encoding.
Useful for adding to mode-line-format if desired."
  (let ((coding buffer-file-coding-system))
    (if coding
        (upcase (symbol-name (coding-system-base coding)))
      "?")))

(defun cafn-mode-line-git-branch ()
  "Return current git branch name for mode-line display.
Returns nil if not in a git repository."
  (when (and (fboundp 'vc-git-mode-line-string)
             buffer-file-name
             (vc-backend buffer-file-name))
    (substring-no-properties (vc-git-mode-line-string buffer-file-name))))

;;; Optional: Enhanced Mode-line Format
;; Uncomment and customize this if you want a completely custom mode-line
;; (setq-default mode-line-format
;;               '("%e"  ; Error status
;;                 mode-line-front-space
;;                 mode-line-mule-info
;;                 mode-line-client
;;                 mode-line-modified
;;                 mode-line-remote
;;                 mode-line-frame-identification
;;                 mode-line-buffer-identification  ; Our custom format
;;                 "   "
;;                 mode-line-position
;;                 "  "
;;                 mode-line-modes
;;                 mode-line-misc-info
;;                 mode-line-end-spaces))

(provide 'cafn-modeline)
;;; cafn-modeline.el ends here