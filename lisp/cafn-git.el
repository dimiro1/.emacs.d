;;; cafn-git.el --- Git integration with built-in VC package  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Git integration using Emacs' built-in VC (Version Control) package
;; instead of external packages like Magit. Provides fast, reliable
;; git operations with a lightweight footprint.
;;

;;; Code:

;;; Built-in VC Configuration
(use-package vc
  :ensure nil
  :custom
  (vc-follow-symlinks t)
  (vc-handled-backends '(Git))  ; Git only for better performance
  (vc-diff-switches "-u")
  (vc-git-diff-switches '("-w" "-C3"))  ; Ignore whitespace, 3 lines context
  (vc-git-log-switches '("--graph" "--decorate" "--oneline"))

  :bind
  (("C-x g"   . vc-dir)
   ("C-x G"   . vc-root-diff)
   ("C-c g d" . vc-diff)
   ("C-c g l" . vc-print-log)
   ("C-c g b" . vc-annotate)
   ("C-c g r" . vc-revert)
   ("C-c g c" . vc-next-action)
   ("C-c g =" . vc-diff)
   ("C-c g ~" . vc-revision-other-window)
   ("C-c g s" . vc-create-tag)
   ("C-c g w" . vc-switch-backend)))

;;; VC Directory Mode Enhancements
(use-package vc-dir
  :ensure nil
  :hook
  (vc-dir-mode . hl-line-mode)
  :bind (:map vc-dir-mode-map
              ("q" . quit-window)
              ("g" . revert-buffer)
              ("RET" . vc-dir-find-file)
              ("o" . vc-dir-find-file-other-window)
              ("=" . vc-diff)
              ("l" . vc-print-log)
              ("a" . vc-annotate)
              ("c" . vc-next-action)
              ("r" . vc-revert)))

;;; VC Diff Mode Enhancements
(use-package diff-mode
  :ensure nil
  :custom
  (diff-default-read-only t)
  (diff-advance-after-apply-hunk t)
  :hook
  (diff-mode . whitespace-mode))

;;; VC Log Mode Enhancements
(use-package vc-git
  :ensure nil
  :custom
  (vc-git-print-log-follow t))  ; Follow renames in git log

;;; Git Gutter - Visual diff indicators in the fringe
(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :custom
  (git-gutter:update-interval 0.5)
  ;; Different symbols for GUI vs terminal
  (git-gutter:modified-sign (if (display-graphic-p) " " "▒"))
  (git-gutter:added-sign (if (display-graphic-p) " " "▒"))
  (git-gutter:deleted-sign (if (display-graphic-p) " " "▒"))
  :bind
  (("C-c g n" . git-gutter:next-hunk)
   ("C-c g p" . git-gutter:previous-hunk)
   ("C-c g h" . git-gutter:popup-hunk)
   ("C-c g R" . git-gutter:revert-hunk))
  :config
  ;; Customize colors
  (set-face-foreground 'git-gutter:modified (if (display-graphic-p) "yellow" "orange"))
  (set-face-foreground 'git-gutter:added (if (display-graphic-p) "green" "LimeGreen"))
  (set-face-foreground 'git-gutter:deleted "red"))

(provide 'cafn-git)
;;; cafn-git.el ends here
