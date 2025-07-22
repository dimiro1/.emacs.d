;;; cafn-magit.el --- Git integration with Magit  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Git integration with Magit.
;;
;;; Code:

(use-package magit
  :ensure t
  :bind
  ;; Global binding for quick access to git status
  (("C-x g" . magit-status)
   ;; Additional useful bindings
   ("C-x M-g" . magit-dispatch)    ; Magit command dispatcher
   ("C-c g b" . magit-blame)        ; Git blame for current file
   ("C-c g l" . magit-log-buffer-file)) ; Git log for current file
  :custom
  (magit-diff-refine-hunk t)         ; Show word-level differences in hunks
  :config
  ;; Fullscreen magit status
  (setopt magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

;;; Additional Git-related packages

;; Git Gutter - Show git diff indicators in the fringe
(use-package git-gutter
  :ensure t
  :hook (prog-mode . git-gutter-mode)
  :custom
  (git-gutter:update-interval 0.5)
  :config
  ;; Set symbols based on environment
  (if (display-graphic-p)
      ;; GUI environment - use spaces for cleaner look
      (progn
        (setopt git-gutter:modified-sign " ")
        (setopt git-gutter:added-sign " ")
        (setopt git-gutter:deleted-sign " "))
    ;; Terminal environment - use block symbols
    (progn
      (setopt git-gutter:modified-sign "▒")
      (setopt git-gutter:added-sign "▒")
      (setopt git-gutter:deleted-sign "▒")))

  ;; Customize git-gutter faces for better visibility
  (set-face-foreground 'git-gutter:modified (if (display-graphic-p) "yellow" "orange"))
  (set-face-foreground 'git-gutter:added (if (display-graphic-p) "green" "LimeGreen"))
  (set-face-foreground 'git-gutter:deleted "red"))

;; Git Time Machine - Walk through git history of a file
(use-package git-timemachine
  :ensure t
  :bind ("C-c g t" . git-timemachine))

(provide 'cafn-magit)
;;; cafn-magit.el ends here
