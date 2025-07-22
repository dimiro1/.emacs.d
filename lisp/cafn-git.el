;;; cafn-git.el --- Git integration with built-in VC package  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Git integration using Emacs' built-in VC (Version Control) package
;; instead of external packages like Magit. Provides fast, reliable
;; git operations with a lightweight footprint.
;;

;;; Code:

;;; Built-in VC Configuration
;; Configure the built-in version control system for optimal Git workflow
(use-package vc
  :ensure nil
  :custom
  ;; Always follow symbolic links to version-controlled files
  ;; This prevents VC from asking every time
  (vc-follow-symlinks t)

  ;; Make VC more responsive by reducing some checks
  (vc-handled-backends '(Git))  ; Focus on Git only for better performance

  ;; Configure diff behavior
  (vc-diff-switches "-u")       ; Use unified diff format
  (vc-git-diff-switches '("-w" "-C3"))  ; Ignore whitespace, show 3 lines context

  ;; Better log display
  (vc-git-log-switches '("--graph" "--decorate" "--oneline"))

  :bind
  ;; Main git interface - directory status (replaces magit-status)
  (("C-x g"   . vc-dir)              ; Git status for project/directory
   ("C-x G"   . vc-root-diff)        ; Show all changes in repository

   ;; File-specific git operations
   ("C-c g d" . vc-diff)             ; Diff current file
   ("C-c g l" . vc-print-log)        ; Log for current file
   ("C-c g b" . vc-annotate)         ; Git blame (annotate) for current file
   ("C-c g r" . vc-revert)           ; Revert current file

   ;; Commit and staging operations
   ("C-c g c" . vc-next-action)      ; Context-sensitive action (add, commit, etc.)
   ("C-c g =" . vc-diff)             ; Alternative diff binding
   ("C-c g ~" . vc-revision-other-window)  ; View specific revision

   ;; Branch and tag operations
   ("C-c g s" . vc-create-tag)       ; Create tag
   ("C-c g w" . vc-switch-backend)))  ; Switch VC backend if needed

;;; VC Directory Mode Enhancements
;; Improve the vc-dir interface (Git status equivalent)
(use-package vc-dir
  :ensure nil
  :hook
  ;; Enable hl-line-mode in vc-dir for better visibility
  (vc-dir-mode . hl-line-mode)
  :bind (:map vc-dir-mode-map
         ;; Add convenient keybindings in vc-dir mode
         ("q" . quit-window)          ; Quick quit
         ("g" . revert-buffer)        ; Refresh status
         ("RET" . vc-dir-find-file)   ; Open file
         ("o" . vc-dir-find-file-other-window) ; Open in other window
         ("=" . vc-diff)              ; Diff selected files
         ("l" . vc-print-log)         ; Log for selected files
         ("a" . vc-annotate)          ; Annotate selected file
         ("c" . vc-next-action)       ; Commit or add selected files
         ("r" . vc-revert)))          ; Revert selected files

;;; VC Diff Mode Enhancements
;; Better diff viewing experience
(use-package diff-mode
  :ensure nil
  :custom
  ;; Better diff colors and display
  (diff-default-read-only t)      ; Make diff buffers read-only
  (diff-advance-after-apply-hunk t)  ; Move to next hunk after applying
  :hook
  ;; Enable whitespace visualization in diff mode
  (diff-mode . whitespace-mode))

;;; VC Log Mode Enhancements
;; Better git log viewing
(use-package vc-git
  :ensure nil
  :custom
  ;; Show more context in git logs
  (vc-git-print-log-follow t))      ; Follow renames in git log

;;; Git Gutter - Visual diff indicators in the fringe
;; Keep git-gutter for visual feedback, works well with VC
(use-package git-gutter
  :ensure t
  :hook (prog-mode . git-gutter-mode)
  :custom
  (git-gutter:update-interval 0.5)
  :bind
  ;; Git gutter navigation
  (("C-c g n" . git-gutter:next-hunk)     ; Next change
   ("C-c g p" . git-gutter:previous-hunk) ; Previous change
   ("C-c g h" . git-gutter:popup-hunk)    ; Show hunk details
   ("C-c g R" . git-gutter:revert-hunk))  ; Revert hunk
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

(provide 'cafn-git)
;;; cafn-git.el ends here
