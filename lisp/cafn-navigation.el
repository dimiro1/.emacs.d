;;; cafn-navigation.el --- File and project navigation tools  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; File and project navigation tools.
;;
;;; Code:

;;; rg Configuration
;; rg (Ripgrep) integrates the ripgrep search tool with Emacs for fast, recursive searches.
(use-package rg
  :ensure t)

(use-package consult
  :ensure t
  :bind
  ("C-x b" . consult-buffer)
  ("C-c b" . consult-buffer)
  ("C-c s r" . consult-ripgrep)
  ("C-c o" . consult-outline)
  ("C-c s l" . consult-line-multi)
  ("C-c f r" . consult-recent-file))

(use-package consult-project-extra
  :ensure t
  :bind
  (("C-c p f" . consult-project-extra-find)
   ("C-c p o" . consult-project-extra-find-other-window)))

(use-package consult-eglot
  :ensure t)

;; Configure consult's built-in flymake integration
(use-package consult
  :after flymake
  :bind (("C-c ! l" . consult-flymake)
         ("C-c ! p" . cafn-consult-flymake-project))
  :config
  ;; Customize the preview behavior for flymake diagnostics
  (consult-customize
   consult-flymake
   :preview-key '(:debounce 0.2 any))

  ;; Custom function for project-wide flymake diagnostics
  (defun cafn-consult-flymake-project ()
    "Show flymake diagnostics for all buffers in the current project."
    (interactive)
    (let ((project (project-current)))
      (if project
          (consult-flymake (project-root project))
        (consult-flymake)))))

;;; Avy Configuration
;; Avy is a powerful navigation package that allows you to jump to visible text
;; in the current window quickly using a minimal number of keystrokes.
(use-package avy
  :ensure t
  :config
  (avy-setup-default) ;; Set up Avy's default configurations
  :bind
  (("M-g c"   . avy-goto-char)	 ;; Jump to a single character
   ("M-g C"   . avy-goto-char-2) ;; Jump to a two-character sequence
   ("M-g l"   . avy-goto-line)	 ;; Jump to a specific line
   ("M-g w"   . avy-goto-word-1) ;; Jump to the beginning of a word
   ("M-g e"   . avy-goto-word-0) ;; Jump to any part of a word
   ("C-c g r" . avy-resume)))    ;; Resume the last Avy command

;;; Dired Configuration
;; Enhanced file manager with better defaults and usability improvements
(use-package dired
  :ensure nil
  :hook
  ;; Hide detailed file information by default for cleaner view
  ((dired-mode . dired-hide-details-mode)
   ;; Highlight current line in dired for better visibility
   (dired-mode . hl-line-mode))
  :custom
  ;; Allow recursive operations without confirmation
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  ;; Use system trash instead of permanent deletion
  (delete-by-moving-to-trash t)
  ;; Smart target selection - if two dired windows are open,
  ;; use the other one as default target for copy/move operations
  (dired-dwim-target t)
  ;; Show directories first, then files (requires GNU ls)
  ;; This will be overridden in cafn-environment.el if GNU ls is not available
  (dired-listing-switches "-alh --group-directories-first")
  ;; Auto-refresh dired buffers when files change
  (dired-auto-revert-buffer t)
  ;; Don't ask for confirmation when opening large files
  (large-file-warning-threshold nil)
  :config
  ;; Ensure trash directory exists on macOS
  (when (eq system-type 'darwin)
    (setopt trash-directory "~/.Trash"))

  ;; Add some useful dired keybindings
  :bind (:map dired-mode-map
         (")" . dired-hide-details-mode)  ; Toggle details view
         ("(" . dired-hide-details-mode)  ; Toggle details view
         ("h" . dired-up-directory)       ; Go up directory (vim-like)
         ("l" . dired-find-file)          ; Open file/directory (vim-like)
         ("e" . dired-find-file)          ; Alternative open binding
         ("E" . dired-find-file-other-window))) ; Open in other window

(provide 'cafn-navigation)
;;; cafn-navigation.el ends here
