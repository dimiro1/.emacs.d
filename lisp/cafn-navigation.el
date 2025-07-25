;;; cafn-navigation.el --- File and project navigation tools  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; File and project navigation tools.
;;
;;; Code:

;; Ripgrep integration for fast project searches
(use-package rg)

(use-package consult
  :bind
  (("C-x b"   . consult-buffer)
   ("C-c b"   . consult-buffer)
   ("C-c s r" . consult-ripgrep)
   ("C-c o"   . consult-outline)
   ("C-c s l" . consult-line-multi)
   ("C-c f r" . consult-recent-file)
   ("C-c ! l" . consult-flymake)
   ("C-c ! p" . cafn-consult-flymake-project))
  :config
  (consult-customize
   consult-flymake
   :preview-key '(:debounce 0.2 any))

  (defun cafn-consult-flymake-project ()
    "Show flymake diagnostics for all buffers in the current project."
    (interactive)
    (let ((project (project-current)))
      (if project
          (consult-flymake (project-root project))
        (consult-flymake)))))

;; Enhanced project file navigation
(use-package consult-project-extra
  :bind
  (("C-c p f" . consult-project-extra-find)
   ("C-c p o" . consult-project-extra-find-other-window)))

;; Consult integration with Eglot LSP
(use-package consult-eglot)

;;; Avy Configuration
(use-package avy
  :config
  (avy-setup-default)
  :bind
  (("M-g c"   . avy-goto-char)
   ("M-g C"   . avy-goto-char-2)
   ("M-g l"   . avy-goto-line)
   ("M-g w"   . avy-goto-word-1)
   ("M-g e"   . avy-goto-word-0)
   ("C-c g r" . avy-resume)))

;;; Dired Configuration
(use-package dired
  :ensure nil
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  (dired-dwim-target t)
  (dired-listing-switches "-alh --group-directories-first")
  (dired-auto-revert-buffer t)
  (large-file-warning-threshold nil)
  (trash-directory (when (eq system-type 'darwin) "~/.Trash"))
  :bind (:map dired-mode-map
              (")" . dired-hide-details-mode)
              ("(" . dired-hide-details-mode)
              ("h" . dired-up-directory)
              ("l" . dired-find-file)
              ("e" . dired-find-file)
              ("E" . dired-find-file-other-window)))

(provide 'cafn-navigation)
;;; cafn-navigation.el ends here
