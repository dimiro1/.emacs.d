;;; d1-navigation.el --- File and project navigation tools  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; File and project navigation tools.
;;
;;; Code:

;; Display project name in mode line
(use-package project
  :ensure nil
  :custom
  (project-mode-line 1))

(use-package consult
  :bind
  (;; C-c bindings (mode-specific-map)
   ("C-c M-x" . consult-mode-command)
   ("C-c h"   . consult-history)
   ("C-c k"   . consult-kmacro)
   ("C-c m"   . consult-man)
   ("C-c i"   . consult-info)
   ([remap Info-search] . consult-info)
   ;; C-x bindings (ctl-x-map)
   ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
   ("C-x b"   . consult-buffer)              ;; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
   ;; Custom bindings
   ("C-c b"   . consult-buffer)
   ("C-c o"   . consult-outline)
   ("C-c s l" . consult-line-multi)
   ("C-c f r" . consult-recent-file)
   ;; M-g bindings (goto-map)
   ("M-g e"   . consult-compile-error)
   ("M-g g"   . consult-goto-line)           ;; orig. goto-line
   ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
   ("M-g o"   . consult-outline)             ;; Alternative: consult-org-heading
   ("M-g m"   . consult-mark)
   ("M-g k"   . consult-global-mark)
   ("M-g i"   . consult-imenu)
   ("M-g I"   . consult-imenu-multi)
   ;; M-s bindings (search-map)
   ("M-s d"   . consult-find)
   ("M-s D"   . consult-locate)
   ("M-s g"   . consult-grep)
   ("M-s G"   . consult-git-grep)
   ("M-s r"   . consult-ripgrep)
   ("M-s l"   . consult-line)
   ("M-s L"   . consult-line-multi)
   ("M-s k"   . consult-keep-lines)
   ("M-s u"   . consult-focus-lines)
   ;; Isearch integration
   ("M-s e"   . consult-isearch-history)
   ;; Flymake
   ("C-c ! l" . consult-flymake)
   ("C-c ! p" . d1-consult-flymake-project)
   ;; Project specific
   ("C-c p b" . consult-project-buffer)
   ("C-c p g" . consult-ripgrep)
   ("C-c p r" . consult-recent-file))
  ;; use consult for completion
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  ;; Integrate with xref.
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  (consult-preview-key 'any)
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (consult-narrow-key "<") ;; "C-+"
  :config
  ;; Tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Make narrowing help available in the minibuffer.
  (keymap-set consult-narrow-map "?" #'consult-narrow-help)

  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any)
   consult-flymake
   :preview-key '(:debounce 0.2 any))

  (defun d1-consult-flymake-project ()
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
  (dired-listing-switches (if (executable-find "gls")
                              "-alh --group-directories-first"
                            "-alh"))
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

(provide 'd1-navigation)
;;; d1-navigation.el ends here
