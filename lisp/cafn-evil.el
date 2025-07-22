;;; cafn-evil.el --- Evil mode configuration for Vim emulation  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Evil mode configuration for Vim emulation.
;;
;;; Code:

(use-package evil
  :ensure t
  :init
  (setopt evil-want-integration t)
  (setopt evil-want-keybinding nil)
  (setopt evil-want-C-u-scroll t)
  (setopt evil-want-Y-yank-to-eol t)
  :config
  ;; Define prefix maps
  (define-prefix-command 'cafn-leader-map)
  (define-prefix-command 'cafn-workspace-map)
  (define-prefix-command 'cafn-project-map)
  (define-prefix-command 'cafn-help-map)
  (define-prefix-command 'cafn-quit-map)

  ;; Set up leader key
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-define-key 'normal 'global (kbd "<leader>") 'cafn-leader-map)
  (define-key cafn-leader-map "w" cafn-workspace-map)
  (define-key cafn-leader-map "p" cafn-project-map)
  (define-key cafn-leader-map "h" cafn-help-map)
  (define-key cafn-leader-map "q" cafn-quit-map)

  ;; Quit commands
  (define-key cafn-quit-map "q" 'save-buffers-kill-terminal)

  ;; Help commands
  (define-key cafn-help-map "f" 'describe-function)
  (define-key cafn-help-map "v" 'describe-variable)
  (define-key cafn-help-map "k" 'describe-key)
  (define-key cafn-help-map "m" 'describe-mode)
  (define-key cafn-help-map "b" 'describe-bindings)
  (define-key cafn-help-map "p" 'describe-package)
  (define-key cafn-help-map "t" 'consult-theme)
  (define-key cafn-help-map "i" 'info)
  (define-key cafn-help-map "l" 'view-lossage)  ;; Show recent keystrokes
  (define-key cafn-help-map "r" 'reload-init-file)  ;; Custom function defined below

  ;; File and search operations
  (define-key cafn-leader-map "f" 'consult-find)
  (define-key cafn-leader-map "s" 'consult-outline)
  (define-key cafn-leader-map "S" 'consult-eglot-symbols)
  (define-key cafn-leader-map "t" 'consult-theme)
  (define-key cafn-leader-map "d" 'cafn-consult-flymake-project)
  (define-key cafn-leader-map "l" 'consult-line)
  (define-key cafn-leader-map "b" 'consult-buffer)
  (define-key cafn-leader-map "/" 'consult-ripgrep)
  (define-key cafn-leader-map "j" 'evil-collection-consult-jump-list)

  ;; Enhanced project commands
  (define-key cafn-project-map "f" 'project-find-file)
  (define-key cafn-project-map "b" 'project-switch-to-buffer)
  (define-key cafn-project-map "p" 'project-switch-project)
  (define-key cafn-project-map "d" 'project-find-dir)
  (define-key cafn-project-map "g" 'consult-ripgrep)  ;; Better search with consult
  (define-key cafn-project-map "e" 'project-eshell)
  (define-key cafn-project-map "k" 'project-kill-buffers)
  (define-key cafn-project-map "!" 'project-shell-command)
  (define-key cafn-project-map "&" 'project-async-shell-command)
  (define-key cafn-project-map "c" 'project-compile)
  (define-key cafn-project-map "D" 'project-dired)
  (define-key cafn-project-map "v" 'project-vc-dir)

  ;; Workspace commands
  (define-key cafn-workspace-map "a" 'eglot-add-workspace-folder)
  (define-key cafn-workspace-map "r" 'eglot-remove-workspace-folder)
  (define-key cafn-workspace-map "v" 'split-window-right)
  (define-key cafn-workspace-map "s" 'split-window-below)
  (define-key cafn-workspace-map "d" 'delete-window)
  (define-key cafn-workspace-map "D" 'delete-other-windows)
  (define-key cafn-workspace-map "n" 'windmove-left)
  (define-key cafn-workspace-map "i" 'windmove-right)
  (define-key cafn-workspace-map "e" 'windmove-down)
  (define-key cafn-workspace-map "u" 'windmove-up)

  ;; Code actions and refactoring
  (define-key cafn-leader-map "a" 'eglot-code-actions)
  (define-key cafn-leader-map "r" 'eglot-rename)
  (define-key cafn-leader-map "F" 'eglot-format-buffer)

  ;; Symbols
  (define-key cafn-leader-map "i" 'consult-imenu)  ;; Changed from S to i for consistency

  ;; LSP navigation
  (evil-define-key 'normal 'global (kbd "gh") 'eglot-help-at-point)
  (evil-define-key 'normal 'global (kbd "gd") 'eglot-find-declaration)
  (evil-define-key 'normal 'global (kbd "gD") 'eglot-find-typeDefinition)
  (evil-define-key 'normal 'global (kbd "gr") 'xref-find-references)
  (evil-define-key 'normal 'global (kbd "gI") 'eglot-find-implementation)
  (evil-define-key 'normal 'global (kbd "gi") 'eglot-find-implementation)

  ;; avy (go to any word)
  (evil-define-key 'normal 'global (kbd "gw") 'avy-goto-char)

  ;; Diagnostics
  (evil-define-key 'normal 'global (kbd "]d") 'flymake-goto-next-error)
  (evil-define-key 'normal 'global (kbd "[d") 'flymake-goto-prev-error)

  ;; Navigation
  (define-key evil-normal-state-map (kbd "C-w n") 'windmove-left)
  (define-key evil-normal-state-map (kbd "C-w i") 'windmove-right)
  (define-key evil-normal-state-map (kbd "C-w e") 'windmove-down)
  (define-key evil-normal-state-map (kbd "C-w u") 'windmove-up)

  ;; Enable auto-indentation
  (electric-indent-mode 1)
  ;; Enable evil
  (evil-mode 1))

;;; Avy Configuration
;; Avy provides quick navigation to any visible text
(use-package avy
  :ensure t
  :custom
  ;; Use Colemak-friendly keys for avy navigation
  ;; These keys are positioned for easy access on Colemak layout
  (avy-keys '(97 114 115 116 103 109 110 101 105)) ; a r s t g m n e i
  :bind
  ;; Additional avy bindings
  ("C-;" . avy-goto-char)
  ("C-'" . avy-goto-char-2)
  ("M-g f" . avy-goto-line)
  ("M-g w" . avy-goto-word-1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package consult
  :ensure t
  :bind (:map evil-ex-completion-map
              ("C-n" . consult-history)
              ("C-p" . consult-history)))

(provide 'cafn-evil)
;;; cafn-evil.el ends here