(use-package evil
  :ensure t
  :init
  (setopt evil-want-integration t)
  (setopt evil-want-keybinding nil)
  (setopt evil-want-C-u-scroll t)
  (setopt evil-want-Y-yank-to-eol t)
  :config
  ;; Define prefix maps
  (define-prefix-command 'my-leader-map)
  (define-prefix-command 'my-workspace-map)
  (define-prefix-command 'my-project-map)
  (define-prefix-command 'my-help-map)

  ;; Set up leader key
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-define-key 'normal 'global (kbd "<leader>") 'my-leader-map)
  (define-key my-leader-map "w" my-workspace-map)
  (define-key my-leader-map "p" my-project-map)
  (define-key my-leader-map "h" my-help-map)

  ;; Help commands
  (define-key my-help-map "f" 'describe-function)
  (define-key my-help-map "v" 'describe-variable)
  (define-key my-help-map "k" 'describe-key)
  (define-key my-help-map "m" 'describe-mode)
  (define-key my-help-map "b" 'describe-bindings)
  (define-key my-help-map "p" 'describe-package)
  (define-key my-help-map "t" 'consult-theme)
  (define-key my-help-map "i" 'info)
  (define-key my-help-map "l" 'view-lossage)  ;; Show recent keystrokes
  (define-key my-help-map "r" 'reload-init-file)  ;; Custom function defined below


  ;; File and search operations
  (define-key my-leader-map "f" 'consult-find)
  (define-key my-leader-map "s" 'consult-outline)
  (define-key my-leader-map "S" 'consult-eglot-symbols)
  (define-key my-leader-map "t" 'consult-theme)
  (define-key my-leader-map "d" 'consult-flymake)
  (define-key my-leader-map "l" 'consult-line)
  (define-key my-leader-map "b" 'consult-buffer)
  (define-key my-leader-map "/" 'consult-ripgrep)
  (define-key my-leader-map "j" 'evil-collection-consult-jump-list)

  ;; Enhanced project commands
  (define-key my-project-map "f" 'project-find-file)
  (define-key my-project-map "b" 'project-switch-to-buffer)
  (define-key my-project-map "p" 'project-switch-project)
  (define-key my-project-map "d" 'project-find-dir)
  (define-key my-project-map "g" 'consult-ripgrep)  ;; Better search with consult
  (define-key my-project-map "e" 'project-eshell)
  (define-key my-project-map "k" 'project-kill-buffers)
  (define-key my-project-map "!" 'project-shell-command)
  (define-key my-project-map "&" 'project-async-shell-command)
  (define-key my-project-map "c" 'project-compile)
  (define-key my-project-map "D" 'project-dired)
  (define-key my-project-map "v" 'project-vc-dir)

  ;; LSP navigation
  (evil-define-key 'normal 'global (kbd "gh") 'eglot-help-at-point)
  (evil-define-key 'normal 'global (kbd "gd") 'eglot-find-declaration)
  (evil-define-key 'normal 'global (kbd "gD") 'eglot-find-typeDefinition)
  (evil-define-key 'normal 'global (kbd "gr") 'xref-find-references)
  (evil-define-key 'normal 'global (kbd "gI") 'eglot-find-implementation)
  (evil-define-key 'normal 'global (kbd "gi") 'eglot-find-implementation)

  ;; Workspace commands
  (define-key my-workspace-map "a" 'eglot-add-workspace-folder)
  (define-key my-workspace-map "r" 'eglot-remove-workspace-folder)
  (define-key my-workspace-map "v" 'split-window-right)
  (define-key my-workspace-map "s" 'split-window-below)

  ;; Code actions and refactoring
  (define-key my-leader-map "a" 'eglot-code-actions)
  (define-key my-leader-map "r" 'eglot-rename)
  (define-key my-leader-map "F" 'eglot-format-buffer)

  ;; Diagnostics
  (evil-define-key 'normal 'global (kbd "]d") 'flymake-goto-next-error)
  (evil-define-key 'normal 'global (kbd "[d") 'flymake-goto-prev-error)

  ;; Symbols
  (define-key my-leader-map "i" 'consult-imenu)  ;; Changed from S to i for consistency

  ;; Navigation
  (define-key evil-normal-state-map (kbd "C-w h") 'windmove-left)
  (define-key evil-normal-state-map (kbd "C-w l") 'windmove-right)
  (define-key evil-normal-state-map (kbd "C-w j") 'windmove-down)
  (define-key evil-normal-state-map (kbd "C-w k") 'windmove-up)

  ;; Enable auto-indentation
  (electric-indent-mode 1)
  ;; Enable evil
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :custom
  ;; Configure how mode line looks
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-persp-name t)
  (doom-modeline-persp-icon t)
  (doom-modeline-window-width-limit 85)
  (doom-modeline-project-detection 'project)
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  ;; Evil state display customization
  (doom-modeline-modal-icon t)
  (doom-modeline-modal t))

;; Optional: Make the modeline more colorful for different Evil states
(use-package all-the-icons
  :ensure t)

;; If you want custom colors for different evil states
(setopt evil-normal-state-tag   (propertize " <N> " 'face '(:foreground "green"))
		evil-emacs-state-tag    (propertize " <E> " 'face '(:foreground "orange"))
		evil-insert-state-tag   (propertize " <I> " 'face '(:foreground "red"))
		evil-motion-state-tag   (propertize " <M> " 'face '(:foreground "blue"))
		evil-visual-state-tag   (propertize " <V> " 'face '(:foreground "purple"))
		evil-operator-state-tag (propertize " <O> " 'face '(:foreground "cyan")))

(use-package consult
  :ensure t
  :bind (:map evil-ex-completion-map
              ("C-n" . consult-history)
              ("C-p" . consult-history)))

(provide 'my-evil)
