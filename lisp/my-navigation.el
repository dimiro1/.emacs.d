;;; -*- lexical-binding: t; -*-

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

(use-package embark
  :ensure t

  :bind
  (("C-," . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setopt prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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

;; Dired Configuration
(use-package dired
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode))
  :custom
  (dired-recursive-copies 'always)  ; Allow recursive copies
  (dired-recursive-deletes 'always) ; Allow recursive deletes
  (delete-by-moving-to-trash t)     ; Delete files by moving them to trash
  (dired-dwim-target t))            ; Enable "do what I mean" target selection

;; The built-in `recentf-mode' keeps track of recently visited files.
(use-package emacs
  :hook (after-init . recentf-mode))

(provide 'my-navigation)
