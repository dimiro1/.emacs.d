;;; Character Encoding
(use-package emacs
  :init
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  :hook
  (term-mode . term-line-mode)) ;; Enable line mode in term-mode

;;; Open init.el Configuration
;; This block defines a custom function to quickly open the user's init.el file
;; for editing. It also binds the function to "C-c i" for easy access.
(use-package emacs
  :init
  (defun open-init-file ()
    "Open the init.el file quickly."
    (interactive)
    (find-file (expand-file-name "init.el" user-emacs-directory)))
  :bind (("C-c i" . open-init-file)))

;;; UI Preferences
(use-package emacs
  :init
  ;; Highlight current line
  (global-hl-line-mode 1)
  ;; Auto-wrap lines longer than 80 characters
  (setopt fill-column 80)
  (auto-fill-mode 1)
  ;; Show trailing whitespace
  (setopt show-trailing-whitespace t)
  ;; Delete selected text when typing
  (delete-selection-mode 1)
  ;; Confirm before quitting Emacs
  (setopt confirm-kill-emacs 'y-or-n-p)
  ;; Smooth scrolling
  (setopt scroll-step 1
          scroll-conservatively 10000)
  ;; Make Emacs start in fullscreen mode
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  ;; Disable the startup screen
  (setopt inhibit-startup-screen t)
  ;; Remove menu bar, tool bar, and scroll bar
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  ;; Highlight matching parentheses
  (show-paren-mode t))

;;; Text Spacing
(use-package emacs
  :init
  (setopt line-spacing 2)
  (setopt default-tab-width 2))

;;; CamelCase Support
(use-package emacs
  :init
  (subword-mode t)) ;; Enable navigation within CamelCase words

;;; Display Settings
(use-package emacs
  :init
  (setopt display-line-numbers-type 'relative)
  (global-display-line-numbers-mode t)
  (column-number-mode t)) ;; Show column numbers

;;; Custom File Loading
(use-package emacs
  :init
  (setopt custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file))

;;; Backup Settings
(use-package emacs
  :init
  (setopt backup-directory-alist `(("." . "~/.emacs-saves"))
          backup-by-copying t
          delete-old-versions t
          kept-new-versions 6
          kept-old-versions 2
          version-control t))

;;; Path Settings
(use-package emacs
  :init
  (add-to-list 'exec-path (concat (getenv "HOME") "/go/bin"))
  (add-to-list 'exec-path (concat (getenv "HOME") "/.cargo/bin")))

;;; Repeat Mode
(use-package emacs
  :init
  :hook (after-init . repeat-mode)) ;; Enable repeat mode
(provide 'my-common)
