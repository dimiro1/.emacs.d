;;; d1-completion.el --- Completion system configuration  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;;
;; Provides a modern, fast completion experience throughout Emacs.
;;

;;; Code:

;;; Built-in Completion Configuration
(use-package emacs
  :config
  ;; Enables the vertical minibuffer for command completion.
  (fido-vertical-mode 1)

  :custom
  ;; Automatically select first completion candidate
  ;; This allows immediate selection without extra TAB presses
  (completion-auto-select t)

  ;; Show *Completions* buffer immediately on first TAB
  ;; No need to press TAB twice to see options
  (completion-auto-help 'visible)

  ;; Display completions in a single column for better readability
  ;; Alternative: 'horizontal for side-by-side display
  (completions-format 'one-column)

  ;; Sort completions by usage history
  ;; Most recently/frequently used items appear first
  ;; Alternative: 'alphabetical for A-Z sorting
  (completions-sort 'historical)

  ;; Limit completion window height to avoid taking too much space
  ;; Adjust this based on your screen size
  (completions-max-height 20)

  ;; Enable case-insensitive completion
  ;; Makes completion more forgiving for mixed-case identifiers
  (completion-ignore-case t)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p))


;; Persist history over Emacs restarts.
(use-package savehist
  :config
  (savehist-mode 1))

;; Optionally use the `orderless' completion style for fuzzy matching
(use-package orderless
  :custom
  (completion-styles '(orderless flex basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;;; Marginalia: Adds annotations to completion candidates
(use-package marginalia
  :config
  (marginalia-mode 1))

;;; Corfu: In-buffer completion UI
(use-package corfu
  :init
  (global-corfu-mode))

;;; Cape: Additional completion backends
(use-package cape
  :init
  ;; Add completion functions to the global completion list
  ;; Order matters: more specific completions should come first
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)

  :hook
  ;; Add mode-specific completion functions
  ((emacs-lisp-mode . (lambda ()
						(add-hook 'completion-at-point-functions #'cape-elisp-symbol nil t)))
   (org-mode . (lambda ()
				 (add-hook 'completion-at-point-functions #'cape-elisp-block nil t)))
   (eshell-mode . (lambda ()
					(add-hook 'completion-at-point-functions #'cape-history nil t))))

  :bind ("C-c p" . cape-prefix-map)

  :config
  ;; Optional: Make dabbrev case-sensitive
  (setq cape-dabbrev-check-other-buffers 'some))

;;; Expand the region syntatically.
(use-package expand-region
  :bind (("C-=" . er/expand-region)
		 ("C--" . er/contract-region))
  :config
  ;; Enable repeat mode for expand-region commands
  (defvar-keymap d1-expand-region-repeat-map
	:doc "Repeat map for expand-region commands."
	:repeat t
	"=" #'er/expand-region
	"-" #'er/contract-region))

;;; Github copilot
(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
			:rev :newest
			:branch "main")
  ;; :hook prog-mode
  :bind
  ((:map copilot-completion-map
		 ("M-<tab>" . copilot-accept-completion)
		 ("M-TAB" . copilot-accept-completion))))

(provide 'd1-completion)
;;; d1-completion.el ends here.
