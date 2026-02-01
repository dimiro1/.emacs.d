;;; d1-completion.el --- Completion system configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Provides a modern, fast completion experience throughout Emacs.
;;

;;; Code:

;;; Built-in Completion Configuration
(use-package emacs
  :custom
  ;; Enable case-insensitive completion
  (completion-ignore-case t)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Disable Ispell completion function.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p))

;;; Vertico: Vertical minibuffer completion UI
(use-package vertico
  :ensure t
  :config
  (vertico-mode 1))


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
