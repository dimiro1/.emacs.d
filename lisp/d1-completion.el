;;; d1-completion.el --- Completion system configuration  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;;
;; Provides a modern, fast completion experience throughout Emacs.
;;

;;; Code:

;;; Built-in Completion Configuration
(use-package emacs
  :init
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

;;; Expand the region syntatically.
(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

(provide 'd1-completion)
;;; d1-completion.el ends here.
