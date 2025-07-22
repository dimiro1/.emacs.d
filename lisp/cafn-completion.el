;;; cafn-completion.el --- Completion system configuration (Corfu + Vertico)  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Complete completion system configuration combining in-buffer completion (Corfu)
;; with minibuffer completion (Vertico + Marginalia + Orderless).
;; This provides a modern, fast completion experience throughout Emacs.
;;

;;; Code:

;;; Built-in Completion Configuration
(use-package emacs
  :custom
  ;; Use both basic and flex completion styles
  ;; Basic: exact matches and prefix completion
  ;; Flex: fuzzy matching (e.g., "fb" matches "foo-bar")
  (completion-styles '(basic flex))

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
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

;;; Corfu - In-buffer completion UI
;; Provides popup completion at point for programming and text editing
(use-package corfu
  :ensure t
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

;;; Vertico: Vertical completion system for minibuffer
;;; Provides a clean vertical list of completion candidates in the minibuffer.
;;; Example: Typing `M-x find` shows:
;;;
;;; find-file
;;; find-function
;;; find-library
;;;
;;; Use arrow keys or bindings to navigate.
(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :hook (after-init . savehist-mode))

;; Optionally use the `orderless' completion style for fuzzy matching
(use-package orderless
  :ensure t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;;; Marginalia: Adds annotations to completion candidates
;;; Works with Vertico to display metadata for each candidate.
;;; Example: `C-x C-f` (find-file) shows:
;;;
;;; init.el       ~/.emacs.d/    2 KB  2024-12-20  rw-r--r--
;;; config.org    ~/.emacs.d/    5 KB  2024-12-19  rw-r--r--
;;; README.md     ~/projects/    3 KB  2024-12-18  rw-r--r--
;;;
;;; Enhances context and usability.
(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(provide 'cafn-completion)
;;; cafn-completion.el ends here