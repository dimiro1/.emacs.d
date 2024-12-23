
;;; Vertico: Vertical completion system for Emacs.
;;; Provides a clean vertical list of completion candidates in the minibuffer.
;;; Example: Typing `M-x find` shows:
;;;
;;; find-file
;;; find-function
;;; find-library
;;;
;;; Use arrow keys or bindings to navigate.
(use-package vertico
  :straight t
  :ensure t
  :hook (after-init . vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  :hook (after-init . savehist-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :straight t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;;; Marginalia: Adds annotations to completion candidates.
;;; Works with Vertico to display metadata for each candidate.
;;; Example: `C-x C-f` (find-file) shows:
;;;
;;; init.el       ~/.emacs.d/    2 KB  2024-12-20  rw-r--r--
;;; config.org    ~/.emacs.d/    5 KB  2024-12-19  rw-r--r--
;;; README.md     ~/projects/    3 KB  2024-12-18  rw-r--r--
;;;
;;; Enhances context and usability.
(use-package marginalia
  :straight t
  :ensure t
  :hook (after-init . marginalia-mode))

(provide 'my-minibuffer)
