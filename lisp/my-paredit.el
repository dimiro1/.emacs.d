;;; Paredit Configuration
;; Paredit is a minor mode for editing Lisp code. It ensures the structural
;; integrity of s-expressions by automatically managing parentheses.
;;
;; Example:
;; - Typing `(` automatically inserts the matching `)`.
;; - Deleting a parenthesis ensures the expression remains valid.
(use-package paredit
  :straight t
  :hook (emacs-lisp-mode . paredit-mode))

(provide 'my-paredit)
