;;; Conditional Configuration for `which-key`
;; Enable and configure `which-key`, a helpful package that displays available keybindings
;; in a popup. The configuration differs based on the Emacs version:
;;
;; - For Emacs versions below 30:
;;   `which-key` is installed and configured using `use-package`.
;;
;; - For Emacs 30 and above:
;;   Assumes `which-key` is already available (e.g., pre-installed) and enables it directly.
(if (< emacs-major-version 30)
    (use-package which-key ;; Emacs < 30: Use `use-package` to install and configure `which-key`
      :straight t
      :hook (after-init . which-key-mode))
  (use-package emacs ;; Emacs 30+: Assume `which-key` is available and enable it directly
    :hook (after-init . which-key-mode)))

(provide 'my-which-key)
