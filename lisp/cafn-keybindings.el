;;; cafn-keybindings.el --- Custom keybindings and shortcuts  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Custom keybindings and shortcuts.
;;
;;; Code:

;;; Window Management
(use-package emacs
  :bind
  (("C-M-<left>"  . shrink-window-horizontally)  ;; Shrink window horizontally
   ("C-M-<right>" . enlarge-window-horizontally) ;; Enlarge window horizontally
   ("C-M-<down>"  . shrink-window)               ;; Shrink window vertically
   ("C-M-<up>"    . enlarge-window)))            ;; Enlarge window vertically

;;; Move up and down whole lines.
(use-package move-text
  :ensure t
  :bind
  (("s-<down>" . move-text-down)
   ("s-<up>" . move-text-up)))


(defun cafn-toggle-between-go-test-and-impl-file ()
  "Toggle between a Go file and its test file (with _test suffix).
For example, switches between 'hello.go' and 'hello_test.go'."
  (interactive)
  (if (not buffer-file-name)
      (message "Buffer is not visiting a file")
    (let* ((file (buffer-file-name))
           (base (file-name-sans-extension file))
           (ext (file-name-extension file))
           (is-test (string-match-p "_test$" base))
           (target (if is-test
                      (concat (replace-regexp-in-string "_test$" "" base) "." ext)
                    (concat base "_test." ext))))
      (if (file-exists-p target)
          (find-file target)
        (message "File %s does not exist" target)))))

(use-package go-ts-mode
  :bind (:map go-ts-mode-map
          ("C-c t" . cafn-toggle-between-go-test-and-impl-file)))
(provide 'cafn-keybindings)
;;; cafn-keybindings.el ends here
