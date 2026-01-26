;;; d1-eshell.el --- Eshell configuration  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;;
;; Eshell shell configuration including prompt, aliases, zoxide integration,
;; and enhanced commands.
;;

;;; Code:

;;;; ============================================================
;;;; Eshell Core
;;;; ============================================================

(use-package eshell
  :custom
  (eshell-prompt-function #'d1-eshell-prompt)
  (eshell-prompt-regexp "^❯ ")

  :config
  ;; Update zoxide when changing directories
  (add-hook 'eshell-directory-change-hook #'d1-eshell-zoxide-add)
  ;; Enhanced cat with image rendering
  (advice-add 'eshell/cat :around #'d1-eshell-cat-with-images))

;;;; ============================================================
;;;; Aliases
;;;; ============================================================

(defvar d1-eshell-aliases
  '(;; File listing
    ("l" "ls $*")
    ("ll" "ls -la $*")
    ("la" "ls -a $*")
    ;; Directory navigation
    (".." "cd ..")
    ("..." "cd ../..")
    ("...." "cd ../../..")
    ;; Git shortcuts
    ("gst" "git status $*")
    ("ga" "git add $*")
    ("gaa" "git add --all")
    ("gc" "git commit $*")
    ("gcm" "git commit -m $*")
    ("gco" "git checkout $*")
    ("gcb" "git checkout -b $*")
    ("gs" "git switch $*")
    ("gp" "git push $*")
    ("gl" "git pull $*")
    ("gd" "git diff $*")
    ("gds" "git diff --staged $*")
    ("glg" "git log --oneline --graph $*")
    ("gb" "git branch $*")
    ("gf" "git fetch $*"))
  "Eshell aliases.")

(defun d1-eshell-add-aliases ()
  "Add custom aliases to eshell, merging with any existing aliases."
  (dolist (alias d1-eshell-aliases)
    (add-to-list 'eshell-command-aliases-list alias)))

(add-hook 'eshell-mode-hook #'d1-eshell-add-aliases)

;;;; ============================================================
;;;; Prompt
;;;; ============================================================

(defun d1-eshell-prompt ()
  "Minimalistic eshell prompt showing last 3 path components."
  (let* ((path (abbreviate-file-name (eshell/pwd)))
         (parts (split-string path "/" t))
         (last-3 (last parts 3)))
    (concat
     (propertize (string-join last-3 "/") 'face '(:foreground "cyan"))
     "\n"
     (propertize "❯" 'face '(:foreground "magenta"))
     " ")))

;;;; ============================================================
;;;; Zoxide Integration
;;;; ============================================================

(defun d1-eshell-zoxide-add ()
  "Add current directory to zoxide database."
  (let ((dir (eshell/pwd)))
    (call-process "zoxide" nil 0 nil "add" dir)))

(defun eshell/z (&rest args)
  "Jump to a directory using zoxide.
Usage: z [ARGS...]

- No args: cd to home
- Single arg '-': cd to previous directory
- Valid directory path: use regular cd
- Otherwise: query zoxide database"
  (cond
   ((null args)
    (eshell/cd "~"))
   ((and (= (length args) 1)
         (string= (car args) "-"))
    (eshell/cd "-"))
   ((and (= (length args) 1)
         (file-directory-p (expand-file-name (car args))))
    (eshell/cd (car args)))
   (t
    (let* ((query (string-join args " "))
           (result (string-trim
                    (shell-command-to-string
                     (concat "zoxide query --exclude "
                             (shell-quote-argument (eshell/pwd))
                             " -- "
                             (shell-quote-argument query))))))
      (if (string-empty-p result)
          (eshell-error (format "zoxide: no match found for '%s'" query))
        (eshell/cd result))))))

(defun eshell/zi (&rest args)
  "Interactively select a directory using zoxide and Emacs completion.
Usage: zi [ARGS...]"
  (let* ((query (if args (string-join args " ") ""))
         (cmd (if (string-empty-p query)
                  "zoxide query --list"
                (concat "zoxide query --list -- " (shell-quote-argument query))))
         (output (shell-command-to-string cmd))
         (dirs (split-string output "\n" t)))
    (if (null dirs)
        (eshell-error (format "zoxide: no matches found%s"
                              (if (string-empty-p query) "" (format " for '%s'" query))))
      (let ((selected (completing-read "Select directory: " dirs nil t)))
        (when selected
          (eshell/cd selected))))))

;;;; ============================================================
;;;; Enhanced Commands
;;;; ============================================================

(defun d1-eshell-cat-with-images (orig-fun &rest args)
  "Display images inline when using cat in eshell.
Falls back to ORIG-FUN for non-image ARGS."
  (if (seq-every-p (lambda (arg)
                     (and (stringp arg)
                          (file-exists-p arg)
                          (image-type-from-file-name arg)))
                   args)
      (with-temp-buffer
        (insert "\n")
        (dolist (path args)
          (let ((image (create-image (expand-file-name path)
                                     (image-type-from-file-name path)
                                     nil :max-width 800)))
            (insert-image image))
          (insert "\n"))
        (buffer-string))
    (apply orig-fun args)))

(defun eshell/ff (&rest args)
  "Open a file in the current window.
Usage: ff ARGS"
  (if (null args)
      (call-interactively #'find-file)
    (mapc #'find-file (flatten-tree args))
    nil))

(defun eshell/4f (&rest args)
  "Open a file in the other window.
Usage: 4f ARGS"
  (if (null args)
      (call-interactively #'find-file-other-window)
    (mapc #'find-file-other-window (flatten-tree args))
    nil))

(provide 'd1-eshell)
;;; d1-eshell.el ends here
