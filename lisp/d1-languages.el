;;; d1-languages.el --- Programming language support with LSP  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Programming language support with LSP.
;;
;;; Code:

;;; Tree-sitter Configuration
;; Modern syntax highlighting and code analysis (requires Emacs 29+)
(use-package treesit
  :ensure nil
  :custom
  ;; Language parsers for Tree-sitter (install with: M-x treesit-install-language-grammar)
  (treesit-language-source-alist
   '((bash       "https://github.com/tree-sitter/tree-sitter-bash")
     (css        "https://github.com/tree-sitter/tree-sitter-css")
     (go         "https://github.com/tree-sitter/tree-sitter-go")
     (gomod      "https://github.com/camdencheek/tree-sitter-go-mod")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json       "https://github.com/tree-sitter/tree-sitter-json")
     (make       "https://github.com/alemuller/tree-sitter-make")
     (markdown   "https://github.com/tree-sitter-grammars/tree-sitter-markdown")
     (python     "https://github.com/tree-sitter/tree-sitter-python")
     (rust       "https://github.com/tree-sitter/tree-sitter-rust")
     (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml       "https://github.com/ikatyang/tree-sitter-yaml")))
  :config
  ;; Auto-install missing grammars
  (dolist (grammar treesit-language-source-alist)
    (unless (treesit-ready-p (car grammar))
      (treesit-install-language-grammar (car grammar))))

  ;; Configure auto-mode-alist for Tree-sitter modes
  (setopt auto-mode-alist
          (append '(("\\.go\\'"		.	go-ts-mode)
                    ("go\\.mod\\'"	.	go-ts-mode)
                    ("\\.rs\\'"		.	rust-ts-mode)
                    ("\\.ts\\'"		.	typescript-ts-mode)
                    ("\\.tsx\\'"	.	tsx-ts-mode)
                    ("\\.py\\'"		.	python-ts-mode)
                    ("\\.js\\'"		.	js-ts-mode)
                    ("\\.json\\'"	.	json-ts-mode)
                    ("\\.yaml\\'"	.	yaml-ts-mode)
                    ("\\.yml\\'"	.	yaml-ts-mode))
                  auto-mode-alist))

  ;; Remap traditional modes to Tree-sitter modes
  (setopt major-mode-remap-alist
          '((python-mode		.	python-ts-mode)
            (javascript-mode	.	js-ts-mode)
            (js-mode			.	js-ts-mode)
            (rust-mode			.	rust-ts-mode)
            (go-mode			.	go-ts-mode))))

;;; Eglot Configuration (LSP Client)
(use-package eglot
  :hook ((go-ts-mode rust-ts-mode typescript-ts-mode) . eglot-ensure)
  :hook (before-save . (lambda ()
                         (when (eglot-managed-p)
                           (ignore-errors (eglot-code-action-organize-imports))
                           (eglot-format))))
  :config
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) . ("rustup" "run" "stable" "rust-analyzer"))))

(use-package flymake
  :hook ((prog-mode . flymake-mode))
  :custom
  (flymake-no-changes-timeout 0.5)
  (flymake-start-on-save-buffer nil))

;;; Go Programming Support
(defun d1/toggle-between-go-test-and-impl-file ()
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
              ("C-c t" . d1/toggle-between-go-test-and-impl-file))
  :custom
  (go-ts-mode-indent-offset 4))

(use-package gotest)

;;; Markdown Support
(use-package markdown-mode
  :mode (("\\.md\\'"		.	markdown-mode)
         ("\\.markdown\\'"	.	markdown-mode)
         ("README\\.md\\'"	.	gfm-mode)) ; GitHub Flavored Markdown for README files
  :custom
  (markdown-fontify-code-blocks-natively t)

  (markdown-code-lang-modes
   '(("elisp"	.	emacs-lisp-mode)
     ("bash"	.	sh-mode)
     ("shell"	.	sh-mode)
     ("go"		.	go-ts-mode)
     ("rust"	.	rust-ts-mode))))

;;; Emacs Lisp Support
(use-package paredit
  :hook ((emacs-lisp-mode lisp-mode scheme-mode clojure-mode)   .       paredit-mode))

(provide 'd1-languages)
;;; d1-languages.el ends here
