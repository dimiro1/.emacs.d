;;; d1-languages.el --- Programming language support with LSP  -*- lexical-binding: t; no-byte-compile: t; -*-

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
          (append '(("\\.go\\'"    . go-ts-mode)
                    ("go\\.mod\\'" . go-ts-mode)
                    ("\\.ts\\'"    . typescript-ts-mode)
                    ("\\.tsx\\'"   . tsx-ts-mode)
                    ("\\.py\\'"    . python-ts-mode)
                    ("\\.js\\'"    . js-ts-mode)
                    ("\\.json\\'"  . json-ts-mode)
                    ("\\.yaml\\'"  . yaml-ts-mode)
                    ("\\.yml\\'"   . yaml-ts-mode))
                  auto-mode-alist))

  ;; Remap traditional modes to Tree-sitter modes
  (setopt major-mode-remap-alist
          '((python-mode     . python-ts-mode)
            (javascript-mode . js-ts-mode)
            (js-mode         . js-ts-mode)
            (go-mode         . go-ts-mode))))

;;; Eglot Configuration (LSP Client)
(use-package eglot
  :hook ((go-ts-mode typescript-ts-mode tsx-ts-mode) . eglot-ensure)
  :hook (before-save . (lambda ()
                         (when (eglot-managed-p)
                           (ignore-errors (eglot-code-action-organize-imports))
                           (eglot-format))))
  :custom
  ;; Show all diagnostics
  (eglot-ignored-server-capabilities '())
  (eglot-extend-to-xref t)
  :config
  (setq-default eglot-workspace-configuration
                '(:gopls (:gofumpt t
                         :usePlaceholders t
                         :staticcheck t
                         :semanticTokens t
                         :vulncheck "Imports"
                         :codelenses (:gc_details t
                                     :generate t
                                     :regenerate_cgo t
                                     :tidy t
                                     :upgrade_dependency t
                                     :vendor t)
                         :analyses (:unreachable t
                                   :unusedvariable t
                                   :unusedparams t
                                   :unusedwrite t
                                   :shadow t)
                         :hints (:assignVariableTypes t
                                :compositeLiteralFields t
                                :compositeLiteralTypes t
                                :constantValues t
                                :functionTypeParameters t
                                :parameterNames t
                                :rangeVariableTypes t)
                         :diagnosticsDelay "250ms"))))

(use-package flymake
  :hook prog-mode
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)
              ("C-c ! p" . flymake-show-project-diagnostics))
  :custom
  (flymake-no-changes-timeout 0.5)
  (flymake-start-on-save-buffer nil)
  ;; Show all diagnostic types including notes and hints
  (flymake-suppress-zero-counters nil)
  (flymake-indicator-type 'margins)
  :config
  ;; Show all severity levels
  (setq flymake-error-bitmap '(flymake-double-exclamation-mark compilation-error))
  (setq flymake-warning-bitmap '(exclamation-mark compilation-warning))
  (setq flymake-note-bitmap '(question-mark compilation-info)))

;;; Go Programming Support
(defun d1-toggle-between-go-test-and-impl-file ()
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
              ("C-c t" . d1-toggle-between-go-test-and-impl-file))
  :custom
  (go-ts-mode-indent-offset 4))

;;; Markdown Support
(use-package markdown-mode
  :mode (("\\.md\\'"        . markdown-mode)
         ("\\.markdown\\'"  . markdown-mode)
         ("README\\.md\\'"  . gfm-mode)) ; GitHub Flavored Markdown for README files
  :custom
  (markdown-fontify-code-blocks-natively t)

  (markdown-code-lang-modes
   '(("elisp" . emacs-lisp-mode)
     ("bash"  . sh-mode)
     ("shell" . sh-mode)
     ("go"    . go-ts-mode))))

;;; Emacs Lisp Support
(use-package paredit
  :hook (emacs-lisp-mode lisp-mode scheme-mode clojure-mode))

(provide 'd1-languages)
;;; d1-languages.el ends here
