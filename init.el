;;; .emacs.d --- My Emacs Config
;;; Commentary:

;; Copyright (C) 2015,2016,2024 Claudemiro Alves Feitosa Neto <dimiro1@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; ====================================
;; Basic Configuration
;; ====================================

;; Character Encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; UI Preferences
;; Highlight current line
(global-hl-line-mode 1)

;; Auto-wrap lines longer than 80 characters
(setq-default fill-column 80)
(auto-fill-mode 1)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

;; Delete selected text when typing
(delete-selection-mode 1)

;; Confirm before quitting Emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Smooth scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)

;; Make Emacs start in fullscreen mode
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable the startup screen
(setq inhibit-startup-screen t)

;; Remove menu bar, tool bar and scroll bar for a cleaner interface
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Enable highlighting of matching parentheses
(show-paren-mode)

;; Font and Spacing
(set-frame-font "Comic Code-13")
;;(setq-default line-spacing 2)
(setq-default default-tab-width 2)

;; CamelCase Support
(subword-mode t)

;; Display Settings
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
(column-number-mode t)

;; Custom File Loading
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; ====================================
;; Package Management
;; ====================================

;; Straight.el Bootstrap
(setq package-enable-at-startup nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; ====================================
;; Path Configuration
;; ====================================

(add-to-list 'exec-path (concat (getenv "HOME") "/go/bin"))
(add-to-list 'exec-path (concat (getenv "HOME") "/.cargo/bin"))
(add-to-list 'exec-path (concat (getenv "HOME") "/.nvm/versions/node/v22.5.1/bin"))
(setenv "PATH" (concat (getenv "PATH") ":" (getenv "HOME") "/.nvm/versions/node/v22.5.1/bin"))

;; ====================================
;; Core Packages
;; ====================================

;; Completion Framework
(use-package company :straight t :ensure t)

;; Git Integration
(use-package magit :straight t :ensure t)

;; File Navigation
(use-package fiplr :straight t :ensure t)
(setq fiplr-root-markers (append fiplr-root-markers '("go.mod")))
(let ((dirs (cadr (assoc 'directories fiplr-ignored-globs))))
  (setcdr (assoc 'directories fiplr-ignored-globs) (list (append dirs '(".idea")))))

;; ====================================
;; Theme Configuration
;; ====================================

(use-package gotham-theme :straight t :ensure t)
(use-package spacemacs-theme :straight t :ensure t)
(use-package gruvbox-theme :straight t :ensure t)
(load-theme 'spacemacs-dark)

;; ====================================
;; Programming Support
;; ====================================

;; Paredit for Lisp-like languages
(use-package paredit :straight t :ensure t)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)

;; Tree-sitter Configuration
(require 'treesit)
(setq treesit-language-source-alist
      '((bash       "https://github.com/tree-sitter/tree-sitter-bash")
        (css        "https://github.com/tree-sitter/tree-sitter-css")
        (json       "https://github.com/tree-sitter/tree-sitter-json")
        (make       "https://github.com/alemuller/tree-sitter-make")
        (go         "https://github.com/tree-sitter/tree-sitter-go")
        (gomod      "https://github.com/camdencheek/tree-sitter-go-mod")
        (markdown   "https://github.com/ikatyang/tree-sitter-markdown")
        (python     "https://github.com/tree-sitter/tree-sitter-python")
        (toml       "https://github.com/tree-sitter/tree-sitter-toml")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml       "https://github.com/ikatyang/tree-sitter-yaml")))

;; Eglot (LSP Client)
(defun my-eglot-format-on-save ()
  "Run eglot-format on save, but only if Eglot is managing the current buffer."
  (when (bound-and-true-p eglot--managed-mode)
    (eglot-format)))

(add-hook 'before-save-hook 'my-eglot-format-on-save)

;; Go Programming Support
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-hook 'go-ts-mode-hook 'eglot-ensure)
(add-hook 'go-ts-mode-hook 'company-mode)

;; Elisp Support
(add-hook 'emacs-lisp-mode-hook 'company-mode)

;; ====================================
;; GitHub Copilot Integration
;; ====================================

(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :after (request org markdown-mode shell-maker))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(add-hook 'git-commit-setup-hook 'copilot-chat-insert-commit-message)
(add-hook 'prog-mode-hook 'copilot-mode)
(setq copilot-chat-frontend 'org)

;; ====================================
;; Terminal Configuration
;; ====================================

(add-hook 'term-mode-hook 'term-line-mode)

;; ====================================
;; IDO Configuration
;; ====================================

(setq-default ido-use-filename-at-point 'guess)
(setq-default ido-create-new-buffer 'always)
(setq-default ido-enable-flex-matching t)
(setq-default ido-everywhere t)
(ido-mode t)

;; ====================================
;; Backup Configuration
;; ====================================

(setq backup-directory-alist `(("." . "~/.emacs-saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versionsp 2
      version-control t)

;; ====================================
;; Custom Keybindings
;; ====================================

;; File Navigation
(global-set-key (kbd "C-c p") 'fiplr-find-file)

;; Window Management
(global-set-key (kbd "C-M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-<down>") 'shrink-window)
(global-set-key (kbd "C-M-<up>") 'enlarge-window)

;; Tab Navigation
(global-set-key (kbd "M-[") 'tab-bar-history-back)
(global-set-key (kbd "M-]") 'tab-bar-history-forward)

;; Window Movement
(global-set-key [s-left] 'windmove-left)
(global-set-key [s-right] 'windmove-right)
(global-set-key [s-up] 'windmove-up)
(global-set-key [s-down] 'windmove-down)
