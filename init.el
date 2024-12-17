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

;; UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Initial Window size
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; No splash screen
(setq inhibit-startup-screen t)

;; Font face
(set-frame-font "Comic Code-13")

;; Update Path
(add-to-list 'exec-path (concat (getenv "HOME") "/go/bin"))
(add-to-list 'exec-path (concat (getenv "HOME") ".cargo/bin"))

;; Load custom first
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Turn off mouse interface early in startup to avoid momentary display
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode)

;; Packages

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

(use-package company :straight t :ensure t)
(use-package magit :straight t :ensure t)
(use-package fiplr :straight t :ensure t)
(use-package gotham-theme :straight t :ensure t)
(use-package spacemacs-theme :straight t :ensure t)
(use-package gruvbox-theme :straight t :ensure t)
(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :after (request org markdown-mode shell-maker))

;; Copilot

; Adds a hook to automatically insert a commit message using GitHub Copilot Chat when preparing a Git commit.
(add-hook 'git-commit-setup-hook 'copilot-chat-insert-commit-message)

;; Theme
(load-theme 'spacemacs-dark)

;; Fiplr
(setq fiplr-root-markers (append fiplr-root-markers '("go.mod")))

(let ((dirs (cadr (assoc 'directories fiplr-ignored-globs))))
  (setcdr (assoc 'directories fiplr-ignored-globs) (list (append dirs '(".idea")))))

;; Display line numbers relative
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; treesit

(require 'treesit)

(setq treesit-language-source-alist
 '((bash "https://github.com/tree-sitter/tree-sitter-bash")
   (css "https://github.com/tree-sitter/tree-sitter-css")
   (go "https://github.com/tree-sitter/tree-sitter-go")
   (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
   (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
   (json "https://github.com/tree-sitter/tree-sitter-json")
   (make "https://github.com/alemuller/tree-sitter-make")
   (markdown "https://github.com/ikatyang/tree-sitter-markdown")
   (python "https://github.com/tree-sitter/tree-sitter-python")
   (toml "https://github.com/tree-sitter/tree-sitter-toml")
   (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
   (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; Golang
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-hook 'go-ts-mode-hook 'eglot-ensure)
(add-hook 'go-ts-mode-hook 'company-mode)

;; elisp
(add-hook 'emacs-lisp-mode-hook 'company-mode)

;; copilot
(setq copilot-chat-frontend 'markdown)

;; IDO
(setq-default ido-use-filename-at-point 'guess)
(setq-default ido-create-new-buffer 'always)
(setq-default ido-enable-flex-matching t)
(setq-default ido-everywhere t)
(ido-mode t)


;; Backups
(setq backup-directory-alist `(("." . "~/.emacs-saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versionsp 2
  version-control t)

;; Custom keybindgs

(global-set-key (kbd "C-c p") 'fiplr-find-file)
(global-set-key (kbd "C-M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-<down>") 'shrink-window)
(global-set-key (kbd "C-M-<up>") 'enlarge-window)
