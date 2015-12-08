;; My Emacs Config
;; Copyright (C) 2015 Claudemiro Alves Feitosa Neto <dimiro1@gmail.com>
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

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

;; Install packages
(unless (package-installed-p 'cider)
  (package-install 'cider))

(unless (package-installed-p 'paredit)
  (package-install 'paredit))

(unless (package-installed-p 'ac-nrepl)
  (package-install 'ac-nrepl))

(unless (package-installed-p 'linum-relative)
  (package-install 'linum-relative))

(unless (package-installed-p 'neotree)
  (package-install 'neotree))

(unless (package-installed-p 'yasnippet)
  (package-install 'yasnippet))

(unless (package-installed-p 'hl-todo)
  (package-install 'hl-todo))

(unless (package-installed-p 'auto-complete)
  (package-install 'auto-complete))

;; Themes
(unless (package-installed-p 'majapahit-theme)
  (package-install 'majapahit-theme))

(unless (package-installed-p 'gotham-theme)
  (package-install 'gotham-theme))

(unless (package-installed-p 'monokai-theme)
  (package-install 'monokai-theme))

(unless (package-installed-p 'atom-dark-theme)
  (package-install 'atom-dark-theme))

(unless (package-installed-p 'atom-one-dark-theme)
  (package-install 'atom-one-dark-theme))


;; minor modes
(require 'linum-relative)
(linum-relative-global-mode)
(setq linum-relative-current-symbol "")
(setq linum-relative-format "%3s ")

(ido-mode)
(global-linum-mode t)
(global-hl-todo-mode t)
(column-number-mode)

;; Auto complete
(global-auto-complete-mode t)

(require 'auto-complete)
(require 'auto-complete-config)

;; NEO Tree
(global-set-key [f8] 'neotree-toggle)

;; Yasnippet
(require 'yasnippet)

;; cider-clojure
(require 'cider)

;; Custom Keybindings
(global-set-key (kbd "C-c l") 'goto-line)
(global-set-key (kbd "C-c d") 'dash-at-point)
(global-set-key (kbd "C-c E") 'Dash-at-point-with-docset)
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c p") 'fiplr-find-file)
(global-set-key (kbd "C-c s") 'magit-status)

;; Custom Editor
(set-frame-font "PragmataPro-13")
(scroll-bar-mode -1)
(menu-bar-mode -1)
(load-theme 'atom-dark t)
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Backups
(setq backup-directory-alist `(("." . "~/.emacs-saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
