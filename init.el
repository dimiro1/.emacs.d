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

;; The first thing to do
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Update env
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Load custom first
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Turn off mouse interface early in startup to avoid momentary display
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode)

;; No splash screen
(setq inhibit-startup-screen t)

;; Common LISP
(require 'cl)

;; Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

;; Install packages
(defvar packages '(
		   web-mode
		   thrift
		   groovy-mode
		   cider
		   paredit
		   ac-nrepl
		   linum-relative
		   neotree
		   yasnippet
		   hl-todo
		   auto-complete
		   majapahit-theme
		   gotham-theme
		   monokai-theme
		   atom-dark-theme
		   magit
		   fiplr
		   atom-one-dark-theme)
  "Packages to install")


(loop for pkg in packages
      unless (package-installed-p pkg) do (package-install pkg))

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
(global-set-key (kbd "C-c p") 'fiplr-find-file)
(global-set-key (kbd "C-c s") 'magit-status)
(global-set-key (kbd "C-c c") 'magit-push-current)

(global-set-key (kbd "C-M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-<down>") 'shrink-window)
(global-set-key (kbd "C-M-<up>") 'enlarge-window)

;; Custom Editor
(if (equal system-type 'darwin)
    (set-frame-font "PragmataPro-12") ;; Using in my Macbook
  (set-frame-font "PragmataPro-11"))  ;; Using in my Gnu/Linux machine with Full HD Resolution

;; Color Theme

(defvar enable-auto-color-theme nil
  "Enable or disable the auto color theme feature")

(defvar default-color-theme 'atom-one-dark
  "The color theme to load case enable-auto-color-theme is false")

(defvar current-theme nil
  "Holds the current theme value.
I have to do this because I think I can not get the current enabled theme from an emacs primitive")

(defvar beginning-of-daylight 5
  "When does daytime start?")

(defvar end-of-daylight 18
  "When does daytime ends?")

(defun is-daylight ()
  "Check if the current time is daylight"
  (let ((hour-of-day (nth 2 (decode-time))))
    (if (and (> hour-of-day beginning-of-daylight) (< hour-of-day end-of-daylight))
	t
      nil)))

(defun change-theme (theme)
  "Change the current theme only if it is not already activated"
  (unless (eq current-theme theme)
    (message "Changing theme to %s" theme)
    (setq current-theme theme)
    (load-theme current-theme)))
  
(defun auto-change-theme ()
  "Change Color Theme of emacs based on the hour of the day"
  (if (eq enable-auto-color-theme t)
      (if (is-daylight)
	  (change-theme 'atom-dark)
	(change-theme 'atom-one-dark))
    (change-theme default-color-theme)))

(auto-change-theme) ;; Running on startup

;; Run periodically the change-color-theme function
;; Every 60 seconds
(run-with-timer 0 60 'auto-change-theme)

;; Backups
(setq backup-directory-alist `(("." . "~/.emacs-saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)
