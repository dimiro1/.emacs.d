;;; .emacs.d --- My Emacs Config
;;; Commentary:

;; Copyright (C) 2015,2016 Claudemiro Alves Feitosa Neto <dimiro1@gmail.com>
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

;;; Code:

;; The first thing to do
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

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

;; No Bold
(set-face-bold 'bold nil)

;; Common LISP

(eval-when-compile (require 'cl))

;; Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

;; Install packages
(defvar my-packages '(
		   web-mode
		   thrift
		   paredit
		   linum-relative
		   helm
		   hl-todo
		   company
		   company-go
		   majapahit-theme
		   gotham-theme
		   ample-zen-theme
		   monokai-theme
		   railscasts-theme
		   atom-dark-theme
		   magit
		   fiplr
		   atom-one-dark-theme
		   markdown-mode+
		   jbeans-theme
		   darcula-theme
		   go-mode
		   go-guru
		   go-rename
		   flycheck
		   rainbow-delimiters
		   exec-path-from-shell)
  "Packages to install.")


(loop for pkg in my-packages
      unless (package-installed-p pkg) do (package-install pkg))

;; env
(when (memq window-system '(mac ns x))
  (setq-default exec-path-from-shell-check-startup-files nil)
  (setq-default exec-path-from-shell-variables '("PATH" "GOPATH" "GOROOT"))
  (exec-path-from-shell-initialize))

;; minor modes
;; Linum relative
(require 'linum-relative)
(linum-relative-global-mode)
(setq linum-relative-current-symbol "")
(setq linum-relative-format "%3s ")

;; Helm

(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)


;; Ido
(setq-default ido-use-filename-at-point 'guess)
(setq-default ido-create-new-buffer 'always)
(setq-default ido-enable-flex-matching t)
(setq-default ido-everywhere t)
(ido-mode t)

;; linum
(global-linum-mode t)
(global-hl-todo-mode t)
(column-number-mode)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Fiplr
(setq-default fiplr-root-markers '(".git"
								   "project.clj"
								   "build.gradle"))

(setq-default fiplr-ignored-globs '((directories (".svn" ".git" ".hg" "CVS" "build" "target"))
									(files ("*.pyc" "*.pyo" "*.exe" "*.dll" "*.obj""*.o"
											"*.a" "*.lib" "*.so" "*.dylib" "*.ncb" "*.sdf"
											"*.suo" "*.pdb" "*.idb" ".DS_Store" "*.class"
											"*.psd" "*.db" "*.jpg" "*.jpeg" "*.png" "*.gif"
											"*.ttf" "*.tga" "*.dds" "*.ico" "*.eot" "*.pdf"
											"*.swf" "*.jar" "*.zip"))))

(windmove-default-keybindings)
(electric-pair-mode)

;; Hooks
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'before-save-hook #'gofmt-before-save)

;; Auto complete
(add-hook 'after-init-hook 'global-company-mode)

(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)
						  (go-guru-hl-identifier-mode)))


;; Custom Keybindings
(global-set-key (kbd "C-c l") 'goto-line)
(global-set-key (kbd "C-c p") 'fiplr-find-file)
(global-set-key (kbd "C-c s") 'magit-status)
(global-set-key (kbd "C-c c") 'magit-push-current)

(global-set-key (kbd "C-M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-<down>") 'shrink-window)
(global-set-key (kbd "C-M-<up>") 'enlarge-window)

(global-set-key (kbd "M-TAB") #'company-complete)

;; (global-set-key [s-left] 'windmove-left)
;; (global-set-key [s-right] 'windmove-right)
;; (global-set-key [s-up] 'windmove-up)
;; (global-set-key [s-down] 'windmove-down)

(setq-default line-spacing 2)
(setq-default default-tab-width 4)

;; Color Theme

(load-theme 'dark-laptop)

;; Backups
(setq backup-directory-alist `(("." . "~/.emacs-saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; Custom Editor
(set-frame-font "Inconsolata-13")

(provide 'init)
;;; init.el ends here
