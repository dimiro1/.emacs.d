;;; cafn-packages.el --- Package management configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Package management with package.el and use-package.
;;
;;; Code:

;;; Initialize package.el
(require 'package)

;;; Configure package archives
;; Add MELPA and other repositories for more package options
(setopt package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Initialize package system
(package-initialize)

;; Refresh package contents if needed
(unless package-archive-contents
  (package-refresh-contents))

;;; Install and configure use-package
;; use-package provides a clean, declarative way to configure packages
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;; Configure use-package for better debugging and performance monitoring
(use-package use-package
  :custom
  ;; Always ensure packages are installed
  (use-package-always-ensure t)
  ;; Compute statistics for debugging startup time
  (use-package-compute-statistics t)
  ;; Expand use-package forms for debugging
  (use-package-expand-minimally nil)
  ;; Enable verbose loading for debugging (set to nil for normal use)
  (use-package-verbose nil))

;;; Helper functions for package management

(defun cafn-package-refresh ()
  "Refresh package contents from all configured archives."
  (interactive)
  (package-refresh-contents)
  (message "Package contents refreshed"))

(defun cafn-package-update-all ()
  "Update all installed packages to their latest versions."
  (interactive)
  (package-refresh-contents)
  (save-window-excursion
    (package-list-packages t)
    (package-menu-mark-upgrades)
    (package-menu-execute t)))

(defun cafn-package-autoremove ()
  "Remove packages that are no longer needed."
  (interactive)
  (package-autoremove))

;;; Provide keybindings for package management operations
(use-package emacs
  :bind
  (("C-c P r" . cafn-package-refresh)
   ("C-c P u" . cafn-package-update-all)
   ("C-c P a" . cafn-package-autoremove)
   ("C-c P l" . package-list-packages)))

(provide 'cafn-packages)
;;; cafn-packages.el ends here