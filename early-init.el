;;; early-init.el --- Early initialization  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Early initialization file loaded before the main init.el.
;; Contains performance optimizations and UI setup that must run early.
;;

;;; Code:

;;; Performance Optimizations
;; Temporarily increase GC threshold during startup for faster loading
(setopt gc-cons-threshold most-positive-fixnum
		gc-cons-percentage 0.6)

;; Restore default GC thresholds after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setopt gc-cons-threshold (* 100 1024 1024) ; 100MB
					gc-cons-percentage 0.1)))

;; Configure package archives before package-enable-at-startup runs
;; This ensures MELPA is available when package-initialize runs
(setopt package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Enable package.el at startup
(setopt package-enable-at-startup t)

;;; use-package Configuration
;; Must be set here (before prepare-user-lisp byte-compiles user-lisp/)
;; so the :ensure behavior is baked into compiled use-package forms.
(setopt use-package-always-ensure t
        use-package-expand-minimally nil
        use-package-verbose nil
        use-package-compute-statistics t)

;;; UI Performance
;; Disable unnecessary UI elements before they're loaded
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;;; Native Compilation
;; Silence compiler warnings as they can be pretty disruptive
(setopt native-comp-async-report-warnings-errors nil)

;; Redirect native compilation cache to var directory
(when (native-comp-available-p)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name "var/eln-cache/" user-emacs-directory))))

;;; early-init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-vc-selected-packages
   '((copilot :url "https://github.com/copilot-emacs/copilot.el" :branch
	      "main"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
