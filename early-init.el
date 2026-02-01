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

;; Enable package.el at startup
(setopt package-enable-at-startup t)

;;; UI Performance
;; Disable unnecessary UI elements before they're loaded
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;;; Startup Time Measurement

;; Allow computing statistics for use-package
;; Check stats with M-x use-package-report
(setopt use-package-compute-statistics t)

;;; Native Compilation
;; Silence compiler warnings as they can be pretty disruptive
(setopt native-comp-async-report-warnings-errors nil)

;; Redirect native compilation cache to var directory
(when (native-comp-available-p)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name "var/eln-cache/" user-emacs-directory))))

;;; early-init.el ends here
