;;; early-init.el --- Early initialization  -*- lexical-binding: t; -*-

;;; Performance Optimizations
;; Temporarily increase GC threshold during startup for faster loading
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Restore default GC thresholds after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024) ; 16MB
                  gc-cons-percentage 0.1)))

;; Enable package.el at startup
(setopt package-enable-at-startup t)

;; Disable file-name-handler-alist during startup for performance
(defvar cafn--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist cafn--file-name-handler-alist)))

;;; UI Performance
;; Disable unnecessary UI elements before they're loaded
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early
(setq tool-bar-mode nil
      menu-bar-mode nil
      scroll-bar-mode nil)

;;; Startup Time Measurement
;; Record the start time for performance monitoring
(defvar cafn--startup-time (current-time)
  "Time when Emacs started loading.")

;; Allow computing statistics for use-package
;; Check stats with M-x use-package-report
(setopt use-package-compute-statistics t)

;;; Native Compilation
;; Configure native compilation if available
(when (featurep 'native-compile)
  ;; Silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil)
  ;; Set the right directory to store native compilation cache
  (add-to-list 'native-comp-eln-load-path
               (expand-file-name "eln-cache/" user-emacs-directory)))

;;; early-init.el ends here

