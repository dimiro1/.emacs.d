;;; d1-typing.el --- Configuration for speed-type package -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures the speed-type package from:
;; https://codeberg.org/rafalraczka/speed-type/

;;; Code:

(use-package speed-type
  :ensure t
  :custom
  ;; Optional customizations
  (speed-type-min-chars 200)
  (speed-type-max-chars 450))

(provide 'd1-typing)
;;; d1-typing.el ends here