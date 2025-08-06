;;; d1-xha-fly-keys.el --- Alternative keyboard input method from xahlee  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; http://xahlee.info/emacs/misc/xah-fly-keys.html
;;
;;; Code:


(use-package xah-fly-keys
  :init
  (xah-fly-keys 1)
  (xah-fly-keys-set-layout "colemak-dh"))

(provide 'd1-xah-fly-keys)
;;; d1-xah-fly-keys.el ends here
