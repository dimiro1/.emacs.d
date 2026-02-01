;;; d1-reading.el --- EPUB and document reading  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Configuration for reading EPUB files and other documents.
;;
;;; Code:

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode))

(provide 'd1-reading)
;;; d1-reading.el ends here
