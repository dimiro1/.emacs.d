;;; d1-rss.el --- Atom and RSS feeds  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Atom and RSS feeds.
;;
;;; Code:

(use-package elfeed
  :ensure t
  :custom
  (elfeed-feeds '("https://kxsh.dev/index.xml"
                  "https://bitmaybewise.substack.com/feed"
                  "https://dimiro1.dev/feed/index.xml"
                  "https://xenodium.com/feed"
                  "https://addyosmani.com/rss.xml")))


(provide 'd1-rss)
;;; d1-rss.el ends here
