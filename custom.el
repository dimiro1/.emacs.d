;;; custom.el --- Emacs customization file  -*- lexical-binding: t; -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-sources
   '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc" macos-keychain-internet
	 macos-keychain-generic))
 '(avy-keys '(97 114 115 116 103 109 110 101 105))
 '(column-number-mode t)
 '(comment-multi-line t)
 '(copilot-chat-model "claude-3.5-sonnet")
 '(custom-safe-themes
   '("0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
	 "d445c7b530713eac282ecdeea07a8fa59692c83045bf84dd112dd738c7bcad1d"
	 "871b064b53235facde040f6bdfa28d03d9f4b966d8ce28fb1725313731a2bcc8"
	 "7b8f5bbdc7c316ee62f271acf6bcd0e0b8a272fdffe908f8c920b0ba34871d98"
	 "94bed81ca0be98f58d2cfa2676221c492c8fd5f76b40abd9d73ac00c0d0c9711"
	 "a372fd35724ebb25694e8f977fde62af3e9dd5e31d71005968545042419fa47d"
	 "0b41a4a9f81967daacd737f83d3eac7e3112d642e3f786cf7613de4da97a830a"
	 "bf4d25079f7d052cb656e099d9c2af9fb61ee377e8e72b7f13cecf8dffb74f92" default))
 '(go-ts-mode-indent-offset 4)
 '(markdown-code-lang-modes
   '(("ocaml" . tuareg-mode) ("elisp" . emacs-lisp-mode) ("ditaa" . artist-mode)
	 ("asymptote" . asy-mode) ("dot" . fundamental-mode) ("sqlite" . sql-mode)
	 ("calc" . fundamental-mode) ("C" . c-mode) ("cpp" . c++-mode)
	 ("C++" . c++-mode) ("screen" . shell-script-mode) ("shell" . sh-mode)
	 ("bash" . sh-mode) ("go" . go-ts-mode) ("rust" . rust-ts-mode)))
 '(markdown-fontify-code-blocks-natively t)
 '(modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
 '(org-agenda-files (directory-files-recursively "~/org" "\\.org$"))
 '(package-selected-packages
   '(avy carbonnow catppuccin-theme consult-eglot consult-project-extra doom-themes
		 eat evil-collection evil-surround gotest magit marginalia markdown-mode
		 move-text neotree orderless paredit rg standard-themes vertico vterm
		 xid yasnippet-snippets))
 '(package-vc-selected-packages
   '((carbonnow :url "http://github.com/dimiro1/carbonnow")
	 (xid :url "http://github.com/dimiro1/xid.el")
	 (copilot :url "https://github.com/copilot-emacs/copilot.el")))
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(use-short-answers t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 130 :width normal :foundry "nil" :family "Berkeley Mono")))))
