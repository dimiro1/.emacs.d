(use-package emacs
  :custom
  (completion-styles '(basic flex))
  (completion-auto-select t)	 ;; Show completion on first call
  (completion-auto-help 'visible) ;; Display *Completions* upon first request
  (completions-format 'one-column) ;; Use only one column
  (completions-sort 'historical)   ;; Order based on minibuffer history
  (completions-max-height 20) ;; Limit completions to 15 (completions start at line 5)
  (completion-ignore-case t))

(Provide 'my-completion)
