;;; d1-evil.el --- Evil mode configuration for Vim emulation  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;;
;; Evil mode configuration for Vim emulation.
;;
;;; Code:

(use-package evil
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t)
  :config
  ;; Define prefix maps
  (define-prefix-command 'd1-leader-map)
  (define-prefix-command 'd1-workspace-map)
  (define-prefix-command 'd1-project-map)
  (define-prefix-command 'd1-help-map)
  (define-prefix-command 'd1-quit-map)

  ;; Set up leader key
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-define-key 'normal 'global (kbd "<leader>") 'd1-leader-map)
  (define-key d1-leader-map "w" d1-workspace-map)
  (define-key d1-leader-map "p" d1-project-map)
  (define-key d1-leader-map "h" d1-help-map)
  (define-key d1-leader-map "q" d1-quit-map)

  ;; Quit commands
  (define-key d1-quit-map "q" 'save-buffers-kill-terminal)

  ;; Help commands
  (define-key d1-help-map "f" 'describe-function)
  (define-key d1-help-map "v" 'describe-variable)
  (define-key d1-help-map "k" 'describe-key)
  (define-key d1-help-map "m" 'describe-mode)
  (define-key d1-help-map "b" 'describe-bindings)
  (define-key d1-help-map "p" 'describe-package)
  (define-key d1-help-map "t" 'consult-theme)
  (define-key d1-help-map "i" 'info)
  (define-key d1-help-map "l" 'view-lossage)  ;; Show recent keystrokes
  (define-key d1-help-map "r" 'reload-init-file)  ;; Custom function defined below

  ;; File and search operations
  (define-key d1-leader-map "f" 'consult-find)
  (define-key d1-leader-map "s" 'consult-outline)
  (define-key d1-leader-map "S" 'consult-eglot-symbols)
  (define-key d1-leader-map "t" 'consult-theme)
  (define-key d1-leader-map "d" 'd1-consult-flymake-project)
  (define-key d1-leader-map "l" 'consult-line)
  (define-key d1-leader-map "b" 'consult-buffer)
  (define-key d1-leader-map "/" 'consult-ripgrep)
  (define-key d1-leader-map "j" 'evil-collection-consult-jump-list)

  ;; Enhanced project commands
  (define-key d1-project-map "f" 'project-find-file)
  (define-key d1-project-map "b" 'project-switch-to-buffer)
  (define-key d1-project-map "p" 'project-switch-project)
  (define-key d1-project-map "d" 'project-find-dir)
  (define-key d1-project-map "g" 'consult-ripgrep)  ;; Better search with consult
  (define-key d1-project-map "e" 'project-eshell)
  (define-key d1-project-map "k" 'project-kill-buffers)
  (define-key d1-project-map "!" 'project-shell-command)
  (define-key d1-project-map "&" 'project-async-shell-command)
  (define-key d1-project-map "c" 'project-compile)
  (define-key d1-project-map "D" 'project-dired)
  (define-key d1-project-map "v" 'project-vc-dir)

  ;; Workspace commands
  (define-key d1-workspace-map "a" 'eglot-add-workspace-folder)
  (define-key d1-workspace-map "r" 'eglot-remove-workspace-folder)
  (define-key d1-workspace-map "v" 'split-window-right)
  (define-key d1-workspace-map "s" 'split-window-below)
  (define-key d1-workspace-map "d" 'delete-window)
  (define-key d1-workspace-map "D" 'delete-other-windows)
  (define-key d1-workspace-map "n" 'windmove-left)
  (define-key d1-workspace-map "i" 'windmove-right)
  (define-key d1-workspace-map "e" 'windmove-down)
  (define-key d1-workspace-map "u" 'windmove-up)

  ;; Code actions and refactoring
  (define-key d1-leader-map "a" 'eglot-code-actions)
  (define-key d1-leader-map "r" 'eglot-rename)
  (define-key d1-leader-map "F" 'eglot-format-buffer)

  ;; Symbols
  (define-key d1-leader-map "i" 'consult-imenu)  ;; Changed from S to i for consistency

  ;; LSP navigation
  (evil-define-key 'normal 'global (kbd "gh") 'eglot-help-at-point)
  (evil-define-key 'normal 'global (kbd "gd") 'eglot-find-declaration)
  (evil-define-key 'normal 'global (kbd "gD") 'eglot-find-typeDefinition)
  (evil-define-key 'normal 'global (kbd "gr") 'xref-find-references)
  (evil-define-key 'normal 'global (kbd "gI") 'eglot-find-implementation)
  (evil-define-key 'normal 'global (kbd "gi") 'eglot-find-implementation)

  ;; avy (go to any word)
  (evil-define-key 'normal 'global (kbd "gw") 'avy-goto-char)

  ;; Diagnostics
  (evil-define-key 'normal 'global (kbd "]d") 'flymake-goto-next-error)
  (evil-define-key 'normal 'global (kbd "[d") 'flymake-goto-prev-error)

  ;; Treesitter Structural Navigation (matching Zed keybindings)
  (evil-define-key 'normal 'global (kbd "]m") 'd1-treesit-goto-next-function)
  (evil-define-key 'normal 'global (kbd "[m") 'd1-treesit-goto-prev-function)
  (evil-define-key 'normal 'global (kbd "]M") 'd1-treesit-goto-next-function-end)
  (evil-define-key 'normal 'global (kbd "[M") 'd1-treesit-goto-prev-function-end)
  (evil-define-key 'normal 'global (kbd "]]") 'd1-treesit-goto-next-class)
  (evil-define-key 'normal 'global (kbd "[[") 'd1-treesit-goto-prev-class)
  (evil-define-key 'normal 'global (kbd "]/") 'd1-treesit-goto-next-comment)
  (evil-define-key 'normal 'global (kbd "[/") 'd1-treesit-goto-prev-comment)
  (evil-define-key 'normal 'global (kbd "]*") 'd1-treesit-goto-next-comment)
  (evil-define-key 'normal 'global (kbd "[*") 'd1-treesit-goto-prev-comment)

  ;; Smart Selection using expand-region (matching Zed keybindings)
  (evil-define-key 'normal 'global (kbd "[x") 'd1-expand-selection)
  (evil-define-key 'normal 'global (kbd "]x") 'd1-contract-selection)
  (evil-define-key 'visual 'global (kbd "[x") 'd1-expand-selection)
  (evil-define-key 'visual 'global (kbd "]x") 'd1-contract-selection)

  ;; Additional LSP keybindings to match Zed
  (evil-define-key 'normal 'global (kbd "gy") 'eglot-find-typeDefinition)
  (evil-define-key 'normal 'global (kbd "g.") 'eglot-code-actions)

  ;; Navigation
  (define-key evil-normal-state-map (kbd "C-w n") 'windmove-left)
  (define-key evil-normal-state-map (kbd "C-w i") 'windmove-right)
  (define-key evil-normal-state-map (kbd "C-w e") 'windmove-down)
  (define-key evil-normal-state-map (kbd "C-w u") 'windmove-up)

  ;; Enable auto-indentation
  (electric-indent-mode 1)
  ;; Enable evil
  (evil-mode 1))

;;; Treesitter Navigation Functions
;; Helper function and navigation commands

(defun d1-treesit-navigate (node-types direction position-fn not-found-msg)
  "Navigate to next/previous treesitter node of given types.
NODE-TYPES: list of treesitter node type strings to search for, or list of lists for fallback
DIRECTION: 'next or 'prev
POSITION-FN: function to get position from node (treesit-node-start or 'treesit-node-end')
NOT-FOUND-MSG: message to display when no node found"
  (when (treesit-parser-list)
    (let* ((current-point (point))
		 (current-node (treesit-node-at current-point))
		 (backward (eq direction 'prev))
		 (comparator (if backward #'< #'>))
		 ;; Handle both simple list and nested list (for fallbacks)
		 (type-lists (if (stringp (car node-types)) (list node-types) node-types))
		 (node (cl-some
				(lambda (types)
				  (treesit-search-forward
				   current-node
				   (lambda (n)
					 (let ((type (treesit-node-type n))
						   (node-pos (funcall position-fn n)))
					   (and (member type types)
							(funcall comparator node-pos current-point))))
				   backward))
				type-lists)))
      (if node
          (goto-char (funcall position-fn node))
        (message not-found-msg)))))

(defun d1-treesit-goto-next-function ()
  "Move to the beginning of the next function using treesitter."
  (interactive)
  (d1-treesit-navigate '("function_declaration" "method_declaration")
					   'next
					   #'treesit-node-start
					   "No next function found"))

(defun d1-treesit-goto-prev-function ()
  "Move to the beginning of the previous function using treesitter."
  (interactive)
  (d1-treesit-navigate '("function_declaration" "method_declaration")
					   'prev
					   #'treesit-node-start
					   "No previous function found"))

(defun d1-treesit-goto-next-function-end ()
  "Move to the end of the next function using treesitter."
  (interactive)
  (d1-treesit-navigate '("function_declaration" "method_declaration")
					   'next
					   #'treesit-node-end
					   "No next function found"))

(defun d1-treesit-goto-prev-function-end ()
  "Move to the end of the previous function using treesitter."
  (interactive)
  (d1-treesit-navigate '("function_declaration" "method_declaration")
					   'prev
					   #'treesit-node-end
					   "No previous function found"))

(defun d1-treesit-goto-next-class ()
  "Move to the beginning of the next class/struct using treesitter.
Fallback to functions if no type declarations found."
  (interactive)
  (d1-treesit-navigate '(("type_declaration") ("function_declaration" "method_declaration"))
					   'next
					   #'treesit-node-start
					   "No next type declaration or function found"))

(defun d1-treesit-goto-prev-class ()
  "Move to the beginning of the previous class/struct using treesitter.
Fallback to functions if no type declarations found."
  (interactive)
  (d1-treesit-navigate '(("type_declaration") ("function_declaration" "method_declaration"))
					   'prev
					   #'treesit-node-start
					   "No previous type declaration or function found"))

(defun d1-treesit-goto-next-comment ()
  "Move to the next comment using treesitter."
  (interactive)
  (d1-treesit-navigate '("comment" "line_comment" "block_comment")
					   'next
					   #'treesit-node-start
					   "No next comment found"))

(defun d1-treesit-goto-prev-comment ()
  "Move to the previous comment using treesitter."
  (interactive)
  (d1-treesit-navigate '("comment" "line_comment" "block_comment")
					   'prev
					   #'treesit-node-start
					   "No previous comment found"))

;;; Smart Selection Functions

(defun d1-expand-selection ()
  "Expand selection to larger syntax region."
  (interactive)
  (require 'expand-region)
  (er/expand-region 1))

(defun d1-contract-selection ()
  "Contract selection to smaller syntax region."
  (interactive)
  (require 'expand-region)
  (er/contract-region 1))


;;; Avy Configuration
;; Avy provides quick navigation to any visible text
(use-package avy
  :custom
  ;; Use Colemak-friendly keys for avy navigation
  ;; These keys are positioned for easy access on Colemak layout
  (avy-keys '(97 114 115 116 103 109 110 101 105)) ; a r s t g m n e i
  :bind
  ;; Additional avy bindings
  ("C-;" . avy-goto-char)
  ("C-'" . avy-goto-char-2)
  ("M-g f" . avy-goto-line)
  ("M-g w" . avy-goto-word-1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;;; Evil Textobj Tree-sitter Configuration
;; Provides treesitter-based text objects for Evil mode
(use-package evil-textobj-tree-sitter
  :after evil
  :config
  ;; Function text objects
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))

  ;; Class/struct text objects
  (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.inner"))

  ;; Comment text objects
  (define-key evil-outer-text-objects-map "C" (evil-textobj-tree-sitter-get-textobj "comment.outer"))
  (define-key evil-inner-text-objects-map "C" (evil-textobj-tree-sitter-get-textobj "comment.outer"))

  ;; Parameter/argument text objects
  (define-key evil-inner-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj "parameter.inner"))
  (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj "parameter.outer"))

  ;; Conditional text objects
  (define-key evil-inner-text-objects-map "i" (evil-textobj-tree-sitter-get-textobj "conditional.inner"))
  (define-key evil-outer-text-objects-map "i" (evil-textobj-tree-sitter-get-textobj "conditional.outer"))

  ;; Loop text objects
  (define-key evil-inner-text-objects-map "l" (evil-textobj-tree-sitter-get-textobj "loop.inner"))
  (define-key evil-outer-text-objects-map "l" (evil-textobj-tree-sitter-get-textobj "loop.outer")))

(use-package consult
  :bind (:map evil-ex-completion-map
			  ("C-n" . consult-history)
			  ("C-p" . consult-history)))

(provide 'd1-evil)
;;; d1-evil.el ends here
