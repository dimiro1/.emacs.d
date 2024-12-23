;;; Tree-sitter Configuration
;; Define language parsers and their source repositories for Tree-sitter.
(setopt treesit-language-source-alist
	'((make       "https://github.com/alemuller/tree-sitter-make")
          (gomod      "https://github.com/camdencheek/tree-sitter-go-mod")
          (markdown   "https://github.com/ikatyang/tree-sitter-markdown")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml       "https://github.com/ikatyang/tree-sitter-yaml")))

(provide 'my-treesit)
