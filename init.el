;;; -*- lexical-binding: t; -*-
;;; .emacs.d --- My Emacs Config
;;; Commentary:

;; Copyright (C) 2015,2016,2024 Claudemiro Alves Feitosa Neto <dimiro1@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Add Custom `lisp` Directory to Load Path
(use-package emacs
  :init
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))

;;; List of Custom Packages
;; This variable contains a list of custom configuration modules to be loaded.
;; Each entry corresponds to a feature or functionality managed in a separate file.
(defvar-local my-packages
    '(
      my-straight    ;; Important: This must come first
      my-common      ;; Common Configurations
      my-minibuffer  ;; Minibuffer Config
      my-company     ;; Completion Framework
      my-magit       ;; Git Integration
      my-navigation  ;; File Navigation
      my-colorscheme ;; Theme Configuration
      my-paredit     ;; Paredit Config
      my-treesit     ;; Tree-sitter Configuration
      my-lsp         ;; Language Server
      my-gotest      ;; Setup golang testing support
      my-which-key   ;; Which-Key
      my-llm         ;; LLM Integration
      my-modes       ;; Extra modes
      my-misc        ;; Uncategorized packages
      my-snippets    ;; Snippets
      my-keybindings ;; Custom Keybindings
      )
  "My personal list of internal packages to load.")

;;; Load Custom Packages
;; Iterates through the list of custom packages (`my-packages`) and attempts to load each one.
;; Provides informative messages on success or failure.
(dolist (pkg my-packages)
  (condition-case err
      (when (locate-library (symbol-name pkg))
        (require pkg)
        (message "Package %s loaded successfully" pkg))
    (error (message "Failed to load %s: %s" pkg err))
    (:success (message "Package %s processed successfully without errors" pkg))))

