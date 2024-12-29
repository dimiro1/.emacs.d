;;; -*- lexical-binding: t; -*-

;;; GoTest Configuration
;; This block sets up the GoTest package for running Go tests.
(use-package gotest
  :straight t
  :config
  (setopt go-test-go-command "encore"))

(provide 'my-gotest)
