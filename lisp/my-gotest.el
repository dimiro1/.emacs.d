;;; -*- lexical-binding: t; -*-

;;; GoTest Configuration
;; This block sets up the GoTest package for running Go tests.
(use-package gotest
  :straight t
  :custom
  (go-test-go-command "encore"))

(provide 'my-gotest)
