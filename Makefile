# Makefile for running Emacs Lisp tests

EMACS ?= emacs
BATCH = $(EMACS) -batch -Q

.PHONY: test clean

test:
	@$(BATCH) -L lisp \
		-l ert \
		-l d1-git-permalink.el \
		-l d1-git-permalink-test.el \
		-f ert-run-tests-batch-and-exit

clean:
	@rm -f lisp/*.elc
