# Makefile for running Emacs Lisp tests

EMACS ?= emacs
BATCH = $(EMACS) -batch -Q

.PHONY: test clean

test:
	@$(BATCH) -L user-lisp -L user-lisp/test \
		-l ert \
		-l d1-git-permalink.el \
		-l d1-git-permalink-test.el \
		-l d1-homebrew.el \
		-l d1-homebrew-test.el \
		-f ert-run-tests-batch-and-exit

clean:
	@rm -f user-lisp/*.elc
