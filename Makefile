CACHE_DIR := $(wildcard ~/.cache/common-lisp/sbcl-*)
PROJECT_DIR := $(shell pwd)
COMPILE_DIR := $(CACHE_DIR)$(PROJECT_DIR)

clean-compiled:
	rm -rfv $(COMPILE_DIR)

clean: clean-compiled

check: clean-compiled
	sbcl --non-interactive --load runtests.lisp
