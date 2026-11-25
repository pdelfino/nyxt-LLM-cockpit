# SPDX-FileCopyrightText: Pedro Delfino
# SPDX-License-Identifier: BSD-3-Clause

SHELL = /bin/sh
SBCL ?= sbcl
OCICL ?= ocicl

# The Nyxt release this extension is written against, and where its source
# is kept for the tests that need Nyxt. The folder is not part of the repository.
NYXT_VERSION ?= 5.1.0
NYXT_REPO ?= https://github.com/atlas-engineer/nyxt
NYXT_SRC ?= $(CURDIR)/.nyxt/src
OCICL_RUNTIME ?= $(HOME)/.local/share/ocicl/ocicl-runtime.lisp

.PHONY: help
help:
	@echo "make test        agent and view tests in plain SBCL: no Nyxt, no network, no cost"
	@echo "make nyxt-deps   fetch Nyxt $(NYXT_VERSION) and its libraries into .nyxt/ (once, needs ocicl)"
	@echo "make nyxt-test   every test inside Nyxt's core, with no renderer and no window"
	@echo "make nyxt-install-test   the install lines of README.org, in a Nyxt set up like the packaged app"
	@echo "make check       test, nyxt-test, then nyxt-install-test: the gate before a commit"
	@echo "make real        three Haiku turns against the real Claude Code: needs the network, costs under a cent"
	@echo "make clean       remove compiled files"

.PHONY: test
test:
	$(SBCL) --script tests/run-plain.lisp

$(NYXT_SRC)/nyxt.asd:
	mkdir -p $(dir $(NYXT_SRC))
	git clone --quiet --depth 1 --branch $(NYXT_VERSION) $(NYXT_REPO) $(NYXT_SRC)

$(NYXT_SRC)/ocicl: $(NYXT_SRC)/nyxt.asd
	cd $(NYXT_SRC) && CL_SOURCE_REGISTRY= $(OCICL) -c never install

.PHONY: nyxt-deps
nyxt-deps: $(NYXT_SRC)/ocicl
	@test -f $(OCICL_RUNTIME) || (echo "Run 'ocicl setup' once: $(OCICL_RUNTIME) is missing." && exit 1)

# The same steps Nyxt's own makefile uses to load itself, without a renderer.
NYXT_LISP = cd $(NYXT_SRC) && NASDF_USE_LOGICAL_PATHS=true $(SBCL) --dynamic-space-size 8192 \
	--no-userinit --non-interactive \
	--eval '(require "asdf")' \
	--eval '(load "$(OCICL_RUNTIME)")' \
	--eval '(setf ocicl-runtime:*download* nil)' \
	--eval '(push (lambda (name) (let ((name (asdf:coerce-name name))) (when (or (string= name "nyxt") (uiop:string-prefix-p "nyxt/" name)) (pathname "$(NYXT_SRC)/nyxt.asd")))) asdf:*system-definition-search-functions*)' \
	--eval '(asdf:load-asd "$(NYXT_SRC)/libraries/nasdf/nasdf.asd")' \
	--eval '(asdf:load-asd "$(NYXT_SRC)/nyxt.asd")' \
	--eval '(let ((*standard-output* (make-broadcast-stream)) (*error-output* (make-broadcast-stream))) (handler-bind ((warning (function muffle-warning))) (asdf:load-system :nyxt)))'

.PHONY: nyxt-test
nyxt-test: nyxt-deps
	$(NYXT_LISP) --load '$(CURDIR)/tests/run-nyxt.lisp'

# The install lines of README.org, in a Nyxt whose ASDF is set up like the
# packaged app's. The extensions directory is a folder made for the run, with
# links to the system file and the source, so nothing of yours is touched.
.PHONY: nyxt-install-test
nyxt-install-test: nyxt-deps
	@data=$$(mktemp -d) && \
	mkdir -p "$$data/nyxt/extensions/nyxt-llm-cockpit" && \
	ln -s "$(CURDIR)/nyxt-llm-cockpit.asd" "$(CURDIR)/source" "$$data/nyxt/extensions/nyxt-llm-cockpit/" && \
	( export XDG_DATA_HOME="$$data" && $(NYXT_LISP) --load '$(CURDIR)/tests/run-install.lisp' ); \
	status=$$?; /bin/rm -rf "$$data"; exit $$status

.PHONY: check
check: test nyxt-test nyxt-install-test

.PHONY: real
real:
	$(SBCL) --script tests/real.lisp

.PHONY: clean
clean:
	find . -name '*.fasl' -not -path './.nyxt/*' -delete
