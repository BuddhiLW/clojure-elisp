# ClojureElisp — Makefile
# Version read from VERSION file (shared with build.clj)

JAR_VERSION := $(shell cat VERSION)
JAR_NAME    := clel-$(JAR_VERSION).jar
TARGET_JAR  := target/$(JAR_NAME)
INSTALL_DIR := $(HOME)/.local/lib
INSTALL_JAR := $(INSTALL_DIR)/clel.jar
BIN_DIR     := $(HOME)/.local/bin/blw
BIN_LINK    := $(BIN_DIR)/clel

.PHONY: build install uninstall runtime test test-clj test-elisp parity clean

RUNTIME_EL := resources/clojure-elisp/clel.el

build: $(TARGET_JAR)

# The runtime, the Emacs package clel, is compiled from its cljel source. The
# .el is committed: MELPA builds packages from the repository as it is.
runtime:
	clojure -M -e "(require '[clojure-elisp.core :as clel]) \
	  (clel/compile-runtime \"resources/clojure-elisp/runtime.cljel\" \"$(RUNTIME_EL)\")"

$(TARGET_JAR): src/**/*.clj deps.edn VERSION
	clojure -T:build uber

install: $(TARGET_JAR)
	@mkdir -p $(INSTALL_DIR)
	cp $(TARGET_JAR) $(INSTALL_JAR)
	@echo "Installed $(INSTALL_JAR)"
	@mkdir -p $(BIN_DIR)
	ln -sf $(CURDIR)/bin/clel $(BIN_LINK)
	@echo "Linked $(BIN_LINK) -> $(CURDIR)/bin/clel"
	@echo ""
	@echo "Make sure $(BIN_DIR) is on your PATH:"
	@echo '  export PATH="$$HOME/.local/bin:$$PATH"'

uninstall:
	rm -f $(INSTALL_JAR)
	rm -f $(BIN_LINK)
	@echo "Removed $(INSTALL_JAR) and $(BIN_LINK)"

test: test-clj test-elisp

test-clj:
	clojure -M:test

GUARD_FIXTURE := test/elisp/fixtures/guarded.el

# A real compiled file, written by the compiler. Generated rather than
# committed so it cannot drift from the emitter it stands for.
$(GUARD_FIXTURE): src/clojure_elisp/emitter.clj src/clojure_elisp/version.clj
	@mkdir -p $(dir $@)
	clojure -M -e "(require '[clojure-elisp.core :as clel]) \
	  (spit \"$@\" (clel/compile-file-string \"(ns guarded)\n(defn ok [] :ok)\"))"

PACKAGE_FIXTURE := test/elisp/fixtures/packaged.el

$(PACKAGE_FIXTURE): test/elisp/sources/packaged.cljel src/clojure_elisp/package_header.clj src/clojure_elisp/emitter.clj
	@mkdir -p $(dir $@)
	clojure -M -e "(require '[clojure-elisp.core :as clel]) \
	  (spit \"$@\" (clel/compile-file-string (slurp \"$<\")))"

MELPA_FIXTURE := test/elisp/fixtures/melpa/clelfix.el
MELPA_SOURCES := test/elisp/sources/melpa/clel.edn \
	$(shell find test/elisp/sources/melpa/src -name '*.cljel')

# A three-file package compiled the way a consumer compiles it, from its
# clel.edn. The directory is removed first: compile-project's incremental
# cache would otherwise keep outputs of an older compiler.
$(MELPA_FIXTURE): $(MELPA_SOURCES) $(wildcard src/clojure_elisp/*.clj)
	rm -rf $(dir $@)
	clojure -M -e "(require '[clojure-elisp.core :as clel]) \
	  (clel/compile-project-from-config \"test/elisp/sources/melpa/clel.edn\")"

SEMANTICS_FIXTURE := test/elisp/fixtures/semantics.el

$(SEMANTICS_FIXTURE): test/elisp/sources/semantics.cljel $(wildcard src/clojure_elisp/*.clj)
	@mkdir -p $(dir $@)
	clojure -M -e "(require '[clojure-elisp.core :as clel]) \
	  (spit \"$@\" (clel/compile-file-string (slurp \"$<\")))"

test-elisp: $(GUARD_FIXTURE) $(PACKAGE_FIXTURE) $(MELPA_FIXTURE) $(SEMANTICS_FIXTURE)
	emacs -Q -batch -l ert \
		-l test/elisp/cider-clojure-elisp-test.el \
		-f ert-run-tests-batch-and-exit
	emacs -Q -batch -l ert \
		-l test/elisp/clojure-elisp-semantics-test.el \
		-f ert-run-tests-batch-and-exit
	emacs -Q -batch -l ert \
		-l test/elisp/clojure-elisp-runtime-test.el \
		-f ert-run-tests-batch-and-exit
	emacs -Q -batch -l ert \
		-l test/elisp/clojure-elisp-runtime-guard-test.el \
		-f ert-run-tests-batch-and-exit
	emacs -Q -batch -l ert \
		-l test/elisp/clojure-elisp-package-header-test.el \
		-f ert-run-tests-batch-and-exit
	emacs -Q -batch -l ert \
		-l test/elisp/clojure-elisp-melpa-test.el \
		-f ert-run-tests-batch-and-exit

clean:
	rm -rf target

# Host parity: the compiler must emit the same bytes on the JVM, Babashka
# and ClojureWasm. Every .cljel under examples/ and test/, plus the runtime,
# is compiled on each host (test/parity/compile_corpus.clj) and the three
# output trees are diffed. Needs clojure, bb and cljw on PATH.
PARITY_OUT := target/parity

parity:
	rm -rf $(PARITY_OUT)
	clojure -M test/parity/compile_corpus.clj $(PARITY_OUT)/jvm
	bb test/parity/compile_corpus.clj $(PARITY_OUT)/bb
	cljw -A:cljw -M test/parity/compile_corpus.clj $(PARITY_OUT)/cljw
	diff -r $(PARITY_OUT)/jvm $(PARITY_OUT)/bb
	diff -r $(PARITY_OUT)/jvm $(PARITY_OUT)/cljw
	@echo "parity: the JVM, Babashka and ClojureWasm emit identical Elisp"
