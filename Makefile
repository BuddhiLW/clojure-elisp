# ClojureElisp — Makefile
# Version read from VERSION file (shared with build.clj)

JAR_VERSION := $(shell cat VERSION)
JAR_NAME    := clel-$(JAR_VERSION).jar
TARGET_JAR  := target/$(JAR_NAME)
INSTALL_DIR := $(HOME)/.local/lib
INSTALL_JAR := $(INSTALL_DIR)/clel.jar
BIN_DIR     := $(HOME)/.local/bin/blw
BIN_LINK    := $(BIN_DIR)/clel

.PHONY: build install uninstall test clean

build: $(TARGET_JAR)

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

test:
	clojure -M:test

clean:
	rm -rf target
