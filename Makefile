JAR_VERSION := 0.4.0
JAR_NAME    := clel-$(JAR_VERSION).jar
TARGET_JAR  := target/$(JAR_NAME)
INSTALL_DIR := $(HOME)/.local/lib
INSTALL_JAR := $(INSTALL_DIR)/clel.jar

.PHONY: build install clean

build: $(TARGET_JAR)

$(TARGET_JAR): src/**/*.clj deps.edn
	clojure -T:build uber

install: $(TARGET_JAR)
	@mkdir -p $(INSTALL_DIR)
	cp $(TARGET_JAR) $(INSTALL_JAR)
	@echo "Installed $(INSTALL_JAR)"

clean:
	rm -rf target
