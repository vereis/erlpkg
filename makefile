SHELL = /bin/sh

# ========================== #
# === Makefile Variables === #
# ========================== #

# Compilation Variables
ERL = $(shell which erl)
ERLC = $(shell which erlc)
ERLFLAGS = -Werror -v -o
ERLFLAGS_NONSTRICT = -Wall -v -o
ERLFLAGS_DEBUG = -Ddebug +debug_info -W0 -o
ERLFLAGS_TEST = -Ddebug -DTEST +debug_info -W0 -o

# Directory Variables
SRCDIR = src
OUTDIR = ebin
DEBUGDIR = edebug
TESTDIR = etesting
UTILDIR = util

# Utility Variables
DIALYZER = $(shell which dialyzer)
ELVIS = $(UTILDIR)/elvis rock --config $(UTILDIR)/elvis.config
STDOUT = &1
DEVNULL = /dev/null

# Colors
RED = \033[0;31m
GREEN = \033[0;32m
BLUE = \033[0;34m
ORANGE = \033[0;33m
PURPLE = \033[0;35m
CYAN = \033[0;36m
NORMAL = \033[0m

# ====================== #
# === Target recipes === #
# ====================== #

release:
	@ echo "$(GREEN)==> Building RELEASE$(NORMAL)"
	@ echo "    Any warnings or errors will stop the build."
	$(call compile, $(ERLFLAGS), $(OUTDIR), $(GREEN), $(DEVNULL))
	$(call package, $(OUTDIR), $(GREEN))

nonstrict:
	@ echo "$(GREEN)==> Building RELEASE NONSTRICT$(NORMAL)"
	@ echo "    Compiling with debug options enabled."
	@ echo "    Debug macro enabled."
	$(call compile, $(ERLFLAGS), $(OUTDIR), $(GREEN), $(STDOUT))
	$(call package, $(OUTDIR), $(GREEN))

debug:
	@ echo "$(BLUE)==> Building DEBUG$(NORMAL)"
	@ echo "    Compiling with debug options enabled."
	@ echo "    Debug macro enabled."
	$(call compile, $(ERLFLAGS), $(DEBUGDIR), $(BLUE), $(STDOUT))
	$(call package, $(DEBUGDIR), $(BLUE))

.PHONY: test
test:
	@ echo "$(CYAN)==> Building TEST$(NORMAL)"
	@ echo "    Compiling with debug options enabled."
	@ echo "    Debug macro enabled."
	@ echo "    Test macro enabled."
	$(call compile, $(ERLFLAGS_TEST), $(TESTDIR), $(CYAN), $(STDOUT))
	$(call dialyze, $(TESTDIR), $(CYAN))
	$(call eunit, $(TESTDIR), $(CYAN))
	$(call lint, $(CYAN))

.PHONY: lint
lint:
	@ echo "$(PURPLE)==> Building LINT$(NORMAL)"
	$(call lint, $(PURPLE))

.PHONY: dialyze
dialyze:
	@ echo "$(ORANGE)==> Building DIALYZE$(NORMAL)"
	@ echo "    Compiling with debug options enabled."
	@ echo "    Debug macro enabled."
	@ echo "    Test macro enabled."
	$(call compile, $(ERLFLAGS_TEST), $(TESTDIR), $(ORANGE), $(STDOUT))
	$(call dialyze, $(TESTDIR), $(ORANGE))

.PHONY: eunit
eunit:
	@ echo "$(ORANGE)==> Building EUNIT$(NORMAL)"
	@ echo "    Compiling with debug options enabled."
	@ echo "    Debug macro enabled."
	@ echo "    Test macro enabled."
	$(call compile, $(ERLFLAGS_TEST), $(TESTDIR), $(ORANGE), $(STDOUT))
	$(call eunit, $(TESTDIR), $(ORANGE))

.PHONY: clean
clean:
	@ echo "$(RED)==> Cleaning builds"
	@ find . -name "*.beam" -delete
	@ echo "==> Removing all BEAM files from workspace"
	@ find . -name "*.dump" -delete
	@ echo "==> Removing all DUMP files from workspace"
	@ rm -rf $(OUTDIR)
	@ echo "==> Removing $(OUTDIR)/"
	@ rm -rf $(DEBUGDIR)
	@ echo "==> Removing $(DEBUGDIR)/"
	@ rm -rf $(TESTDIR)
	@ echo "==> Removing $(TESTDIR)/"
	@ echo "==> Cleaned\n$(NORMAL)"

# ========================= #
# === Recipe Procedures === #
# ========================= #

define compile
	@ $(eval COMPILE_MODE = $(1))
	@ $(eval OUTPUT_DIR = $(2))
	@ $(eval COLOR = $(3))
	@ $(eval PIPE_TO = $(4))

	@ mkdir -p $(OUTPUT_DIR)
	@ rm -f $(OUTPUT_DIR)/*

	@ echo "$(COLOR)==> Compiling Source Files$(RED)"
	@ $(ERLC) $(COMPILE_MODE) $(OUTPUT_DIR) $(SRCDIR)/*.erl >$(PIPE_TO)
	@ echo "$(NORMAL)    Done"

	@ echo "$(COLOR)==> Compiling complete in: './$(OUTPUT_DIR)/'$(NORMAL)"
	@ echo "    Done\n"
endef

define package
	@ $(eval OUTPUT_DIR = $(1))
	@ $(eval COLOR = $(2))

	@ echo "$(COLOR)==> Creating erlpkg package in './$(OUTPUT_DIR)/'$(NORMAL)"
	@ cp $(SRCDIR)/*.erl $(OUTPUT_DIR)/
	@ cd $(OUTPUT_DIR) && ($(ERL) -pa $(OUTPUT_DIR) -noinput -noshell -s erlpkg main erlpkg.erl pkgargs.erl pkgutils.erl) && cd ..
	@ mv $(OUTPUT_DIR)/erlpkg.erlpkg $(OUTPUT_DIR)/erlpkg
	@ chmod +x $(OUTPUT_DIR)/erlpkg
	@ echo "$(NORMAL)    Done"

	@ echo "$(COLOR)==> Cleaning directory './$(OUTPUT_DIR)/'$(NORMAL)"
	@ rm $(OUTPUT_DIR)/*.beam
	@ rm $(OUTPUT_DIR)/*.erl
	@ echo "    Done\n"

	@ echo "$(COLOR)==> Packaging complete in: './$(OUTPUT_DIR)/'$(NORMAL)"
	@ echo "    Done\n"
endef

define dialyze
	@ $(eval TARGET_DIR = $(1))
	@ $(eval COLOR = $(2))
	@ echo "$(COLOR)==> Running Dialyzer$(NORMAL)"
	@ $(DIALYZER) $(TARGET_DIR)/*.beam || true

	@ echo "$(COLOR)==> Dialyzing complete in: './$(TARGET_DIR)/'$(NORMAL)"
	@ echo "    Done\n"
endef

define eunit
	@ $(eval TARGET_DIR = $(1))
	@ $(eval COLOR = $(2))
	@ echo "$(COLOR)==> Running EUnit Tests$(NORMAL)"
	@ echo "  Running tests for module: erlpkg"
	@ cd $(TARGET_DIR) && $(ERL) -pa $(TARGET_DIR) -noinput -noshell -s erlpkg eunit

	@ echo "  Running tests for module: pkgargs"
	@ cd $(TARGET_DIR) && $(ERL) -pa $(TARGET_DIR) -noinput -noshell -s pkgargs eunit

	@ echo "  Running tests for module: pkgutils"
	@ cd $(TARGET_DIR) && $(ERL) -pa $(TARGET_DIR) -noinput -noshell -s pkgutils eunit

	@ echo "$(COLOR)==> EUnit Tests complete in './$(TARGET_DIR)/'$(NORMAL)"
	@ echo "    Done\n"
endef

define lint
	@ $(eval COLOR = $(1))

	@ echo "$(COLOR)==> Linting Project with Elvis$(NORMAL)"
	@ $(ELVIS) || true
	@ echo "    Done\n"
endef