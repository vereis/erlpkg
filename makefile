SHELL = /bin/sh

# Compilation Variables
ERL = $(shell which erl)
ERLC = $(shell which erlc)
ERLFLAGS = -Werror -v -o
DEBUGFLAGS = -Ddebug +debug_info -W0 -o
TESTFLAGS = -Ddebug -DTEST +debug_info -W0 -o

# Directory Variables
SRCDIR = src
OUTDIR = ebin
DEBUGDIR = edebug
TESTDIR = etesting
UTILDIR = util

# Utility Variables
DIALYZER = $(shell which dialyzer)
ELVIS = $(UTILDIR)/elvis rock --config $(UTILDIR)/elvis.config

# Colors
RED = \033[0;31m
GREEN = \033[0;32m
BLUE = \033[0;34m
ORANGE = \033[0;33m
PURPLE = \033[0;35m
CYAN = \033[0;36m
NORMAL = \033[0m

release:
	@ echo "$(GREEN)==> Building RELEASE$(NORMAL)"
	@ echo "    Compiling files with debug_info disabled"
	@ echo "    Compiling files with warnings being considered errors"
	@ echo "    Compiling files will fail if any errors occur"
	@ mkdir -p $(OUTDIR)
	@ rm -f $(OUTDIR)/*
	@ echo "$(GREEN)==> Compiling Source Files$(RED)"
	@ $(ERLC) $(ERLFLAGS) $(OUTDIR) $(SRCDIR)/*.erl
	@ echo "$(NORMAL)    Done"
	@ echo "$(GREEN)==> Building Erlpkg Binary$(NORMAL)"
	@ cp $(SRCDIR)/*.erl $(OUTDIR)/
	@ cd $(OUTDIR) && ($(ERL) -pa $(OUTDIR) -noinput -noshell -s erlpkg main erlpkg.erl pkgargs.erl pkgutils.erl >> /dev/null) && cd ..
	@ rm -f $(OUTDIR)/*.beam
	@ rm -f $(OUTDIR)/*.erl
	@ mv $(OUTDIR)/erlpkg.erlpkg $(OUTDIR)/erlpkg
	@ echo "$(NORMAL)    Done"
	@ echo "$(GREEN)==> RELEASE release successfully built in './$(OUTDIR)/'$(NORMAL)"
	@ echo "    Done\n"
debug:
	@ echo "$(BLUE)==> Building DEBUG$(NORMAL)"
	@ echo "    Compiling files with debug_info enabled"
	@ echo "    Compiling files with warnings ignored"
	@ echo "    Compiling files will fail if any errors occur"
	@ mkdir -p $(DEBUGDIR)
	@ rm -f $(DEBUGDIR)/*
	@ echo "$(BLUE)==> Compiling Source Files$(RED)"
	@ $(ERLC) $(DEBUGFLAGS) $(DEBUGDIR) $(SRCDIR)/*.erl
	@ echo "$(NORMAL)    Done"
	@ echo "$(BLUE)==> Building Erlpkg Binary"
	@ cp $(SRCDIR)/*.erl $(DEBUGDIR)/
	@ cd $(DEBUGDIR) && ($(ERL) -pa $(DEBUGDIR) -noinput -noshell -s erlpkg main erlpkg.erl pkgargs.erl pkgutils.erl) && cd ..
	@ rm -f $(DEBUGDIR)/*.beam
	@ rm -f $(DEBUGDIR)/*.erl
	@ mv $(DEBUGDIR)/erlpkg.erlpkg $(DEBUGDIR)/erlpkg
	@ echo "$(NORMAL)    Done"
	@ echo "$(BLUE)==> DEBUG release successfully built in './$(DEBUGDIR)/'$(NORMAL)"
	@ echo "    Done\n"
.PHONY: test
test:
	@ echo "$(PURPLE)==> Building TEST$(NORMAL)"
	@ echo "    Compiling files with debug_info enabled"
	@ echo "    Compiling files with warnings ignored"
	@ echo "    Compiling files will fail if any errors occur"
	@ mkdir -p $(TESTDIR)
	@ rm -rf $(TESTDIR)/*
	@ echo "$(PURPLE)==> Compiling Source Files$(RED)"
	@ $(ERLC) $(TESTFLAGS) $(TESTDIR) $(SRCDIR)/*.erl
	@ echo "$(NORMAL)    Done"
	@ echo "$(PURPLE)==> Hiding Test Files$(NORMAL)"
	@ mkdir -p $(TESTDIR)/tmp
	@ mv $(TESTDIR)/*_tests.beam $(TESTDIR)/tmp || true
	@ echo "    Done"
	@ echo "$(PURPLE)==> Running Dialyzer$(NORMAL)"
	@ $(DIALYZER) $(TESTDIR)/*.beam || true
	@ echo "$(PURPLE)==> Running Elvis$(NORMAL)"
	@ $(ELVIS) || true
	@ echo "    Done"
	@ echo "$(PURPLE)==> Revealing Test Files$(NORMAL)"
	@ mv $(TESTDIR)/tmp/*_tests.beam $(TESTDIR)/ || true
	@ rm -rf $(TESTDIR)/tmp/
	@ echo "    Done"
	@ echo "$(PURPLE)==> Running EUnit Tests$(NORMAL)"
	@ echo "  Running tests for module: erlpkg"
	@ cd $(TESTDIR) && $(ERL) -pa $(TESTDIR) -noinput -noshell -s erlpkg eunit
	@ echo "  Running tests for module: pkgargs"
	@ cd $(TESTDIR) && $(ERL) -pa $(TESTDIR) -noinput -noshell -s pkgargs eunit
	@ echo "  Running tests for module: pkgutils"
	@ cd $(TESTDIR) && $(ERL) -pa $(TESTDIR) -noinput -noshell -s pkgutils eunit
	@ echo "    Done"
	@ echo "$(PURPLE)==> Finished Testing, results are printed to console$(NORMAL)"
	@ echo "    Done\n"
.PHONY: lint
lint:
	@ echo "==> Linting Project with Elvis"
	@ $(ELVIS) || true
	@ echo "    Done\n"
.PHONY: dialyze
dialyze:
	@ echo "==> Building TEST"
	@ echo "    Compiling files with debug_info enabled"
	@ echo "    Compiling files with warnings ignored"
	@ echo "    Compiling files will fail if any errors occur"
	@ mkdir -p $(TESTDIR)
	@ rm -rf $(TESTDIR)/*
	@ echo "==> Compiling Source Files"
	@ $(ERLC) $(TESTFLAGS) $(TESTDIR) $(SRCDIR)/*.erl
	@ rm -rf $(TESTDIR)/*_tests.beam
	@ echo "    Done"
	@ echo "==> Running Dialyzer"
	@ $(DIALYZER) $(TESTDIR)/*.beam || true
.PHONY: eunit
eunit:
	@ echo "==> Building TEST"
	@ echo "    Compiling files with debug_info enabled"
	@ echo "    Compiling files with warnings ignored"
	@ echo "    Compiling files will fail if any errors occur"
	@ mkdir -p $(TESTDIR)
	@ rm -rf $(TESTDIR)/*
	@ echo "==> Compiling Source Files"
	@ $(ERLC) $(TESTFLAGS) $(TESTDIR) $(SRCDIR)/*.erl
	@ echo "    Done"
	@ echo "==> Running EUnit Tests"
	@ echo "  Running tests for module: erlpkg"
	@ cd $(TESTDIR) && $(ERL) -pa $(TESTDIR) -noinput -noshell -s erlpkg eunit
	@ echo "  Running tests for module: pkgargs"
	@ cd $(TESTDIR) && $(ERL) -pa $(TESTDIR) -noinput -noshell -s pkgargs eunit
	@ echo "  Running tests for module: pkgutils"
	@ cd $(TESTDIR) && $(ERL) -pa $(TESTDIR) -noinput -noshell -s pkgutils eunit
	@ echo "    Done\n"
.PHONY: clean
clean:
	@ echo "$(ORANGE)==> Cleaning builds"
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
