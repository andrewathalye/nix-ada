PREFIX   ?= $(HOME)/libraries
TESTDIR   = tests
OBJDIR    = obj
LIBDIR    = lib

PKGCONF = `pkg-config --cflags --libs dbus-glib-1`
TESTBIN = $(OBJDIR)/tests/rebounder
TESTPID = `pidof rebounder`

MAJOR   = 0
MINOR   = 6
REV     = 2
VERSION = $(MAJOR).$(MINOR).$(REV)
# Set VERSION to '' for a static library.
DBUSADA = libdbusada-$(VERSION)
TARBALL = $(DBUSADA).tar.bz2

NUM_CPUS ?= 1

# GNAT_BUILDER_FLAGS, ADAFLAGS and LDFLAGS may be overridden in the
# environment or on the command line.
GNAT_BUILDER_FLAGS ?= -R -j$(NUM_CPUS)

# GMAKE_OPTS should not be overridden because -p is essential.
GMAKE_OPTS = -p ${GNAT_BUILDER_FLAGS} \
  $(foreach v,ADAFLAGS LDFLAGS,"-X$(v)=$($(v))")

# This variable explicitly exists for override by people with a
# different directory hierarchy.
# exec is unrelated and currently only used by tests
GPRINSTALLFLAGS := \
  --prefix=$(PREFIX) \
  --no-manifest \
  --exec-subdir=tests \
  --ali-subdir=lib/dbus-ada \
  --lib-subdir=lib \
  --project-subdir=lib/gnat \
  --sources-subdir=include/dbus-ada \
  # EOL

all: build-lib

build-lib:
	gprbuild $(GMAKE_OPTS) -Pdbusada -XVERSION=$(VERSION)

build-tests:
	gprbuild $(GMAKE_OPTS) -Pd_bus_ada_tests -XVERSION=

build-examples:
	gprbuild $(GMAKE_OPTS) -Pd_bus_ada_examples -XVERSION=

install: build-lib
	gprinstall -f -p -Pdbusada -XVERSION=$(VERSION) $(GPRINSTALLFLAGS)

tests: build-tests $(TESTBIN)
	@$(TESTBIN) &
	@$(OBJDIR)/tests/runner || true
	@kill $(TESTPID)

$(OBJDIR)/tests:
	@mkdir -p $@

$(TESTBIN): $(TESTDIR)/c/dbus-rebound.c | $(OBJDIR)/tests
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ $^ $(PKGCONF) $(LDLIBS)

clean:
	@rm -rf $(OBJDIR)
	@rm -rf $(LIBDIR)

dist:
	@echo "Creating release tarball $(TARBALL) ... "
	@git archive --format=tar HEAD --prefix $(DBUSADA)/ | bzip2 > $(TARBALL)

.PHONY: tests

include doc/doc.mk
