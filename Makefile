
# This is not a Perl distribution, but it can build one using Dist::Zilla.
# This Makefile is just a convenience.
#
# - Run "make bootstrap" to install all dependencies required to build
#   a distribution.
# - To do a release, make sure that the Changes file lists Changes under the
#   {{$NEXT}} keyword and run "dzil release".

COVER       ?= cover
CPANM       ?= cpanm
DZIL        ?= dzil
PERLCRITIC  ?= perlcritic
PROVE       ?= prove

all: dist

bootstrap:
	$(CPANM) $(CPANM_FLAGS) -n Dist::Zilla
	$(DZIL) authordeps --missing |$(CPANM) $(CPANM_FLAGS) -n
	$(DZIL) listdeps --develop --missing |$(CPANM) $(CPANM_FLAGS) -n

check:
	$(PERLCRITIC) lib t

clean:
	$(DZIL) $@

cover:
	$(COVER) -test

dist:
	$(DZIL) build

distclean: clean
	rm -rf cover_db

test:
	$(PROVE) -l$(if $(findstring 1,$(V)),v) t

.PHONY: all bootstrap check clean cover dist distclean test

