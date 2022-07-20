# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

# *******************************************************
# WARNING! If you edit this file, also edit Makefile.win!
# *******************************************************

include version.mk

REBAR3?=$(shell echo `pwd`/bin/rebar3)
ERLFMT?=$(shell echo `pwd`/bin/erlfmt)

# Handle the following scenarios:
#   1. When building from a tarball, use version.mk.
#   2. When building from a clean release tag (#.#.#), use that tag.
#   3. When building from a clean RC tag (#.#.#-RC#), use JUST the version
#      number inside the tarball, but use the full name for the name of the
#      tarball itself.
#   4. When not on a clean tag, use version.mk + git sha + dirty status.

COUCHDB_GIT_SHA=$(git_sha)

IN_RELEASE = $(shell if [ ! -d .git ]; then echo true; fi)
ifeq ($(IN_RELEASE), true)

# 1. Building from tarball, use version.mk.
COUCHDB_VERSION = $(vsn_major).$(vsn_minor).$(vsn_patch)

else

# Gather some additional information.
# We do it this way so we don't bake shell-isms into Makefile
# to make it easier to port to Windows. I know, I know. -jst
# IN_RC contains the -RCx suffix in the name if present
IN_RC = $(shell git describe --tags --always --first-parent \
        | grep -Eo -- '-RC[0-9]+' 2>/dev/null)
# ON_TAG matches *ONLY* if we are on a release or RC tag
ON_TAG = $(shell git describe --tags --always --first-parent \
        | grep -Eo -- '^[0-9]+\.[0-9]\.[0-9]+(-RC[0-9]+)?$$' 2>/dev/null)
# REL_TAG contains the #.#.# from git describe, which might be used
REL_TAG = $(shell git describe --tags --always --first-parent \
        | grep -Eo -- '^[0-9]+\.[0-9]\.[0-9]+' 2>/dev/null)
# DIRTY identifies if we're not on a commit
DIRTY = $(shell git describe --dirty | grep -Eo -- '-dirty' 2>/dev/null)
# COUCHDB_GIT_SHA is our current git hash.
COUCHDB_GIT_SHA=$(shell git rev-parse --short=7 --verify HEAD)

ifeq ($(ON_TAG),)
# 4. Not on a tag.
COUCHDB_VERSION_SUFFIX = $(COUCHDB_GIT_SHA)$(DIRTY)
COUCHDB_VERSION = $(vsn_major).$(vsn_minor).$(vsn_patch)-$(COUCHDB_VERSION_SUFFIX)
else
# 2 and 3. On a tag.
COUCHDB_VERSION = $(REL_TAG)$(DIRTY)
endif
endif

# needed to do text substitutions
comma:= ,
empty:=
space:= $(empty) $(empty)

DESTDIR=

# Rebar options
apps=
skip_deps=folsom,meck,mochiweb,triq,proper,snappy,bcrypt,hyper,ibrowse
suites=
tests=

EXUNIT_OPTS=$(subst $(comma),$(space),$(tests))

TEST_OPTS="-c 'startup_jitter=0' -c 'default_security=admin_local'"

################################################################################
# Main commands
################################################################################


.PHONY: all
# target: all - Build everything
all: couch fauxton docs escriptize


.PHONY: help
# target: help - Print this help
help:
	@egrep "^# target: " Makefile \
		| sed -e 's/^# target: //g' \
		| sort \
		| awk '{printf("    %-20s", $$1); $$1=$$2=""; print "-" $$0}'


################################################################################
# Building
################################################################################


.PHONY: couch
# target: couch - Build CouchDB core, use ERL_COMPILER_OPTIONS to provide custom compiler's options
couch: config.erl
	@[ -e bin/couchjs ] || COUCHDB_VERSION=$(COUCHDB_VERSION) COUCHDB_GIT_SHA=$(COUCHDB_GIT_SHA) $(REBAR3) compile
	@cp apps/couch/priv/couchjs bin/


.PHONY: docs
# target: docs - Build documentation
ifeq ($(IN_RELEASE), true)
docs: share/docs/html
else
docs: apps/docs/build
endif

.PHONY: fauxton
# target: fauxton - Build Fauxton web UI
fauxton: share/www


.PHONY: escriptize
# target: escriptize - Build CLI tools
escriptize: couch
	@[ -e bin/weatherreport ] || COUCHDB_VERSION=$(COUCHDB_VERSION) COUCHDB_GIT_SHA=$(COUCHDB_GIT_SHA) $(REBAR3) escriptize -a weatherreport
	@cp _build/default/bin/weatherreport bin/weatherreport


################################################################################
# Testing
################################################################################


.PHONY: check
# target: check - Test everything
check: all python-black
	@$(MAKE) exunit
	@$(MAKE) eunit
	@$(MAKE) mango-test
	@$(MAKE) elixir-suite
	@$(MAKE) weatherreport-test

.PHONY: eunit
# target: eunit - Run EUnit tests, use apps to provide custom options
eunit: export BUILDDIR = $(shell pwd)
eunit: export ERL_AFLAGS = -config $(shell pwd)/rel/files/eunit.config
eunit: export COUCHDB_QUERY_SERVER_JAVASCRIPT = $(shell pwd)/bin/couchjs $(shell pwd)/share/server/main.js
eunit: export COUCHDB_TEST_ADMIN_PARTY_OVERRIDE=1
eunit: couch
	@[ -d $(shell pwd)/apps/b64url ] || cp -r $(shell pwd)/_build/default/lib/b64url $(shell pwd)/apps
	@[ -d $(shell pwd)/apps/config ] || cp -r $(shell pwd)/_build/default/lib/config $(shell pwd)/apps
	@[ -d $(shell pwd)/apps/ets_lru ] || cp -r $(shell pwd)/_build/default/lib/ets_lru $(shell pwd)/apps
	@[ -d $(shell pwd)/apps/jiffy ] || cp -r $(shell pwd)/_build/default/lib/jiffy $(shell pwd)/apps
	@[ -d $(shell pwd)/apps/khash ] || cp -r $(shell pwd)/_build/default/lib/khash $(shell pwd)/apps
	@[ -d $(shell pwd)/apps/recon ] || cp -r $(shell pwd)/_build/default/lib/recon $(shell pwd)/apps
	@COUCHDB_VERSION=$(COUCHDB_VERSION) COUCHDB_GIT_SHA=$(COUCHDB_GIT_SHA) $(REBAR3) ic setup_eunit 2> /dev/null
ifdef apps
	@COUCHDB_VERSION=$(COUCHDB_VERSION) COUCHDB_GIT_SHA=$(COUCHDB_GIT_SHA) $(REBAR3) eunit --app $(apps) || exit 1
else
	@COUCHDB_VERSION=$(COUCHDB_VERSION) COUCHDB_GIT_SHA=$(COUCHDB_GIT_SHA) $(REBAR3) eunit || exit 1
endif


.PHONY: exunit
# target: exunit - Run ExUnit tests
exunit: export BUILDDIR = $(shell pwd)
exunit: export MIX_ENV=test
exunit: export ERL_LIBS = $(shell pwd)/_build/default/lib
exunit: export ERL_AFLAGS = -config $(shell pwd)/rel/files/eunit.config
exunit: export COUCHDB_QUERY_SERVER_JAVASCRIPT = $(shell pwd)/bin/couchjs $(shell pwd)/share/server/main.js
exunit: export COUCHDB_TEST_ADMIN_PARTY_OVERRIDE=1
exunit: couch elixir-init setup-eunit elixir-check-formatted elixir-credo
	@mix test --trace $(EXUNIT_OPTS)

setup-eunit: export BUILDDIR = $(shell pwd)
setup-eunit: export ERL_AFLAGS = -config $(shell pwd)/rel/files/eunit.config
setup-eunit:
	@$(REBAR3) ic setup_eunit 2> /dev/null

just-eunit: export BUILDDIR = $(shell pwd)
just-eunit: export ERL_AFLAGS = -config $(shell pwd)/rel/files/eunit.config
just-eunit:
	@$(REBAR3) eunit

.PHONY: soak-eunit
soak-eunit: export BUILDDIR = $(shell pwd)
soak-eunit: export ERL_AFLAGS = -config $(shell pwd)/rel/files/eunit.config
soak-eunit: couch
	@$(REBAR3) ic setup_eunit 2> /dev/null
	while [ $$? -eq 0 ] ; do $(REBAR3) eunit ; done

erlfmt-check:
	ERLFMT_PATH=$(ERLFMT) python3 dev/format_check.py

erlfmt-format:
	ERLFMT_PATH=$(ERLFMT) python3 dev/format_all.py

.venv/bin/black:
	@python3 -m venv .venv
	@.venv/bin/pip3 install black || touch .venv/bin/black

# Python code formatter - only runs if we're on Python 3.6 or greater
python-black: .venv/bin/black
	@python3 -c "import sys; exit(1 if sys.version_info < (3,6) else 0)" || \
	       echo "Python formatter not supported on Python < 3.6; check results on a newer platform"
	@python3 -c "import sys; exit(1 if sys.version_info >= (3,6) else 0)" || \
		LC_ALL=C.UTF-8 LANG=C.UTF-8 .venv/bin/black --check \
		--exclude="build/|buck-out/|dist/|_build/|\.git/|\.hg/|\.mypy_cache/|\.nox/|\.tox/|\.venv/|apps/erlfmt|apps/jiffy|apps/rebar/pr2relnotes.py|apps/fauxton" \
		build-aux/*.py dev/run dev/format_*.py apps/mango/test/*.py _build/default/lib/docs/src/conf.py _build/default/lib/docs/ext/*.py .

python-black-update: .venv/bin/black
	@python3 -c "import sys; exit(1 if sys.version_info < (3,6) else 0)" || \
	       echo "Python formatter not supported on Python < 3.6; check results on a newer platform"
	@python3 -c "import sys; exit(1 if sys.version_info >= (3,6) else 0)" || \
		LC_ALL=C.UTF-8 LANG=C.UTF-8 .venv/bin/black \
		--exclude="build/|buck-out/|dist/|_build/|\.git/|\.hg/|\.mypy_cache/|\.nox/|\.tox/|\.venv/|apps/rebar/pr2relnotes.py|apps/fauxton" \
		build-aux/*.py dev/run apps/mango/test/*.py _build/default/lib/docs/src/conf.py _build/default/lib/docs/ext/*.py .

.PHONY: elixir
elixir: export MIX_ENV=integration
elixir: export COUCHDB_TEST_ADMIN_PARTY_OVERRIDE=1
elixir: elixir-init elixir-check-formatted elixir-credo devclean
	@dev/run "$(TEST_OPTS)" -a adm:pass -n 1 \
		--enable-erlang-views \
		--locald-config test/elixir/test/config/test-config.ini \
		--no-eval 'mix test --trace --exclude without_quorum_test --exclude with_quorum_test $(EXUNIT_OPTS)'

.PHONY: elixir-init
elixir-init: MIX_ENV=test
elixir-init: config.erl
	@mix local.rebar --force && mix local.hex --force && mix deps.get

.PHONY: elixir-cluster-without-quorum
elixir-cluster-without-quorum: export MIX_ENV=integration
elixir-cluster-without-quorum: elixir-init elixir-check-formatted elixir-credo devclean
	@dev/run -n 3 -q -a adm:pass \
		--degrade-cluster 2 \
		--no-eval 'mix test --trace --only without_quorum_test $(EXUNIT_OPTS)'

.PHONY: elixir-cluster-with-quorum
elixir-cluster-with-quorum: export MIX_ENV=integration
elixir-cluster-with-quorum: elixir-init elixir-check-formatted elixir-credo devclean
	@dev/run -n 3 -q -a adm:pass \
		--degrade-cluster 1 \
		--no-eval 'mix test --trace --only with_quorum_test $(EXUNIT_OPTS)'

.PHONY: elixir-suite
elixir-suite: export MIX_ENV=integration
elixir-suite: export COUCHDB_TEST_ADMIN_PARTY_OVERRIDE=1
elixir-suite: elixir-init elixir-check-formatted elixir-credo devclean
	@dev/run -n 1 -q -a adm:pass \
		--enable-erlang-views \
		--no-join \
		--locald-config test/elixir/test/config/test-config.ini \
		--erlang-config rel/files/eunit.config \
		--no-eval 'mix test --trace --include test/elixir/test/config/suite.elixir --exclude test/elixir/test/config/skip.elixir'

.PHONY: elixir-check-formatted
elixir-check-formatted: elixir-init
	@mix format --check-formatted

# Credo is a static code analysis tool for Elixir.
# We use it in our tests
.PHONY: elixir-credo
elixir-credo: elixir-init
	@mix credo

.PHONY: build-report
# target: build-report - Generate and upload a build report
build-report:
	build-aux/show-test-results.py --suites=10 --tests=10 > test-results.log
	#build-aux/logfile-uploader.py

.PHONY: check-qs
# target: check-qs - Run query server tests (ruby and rspec required!)
check-qs:
	@QS_LANG=js rspec test/view_server/query_server_spec.rb


.PHONY: list-eunit-apps
# target: list-eunit-apps - List EUnit target apps
list-eunit-apps:
	@find ./apps/ -type f -name *_test.erl -o -name *_tests.erl \
		| cut -d '/' -f 3 \
		| sort -u


.PHONY: list-eunit-suites
# target: list-eunit-suites - List EUnit target test suites
list-eunit-suites:
	@find ./apps/ -type f -name *_test.erl -o -name *_tests.erl -exec basename {} \; \
		| cut -d '.' -f -1 \
		| sort


.PHONY: build-test
# target: build-test - Test build script
build-test:
	@test/build/test-configure.sh


.PHONY: mango-test
# target: mango-test - Run Mango tests
mango-test: export COUCHDB_TEST_ADMIN_PARTY_OVERRIDE=1
mango-test: devclean all
	@cd apps/mango && \
		python3 -m venv .venv && \
		.venv/bin/python3 -m pip install -r requirements.txt
	@cd apps/mango && ../../dev/run "$(TEST_OPTS)" -n 1 --admin=testuser:testpass '.venv/bin/python3 -m nose2'


.PHONY: weatherreport-test
# target: weatherreport-test - Run weatherreport against dev cluster
weatherreport-test: devclean escriptize
	@dev/run -n 1 -a adm:pass --no-eval \
		'bin/weatherreport --etc dev/lib/node1/etc --level error'

################################################################################
# Developing
################################################################################


.PHONY: dialyze
# target: dialyze - Analyze the code for discrepancies
dialyze:
	@$(REBAR3) dialyzer


.PHONY: introspect
# target: introspect - Check for commits difference between rebar.config and repository
introspect:
	@$(REBAR3) upgrade --all
	@build-aux/introspect

################################################################################
# Distributing
################################################################################


.PHONY: dist
# target: dist - Make release tarball
dist: all derived
	@./build-aux/couchdb-build-release.sh $(COUCHDB_VERSION)

	@cp -r _build/default/share/www apache-couchdb-$(COUCHDB_VERSION)/share/
	@mkdir -p apache-couchdb-$(COUCHDB_VERSION)/share/docs/html
	@cp -r _build/default/lib/docs/build/html apache-couchdb-$(COUCHDB_VERSION)/share/docs/

	@mkdir -p apache-couchdb-$(COUCHDB_VERSION)/share/docs/man
	@cp _build/default/lib/docs/build/man/apachecouchdb.1 apache-couchdb-$(COUCHDB_VERSION)/share/docs/man/

	@tar czf apache-couchdb-$(COUCHDB_VERSION)$(IN_RC).tar.gz apache-couchdb-$(COUCHDB_VERSION)
	@echo "Done: apache-couchdb-$(COUCHDB_VERSION)$(IN_RC).tar.gz"


.PHONY: release
# target: release - Create an Erlang release including CouchDB!
-include install.mk
release: all
	@echo "Installing CouchDB into rel/couchdb/ ..."
	@rm -rf rel/couchdb
	@$(REBAR) generate # make full erlang release
	@cp bin/weatherreport rel/couchdb/bin/weatherreport

ifeq ($(with_fauxton), 1)
	@mkdir -p rel/couchdb/share/
	@cp -R share/www rel/couchdb/share/
endif

ifeq ($(with_docs), 1)
ifeq ($(IN_RELEASE), true)
	@mkdir -p rel/couchdb/share/www/docs/
	@mkdir -p rel/couchdb/share/docs/
	@cp -R share/docs/html/* rel/couchdb/share/www/docs/
	@cp share/docs/man/apachecouchdb.1 rel/couchdb/share/docs/couchdb.1
else
	@mkdir -p rel/couchdb/share/www/docs/
	@mkdir -p rel/couchdb/share/docs/
	@cp -R apps/docs/build/html/ rel/couchdb/share/www/docs
	@cp apps/docs/build/man/apachecouchdb.1 rel/couchdb/share/docs/couchdb.1
endif
endif

	@echo "... done"
	@echo
	@echo "    You can now copy the rel/couchdb directory anywhere on your system."
	@echo "    Start CouchDB with ./bin/couchdb from within that directory."
	@echo

.PHONY: install
# target: install- install CouchDB :)
install: release
	@echo
	@echo "Notice: There is no 'make install' command for CouchDB 2.x+."
	@echo
	@echo "    To install CouchDB into your system, copy the rel/couchdb"
	@echo "    to your desired installation location. For example:"
	@echo "    cp -r rel/couchdb /usr/local/lib"
	@echo

################################################################################
# Cleaning
################################################################################


.PHONY: clean
# target: clean - Remove build artifacts
clean:
	@$(REBAR) -r clean
	@rm -rf .rebar/
	@rm -f bin/couchjs
	@rm -f bin/weatherreport
	@rm -rf apps/*/ebin
	@rm -rf apps/*/.rebar
	@rm -rf apps/*/priv/*.so
	@rm -rf apps/couch/priv/{couchspawnkillable,couchjs}
	@rm -rf share/server/main.js share/server/main-coffee.js
	@rm -rf tmp dev/data dev/lib dev/logs
	@rm -rf apps/mango/.venv
	@rm -f apps/couch/priv/couchspawnkillable
	@rm -f apps/couch/priv/couch_js/config.h
	@rm -f dev/*.beam dev/devnode.* dev/pbkdf2.pyc log/crash.log
	@rm -f dev/erlserver.pem dev/couch_ssl_dist.conf


.PHONY: distclean
# target: distclean - Remove build and release artifacts
distclean: clean
	@rm -f install.mk
	@rm -f config.erl
	@rm -f rel/couchdb.config
ifneq ($(IN_RELEASE), true)
# when we are in a release, don’t delete the
# copied sources, generated docs, or fauxton
	@rm -rf rel/couchdb
	@rm -rf share/www
	@rm -rf apps/docs
endif


.PHONY: devclean
# target: devclean - Remove dev cluster artifacts
devclean:
	@rm -rf dev/lib/*/data
	@rm -rf dev/lib/*/etc

################################################################################
# Misc
################################################################################


.rebar: build-plt

config.erl:
	@echo "Apache CouchDB has not been configured."
	@echo "Try \"./configure -h\" for help."
	@echo
	@false


apps/docs/build:
ifeq ($(with_docs), 1)
	@cd _build/default/lib/docs; $(MAKE)
endif


share/www:
ifeq ($(with_fauxton), 1)
	@echo "Building Fauxton"
	@cd _build/default/lib/fauxton && npm install && ./node_modules/grunt-cli/bin/grunt couchdb
endif


derived:
	@echo "COUCHDB_GIT_SHA:        $(COUCHDB_GIT_SHA)"
	@echo "COUCHDB_VERSION:        $(COUCHDB_VERSION)"
	@echo "COUCHDB_VERSION_SUFFIX: $(COUCHDB_VERSION_SUFFIX)"
	@echo "DIRTY:                  $(DIRTY)"
	@echo "IN_RC:                  $(IN_RC)"
	@echo "IN_RELEASE:             $(IN_RELEASE)"
	@echo "ON_TAG:                 $(ON_TAG)"
	@echo "REL_TAG:                $(REL_TAG)"
	@echo "SUB_VSN:                $(SUB_VSN)"
