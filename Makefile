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

include version.mk

REBAR?=$(shell echo `pwd`/bin/rebar)
IN_RELEASE = $(shell if [ ! -d .git ]; then echo true; fi)
ifeq ($(IN_RELEASE), true)
COUCHDB_VERSION = $(vsn_major).$(vsn_minor).$(vsn_patch)
else
RELTAG = $(shell git describe | grep -E '^[0-9]+\.[0-9]\.[0-9]+(-RC[0-9]+)?$$')
ifeq ($(RELTAG),)
COUCHDB_VERSION_SUFFIX = $(shell git rev-parse --short --verify HEAD)
COUCHDB_VERSION = $(vsn_major).$(vsn_minor).$(vsn_patch)-$(COUCHDB_VERSION_SUFFIX)
else
COUCHDB_VERSION = $(RELTAG)
endif
endif

DESTDIR=

# Rebar options
apps=
skip_deps=folsom,meck,mochiweb,proper,snappy
suites=
tests=

EUNIT_OPTS=$(shell echo "\
	apps=$(apps) \
	skip_deps=$(skip_deps) \
	suites=$(suites) \
	tests=$(tests) \
	" | sed -e 's/[a-z]\+= / /g')
DIALYZE_OPTS=$(shell echo "\
	apps=$(apps) \
	skip_deps=$(skip_deps) \
	" | sed -e 's/[a-z]\+= / /g')

#ignore javascript tests
ignore_js_suites=

################################################################################
# Main commands
################################################################################


.PHONY: all
# target: all - Build everything
all: couch fauxton docs


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
# target: couch - Build CouchDB core
couch: config.erl
	@COUCHDB_VERSION=$(COUCHDB_VERSION) $(REBAR) compile
	@cp src/couch/priv/couchjs bin/


.PHONY: docs
# target: docs - Build documentation
ifeq ($(IN_RELEASE), true)
docs: share/docs/html
else
docs: src/docs/build
endif

.PHONY: fauxton
# target: fauxton - Build Fauxton web UI
fauxton: share/www


################################################################################
# Testing
################################################################################


.PHONY: check
# target: check - Test everything
check: all
	@$(MAKE) test-cluster-with-quorum
	@$(MAKE) test-cluster-without-quorum
	@$(MAKE) eunit
	@$(MAKE) javascript
	@$(MAKE) mango-test
#	@$(MAKE) build-test


.PHONY: eunit
# target: eunit - Run EUnit tests, use EUNIT_OPTS to provide custom options
eunit: export BUILDDIR = $(shell pwd)
eunit: export ERL_AFLAGS = -config $(shell pwd)/rel/files/eunit.config
eunit: couch
	@$(REBAR) setup_eunit 2> /dev/null
	@$(REBAR) -r eunit $(EUNIT_OPTS)


setup-eunit: export BUILDDIR = $(shell pwd)
setup-eunit: export ERL_AFLAGS = -config $(shell pwd)/rel/files/eunit.config
setup-eunit:
	@$(REBAR) setup_eunit 2> /dev/null

just-eunit: export BUILDDIR = $(shell pwd)
just-eunit: export ERL_AFLAGS = -config $(shell pwd)/rel/files/eunit.config
just-eunit:
	@$(REBAR) -r eunit $(EUNIT_OPTS)

.PHONY: soak-eunit
soak-eunit: export BUILDDIR = $(shell pwd)
soak-eunit: export ERL_AFLAGS = -config $(shell pwd)/rel/files/eunit.config
soak-eunit: couch
	@$(REBAR) setup_eunit 2> /dev/null
	while [ $$? -eq 0 ] ; do $(REBAR) -r eunit $(EUNIT_OPTS) ; done

.PHONY: javascript
# target: javascript - Run JavaScript test suites or specific ones defined by suites option
javascript: devclean
	@mkdir -p share/www/script/test
ifeq ($(IN_RELEASE), true)
	@cp test/javascript/tests/lorem*.txt share/www/script/test/
else
	@mkdir -p src/fauxton/dist/release/test
	@cp test/javascript/tests/lorem*.txt src/fauxton/dist/release/test/
endif
	@dev/run -n 1 -q --with-admin-party-please \
            --enable-erlang-views \
            -c 'startup_jitter=0' \
            'test/javascript/run --suites "$(suites)" \
            --ignore "$(ignore_js_suites)"'

# TODO: port to Makefile.win
.PHONY: test-cluster-with-quorum
test-cluster-with-quorum: devclean
	@mkdir -p share/www/script/test
ifeq ($(IN_RELEASE), true)
	@cp test/javascript/tests/lorem*.txt share/www/script/test/
else
	@mkdir -p src/fauxton/dist/release/test
	@cp test/javascript/tests/lorem*.txt src/fauxton/dist/release/test/
endif
	@dev/run -n 3 -q --with-admin-party-please \
            --enable-erlang-views --degrade-cluster 1 \
            -c 'startup_jitter=0' \
            'test/javascript/run --suites "$(suites)" \
            --ignore "$(ignore_js_suites)" \
	    --path test/javascript/tests-cluster/with-quorum'

# TODO: port to Makefile.win
.PHONY: test-cluster-without-quorum
test-cluster-without-quorum: devclean
	@mkdir -p share/www/script/test
ifeq ($(IN_RELEASE), true)
	@cp test/javascript/tests/lorem*.txt share/www/script/test/
else
	@mkdir -p src/fauxton/dist/release/test
	@cp test/javascript/tests/lorem*.txt src/fauxton/dist/release/test/
endif
	@dev/run -n 3 -q --with-admin-party-please \
            --enable-erlang-views --degrade-cluster 2 \
            -c 'startup_jitter=0' \
            'test/javascript/run --suites "$(suites)" \
            --ignore "$(ignore_js_suites)" \
            --path test/javascript/tests-cluster/without-quorum'

.PHONY: soak-javascript
soak-javascript:
	@mkdir -p share/www/script/test
ifeq ($(IN_RELEASE), true)
	@cp test/javascript/tests/lorem*.txt share/www/script.test/
else
	@mkdir -p src/fauxton/dist/release/test
	@cp test/javascript/tests/lorem*.txt src/fauxton/dist/release/test/
endif
	@rm -rf dev/lib
	while [ $$? -eq 0 ]; do \
		dev/run -n 1 -q --with-admin-party-please \
				-c 'startup_jitter=0' \
				'test/javascript/run --suites "$(suites)" \
				--ignore "$(ignore_js_suites)"'  \
	done

.PHONY: check-qs
# target: check-qs - Run query server tests (ruby and rspec required!)
check-qs:
	@QS_LANG=js rspec test/view_server/query_server_spec.rb


.PHONY: list-eunit-apps
# target: list-eunit-apps - List EUnit target apps
list-eunit-apps:
	@find ./src/ -type f -name *_test.erl -o -name *_tests.erl \
		| cut -d '/' -f 3 \
		| sort -u


.PHONY: list-eunit-suites
# target: list-eunit-suites - List EUnit target test suites
list-eunit-suites:
	@find ./src/ -type f -name *_test.erl -o -name *_tests.erl -printf "%f\n" \
		| cut -d '.' -f -1 \
		| sort


.PHONY: list-js-suites
# target: list-js-suites - List JavaScript test suites
list-js-suites:
	@find ./test/javascript/tests/ -type f -name *.js -printf "%f\n" \
		| cut -d '.' -f -1 \
		| sort


.PHONY: build-test
# target: build-test - Test build script
build-test:
	@test/build/test-configure.sh


.PHONY: mango-test
# target: mango-test - Run Mango tests
mango-test: devclean all
	./test/build/test-run-couch-for-mango.sh \


################################################################################
# Developing
################################################################################


.PHONY: build-plt
# target: build-plt - Build project-specific PLT
build-plt:
	@$(REBAR) -r build-plt $(DIALYZE_OPTS)


.PHONY: check-plt
# target: check-plt - Check the PLT for consistency and rebuild it if it is not up-to-date
check-plt:
	@$(REBAR) -r check-plt $(DIALYZE_OPTS)


.PHONY: dialyze
# target: dialyze - Analyze the code for discrepancies
dialyze: .rebar
	@$(REBAR) -r dialyze $(DIALYZE_OPTS)


.PHONY: introspect
# target: introspect - Check for commits difference between rebar.config and repository
introspect:
	@$(REBAR) -r update-deps
	@build-aux/introspect

################################################################################
# Distributing
################################################################################


.PHONY: dist
# target: dist - Make release tarball
dist: all
	@./build-aux/couchdb-build-release.sh $(COUCHDB_VERSION)

	@cp -r share/www apache-couchdb-$(COUCHDB_VERSION)/share/
	@mkdir -p apache-couchdb-$(COUCHDB_VERSION)/share/docs/html
	@cp -r src/docs/build/html apache-couchdb-$(COUCHDB_VERSION)/share/docs/

	@mkdir -p apache-couchdb-$(COUCHDB_VERSION)/share/docs/man
	@cp src/docs/build/man/apachecouchdb.1 apache-couchdb-$(COUCHDB_VERSION)/share/docs/man/

	@tar czf apache-couchdb-$(COUCHDB_VERSION).tar.gz apache-couchdb-$(COUCHDB_VERSION)
	@echo "Done: apache-couchdb-$(COUCHDB_VERSION).tar.gz"


.PHONY: release
# target: release - Create an Erlang release including CouchDB!
-include install.mk
release: all
	@echo "Installing CouchDB into rel/couchdb/ ..."
	@rm -rf rel/couchdb
	@$(REBAR) generate # make full erlang release

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
	@cp -R src/docs/build/html/ rel/couchdb/share/www/docs
	@cp src/docs/build/man/apachecouchdb.1 rel/couchdb/share/docs/couchdb.1
endif
endif

	@echo "... done"
	@echo
	@echo "    You can now copy the rel/couchdb directory anywhere on your system."
	@echo "    Start CouchDB with ./bin/couchdb from within that directory."
	@echo

.PHONY: install
# target: install- install CouchDB :)
install:
	@echo
	@echo "Notice: There is no 'make install' command for CouchDB 2.x."
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
	@rm -rf src/*/ebin
	@rm -rf src/*/.rebar
	@rm -rf src/*/priv/*.so
	@rm -rf src/couch/priv/{couchspawnkillable,couchjs}
	@rm -rf share/server/main.js share/server/main-coffee.js
	@rm -rf tmp dev/data dev/lib dev/logs
	@rm -f src/couch/priv/couchspawnkillable
	@rm -f src/couch/priv/couch_js/config.h
	@rm -f dev/boot_node.beam dev/pbkdf2.pyc log/crash.log


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
	@rm -rf src/docs
endif


.PHONY: devclean
# target: devclean - Remove dev cluster artifacts
devclean:
	@rm -rf dev/lib/*/data


.PHONY: uninstall
# target: uninstall - Uninstall CouchDB :-(
uninstall:
	@rm -rf $(DESTDIR)/$(install_dir)
	@rm -f $(DESTDIR)/$(bin_dir)/couchdb
	@rm -f $(DESTDIR)/$(libexec_dir)
	@rm -rf $(DESTDIR)/$(sysconf_dir)
	@rm -rf $(DESTDIR)/$(data_dir)
	@rm -rf $(DESTDIR)/$(doc_dir)
	@rm -rf $(DESTDIR)/$(html_dir)
	@rm -rf $(DESTDIR)/$(man_dir)


################################################################################
# Misc
################################################################################


.rebar: build-plt

config.erl:
	@echo "Apache CouchDB has not been configured."
	@echo "Try \"./configure -h\" for help."
	@echo
	@false


src/docs/build:
ifeq ($(with_docs), 1)
	@cd src/docs; $(MAKE)
endif


share/www:
ifeq ($(with_fauxton), 1)
	@echo "Building Fauxton"
	@cd src/fauxton && npm install --production && ./node_modules/grunt-cli/bin/grunt couchdb
endif
