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
COUCHDB_VERSION_SUFFIX = $(shell if [ -d .git ]; then echo '-`git rev-parse --short --verify HEAD`'; fi)
COUCHDB_VERSION = $(vsn_major).$(vsn_minor).$(vsn_patch)$(COUCHDB_VERSION_SUFFIX)

DESTDIR=

# Rebar options
apps=
skip_deps=folsom,lager,meck,mochiweb,proper,snappy
suites=
tests=

EUNIT_OPTS=$(shell echo "\
	apps=$(apps) \
	skip_deps=$(skip_deps) \
	suites=$(suites) \
	tests=$(tests) \
	" | sed -e 's/[a-z]\+= / /g')


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
		| awk '{printf("    %-15s", $$1); $$1=$$2=""; print "-" $$0}'


################################################################################
# Building
################################################################################


.PHONY: couch
# target: couch - Build CouchDB core
couch: config.erl
	@$(REBAR) compile
	@cp src/couch/priv/couchjs bin/


.PHONY: docs
# target: docs - Build documentation
docs: src/docs/build


.PHONY: fauxton
# target: fauxton - Build Fauxton web UI
fauxton: share/www


################################################################################
# Testing
################################################################################


.PHONY: check
# target: check - Test everything
check:
	@$(MAKE) eunit
	@$(MAKE) javascript
	@$(MAKE) build-test


.PHONY: eunit
# target: eunit - Run EUnit tests, use EUNIT_OPTS to provide custom options
eunit: export BUILDDIR = $(shell pwd)
eunit: export ERL_AFLAGS = -config $(shell pwd)/rel/files/eunit.config
eunit: couch
	@$(REBAR) setup_eunit 2> /dev/null
	@$(REBAR) -r eunit $(EUNIT_OPTS)


.PHONY: javascript
# target: javascript - Run JavaScript tests suite
javascript: all
	# TODO: Fix tests to look for these files in their new path
	@mkdir -p share/www/script/test
	@cp test/javascript/tests/lorem*.txt share/www/script/test/
	@dev/run -q --with-admin-party-please test/javascript/run $(tests)
	@rm -rf share/www/script


.PHONY: build-test
# target: build-test - Test build script
build-test:
	@test/build/test-configure.sh


################################################################################
# Developing
################################################################################


.PHONY: docker-image
# target: docker-image - Build Docker image
docker-image:
	@docker build --rm -t couchdb/dev-cluster .


.PHONY: docker-start
# target: docker-start - Start CouchDB in Docker container
docker-start:
	@docker run -d -P -t couchdb/dev-cluster > .docker-id


.PHONY: docker-stop
# target: docker-stop - Stop Docker container
docker-stop:
	@docker stop `cat .docker-id`


.PHONY: introspect
# target: introspect - Check for commits difference between rebar.config and repository
introspect:
	@$(REBAR) -r update-deps
	@./introspect


################################################################################
# Distributing
################################################################################


.PHONY: dist
# target: dist - Make release tarball
dist: all
	@./build-aux/couchdb-build-release.sh $(COUCHDB_VERSION)

	@cp -r share/www apache-couchdb-$(COUCHDB_VERSION)/share/
	@mkdir -p apache-couchdb-$(COUCHDB_VERSION)/share/docs/html
	@cp -r src/docs/build/html apache-couchdb-$(COUCHDB_VERSION)/share/docs/html
	@mkdir -p apache-couchdb-$(COUCHDB_VERSION)/share/docs/pdf
	@cp src/docs/build/latex/CouchDB.pdf apache-couchdb-$(COUCHDB_VERSION)/share/docs/pdf/

	@mkdir -p apache-couchdb-$(COUCHDB_VERSION)/share/docs/man
	@cp src/docs/build/man/apachecouchdb.1 apache-couchdb-$(COUCHDB_VERSION)/share/docs/man/
	@mkdir -p apache-couchdb-$(COUCHDB_VERSION)/share/docs/info
	@cp src/docs/build/texinfo/CouchDB.info apache-couchdb-$(COUCHDB_VERSION)/share/docs/info/

	@tar czf apache-couchdb-$(COUCHDB_VERSION).tar.gz apache-couchdb-$(COUCHDB_VERSION)
	@echo "Done: apache-couchdb-$(COUCHDB_VERSION).tar.gz"


.PHONY: install
# target: install - Install CouchDB :-)
-include install.mk
install: all
	@echo "Installing CouchDB into $(DESTDIR)/$(install_dir)..." | sed -e 's,///,/,'
	@rm -rf rel/couchdb
	@$(REBAR) generate # make full erlang release

	@mkdir -p $(DESTDIR)/$(install_dir)
	@cp -R rel/couchdb/* $(DESTDIR)/$(install_dir)

	@mkdir -p $(DESTDIR)/$(database_dir)
	@chown $(user) $(DESTDIR)/$(database_dir)

	@mkdir -p $(DESTDIR)/$(view_index_dir)
	@chown $(user) $(DESTDIR)/$(view_index_dir)

	@mkdir -p $(DESTDIR)/`dirname $(log_file)`
	@touch $(DESTDIR)/$(log_file)
	@chown $(user) $(DESTDIR)/$(log_file)

	@mkdir -p $(DESTDIR)/$(bin_dir)
	@cp rel/couchdb/bin/couchdb $(DESTDIR)/$(bin_dir)

	@mkdir -p $(DESTDIR)/$(libexec_dir)
	@cp rel/couchdb/bin/couchjs $(DESTDIR)/$(libexec_dir)

	@mkdir -p $(DESTDIR)/$(sysconf_dir)
	@mkdir -p $(DESTDIR)/$(sysconf_dir)/default.d
	@mkdir -p $(DESTDIR)/$(sysconf_dir)/local.d
	@cp rel/couchdb/etc/default.ini rel/couchdb/etc/local.ini $(DESTDIR)/$(sysconf_dir)

	@mkdir -p $(DESTDIR)/$(data_dir)
	@cp -R share/server share/www $(DESTDIR)/$(data_dir)

	@mkdir -p $(DESTDIR)/$(doc_dir)
	@mkdir -p $(DESTDIR)/$(html_dir)
	@mkdir -p $(DESTDIR)/$(pdf_dir)
	@mkdir -p $(DESTDIR)/$(man_dir)
	@mkdir -p $(DESTDIR)/$(info_dir)
	@cp -R share/docs/html $(DESTDIR)/$(html_dir)/html
	@cp share/docs/pdf/CouchDB.pdf $(DESTDIR)/$(pdf_dir)/CouchDB.pdf
	@cp share/docs/info/CouchDB.info $(DESTDIR)/$(info_dir)/CouchDB.info
	@cp share/docs/man/apachecouchdb.1 $(DESTDIR)/$(man_dir)/couchdb.1

	@echo "...done"


################################################################################
# Cleaning
################################################################################


.PHONY: clean
# target: clean - Remove build artifacts
clean:
	@$(REBAR) -r clean
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
	@rm -rf $(DESTDIR)/$(pdf_dir)
	@rm -rf $(DESTDIR)/$(man_dir)
	@rm -rf $(DESTDIR)/$(info_dir)


################################################################################
# Misc
################################################################################


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
	@cd src/fauxton && npm install && ./node_modules/grunt-cli/bin/grunt couchdb
endif
