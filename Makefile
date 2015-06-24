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

IN_RELEASE = $(shell if [ ! -d .git ]; then echo true; fi)
COUCHDB_VERSION_SUFFIX = $(shell if [ -d .git ]; then echo '-`git rev-parse --short --verify HEAD`'; fi)
COUCHDB_VERSION = $(vsn_major).$(vsn_minor).$(vsn_patch)$(COUCHDB_VERSION_SUFFIX)

DESTDIR=

all: couch fauxton docs

config.erl:
	@echo "Apache CouchDB has not been configured."
	@echo "Try \"./configure -h\" for help."
	@echo
	@false

couch: config.erl
	@rebar compile
	@cp src/couch/priv/couchjs bin/

clean:
	@rebar -r clean
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

check: javascript eunit build-test

# creates a full erlang release
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

# Tar!
	@tar czf apache-couchdb-$(COUCHDB_VERSION).tar.gz apache-couchdb-$(COUCHDB_VERSION)
	@echo "Done: apache-couchdb-$(COUCHDB_VERSION).tar.gz"

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

devclean:
	@rm -rf dev/lib/*/data

-include install.mk
install: all
	@echo "Installing CouchDB into $(DESTDIR)/$(install_dir)..." | sed -e 's,///,/,'
	@rm -rf rel/couchdb
	@rebar generate # make full erlang release

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

install.mk:
# ignore install.mk missing if we are running
# `make clean` without having run ./configure first
ifneq ($(MAKECMDGOALS), clean)
	@echo "No install.mk found. Run ./configure"
	@exit 1
endif

docker-image:
	@docker build --rm -t couchdb/dev-cluster .

docker-start:
	@docker run -d -P -t couchdb/dev-cluster > .docker-id

docker-stop:
	@docker stop `cat .docker-id`

eunit: export BUILDDIR = $(shell pwd)
eunit: couch
	@rebar setup_eunit
	@rebar -r eunit skip_deps=meck,mochiweb,lager,snappy,couch_replicator,fabric,folsom

javascript: all
	# TODO: Fix tests to look for these files in their new path
	@mkdir -p share/www/script/test
	@cp test/javascript/tests/lorem*.txt share/www/script/test/
	@dev/run -q --with-admin-party-please test/javascript/run
	@rm -rf share/www/script

build-test:
	@test/build/test-configure.sh

# build docs
docs: src/docs/build

src/docs/build:
ifeq ($(with_docs), 1)
	@cd src/docs; $(MAKE)
endif

# build fauxton
fauxton: share/www

share/www:
ifeq ($(with_fauxton), 1)
	@echo "Building Fauxton"
	@cd src/fauxton && npm install && ./node_modules/grunt-cli/bin/grunt couchdb
endif
