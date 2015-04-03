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

IN_RELEASE = $(shell if [ ! -d .git ]; then echo true; fi)

all: couch fauxton

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
	@rm -rf src/{jiffy,khash,snappy,b64url}/priv
	@rm -rf share/server/main.js share/server/main-coffee.js
	@rm -f src/couch/priv/couchspawnkillable
	@rm -f src/couch/priv/couch_js/config.h

check: javascript eunit

# creates a a full erlang release
dist: all
	@rm -rf rel/couchdb
	@rebar generate
	@cp -r share/www rel/couchdb/share/www
	@cp -r share/docs rel/couchdb/share/docs

# creates a source tarball
release:
	./build-aux/couchdb-build-release.sh

	# build fauxton
	$(MAKE) fauxton
	cp -r share/www apache-couchdb/share/

	# build docs
	cd src/docs; make
	mkdir apache-couchdb/share/docs
	cp -r src/docs/build/html apache-couchdb/share/docs/html

	# Tar!
	tar czf apache-couchdb.tar.gz apache-couchdb
	echo "Done: apache-couchdb.tar.gz"

distclean: clean
	@rm install.mk
	@rm config.erl
	@rm rel/couchdb.config
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
install: dist
	@mkdir -p $(prefix)
	@cp -R rel/couchdb/* $(prefix)
	@mkdir -p $(data_dir)
	@chown $(user) $(data_dir)
	@mkdir -p $(view_index_dir)
	@chown $(user) $(view_index_dir)
	@touch $(prefix)/var/log/couchdb.log
	@chown $(user) $(prefix)/var/log/couchdb.log

uninstall:
	@rm -rf $(prefix)

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
	@mkdir -p share/www/script/test
	@cp test/javascript/tests/lorem*.txt share/www/script/test/
	@dev/run -q --with-admin-party-please test/javascript/run

fauxton: share/www

share/www:
	@echo "Building Fauxton"
	@cd src/fauxton && npm install && ./node_modules/grunt-cli/bin/grunt couchdb
