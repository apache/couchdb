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

check: javascript eunit


dist: all
	@rm -rf rel/couchdb
	@rebar generate
	@cp -r share/www rel/couchdb/share/www

distclean: clean
	@rm -rf rel/couchdb
	@rm -rf share/www

devclean:
	@rm -rf dev/lib/*/data

include install.mk
install: dist
	@mkdir -p $(prefix)
	@cp -R rel/couchdb/* $(prefix)
	@mkdir -p $(data_dir)
	@chown $(user) $(data_dir)
	@mkdir -p $(view_index_dir)
	@chown $(user) $(view_index_dir)
	@touch $(prefix)/var/log/couchdb.log
	@chown $(user) $(prefix)/var/log/couchdb.log

install.mk:
	@echo "No install.mk found. Run ./configure"
	@exit 1

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
	@dev/run -q test/javascript/run

fauxton: share/www

share/www:
	@echo "Building Fauxton"
	@cd src/fauxton && npm install && ./node_modules/grunt-cli/bin/grunt couchdb
