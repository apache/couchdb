## Licensed under the Apache License, Version 2.0 (the "License"); you may not
## use this file except in compliance with the License.  You may obtain a copy
## of the License at
##
##   http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
## WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
## License for the specific language governing permissions and limitations under
## the License.

ddoc_cacheebindir = $(localerlanglibdir)/ddoc_cache/ebin

ddoc_cacheebin_DATA = $(compiled_files)

source_files = \
	src/ddoc_cache.app.src \
    src/ddoc_cache.erl \
	src/ddoc_cache_app.erl \
	src/ddoc_cache_opener.erl \
	src/ddoc_cache_sup.erl \
	src/ddoc_cache_util.erl

compiled_files = \
	ebin/ddoc_cache.app \
    ebin/ddoc_cache.beam \
	ebin/ddoc_cache_app.beam \
	ebin/ddoc_cache_opener.beam \
	ebin/ddoc_cache_sup.beam \
	ebin/ddoc_cache_util.beam

EXTRA_DIST = $(source_files)
CLEANFILES = $(compiled_files)

ebin/%.app: src/%.app.src
	@mkdir -p ebin/
	sed -e "s|%version%|@version@|g" < $< > $@

ebin/%.beam: src/%.erl
	@mkdir -p ebin/
	$(ERLC) -Wall -I$(top_srcdir)/src -o ebin/ $(ERLC_FLAGS) $<
