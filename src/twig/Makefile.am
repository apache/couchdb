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

twigebindir = $(localerlanglibdir)/twig/ebin

twigebin_DATA = $(compiled_files)

include_files = \
	twig_int.hrl

source_files = \
	src/twig.app.src \
    src/twig.erl \
	src/twig_app.erl \
	src/twig_event_handler.erl \
	src/twig_monitor.erl \
	src/twig_sup.erl \
	src/twig_util.erl \
	src/trunc_io.erl

compiled_files = \
	ebin/twig.app \
    ebin/twig.beam \
	ebin/twig_app.beam \
	ebin/twig_event_handler.beam \
	ebin/twig_monitor.beam \
	ebin/twig_sup.beam \
	ebin/twig_util.beam \
	ebin/trunc_io.beam

EXTRA_DIST = $(include_files) $(source_files)
CLEANFILES = $(compiled_files)

ebin/%.app: src/%.app.src
	@mkdir -p ebin/
	sed -e "s|%version%|@version@|g" < $< > $@

ebin/%.beam: src/%.erl
	@mkdir -p ebin/
	$(ERLC) -Wall -I$(top_srcdir)/src -o ebin/ $(ERLC_FLAGS) $<
