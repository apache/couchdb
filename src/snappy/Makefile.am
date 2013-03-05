## Licensed under the Apache License, Version 2.0 (the "License"); you may not
## use this file except in compliance with the License. You may obtain a copy of
## the License at
##
##   http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
## WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
## License for the specific language governing permissions and limitations under
## the License.

SUBDIRS = c_src

snappyebindir = $(localerlanglibdir)/snappy-1.0.5/ebin

snappy_file_collection = \
    src/snappy.app.in \
    src/snappy.erl

snappyebin_make_generated_file_list = \
    ebin/snappy.app \
    ebin/snappy.beam

snappyebin_DATA = \
	$(snappyebin_make_generated_file_list)

EXTRA_DIST = \
    $(snappy_file_collection)

CLEANFILES = \
    $(snappyebin_make_generated_file_list)

ebin/%.app: src/%.app.in
	@mkdir -p ebin/
	cp $< $@

ebin/%.beam: src/%.erl
	@mkdir -p ebin/
	$(ERLC) -o ebin/ $(ERLC_FLAGS) $<
