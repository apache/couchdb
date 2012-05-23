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

snappyebindir = $(localerlanglibdir)/snappy-1.0.3/ebin
snappyprivdir = $(localerlanglibdir)/snappy-1.0.3/priv

snappy_cxx_srcs = \
	snappy_nif.cc \
	google-snappy/snappy.cc \
	google-snappy/snappy-sinksource.cc \
	google-snappy/snappy-stubs-internal.cc

snappy_cxx_hdrs = \
	erl_nif_compat.h \
	google-snappy/snappy.h \
	google-snappy/snappy-internal.h \
	google-snappy/snappy-sinksource.h \
	google-snappy/snappy-stubs-internal.h \
	google-snappy/snappy-stubs-public.h.in

snappy_file_collection = \
    snappy.app.in \
    snappy.erl

snappyebin_make_generated_file_list = \
    snappy.app \
    snappy.beam

EXTRA_DIST = \
    google-snappy/AUTHORS \
    google-snappy/COPYING \
	$(snappy_cxx_hdrs) \
    $(snappy_file_collection)

CLEANFILES = \
    $(snappyebin_make_generated_file_list) \
	priv/snappy_nif.so

snappyebin_DATA = \
    $(snappyebin_make_generated_file_list)

if USE_OTP_NIFS
snappypriv_LTLIBRARIES = snappy_nif.la
snappy_nif_la_SOURCES = $(snappy_cxx_srcs)
snappy_nif_la_CXXFLAGS = $(ERLANG_FLAGS)
snappy_nif_la_LDFLAGS = -module -avoid-version

if WINDOWS
snappy_nif_la_LDFLAGS += -no-undefined
snappy_nif_la_CXXFLAGS += -EHsc -Ox
SNAPPY_SO_NAME = snappy_nif.dll
else
SNAPPY_SO_NAME = snappy_nif.so
endif

priv/$(SNAPPY_SO_NAME): snappy_nif.la
	@mkdir -p ./priv
	cp .libs/$(SNAPPY_SO_NAME) $@

all: priv/$(SNAPPY_SO_NAME)
endif

%.app: %.app.in
	cp $< $@

%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) $<
