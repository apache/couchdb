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

snappyebindir = $(localerlanglibdir)/snappy-1.0.1/ebin
snappyprivdir = $(localerlanglibdir)/snappy-1.0.1/priv

CLEANFILES = \
	$(snappyebin_make_generated_file_list) \
	$(snappypriv_make_generated_file_list) \
	priv/snappy_nif.so

if USE_OTP_NIFS
snappypriv_LTLIBRARIES = snappy_nif.la
endif

SNAPPY_CXX_SRCS = \
	snappy_nif.cc \
	google-snappy/snappy.cc \
	google-snappy/snappy-sinksource.cc \
	google-snappy/snappy-stubs-internal.cc

SNAPPY_CXX_HDRS = \
	erl_nif_compat.h \
	google-snappy/snappy.h \
	google-snappy/snappy-internal.h \
	google-snappy/snappy-sinksource.h \
	google-snappy/snappy-stubs-internal.h \
	google-snappy/snappy-stubs-public.h

if USE_OTP_NIFS
snappy_nif_la_SOURCES = $(SNAPPY_CXX_SRCS)
snappy_nif_la_LDFLAGS = -module -avoid-version

if WINDOWS
snappy_nif_la_LDFLAGS += -no-undefined
endif
endif

snappy_file_collection = \
    snappy.app.in \
    snappy.erl

snappyebin_make_generated_file_list = \
    snappy.app \
    snappy.beam

EXTRA_DIST = \
	$(SNAPPY_CXX_HDRS) \
	$(snappy_file_collection)

snappyebin_DATA = \
    $(snappyebin_make_generated_file_list)

if USE_OTP_NIFS
priv/snappy_nif.so: snappy_nif.la
	@mkdir -p ./priv
	cp .libs/snappy_nif.so $@

all: priv/snappy_nif.so
endif

%.app: %.app.in
	cp $< $@

%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) $<
