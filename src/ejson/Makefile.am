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

ejsonebindir = $(localerlanglibdir)/ejson-0.1.0/ebin
ejsonprivdir = $(localerlanglibdir)/ejson-0.1.0/priv

CLEANFILES = \
    $(ejsonebin_make_generated_file_list) \
    $(ejsonpriv_make_generated_file_list) \
    priv/ejson.so

EJSON_C_SRCS = \
	c_src/ejson.c \
	c_src/decode.c \
	c_src/encode.c \
	c_src/yajl/yajl_alloc.c \
	c_src/yajl/yajl_buf.c \
	c_src/yajl/yajl.c \
	c_src/yajl/yajl_encode.c \
	c_src/yajl/yajl_gen.c \
	c_src/yajl/yajl_lex.c \
	c_src/yajl/yajl_parser.c

EJSON_C_HDRS = \
    c_src/erl_nif_compat.h \
    c_src/yajl/yajl_alloc.h \
    c_src/yajl/yajl_buf.h \
    c_src/yajl/yajl_bytestack.h \
    c_src/yajl/yajl_common.h \
    c_src/yajl/yajl_encode.h \
    c_src/yajl/yajl_gen.h \
    c_src/yajl/yajl_lex.h \
    c_src/yajl/yajl_parse.h \
    c_src/yajl/yajl_parser.h

ejson_file_collection = \
    src/ejson.app.in \
    src/ejson.erl \
    src/mochijson2.erl \
    src/mochinum.erl

ejsonebin_make_generated_file_list = \
    ebin/ejson.app \
    ebin/ejson.beam \
    ebin/mochijson2.beam \
    ebin/mochinum.beam

EXTRA_DIST = \
	$(EJSON_C_HDRS) \
	$(ejson_file_collection)

ejsonebin_DATA = \
    $(ejsonebin_make_generated_file_list)

if USE_OTP_NIFS
ejsonpriv_LTLIBRARIES = ejson.la

ejson_la_SOURCES = $(EJSON_C_SRCS)
ejson_la_CFLAGS = $(ERLANG_FLAGS)
ejson_la_LDFLAGS = -module -avoid-version

if WINDOWS
ejson_la_LDFLAGS += -no-undefined
EJSON_SO_NAME = ejson.dll
else
EJSON_SO_NAME = ejson.so
endif

priv/$(EJSON_SO_NAME): ejson.la
	@mkdir -p ./priv
	cp .libs/$(EJSON_SO_NAME) $@

all: priv/$(EJSON_SO_NAME)
endif

ebin/%.app: src/%.app.in
	@mkdir -p ebin/
	cp $< $@

ebin/%.beam: src/%.erl
	@mkdir -p ebin/
	$(ERLC) $(ERLC_FLAGS) -o ebin/ $<
