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
    $(ejsonpriv_make_generated_file_list)

if USE_OTP_NIFS
ejsonpriv_LTLIBRARIES = ejson.la
endif

EJSON_C_SRCS = \
	ejson.c \
	decode.c \
	encode.c \
	yajl/yajl_alloc.c \
	yajl/yajl_buf.c \
	yajl/yajl.c \
	yajl/yajl_encode.c \
	yajl/yajl_gen.c \
	yajl/yajl_lex.c \
	yajl/yajl_parser.c

if USE_OTP_NIFS
ejson_la_SOURCES = $(EJSON_C_SRCS)
ejson_la_LDFLAGS = -module -avoid-version

if WINDOWS
ejson_la_LDFLAGS += -no-undefined
endif
endif

ejson_file_collection = \
    ejson.app.in \
    ejson.erl \
    mochijson2.erl \
    mochinum.erl \
    $(JSON_C_SRCS)

ejsonebin_make_generated_file_list = \
    ejson.app \
    ejson.beam \
    mochijson2.beam \
    mochinum.beam

ejsonebin_DATA = \
    $(ejsonebin_make_generated_file_list)

EXTRA_DIST =  \
    $(ejson_file_collection) \
    erl_nif_compat.h \
    yajl/yajl_alloc.h \
    yajl/yajl_buf.h \
    yajl/yajl_bytestack.h \
    yajl/yajl_common.h \
    yajl/yajl_encode.h \
    yajl/yajl_gen.h \
    yajl/yajl_lex.h \
    yajl/yajl_parse.h \
    yajl/yajl_parser.h \
    priv

if USE_OTP_NIFS
priv/ejson.so: .libs/ejson.so
	$(LN_S) .libs priv

all: priv/ejson.so
endif

%.app: %.app.in
	cp $< $@

%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) $<
