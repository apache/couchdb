// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

#ifndef COUCHJS_UTIL_H
#define COUCHJS_UTIL_H

#include <jsapi.h>

typedef struct {
    int          eval;
    int          use_http;
    int          use_test_funs;
    int          stack_size;
    const char** scripts;
    const char*  uri_file;
    JSString*    uri;
} couch_args;

std::string js_to_string(JSContext* cx, JS::HandleValue val);
bool js_to_string(JSContext* cx, JS::HandleValue val, std::string& str);
JSString* string_to_js(JSContext* cx, const std::string& s);

couch_args* couch_parse_args(int argc, const char* argv[]);
int couch_fgets(char* buf, int size, FILE* fp);
JSString* couch_readline(JSContext* cx, FILE* fp);
size_t couch_readfile(const char* file, char** outbuf_p);
void couch_print(JSContext* cx, JS::HandleValue str, bool use_stderr);
void couch_error(JSContext* cx, JSErrorReport* report);
void couch_oom(JSContext* cx, void* data);
bool couch_load_funcs(JSContext* cx, JS::HandleObject obj, JSFunctionSpec* funcs);

#endif // Included util.h
