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

#include <stdlib.h>
#include <string.h>

#include <sstream>

#include <jsapi.h>
#include <jsfriendapi.h>
#include <js/CharacterEncoding.h>
#include <js/Conversions.h>
#include <js/Initialization.h>
#include <js/MemoryFunctions.h>
#include <js/RegExp.h>

#include "help.h"
#include "util.h"

std::string
js_to_string(JSContext* cx, JS::HandleValue val)
{
    JS::AutoSaveExceptionState exc_state(cx);
    JS::RootedString sval(cx);
    sval = val.toString();

    JS::UniqueChars chars(JS_EncodeStringToUTF8(cx, sval));
    if(!chars) {
        JS_ClearPendingException(cx);
        return std::string();
    }

    return chars.get();
}

bool
js_to_string(JSContext* cx, JS::HandleValue val, std::string& str)
{
    if(!val.isString()) {
        return false;
    }

    if(JS_GetStringLength(val.toString()) == 0) {
        str = "";
        return true;
    }

    std::string conv = js_to_string(cx, val);
    if(!conv.size()) {
        return false;
    }

    str = conv;
    return true;
}

JSString*
string_to_js(JSContext* cx, const std::string& raw)
{
    JS::UTF8Chars utf8(raw.c_str(), raw.size());
    JS::UniqueTwoByteChars utf16;
    size_t len;

    utf16.reset(JS::UTF8CharsToNewTwoByteCharsZ(cx, utf8, &len, js::MallocArena).get());
    if(!utf16) {
        return nullptr;
    }

    return JS_NewUCString(cx, std::move(utf16), len);
}

size_t
couch_readfile(const char* file, char** outbuf_p)
{
    FILE* fp;
    char fbuf[16384];
    char *buf = NULL;
    char* tmp;
    size_t nread = 0;
    size_t buflen = 0;

    if(strcmp(file, "-") == 0) {
        fp = stdin;
    } else {
        fp = fopen(file, "r");
        if(fp == NULL) {
            fprintf(stderr, "Failed to read file: %s\n", file);
            exit(3);
        }
    }

    while((nread = fread(fbuf, 1, 16384, fp)) > 0) {
        if(buf == NULL) {
            buf = new char[nread + 1];
            if(buf == NULL) {
                fprintf(stderr, "Out of memory.\n");
                exit(3);
            }
            memcpy(buf, fbuf, nread);
        } else {
            tmp = new char[buflen + nread + 1];
            if(tmp == NULL) {
                fprintf(stderr, "Out of memory.\n");
                exit(3);
            }
            memcpy(tmp, buf, buflen);
            memcpy(tmp+buflen, fbuf, nread);
            delete buf;
            buf = tmp;
        }
        buflen += nread;
        buf[buflen] = '\0';
    }
    *outbuf_p = buf;
    return buflen ;
}

couch_args*
couch_parse_args(int argc, const char* argv[])
{
    couch_args* args;
    int i = 1;

    args = new couch_args();
    if(args == NULL)
        return NULL;

    args->eval = 0;
    args->use_http = 0;
    args->use_test_funs = 0;
    args->stack_size = 64L * 1024L * 1024L;
    args->scripts = nullptr;
    args->uri_file = nullptr;
    args->uri = nullptr;

    while(i < argc) {
        if(strcmp("-h", argv[i]) == 0) {
            DISPLAY_USAGE;
            exit(0);
        } else if(strcmp("-V", argv[i]) == 0) {
            DISPLAY_VERSION;
            exit(0);
        } else if(strcmp("-H", argv[i]) == 0) {
            args->use_http = 1;
        } else if(strcmp("-T", argv[i]) == 0) {
            args->use_test_funs = 1;
        } else if(strcmp("-S", argv[i]) == 0) {
            args->stack_size = atoi(argv[++i]);
            if(args->stack_size <= 0) {
                fprintf(stderr, "Invalid stack size.\n");
                exit(2);
            }
        } else if(strcmp("-u", argv[i]) == 0) {
            args->uri_file = argv[++i];
        } else if(strcmp("--eval", argv[i]) == 0) {
            args->eval = 1;
        } else if(strcmp("--", argv[i]) == 0) {
            i++;
            break;
        } else {
            break;
        }
        i++;
    }

    if(i >= argc) {
        DISPLAY_USAGE;
        exit(3);
    }
    args->scripts = argv + i;

    return args;
}


int
couch_fgets(char* buf, int size, FILE* fp)
{
    int n, i, c;

    if(size <= 0) return -1;
    n = size - 1;

    for(i = 0; i < n && (c = getc(fp)) != EOF; i++) {
        buf[i] = c;
        if(c == '\n') {
            i++;
            break;
        }
    }

    buf[i] = '\0';
    return i;
}


JSString*
couch_readline(JSContext* cx, FILE* fp)
{
    JSString* str;
    char* bytes = NULL;
    char* tmp = NULL;
    size_t used = 0;
    size_t byteslen = 256;
    size_t oldbyteslen = 256;
    size_t readlen = 0;

    bytes = static_cast<char*>(JS_malloc(cx, byteslen));
    if(bytes == NULL) return NULL;

    while((readlen = couch_fgets(bytes+used, byteslen-used, fp)) > 0) {
        used += readlen;

        if(bytes[used-1] == '\n') {
            bytes[used-1] = '\0';
            break;
        }

        // Double our buffer and read more.
        oldbyteslen = byteslen;
        byteslen *= 2;
        tmp = static_cast<char*>(JS_realloc(cx, bytes, oldbyteslen, byteslen));
        if(!tmp) {
            JS_free(cx, bytes);
            return NULL;
        }

        bytes = tmp;
    }

    // Treat empty strings specially
    if(used == 0) {
        JS_free(cx, bytes);
        return JS_NewStringCopyZ(cx, nullptr);
    }

    // Shrink the buffer to the actual data size
    tmp = static_cast<char*>(JS_realloc(cx, bytes, byteslen, used));
    if(!tmp) {
        JS_free(cx, bytes);
        return NULL;
    }
    bytes = tmp;
    byteslen = used;

    str = string_to_js(cx, std::string(tmp));
    JS_free(cx, bytes);
    return str;
}


void
couch_print(JSContext* cx, JS::HandleValue obj, bool use_stderr)
{
    FILE *stream = stdout;

    if (use_stderr) {
        stream = stderr;
    }
    std::string val = js_to_string(cx, obj);
    fprintf(stream, "%s\n", val.c_str());
    fflush(stream);
}


void
couch_error(JSContext* cx, JSErrorReport* report)
{
    if(!report) {
        return;
    }

    if(JSREPORT_IS_WARNING(report->flags)) {
        return;
    }

    std::ostringstream msg;
    msg << "error: " << report->message().c_str();

    mozilla::Maybe<JSAutoRealm> ar;
    JS::RootedValue exc(cx);
    JS::RootedObject exc_obj(cx);
    JS::RootedObject stack_obj(cx);
    JS::RootedString stack_str(cx);
    JS::RootedValue stack_val(cx);
    JSPrincipals* principals = GetRealmPrincipals(js::GetContextRealm(cx));

    if(!JS_GetPendingException(cx, &exc)) {
        goto done;
    }

    // Clear the exception before an JS method calls or the result is
    // infinite, recursive error report generation.
    JS_ClearPendingException(cx);

    exc_obj.set(exc.toObjectOrNull());
    stack_obj.set(JS::ExceptionStackOrNull(exc_obj));

    if(!stack_obj) {
        // Compilation errors don't have a stack

        msg << " at ";

        if(report->filename) {
            msg << report->filename;
        } else {
            msg << "<unknown>";
        }

        if(report->lineno) {
            msg << ':' << report->lineno << ':' << report->column;
        }

        goto done;
    }

    if(!JS::BuildStackString(cx, principals, stack_obj, &stack_str, 2)) {
        goto done;
    }

    stack_val.set(JS::StringValue(stack_str));
    msg << std::endl << std::endl << js_to_string(cx, stack_val).c_str();

done:
    msg << std::endl;
    fprintf(stderr, "%s", msg.str().c_str());
}


void
couch_oom(JSContext* cx, void* data)
{
    fprintf(stderr, "out of memory\n");
    exit(1);
}


bool
couch_load_funcs(JSContext* cx, JS::HandleObject obj, JSFunctionSpec* funcs)
{
    JSFunctionSpec* f;
    for(f = funcs; f->name; f++) {
        if(!JS_DefineFunction(cx, obj, f->name.string(), f->call.op, f->nargs, f->flags)) {
            fprintf(stderr, "Failed to create function: %s\n", f->name.string());
            return false;
        }
    }
    return true;
}
