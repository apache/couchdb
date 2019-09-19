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

#include <jsapi.h>
#include <js/Initialization.h>
#include <js/Conversions.h>

#include "help.h"
#include "util.h"
#include "utf8.h"

std::string
js_to_string(JSContext* cx, JS::HandleValue val)
{
    JS::RootedString sval(cx);
    sval = val.toString();

    JS::UniqueChars chars(JS_EncodeStringToUTF8(cx, sval));
    if(!chars) {
        JS_ClearPendingException(cx);
        fprintf(stderr, "Error converting value to string.\n");
        exit(3);
    }

    return chars.get();
}

std::string
js_to_string(JSContext* cx, JSString *str)
{
    JS::UniqueChars chars(JS_EncodeString(cx, str));
    if(!chars) {
        JS_ClearPendingException(cx);
        fprintf(stderr, "Error converting  to string.\n");
        exit(3);
    }

    return chars.get();
}

JSString*
string_to_js(JSContext* cx, const std::string& s)
{
    JSString* ret = JS_NewStringCopyN(cx, s.c_str(), s.size());
    if(ret != nullptr) {
        return ret;
    }

    fprintf(stderr, "Unable to allocate string object.\n");
    exit(3);
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
            buf = (char*) malloc(nread + 1);
            if(buf == NULL) {
                fprintf(stderr, "Out of memory.\n");
                exit(3);
            }
            memcpy(buf, fbuf, nread);
        } else {
            tmp = (char*) malloc(buflen + nread + 1);
            if(tmp == NULL) {
                fprintf(stderr, "Out of memory.\n");
                exit(3);
            }
            memcpy(tmp, buf, buflen);
            memcpy(tmp+buflen, fbuf, nread);
            free(buf);
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

    args = (couch_args*) malloc(sizeof(couch_args));
    if(args == NULL)
        return NULL;

    memset(args, '\0', sizeof(couch_args));
    args->stack_size = 64L * 1024L * 1024L;

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

    bytes = (char *)JS_malloc(cx, byteslen);
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
        tmp = (char *)JS_realloc(cx, bytes, oldbyteslen, byteslen);
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

    // Shring the buffer to the actual data size
    tmp = (char *)JS_realloc(cx, bytes, byteslen, used);
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
couch_print(JSContext* cx, unsigned int argc, JS::CallArgs argv)
{
    uint8_t* bytes = nullptr;
    FILE *stream = stdout;

    if (argc) {
        if (argc > 1 && argv[1].isTrue()) {
          stream = stderr;
        }
        JSString* str = JS::ToString(cx, argv.get(0));
        bytes = reinterpret_cast<uint8_t*>(JS_EncodeString(cx, str));
        fprintf(stream, "%s", bytes);
        JS_free(cx, bytes);
    }

    fputc('\n', stream);
    fflush(stream);
}


void
couch_error(JSContext* cx, JSErrorReport* report)
{
    JS::RootedValue v(cx), stack(cx), replace(cx);
    char* bytes;
    JSObject* regexp;

    if(!report || !JSREPORT_IS_WARNING(report->flags))
    {
        fprintf(stderr, "%s\n", report->message().c_str());

        // Print a stack trace, if available.
        if (JSREPORT_IS_EXCEPTION(report->flags) &&
            JS_GetPendingException(cx, &v))
        {
            // Clear the exception before an JS method calls or the result is
            // infinite, recursive error report generation.
            JS_ClearPendingException(cx);

            // Use JS regexp to indent the stack trace.
            // If the regexp can't be created, don't JS_ReportErrorUTF8 since it is
            // probably not productive to wind up here again.
            JS::RootedObject vobj(cx, v.toObjectOrNull());

            if(JS_GetProperty(cx, vobj, "stack", &stack) &&
               (regexp = JS_NewRegExpObject(
                   cx, "^(?=.)", 6, JSREG_GLOB | JSREG_MULTILINE)))
            {
                // Set up the arguments to ``String.replace()``
                JS::AutoValueVector re_args(cx);
                JS::RootedValue arg0(cx, JS::ObjectValue(*regexp));
                auto arg1 = JS::StringValue(string_to_js(cx, "\t"));

                if (re_args.append(arg0) && re_args.append(arg1)) {
                    // Perform the replacement
                    JS::RootedObject sobj(cx, stack.toObjectOrNull());
                    if(JS_GetProperty(cx, sobj, "replace", &replace) &&
                       JS_CallFunctionValue(cx, sobj, replace, re_args, &v))
                    {
                        // Print the result
                        bytes = enc_string(cx, v, NULL);
                        fprintf(stderr, "Stacktrace:\n%s", bytes);
                        JS_free(cx, bytes);
                    }
                }
            }
        }
    }
}


bool
couch_load_funcs(JSContext* cx, JS::HandleObject obj, JSFunctionSpec* funcs)
{
    JSFunctionSpec* f;
    for(f = funcs; f->name != NULL; f++) {
        if(!JS_DefineFunction(cx, obj, f->name, f->call.op, f->nargs, f->flags)) {
            fprintf(stderr, "Failed to create function: %s\n", f->name);
            return false;
        }
    }
    return true;
}
