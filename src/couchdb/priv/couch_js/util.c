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

#include "help.h"
#include "util.h"
#include "utf8.h"


char*
slurp_file(char* buf, const char* file)
{
    FILE* fp;
    char fbuf[16384];
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
            buflen = nread;
            buf = (char*) malloc(nread + 1);
            if(buf == NULL) {
                fprintf(stderr, "Out of memory.\n");
                exit(3);
            }
            memcpy(buf, fbuf, buflen);
            buf[buflen] = '\0';
        } else {
            buflen = strlen(buf);
            tmp = (char*) malloc(buflen + nread + 1);
            if(tmp == NULL) {
                fprintf(stderr, "Out of memory.\n");
                exit(3);
            }
            memcpy(tmp, buf, buflen);
            memcpy(tmp+buflen, fbuf, nread);
            tmp[buflen+nread] = '\0';
            free(buf);
            buf = tmp;
        }
    }
    return buf;
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
    args->stack_size = 8L * 1024L;

    while(i < argc) {
        if(strcmp("-h", argv[i]) == 0) {
            DISPLAY_USAGE;
            exit(0);
        } else if(strcmp("-V", argv[i]) == 0) {
            DISPLAY_VERSION;
            exit(0);
        } else if(strcmp("-H", argv[i]) == 0) {
            args->use_http = 1;
        } else if(strcmp("-S", argv[i]) == 0) {
            args->stack_size = atoi(argv[++i]);
            if(args->stack_size <= 0) {
                fprintf(stderr, "Invalid stack size.\n");
                exit(2);
            }
        } else if(strcmp("--", argv[i]) == 0) {
            i++;
            break;
        } else {
            break;
        }
        i++;
    }

    while(i < argc) {
        args->script = slurp_file(args->script, argv[i]);
        if(args->script_name == NULL) {
            if(strcmp(argv[i], "-") == 0) {
                args->script_name = "<stdin>";
            } else {
                args->script_name = argv[i];
            }
        } else {
            args->script_name = "<multiple_files>";
        }
        i++;
    }

    if(args->script_name == NULL || args->script == NULL) {
        DISPLAY_USAGE;
        exit(3);
    }

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
    size_t readlen = 0;

    bytes = JS_malloc(cx, byteslen);
    if(bytes == NULL) return NULL;
    
    while((readlen = couch_fgets(bytes+used, byteslen-used, fp)) > 0) {
        used += readlen;
        
        if(bytes[used-1] == '\n') {
            bytes[used-1] = '\0';
            break;
        }
        
        // Double our buffer and read more.
        byteslen *= 2;
        tmp = JS_realloc(cx, bytes, byteslen);
        if(!tmp) {
            JS_free(cx, bytes);
            return NULL;
        }
        
        bytes = tmp;
    }

    // Treat empty strings specially
    if(used == 0) {
        JS_free(cx, bytes);
        return JSVAL_TO_STRING(JS_GetEmptyStringValue(cx));
    }

    // Shring the buffer to the actual data size
    tmp = JS_realloc(cx, bytes, used);
    if(!tmp) {
        JS_free(cx, bytes);
        return NULL;
    }
    bytes = tmp;
    byteslen = used;

    str = dec_string(cx, bytes, byteslen);
    JS_free(cx, bytes);
    return str;
}


JSObject*
couch_readfile(JSContext* cx, FILE* fp)
{
    return NULL;    
}


void
couch_print(JSContext* cx, uintN argc, jsval* argv)
{
    char *bytes;
    uintN i;

    for(i = 0; i < argc; i++)
    {
        bytes = enc_string(cx, argv[i], NULL);
        if(!bytes) return;

        fprintf(stdout, "%s%s", i ? " " : "", bytes);
        JS_free(cx, bytes);
    }

    fputc('\n', stdout);
    fflush(stdout);
}


void
couch_error(JSContext* cx, const char* mesg, JSErrorReport* report)
{
    if(!report || !JSREPORT_IS_WARNING(report->flags))
    {
        fprintf(stderr, "[couchjs] %s\n", mesg);
    }
}


JSBool
couch_load_funcs(JSContext* cx, JSObject* obj, JSFunctionSpec* funcs)
{
    JSFunctionSpec* f;
    for(f = funcs; f->name != NULL; f++) {
        if(!JS_DefineFunction(cx, obj, f->name, f->call, f->nargs, f->flags)) {
            fprintf(stderr, "Failed to create function: %s\n", f->name);
            return JS_FALSE;
        }
    }
    return JS_TRUE;
}

