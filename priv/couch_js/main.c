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
#include <stdio.h>
#include <string.h>
#include <jsapi.h>
#include "config.h"

#include "utf8.h"
#include "http.h"

int gExitCode = 0;

#ifdef JS_THREADSAFE
#define SETUP_REQUEST(cx) \
    JS_SetContextThread(cx); \
    JS_BeginRequest(cx);
#define FINISH_REQUEST(cx) \
    JS_EndRequest(cx); \
    JS_ClearContextThread(cx);
#else
#define SETUP_REQUEST(cx)
#define FINISH_REQUEST(cx)
#endif

static JSBool
evalcx(JSContext *cx, JSObject *obj, uintN argc, jsval *argv, jsval *rval)
{
    JSString *str;
    JSObject *sandbox;
    JSContext *subcx;
    const jschar *src;
    size_t srclen;
    JSBool ret = JS_FALSE;
    jsval v;

    sandbox = NULL;
    if(!JS_ConvertArguments(cx, argc, argv, "S / o", &str, &sandbox))
    {
        return JS_FALSE;
    }

    subcx = JS_NewContext(JS_GetRuntime(cx), 8L * 1024L);
    if(!subcx)
    {
        JS_ReportOutOfMemory(cx);
        return JS_FALSE;
    }

    SETUP_REQUEST(subcx);

    src = JS_GetStringChars(str);
    srclen = JS_GetStringLength(str);

    if(!sandbox)
    {
        sandbox = JS_NewObject(subcx, NULL, NULL, NULL);
        if(!sandbox || !JS_InitStandardClasses(subcx, sandbox)) goto done;
    }

    if(srclen == 0)
    {
        *rval = OBJECT_TO_JSVAL(sandbox);
    }
    else
    {
        JS_EvaluateUCScript(subcx, sandbox, src, srclen, NULL, 0, rval);
    }
    
    ret = JS_TRUE;

done:
    FINISH_REQUEST(subcx);
    JS_DestroyContext(subcx);
    return ret;
}

static JSBool
gc(JSContext *cx, JSObject *obj, uintN argc, jsval *argv, jsval *rval)
{
    JS_GC(cx);
    return JS_TRUE;
}

static JSBool
print(JSContext *cx, JSObject *obj, uintN argc, jsval *argv, jsval *rval)
{
    uintN i;
    char *bytes;

    for(i = 0; i < argc; i++)
    {
        bytes = enc_string(cx, argv[i], NULL);
        if(!bytes) return JS_FALSE;

        fprintf(stdout, "%s%s", i ? " " : "", bytes);
        JS_free(cx, bytes);
    }

    fputc('\n', stdout);
    fflush(stdout);
    return JS_TRUE;
}

static JSBool
quit(JSContext *cx, JSObject *obj, uintN argc, jsval *argv, jsval *rval)
{
    JS_ConvertArguments(cx, argc, argv, "/ i", &gExitCode);
    return JS_FALSE;
}

static char*
readfp(JSContext* cx, FILE* fp, size_t* buflen)
{
    char* bytes = NULL;
    char* tmp = NULL;
    size_t used = 0;
    size_t byteslen = 256;
    size_t readlen = 0;

    bytes = JS_malloc(cx, byteslen);
    if(bytes == NULL) return NULL;
    
    while((readlen = js_fgets(bytes+used, byteslen-used, stdin)) > 0)
    {
        used += readlen;

        if(bytes[used-1] == '\n')
        {
            bytes[used-1] = '\0';
            break;
        }

        // Double our buffer and read more.
        byteslen *= 2;
        tmp = JS_realloc(cx, bytes, byteslen);
        if(!tmp)
        {
            JS_free(cx, bytes);
            return NULL;
        }
        bytes = tmp;
    }

    *buflen = used;
    return bytes;
}

static JSBool
readline(JSContext *cx, JSObject *obj, uintN argc, jsval *argv, jsval *rval) {
    jschar *chars;
    JSString *str;
    char* bytes;
    char* tmp;
    size_t byteslen;

    /* GC Occasionally */
    JS_MaybeGC(cx);

    bytes = readfp(cx, stdin, &byteslen);
    if(!bytes) return JS_FALSE;
    
    /* Treat the empty string specially */
    if(byteslen == 0)
    {
        *rval = JS_GetEmptyStringValue(cx);
        JS_free(cx, bytes);
        return JS_TRUE;
    }

    /* Shrink the buffer to the real size */
    tmp = JS_realloc(cx, bytes, byteslen);
    if(!tmp)
    {
        JS_free(cx, bytes);
        return JS_FALSE;
    }
    bytes = tmp;
    
    str = dec_string(cx, bytes, byteslen);
    JS_free(cx, bytes);

    if(!str) return JS_FALSE;

    *rval = STRING_TO_JSVAL(str);

    return JS_TRUE;
}

static JSBool
seal(JSContext *cx, JSObject *obj, uintN argc, jsval *argv, jsval *rval) {
    JSObject *target;
    JSBool deep = JS_FALSE;

    if (!JS_ConvertArguments(cx, argc, argv, "o/b", &target, &deep))
        return JS_FALSE;
    if (!target)
        return JS_TRUE;
    return JS_SealObject(cx, target, deep);
}

static void
execute_script(JSContext *cx, JSObject *obj, const char *filename) {
    FILE *file;
    JSScript *script;
    jsval result;

    if(!filename || strcmp(filename, "-") == 0)
    {
        file = stdin;
    }
    else
    {
        file = fopen(filename, "r");
        if (!file)
        {
            fprintf(stderr, "could not open script file %s\n", filename);
            gExitCode = 1;
            return;
        }
    }

    script = JS_CompileFileHandle(cx, obj, filename, file);
    if(script)
    {
        JS_ExecuteScript(cx, obj, script, &result);
        JS_DestroyScript(cx, script);
    }
}

static void
printerror(JSContext *cx, const char *mesg, JSErrorReport *report)
{
    if(!report || !JSREPORT_IS_WARNING(report->flags))
    {
        fprintf(stderr, "%s\n", mesg);
    }
}

static JSFunctionSpec global_functions[] = {
    {"evalcx", evalcx, 0, 0, 0},
    {"gc", gc, 0, 0, 0},
    {"print", print, 0, 0, 0},
    {"quit", quit, 0, 0, 0},
    {"readline", readline, 0, 0, 0},
    {"seal", seal, 0, 0, 0},
    {0, 0, 0, 0, 0}
};

static JSClass global_class = {
    "GlobalClass",
    JSCLASS_GLOBAL_FLAGS,
    JS_PropertyStub,
    JS_PropertyStub,
    JS_PropertyStub,
    JS_PropertyStub,
    JS_EnumerateStub,
    JS_ResolveStub,
    JS_ConvertStub,
    JS_FinalizeStub,
    JSCLASS_NO_OPTIONAL_MEMBERS
};

int
usage()
{
    fprintf(stderr, "usage: couchjs [-H] [script_name]\n");
    return 1;
}

int
main(int argc, const char * argv[])
{
    JSRuntime* rt = NULL;
    JSContext* cx = NULL;
    JSObject* global = NULL;
    JSFunctionSpec* sp = NULL;
    const char* script_name = NULL;
    int use_http = 0;
    int i = 0;

    if(argc > 3)
    {
        fprintf(stderr, "ERROR: Too many arguments.\n");
        return usage();
    }
    else if(argc == 3)
    {
        if(strcmp(argv[1], "-H"))
        {
            fprintf(stderr, "ERROR: Invalid option: %s\n", argv[1]);
            return usage();
        }
        use_http = 1;
        script_name = argv[2];
    }
    else if(argc == 2 && strcmp(argv[1], "-H") == 0)
    {
        use_http = 1;
    }
    else if (argc == 2)
    {
        script_name = argv[1];
    }
    // else argc == 1, use defaults

    rt = JS_NewRuntime(64L * 1024L * 1024L);
    if (!rt) return 1;

    cx = JS_NewContext(rt, 8L * 1024L);
    if (!cx) return 1;

    JS_SetErrorReporter(cx, printerror);
    JS_ToggleOptions(cx, JSOPTION_XML);
    
    SETUP_REQUEST(cx);

    global = JS_NewObject(cx, &global_class, NULL, NULL);
    if (!global) return 1;
    if (!JS_InitStandardClasses(cx, global)) return 1;
    
    for(sp = global_functions; sp->name != NULL; sp++)
    {
        if(!JS_DefineFunction(cx, global,
               sp->name, sp->call, sp->nargs, sp->flags))
        {
            fprintf(stderr, "Failed to create function: %s\n", sp->name);
            return 1;
        }
    }

    if(use_http && !install_http(cx, global))
    {
        return 2;
    }

    JS_SetGlobalObject(cx, global);
    execute_script(cx, global, script_name);

    FINISH_REQUEST(cx);

    JS_DestroyContext(cx);
    JS_DestroyRuntime(rt);
    JS_ShutDown();

    return gExitCode;
}
