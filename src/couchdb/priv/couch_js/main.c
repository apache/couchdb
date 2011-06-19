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
#include "jscompat.h"

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


static JSClass global_class = {
    "GlobalClass",
    JSCLASS_GLOBAL_FLAGS,
    JS_PropertyStub,
    JS_PropertyStub,
    JS_PropertyStub,
    JS_SETPROPERTY_PROPERTY_STUB,
    JS_EnumerateStub,
    JS_ResolveStub,
    JS_ConvertStub,
    JS_FinalizeStub,
    JSCLASS_NO_OPTIONAL_MEMBERS
};

static JSBool
evalcx(JSContext* cx, uintN argc, jsval* vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    JSString *str;
    JSObject *sandbox;
    JSContext *subcx;
    const jschar *src;
    size_t srclen;
    JSBool ret = JS_FALSE;
    jsval v;
#ifdef HAVE_COMPARTMENTS
    JSCrossCompartmentCall *call = NULL;
#endif

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

#ifdef HAVE_JS_GET_STRING_CHARS_AND_LENGTH
    src = JS_GetStringCharsAndLength(cx, str, &srclen);
#else
    src = JS_GetStringChars(str);
    srclen = JS_GetStringLength(str);
#endif

#ifdef HAVE_COMPARTMENTS
    // Re-use the compartment associated with the main context,
    // rather than creating a new compartment */
    JSObject *global = JS_GetGlobalObject(cx);
    if(!global)
    {
       goto done;
    }
    call = JS_EnterCrossCompartmentCall(subcx, global);
#endif

    if(!sandbox)
    {
#ifdef HAVE_JS_NEW_GLOBAL_OBJECT
        sandbox = JS_NewGlobalObject(subcx, &global_class);
#else
        sandbox = JS_NewObject(subcx, NULL, NULL, NULL);
#endif
        if(!sandbox || !JS_InitStandardClasses(subcx, sandbox)) goto done;
    }

    if(srclen == 0)
    {
        JS_SET_RVAL(cx, vp, OBJECT_TO_JSVAL(sandbox));
    }
    else
    {
        JS_EvaluateUCScript(subcx, sandbox, src, srclen, NULL, 0, &JS_RVAL(cx, vp));
    }
    
    ret = JS_TRUE;

done:
#ifdef HAVE_COMPARTMENTS
    if(call)
    {
        JS_LeaveCrossCompartmentCall(call);
    }
#endif
    FINISH_REQUEST(subcx);
    JS_DestroyContext(subcx);
    return ret;
}

static JSBool
gc(JSContext* cx, uintN argc, jsval* vp)
{
    JS_GC(cx);
    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return JS_TRUE;
}

static JSBool
print(JSContext *cx, uintN argc, jsval *vp)
{
    jsval *argv = JS_ARGV(cx, vp);
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
    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return JS_TRUE;
}

static JSBool
quit(JSContext *cx, uintN argc, jsval *vp)
{
    jsval *argv = JS_ARGV(cx, vp);
    JS_ConvertArguments(cx, argc, argv, "/ i", &gExitCode);
    return JS_FALSE;
}

static int
couchjs_fgets(char *buf, int size, FILE *file)
{
    int n, i, c;
    JSBool crflag;

    n = size - 1;
    if (n < 0)
        return -1;

    crflag = JS_FALSE;
    for (i = 0; i < n && (c = getc(file)) != EOF; i++)
    {
        buf[i] = c;
        if (c == '\n') {        /* any \n ends a line */
            i++;                /* keep the \n; we know there is room for \0 */
            break;
        }
        if (crflag) {           /* \r not followed by \n ends line at the \r */
            ungetc(c, file);
            break;              /* and overwrite c in buf with \0 */
        }
        crflag = (c == '\r');
    }

    buf[i] = '\0';
    return i;
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
    
    while((readlen = couchjs_fgets(bytes+used, byteslen-used, stdin)) > 0)
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
readline(JSContext *cx, uintN argc, jsval *vp) {
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
        JS_SET_RVAL(cx, vp, JS_GetEmptyStringValue(cx));
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

    JS_SET_RVAL(cx, vp, STRING_TO_JSVAL(str));

    return JS_TRUE;
}

static JSBool
seal(JSContext *cx, uintN argc, jsval *vp) {
    jsval *argv = JS_ARGV(cx, vp);
    JSObject *target;
    JSBool deep = JS_FALSE;
    JSBool ret;

    if (!JS_ConvertArguments(cx, argc, argv, "o/b", &target, &deep))
        return JS_FALSE;
    if (!target)
    {
        JS_SET_RVAL(cx, vp, JSVAL_VOID);
        return JS_TRUE;
    }

    COUCHJS_SEAL_OBJECT(ret, cx, target, deep);
    if (ret == JS_TRUE)
        JS_SET_RVAL(cx, vp, JSVAL_VOID);

    return ret;
}

static void
execute_script(JSContext *cx, JSObject *obj, const char *filename) {
    FILE *file;
    COUCHJS_SCRIPT *script;
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
        COUCHJS_DESTROY_SCRIPT(cx, script);
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
    JS_FS("evalcx", COUCHJS_NATIVE_FUNC(evalcx), 0, JSFUN_FAST_NATIVE),
    JS_FS("gc", COUCHJS_NATIVE_FUNC(gc), 0, JSFUN_FAST_NATIVE),
    JS_FS("print", COUCHJS_NATIVE_FUNC(print), 0, JSFUN_FAST_NATIVE),
    JS_FS("quit", COUCHJS_NATIVE_FUNC(quit), 0, JSFUN_FAST_NATIVE),
    JS_FS("readline", COUCHJS_NATIVE_FUNC(readline), 0, JSFUN_FAST_NATIVE),
    JS_FS("seal", COUCHJS_NATIVE_FUNC(seal), 0, JSFUN_FAST_NATIVE),
    JS_FS_END
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
#ifdef HAVE_COMPARTMENTS
    JSCrossCompartmentCall *call = NULL;
#endif
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

#ifdef HAVE_COMPARTMENTS
    global = JS_NewCompartmentAndGlobalObject(cx, &global_class, NULL);
    if (!global) return 1;
    call = JS_EnterCrossCompartmentCall(cx, global);
#elif HAVE_JS_NEW_GLOBAL_OBJECT
    global = JS_NewGlobalObject(cx, &global_class);
    if (!global) return 1;
#else
    global = JS_NewObject(cx, &global_class, NULL, NULL);
    if (!global) return 1;
    JS_SetGlobalObject(cx, global);
#endif
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

    execute_script(cx, global, script_name);

#ifdef HAVE_COMPARTMENTS
    JS_LeaveCrossCompartmentCall(call);
#endif
    FINISH_REQUEST(cx);

    JS_DestroyContext(cx);
    JS_DestroyRuntime(rt);
    JS_ShutDown();

    return gExitCode;
}
