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

#ifdef XP_WIN
#include <windows.h>
#else
#include <unistd.h>
#endif

#include <jsapi.h>
#include "config.h"
#include "http.h"
#include "utf8.h"
#include "util.h"


#define SETUP_REQUEST(cx) \
    JS_SetContextThread(cx); \
    JS_BeginRequest(cx);
#define FINISH_REQUEST(cx) \
    JS_EndRequest(cx); \
    JS_ClearContextThread(cx);


static JSClass global_class = {
    "GlobalClass",
    JSCLASS_GLOBAL_FLAGS,
    JS_PropertyStub,
    JS_PropertyStub,
    JS_PropertyStub,
    JS_StrictPropertyStub,
    JS_EnumerateStub,
    JS_ResolveStub,
    JS_ConvertStub,
    JS_FinalizeStub,
    JSCLASS_NO_OPTIONAL_MEMBERS
};


static JSBool
req_ctor(JSContext* cx, uintN argc, jsval* vp)
{
    JSBool ret;
    JSObject* obj = JS_NewObjectForConstructor(cx, vp);
    if(!obj) {
        JS_ReportError(cx, "Failed to create CouchHTTP instance.\n");
        return JS_FALSE;
    }
    ret = http_ctor(cx, obj);
    JS_SET_RVAL(cx, vp, OBJECT_TO_JSVAL(obj));
    return ret;
}


static void 
req_dtor(JSContext* cx, JSObject* obj)
{
    http_dtor(cx, obj);
}


static JSBool
req_open(JSContext* cx, uintN argc, jsval* vp)
{
    JSObject* obj = JS_THIS_OBJECT(cx, vp);
    jsval* argv = JS_ARGV(cx, vp);
    JSBool ret = JS_FALSE;

    if(argc == 2) {
        ret = http_open(cx, obj, argv[0], argv[1], JSVAL_FALSE);
    } else if(argc == 3) {
        ret = http_open(cx, obj, argv[0], argv[1], argv[2]);
    } else {
        JS_ReportError(cx, "Invalid call to CouchHTTP.open");
    }

    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return ret;
}


static JSBool
req_set_hdr(JSContext* cx, uintN argc, jsval* vp)
{
    JSObject* obj = JS_THIS_OBJECT(cx, vp);
    jsval* argv = JS_ARGV(cx, vp);
    JSBool ret = JS_FALSE;

    if(argc == 2) {
        ret = http_set_hdr(cx, obj, argv[0], argv[1]);
    } else {
        JS_ReportError(cx, "Invalid call to CouchHTTP.set_header");
    }

    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return ret;
}


static JSBool
req_send(JSContext* cx, uintN argc, jsval* vp)
{
    JSObject* obj = JS_THIS_OBJECT(cx, vp);
    jsval* argv = JS_ARGV(cx, vp);
    JSBool ret = JS_FALSE;

    if(argc == 1) {
        ret = http_send(cx, obj, argv[0]);
    } else {
        JS_ReportError(cx, "Invalid call to CouchHTTP.send");
    }

    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return ret;
}


static JSBool
req_status(JSContext* cx, JSObject* obj, jsid pid, jsval* vp)
{
    int status = http_status(cx, obj);
    if(status < 0)
        return JS_FALSE;

    JS_SET_RVAL(cx, vp, INT_TO_JSVAL(status));
    return JS_TRUE;
}


static JSBool
base_url(JSContext *cx, JSObject* obj, jsid pid, jsval* vp)
{
    couch_args *args = (couch_args*)JS_GetContextPrivate(cx);
    return http_uri(cx, obj, args, &JS_RVAL(cx, vp));
}


static JSBool
evalcx(JSContext *cx, uintN argc, jsval* vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    JSString* str;
    JSObject* sandbox;
    JSObject* global;
    JSContext* subcx;
    JSCrossCompartmentCall* call = NULL;
    const jschar* src;
    size_t srclen;
    jsval rval;
    JSBool ret = JS_FALSE;
    char *name = NULL;

    sandbox = NULL;
    if(!JS_ConvertArguments(cx, argc, argv, "S / o", &str, &sandbox)) {
        return JS_FALSE;
    }

    subcx = JS_NewContext(JS_GetRuntime(cx), 8L * 1024L);
    if(!subcx) {
        JS_ReportOutOfMemory(cx);
        return JS_FALSE;
    }

    SETUP_REQUEST(subcx);

    src = JS_GetStringCharsAndLength(cx, str, &srclen);

    // Re-use the compartment associated with the main context,
    // rather than creating a new compartment */
    global = JS_GetGlobalObject(cx);
    if(global == NULL) goto done;
    call = JS_EnterCrossCompartmentCall(subcx, global);

    if(!sandbox) {
        sandbox = JS_NewGlobalObject(subcx, &global_class);
        if(!sandbox || !JS_InitStandardClasses(subcx, sandbox)) {
            goto done;
        }
    }

    if(argc > 2) {
        name = enc_string(cx, argv[2], NULL);
    }

    if(srclen == 0) {
        JS_SET_RVAL(cx, vp, OBJECT_TO_JSVAL(sandbox));
    } else {
        JS_EvaluateUCScript(subcx, sandbox, src, srclen, name, 1, &rval);
        JS_SET_RVAL(cx, vp, rval);
    }
    
    ret = JS_TRUE;

done:
    if(name) JS_free(cx, name);
    JS_LeaveCrossCompartmentCall(call);
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
print(JSContext* cx, uintN argc, jsval* vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    couch_print(cx, argc, argv);
    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return JS_TRUE;
}


static JSBool
quit(JSContext* cx, uintN argc, jsval* vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    int exit_code = 0;
    JS_ConvertArguments(cx, argc, argv, "/i", &exit_code);
    exit(exit_code);
}


static JSBool
readline(JSContext* cx, uintN argc, jsval* vp)
{
    JSString* line;

    /* GC Occasionally */
    JS_MaybeGC(cx);

    line = couch_readline(cx, stdin);
    if(line == NULL) return JS_FALSE;

    JS_SET_RVAL(cx, vp, STRING_TO_JSVAL(line));
    return JS_TRUE;
}


static JSBool
seal(JSContext* cx, uintN argc, jsval* vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    JSObject *target;
    JSBool deep = JS_FALSE;
    JSBool ret;

    if(!JS_ConvertArguments(cx, argc, argv, "o/b", &target, &deep))
        return JS_FALSE;

    if(!target) {
        JS_SET_RVAL(cx, vp, JSVAL_VOID);
        return JS_TRUE;
    }

    
    ret = deep ? JS_DeepFreezeObject(cx, target) : JS_FreezeObject(cx, target);
    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return ret;
}


static JSBool
js_sleep(JSContext* cx, uintN argc, jsval* vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    int duration = 0;
    if(!JS_ConvertArguments(cx, argc, argv, "/i", &duration)) {
        return JS_FALSE;
    }

#ifdef XP_WIN
    Sleep(duration);
#else
    usleep(duration * 1000);
#endif

    return JS_TRUE;
}


JSClass CouchHTTPClass = {
    "CouchHTTP",
    JSCLASS_HAS_PRIVATE
        | JSCLASS_CONSTRUCT_PROTOTYPE
        | JSCLASS_HAS_RESERVED_SLOTS(2),
    JS_PropertyStub,
    JS_PropertyStub,
    JS_PropertyStub,
    JS_StrictPropertyStub,
    JS_EnumerateStub,
    JS_ResolveStub,
    JS_ConvertStub,
    req_dtor,
    JSCLASS_NO_OPTIONAL_MEMBERS
};


JSPropertySpec CouchHTTPProperties[] = {
    {"status", 0, JSPROP_READONLY, req_status, NULL},
    {"base_url", 0, JSPROP_READONLY | JSPROP_SHARED, base_url, NULL},
    {0, 0, 0, 0, 0}
};


JSFunctionSpec CouchHTTPFunctions[] = {
    JS_FS("_open", req_open, 3, 0),
    JS_FS("_setRequestHeader", req_set_hdr, 2, 0),
    JS_FS("_send", req_send, 1, 0),
    JS_FS_END
};


JSFunctionSpec TestSuiteFunctions[] = {
    JS_FS("sleep", js_sleep, 1, 0),
    JS_FS_END
};


static JSFunctionSpec global_functions[] = {
    JS_FS("evalcx", evalcx, 0, 0),
    JS_FS("gc", gc, 0, 0),
    JS_FS("print", print, 0, 0),
    JS_FS("quit", quit, 0, 0),
    JS_FS("readline", readline, 0, 0),
    JS_FS("seal", seal, 0, 0),
    JS_FS_END
};


static JSBool
csp_allows(JSContext* cx)
{
    couch_args *args = (couch_args*)JS_GetContextPrivate(cx);
    if(args->no_eval) {
        return JS_FALSE;
    } else {
        return JS_TRUE;
    }
}


static JSSecurityCallbacks security_callbacks = {
    NULL,
    NULL,
    NULL,
    csp_allows
};


int
main(int argc, const char* argv[])
{
    JSRuntime* rt = NULL;
    JSContext* cx = NULL;
    JSObject* global = NULL;
    JSCrossCompartmentCall *call = NULL;
    JSObject* klass = NULL;
    JSSCRIPT_TYPE script;
    JSString* scriptsrc;
    const jschar* schars;
    size_t slen;
    jsval sroot;
    jsval result;
    int i;

    couch_args* args = couch_parse_args(argc, argv);

    rt = JS_NewRuntime(64L * 1024L * 1024L);
    if(rt == NULL)
        return 1;

    cx = JS_NewContext(rt, args->stack_size);
    if(cx == NULL)
        return 1;

    JS_SetErrorReporter(cx, couch_error);
    JS_ToggleOptions(cx, JSOPTION_XML);
    JS_SetOptions(cx, JSOPTION_METHODJIT);
#ifdef JSOPTION_TYPE_INFERENCE
    JS_SetOptions(cx, JSOPTION_TYPE_INFERENCE);
#endif
    JS_SetContextPrivate(cx, args);
    JS_SetRuntimeSecurityCallbacks(rt, &security_callbacks);

    SETUP_REQUEST(cx);

    global = JS_NewCompartmentAndGlobalObject(cx, &global_class, NULL);
    if(global == NULL)
        return 1;

    call = JS_EnterCrossCompartmentCall(cx, global);

    JS_SetGlobalObject(cx, global);
    
    if(!JS_InitStandardClasses(cx, global))
        return 1;

    if(couch_load_funcs(cx, global, global_functions) != JS_TRUE)
        return 1;
 
    if(args->use_http) {
        http_check_enabled();

        klass = JS_InitClass(
            cx, global,
            NULL,
            &CouchHTTPClass, req_ctor,
            0,
            CouchHTTPProperties, CouchHTTPFunctions,
            NULL, NULL
        );

        if(!klass)
        {
            fprintf(stderr, "Failed to initialize CouchHTTP class.\n");
            exit(2);
        }
    } 

    if(args->use_test_funs) {
        if(couch_load_funcs(cx, global, TestSuiteFunctions) != JS_TRUE)
            return 1;
    }

    for(i = 0 ; args->scripts[i] ; i++) {
        // Convert script source to jschars.
        scriptsrc = couch_readfile(cx, args->scripts[i]);
        if(!scriptsrc)
            return 1;

        schars = JS_GetStringCharsAndLength(cx, scriptsrc, &slen);

        // Root it so GC doesn't collect it.
        sroot = STRING_TO_JSVAL(scriptsrc);
        if(JS_AddValueRoot(cx, &sroot) != JS_TRUE) {
            fprintf(stderr, "Internal root error.\n");
            return 1;
        }

        // Compile and run
        script = JS_CompileUCScript(cx, global, schars, slen,
                                    args->scripts[i], 1);
        if(!script) {
            fprintf(stderr, "Failed to compile script.\n");
            return 1;
        }

        if(JS_ExecuteScript(cx, global, script, &result) != JS_TRUE) {
            fprintf(stderr, "Failed to execute script.\n");
            return 1;
        }

        // Warning message if we don't remove it.
        JS_RemoveValueRoot(cx, &sroot);

        // Give the GC a chance to run.
        JS_MaybeGC(cx);
    }

    JS_LeaveCrossCompartmentCall(call);
    FINISH_REQUEST(cx);
    JS_DestroyContext(cx);
    JS_DestroyRuntime(rt);
    JS_ShutDown();

    return 0;
}
