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
#include <js/Initialization.h>

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

static JSClassOps global_ops = {
    nullptr,
    nullptr,
    nullptr,
    nullptr,
    nullptr,
    nullptr,
    nullptr,
    nullptr,
    nullptr,
    nullptr,
    nullptr,
    JS_GlobalObjectTraceHook
};

/* The class of the global object. */
static JSClass global_class = {
    "global",
    JSCLASS_GLOBAL_FLAGS,
    &global_ops
};

static JSClass CouchHTTPClass = {
    "CouchHTTP",  /* name */
    0,        /* flags */
    nullptr,
    nullptr,
    nullptr,
    JSCLASS_NO_OPTIONAL_MEMBERS
};

static bool
req_ctor(JSContext* cx, unsigned int argc, JS::Value* vp)
{
    bool ret;
    JSObject* obj = JS_NewObjectForConstructor(cx, CouchHTTPClass, vp);
    if(!obj) {
        JS_ReportError(cx, "Failed to create CouchHTTP instance.\n", NULL);
        return false;
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


static bool
req_open(JSContext* cx, unsigned int argc, JS::Value* vp)
{
    JSObject* obj = JS_THIS_OBJECT(cx, vp);
    JS::Value* argv = JS_ARGV(cx, vp);
    bool ret = false;

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


static bool
req_set_hdr(JSContext* cx, unsigned int argc, JS::Value* vp)
{
    JSObject* obj = JS_THIS_OBJECT(cx, vp);
    JS::Value* argv = JS_ARGV(cx, vp);
    bool ret = false;

    if(argc == 2) {
        ret = http_set_hdr(cx, obj, argv[0], argv[1]);
    } else {
        JS_ReportError(cx, "Invalid call to CouchHTTP.set_header");
    }

    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return ret;
}


static bool
req_send(JSContext* cx, unsigned int argc, JS::Value* vp)
{
    JSObject* obj = JS_THIS_OBJECT(cx, vp);
    JS::Value* argv = JS_ARGV(cx, vp);
    bool ret = false;

    if(argc == 1) {
        ret = http_send(cx, obj, argv[0]);
    } else {
        JS_ReportError(cx, "Invalid call to CouchHTTP.send");
    }

    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return ret;
}


static bool
req_status(JSContext* cx, JSObject* obj, jsid pid, JS::Value* vp)
{
    int status = http_status(cx, obj);
    if(status < 0)
        return false;

    JS_SET_RVAL(cx, vp, INT_TO_JSVAL(status));
    return true;
}


static bool
base_url(JSContext *cx, JSObject* obj, jsid pid, JS::Value* vp)
{
    couch_args *args = (couch_args*)JS_GetContextPrivate(cx);
    return http_uri(cx, obj, args, &JS_RVAL(cx, vp));
}


static bool
evalcx(JSContext *cx, unsigned int argc, JS::Value* vp)
{
    JS::Value* argv = JS_ARGV(cx, vp);
    JSString* str;
    JSObject* sandbox;
    JSObject* global;
    JSContext* subcx;
    JSCrossCompartmentCall* call = NULL;
    const jschar* src;
    size_t srclen;
    JS::Value rval;
    bool ret = false;
    char *name = NULL;

    sandbox = NULL;
    if(!JS_ConvertArguments(cx, argc, argv, "S / o", &str, &sandbox)) {
        return false;
    }

    subcx = JS_NewContext(JS_GetRuntime(cx), 8L * 1024L);
    if(!subcx) {
        JS_ReportOutOfMemory(cx);
        return false;
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
    
    ret = true;

done:
    if(name) JS_free(cx, name);
    JS_LeaveCrossCompartmentCall(call);
    FINISH_REQUEST(subcx);
    JS_DestroyContext(subcx);
    return ret;
}


static bool
gc(JSContext* cx, unsigned int argc, JS::Value* vp)
{
    JS_GC(cx);
    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return true;
}


static bool
print(JSContext* cx, unsigned int argc, JS::Value* vp)
{
    JS::Value* argv = JS_ARGV(cx, vp);
    couch_print(cx, argc, argv);
    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return true;
}


static bool
quit(JSContext* cx, unsigned int argc, JS::Value* vp)
{
    JS::Value* argv = JS_ARGV(cx, vp);
    int exit_code = 0;
    JS_ConvertArguments(cx, argc, argv, "/i", &exit_code);
    exit(exit_code);
}


static bool
readline(JSContext* cx, unsigned int argc, JS::Value* vp)
{
    JSString* line;

    /* GC Occasionally */
    JS_MaybeGC(cx);

    line = couch_readline(cx, stdin);
    if(line == NULL) return false;

    JS_SET_RVAL(cx, vp, STRING_TO_JSVAL(line));
    return true;
}


static bool
seal(JSContext* cx, unsigned int argc, JS::Value* vp)
{
    JS::Value* argv = JS_ARGV(cx, vp);
    JSObject *target;
    bool deep = false;
    bool ret;

    if(!JS_ConvertArguments(cx, argc, argv, "o/b", &target, &deep))
        return false;

    if(!target) {
        JS_SET_RVAL(cx, vp, JSVAL_VOID);
        return true;
    }

    
    ret = deep ? JS_DeepFreezeObject(cx, target) : JS_FreezeObject(cx, target);
    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return ret;
}


static bool
js_sleep(JSContext* cx, unsigned int argc, JS::Value* vp)
{
    JS::Value* argv = JS_ARGV(cx, vp);
    int duration = 0;
    if(!JS_ConvertArguments(cx, argc, argv, "/i", &duration)) {
        return false;
    }

#ifdef XP_WIN
    Sleep(duration);
#else
    usleep(duration * 1000);
#endif

    return true;
}

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


static bool
csp_allows(JSContext* cx)
{
    couch_args *args = (couch_args*)JS_GetContextPrivate(cx);
    if(args->eval) {
        return true;
    } else {
        return false;
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
    JS::Value sroot;
    JS::Value result;
    int i;

    couch_args* args = couch_parse_args(argc, argv);

    rt = JS_NewRuntime(args->stack_size);
    if(rt == NULL)
        return 1;

    cx = JS_NewContext(rt, 8L * 1024L);
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

    if(couch_load_funcs(cx, global, global_functions) != true)
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
        if(couch_load_funcs(cx, global, TestSuiteFunctions) != true)
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
        if(JS_AddValueRoot(cx, &sroot) != true) {
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

        if(JS_ExecuteScript(cx, global, script, &result) != true) {
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
