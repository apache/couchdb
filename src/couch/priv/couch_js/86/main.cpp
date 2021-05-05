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
#define NOMINMAX
#include <windows.h>
#else
#include <unistd.h>
#endif

#include <jsapi.h>
#include <js/CompilationAndEvaluation.h>
#include <js/Conversions.h>
#include <js/Initialization.h>
#include <js/SourceText.h>
#include <js/StableStringChars.h>
#include <js/Warnings.h>
#include <js/Wrapper.h>

#include "config.h"
#include "util.h"

static bool enableSharedMemory = true;
static bool enableToSource = true;

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
    JS_GlobalObjectTraceHook
};

/* The class of the global object. */
static JSClass global_class = {
    "global",
    JSCLASS_GLOBAL_FLAGS,
    &global_ops
};

static JSObject*
NewSandbox(JSContext* cx, bool lazy)
{
    JS::RealmOptions options;
    options.creationOptions().setSharedMemoryAndAtomicsEnabled(enableSharedMemory);
    options.creationOptions().setNewCompartmentAndZone();
    // we need this in the query server error handling
    options.creationOptions().setToSourceEnabled(enableToSource);
    JS::RootedObject obj(cx, JS_NewGlobalObject(cx, &global_class, nullptr,
                                            JS::DontFireOnNewGlobalHook, options));
    if (!obj)
        return nullptr;

    {
        JSAutoRealm ac(cx, obj);
        if (!lazy && !JS::InitRealmStandardClasses(cx))
            return nullptr;

        JS::RootedValue value(cx, JS::BooleanValue(lazy));
        if (!JS_DefineProperty(cx, obj, "lazy", value, JSPROP_PERMANENT | JSPROP_READONLY))
            return nullptr;

        JS_FireOnNewGlobalObject(cx, obj);
    }

    if (!JS_WrapObject(cx, &obj))
        return nullptr;
    return obj;
}

static bool
evalcx(JSContext *cx, unsigned int argc, JS::Value* vp)
{
    JS::CallArgs args = JS::CallArgsFromVp(argc, vp);
    bool ret = false;

    JS::RootedString str(cx, args[0].toString());
    if (!str)
        return false;

    JS::RootedObject sandbox(cx);
    if (args.hasDefined(1)) {
        sandbox = JS::ToObject(cx, args[1]);
        if (!sandbox)
            return false;
    }

    if (!sandbox) {
        sandbox = NewSandbox(cx, false);
        if (!sandbox)
            return false;
    }

    JS::AutoStableStringChars strChars(cx);
    if (!strChars.initTwoByte(cx, str))
        return false;

    mozilla::Range<const char16_t> chars = strChars.twoByteRange();
    JS::SourceText<char16_t> srcBuf;
    if (!srcBuf.init(cx, chars.begin().get(), chars.length(),
                     JS::SourceOwnership::Borrowed)) {
        return false;
    }

    if(srcBuf.length() == 0) {
        args.rval().setObject(*sandbox);
    } else {
        mozilla::Maybe<JSAutoRealm> ar;
        unsigned flags;
        JSObject* unwrapped = UncheckedUnwrap(sandbox, true, &flags);
        if (flags & js::Wrapper::CROSS_COMPARTMENT) {
            sandbox = unwrapped;
            ar.emplace(cx, sandbox);
        }

        JS::CompileOptions opts(cx);
        JS::RootedValue rval(cx);
        opts.setFileAndLine("<unknown>", 1);

        if (!JS::Evaluate(cx, opts, srcBuf, args.rval())) {
             return false;
         }
    }
    ret = true;
    if (!JS_WrapValue(cx, args.rval()))
        return false;

    return ret;
}


static bool
gc(JSContext* cx, unsigned int argc, JS::Value* vp)
{
    JS::CallArgs args = JS::CallArgsFromVp(argc, vp);
    JS_GC(cx);
    args.rval().setUndefined();
    return true;
}


static bool
print(JSContext* cx, unsigned int argc, JS::Value* vp)
{
    JS::CallArgs args = JS::CallArgsFromVp(argc, vp);

    bool use_stderr = false;
    if(argc > 1 && args[1].isTrue()) {
        use_stderr = true;
    }

    if(!args[0].isString()) {
        JS_ReportErrorUTF8(cx, "Unable to print non-string value.");
        return false;
    }

    couch_print(cx, args[0], use_stderr);

    args.rval().setUndefined();
    return true;
}


static bool
quit(JSContext* cx, unsigned int argc, JS::Value* vp)
{
    JS::CallArgs args = JS::CallArgsFromVp(argc, vp);

    int exit_code = args[0].toInt32();;
    exit(exit_code);
}


static bool
readline(JSContext* cx, unsigned int argc, JS::Value* vp)
{
    JSString* line;
    JS::CallArgs args = JS::CallArgsFromVp(argc, vp);

    /* GC Occasionally */
    JS_MaybeGC(cx);

    line = couch_readline(cx, stdin);
    if(line == NULL) return false;

    // return with JSString* instead of JSValue in the past
    args.rval().setString(line);
    return true;
}


static bool
seal(JSContext* cx, unsigned int argc, JS::Value* vp)
{
    JS::CallArgs args = JS::CallArgsFromVp(argc, vp);
    JS::RootedObject target(cx);
    target = JS::ToObject(cx, args[0]);
    if (!target) {
        args.rval().setUndefined();
        return true;
    }
    bool deep = false;
    deep = args[1].toBoolean();
    bool ret = deep ? JS_DeepFreezeObject(cx, target) : JS_FreezeObject(cx, target);
    args.rval().setUndefined();
    return ret;
}


static JSFunctionSpec global_functions[] = {
    JS_FN("evalcx", evalcx, 0, 0),
    JS_FN("gc", gc, 0, 0),
    JS_FN("print", print, 0, 0),
    JS_FN("quit", quit, 0, 0),
    JS_FN("readline", readline, 0, 0),
    JS_FN("seal", seal, 0, 0),
    JS_FS_END
};


static bool
csp_allows(JSContext* cx, JS::HandleString code)
{
    couch_args* args = static_cast<couch_args*>(JS_GetContextPrivate(cx));
    if(args->eval) {
        return true;
    } else {
        return false;
    }
}


static JSSecurityCallbacks security_callbacks = {
    csp_allows,
    nullptr
};


int
main(int argc, const char* argv[])
{
    JSContext* cx = NULL;
    int i;

    couch_args* args = couch_parse_args(argc, argv);

    JS_Init();
    cx = JS_NewContext(args->stack_size);
    if(cx == NULL)
        return 1;

    JS_SetGlobalJitCompilerOption(cx, JSJITCOMPILER_BASELINE_ENABLE, 0);
    JS_SetGlobalJitCompilerOption(cx, JSJITCOMPILER_ION_ENABLE, 0);

    if (!JS::InitSelfHostedCode(cx))
        return 1;

    JS::SetWarningReporter(cx, couch_error);
    JS::SetOutOfMemoryCallback(cx, couch_oom, NULL);
    JS_SetContextPrivate(cx, args);
    JS_SetSecurityCallbacks(cx, &security_callbacks);

    JS::RealmOptions options;
    JS::RootedObject global(cx, JS_NewGlobalObject(cx, &global_class, nullptr,
                                                   JS::FireOnNewGlobalHook, options));
    if (!global)
        return 1;

    JSAutoRealm ar(cx, global);

    if(!JS::InitRealmStandardClasses(cx))
        return 1;

    if(couch_load_funcs(cx, global, global_functions) != true)
        return 1;

    for(i = 0 ; args->scripts[i] ; i++) {
        const char* filename = args->scripts[i];

        // Compile and run
        JS::CompileOptions options(cx);
        options.setFileAndLine(filename, 1);
        JS::RootedScript script(cx);
        FILE* fp;

        fp = fopen(args->scripts[i], "r");
        if(fp == NULL) {
            fprintf(stderr, "Failed to read file: %s\n", filename);
            return 3;
        }
        script = JS::CompileUtf8File(cx, options, fp);
        fclose(fp);
        if (!script) {
            JS::RootedValue exc(cx);
            if(!JS_GetPendingException(cx, &exc)) {
                fprintf(stderr, "Failed to compile file: %s\n", filename);
            } else {
                JS::RootedObject exc_obj(cx, &exc.toObject());
                JSErrorReport* report = JS_ErrorFromException(cx, exc_obj);
                couch_error(cx, report);
            }
            return 1;
        }

        JS::RootedValue result(cx);
        if(JS_ExecuteScript(cx, script, &result) != true) {
            JS::RootedValue exc(cx);
            if(!JS_GetPendingException(cx, &exc)) {
                fprintf(stderr, "Failed to execute script.\n");
            } else {
                JS::RootedObject exc_obj(cx, &exc.toObject());
                JSErrorReport* report = JS_ErrorFromException(cx, exc_obj);
                couch_error(cx, report);
            }
        }

        // Give the GC a chance to run.
        JS_MaybeGC(cx);
    }

    return 0;
}
