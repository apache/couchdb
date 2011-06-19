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

#ifndef COUCH_JS_JS_COMPAT_H
#define COUCH_JS_JS_COMPAT_H

#ifdef JS_FS_HAS_EXTRA
# undef JS_FS
# undef JS_FS_END
# define JS_FS(name,call,nargs,flags) {name, call, nargs, flags, 0}
# define JS_FS_END JS_FS(0, 0, 0, 0)
#endif

#ifdef JSFUN_CONSTRUCTOR
# define JSFUN_FAST_NATIVE 0
# define COUCHJS_CONSTRUCTOR_DECLARE(name) \
    static JSBool \
    name(JSContext* cx, uintN argc, jsval* vp)
# define COUCHJS_CONSTRUCTOR_INIT_VARS \
    JSObject *obj = NULL; \
    jsval *argv = JS_ARGV(cx, vp);
# define COUCHJS_CONSTRUCTOR_CONSTRUCT \
    obj = JS_NewObjectForConstructor(cx, vp); \
    if(!obj) { \
        JS_ReportError(cx, "Failed to create 'this' object"); \
        return JS_FALSE; \
    }
# define COUCHJS_CONSTRUCTOR_FINISH \
    JS_SET_RVAL(cx, vp, OBJECT_TO_JSVAL(obj));
# define COUCHJS_NATIVE_FUNC(func) func
#else
# define COUCHJS_CONSTRUCTOR_DECLARE(name) \
    static JSBool \
    name(JSContext* cx, JSObject* obj, uintN argc, jsval* argv, jsval* rval)
# define COUCHJS_CONSTRUCTOR_INIT_VARS
# define COUCHJS_CONSTRUCTOR_CONSTRUCT
# define COUCHJS_CONSTRUCTOR_FINISH
# define COUCHJS_NATIVE_FUNC(func) (JSNative) func
#endif

#ifdef HAVE_JS_STRICT_PROPERTY_STUB
# define JS_SETPROPERTY_PROPERTY_STUB JS_StrictPropertyStub
#else
# define JS_SETPROPERTY_PROPERTY_STUB JS_PropertyStub
#endif

#ifdef HAVE_SCRIPT_TYPE
# define COUCHJS_SCRIPT JSScript
# define COUCHJS_DESTROY_SCRIPT(cx, script) \
    JS_DestroyScript(cx, script)
#else
# define COUCHJS_SCRIPT JSObject
# define COUCHJS_DESTROY_SCRIPT(cx, script)
#endif

#ifdef HAVE_JS_FREEZE_OBJECT
# define COUCHJS_SEAL_OBJECT(res, cx, target, deep) \
    res = deep ? JS_DeepFreezeObject(cx, target) : JS_FreezeObject(cx, target);
#else
# define COUCHJS_SEAL_OBJECT(res, cx, target, deep) \
    res = JS_SealObject(cx, target, deep);
#endif

#define COUCHJS_NATIVE_INIT_VARS(argv, obj) \
    jsval* argv = JS_ARGV(cx, vp); \
    JSObject* obj = JS_THIS_OBJECT(cx, vp); \
    if (!obj) { \
        JS_ReportError(cx, "No 'this' object"); \
        return JS_FALSE; \
    }

#ifdef JS_PROPERTY_OP_HAS_ID_AS_JSID
# define COUCHJS_GETTER_DECLARE(name) \
    static JSBool \
    status(JSContext* cx, JSObject* obj, jsid id, jsval* vp)
#else
# define COUCHJS_GETTER_DECLARE(name) \
    static JSBool \
    status(JSContext* cx, JSObject* obj, jsval idval, jsval* vp)
#endif

#endif
