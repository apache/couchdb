/*

Licensed under the Apache License, Version 2.0 (the "License"); you may not use
this file except in compliance with the License.  You may obtain a copy of the
License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed
under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
CONDITIONS OF ANY KIND, either express or implied.  See the License for the
specific language governing permissions and limitations under the License.

*/

#include <stdio.h>
#include <jsapi.h>

int gExitCode = 0;
int gStackChunkSize = 8L * 1024L;

int
EncodeChar(uint8 *utf8Buffer, uint32 ucs4Char) {
    int utf8Length = 1;

    if (ucs4Char < 0x80) {
        *utf8Buffer = (uint8)ucs4Char;
    } else {
        int i;
        uint32 a = ucs4Char >> 11;
        utf8Length = 2;
        while (a) {
            a >>= 5;
            utf8Length++;
        }
        i = utf8Length;
        while (--i) {
            utf8Buffer[i] = (uint8)((ucs4Char & 0x3F) | 0x80);
            ucs4Char >>= 6;
        }
        *utf8Buffer = (uint8)(0x100 - (1 << (8-utf8Length)) + ucs4Char);
    }
    return utf8Length;
}

JSBool
EncodeString(const jschar *src, size_t srclen, char *dst, size_t *dstlenp) {
    size_t i, utf8Len, dstlen = *dstlenp, origDstlen = dstlen;
    jschar c, c2;
    uint32 v;
    uint8 utf8buf[6];

    if (!dst)
        dstlen = origDstlen = (size_t) -1;

    while (srclen) {
        c = *src++;
        srclen--;
        if ((c >= 0xDC00) && (c <= 0xDFFF))
            goto badSurrogate;
        if (c < 0xD800 || c > 0xDBFF) {
            v = c;
        } else {
            if (srclen < 1)
                goto bufferTooSmall;
            c2 = *src++;
            srclen--;
            if ((c2 < 0xDC00) || (c2 > 0xDFFF)) {
                c = c2;
                goto badSurrogate;
            }
            v = ((c - 0xD800) << 10) + (c2 - 0xDC00) + 0x10000;
        }
        if (v < 0x0080) {
            /* no encoding necessary - performance hack */
            if (!dstlen)
                goto bufferTooSmall;
            if (dst)
                *dst++ = (char) v;
            utf8Len = 1;
        } else {
            utf8Len = EncodeChar(utf8buf, v);
            if (utf8Len > dstlen)
                goto bufferTooSmall;
            if (dst) {
                for (i = 0; i < utf8Len; i++)
                    *dst++ = (char) utf8buf[i];
            }
        }
        dstlen -= utf8Len;
    }
    *dstlenp = (origDstlen - dstlen);
    return JS_TRUE;

badSurrogate:
    *dstlenp = (origDstlen - dstlen);
    return JS_FALSE;

bufferTooSmall:
    *dstlenp = (origDstlen - dstlen);
    return JS_FALSE;
}

static uint32
DecodeChar(const uint8 *utf8Buffer, int utf8Length) {
    uint32 ucs4Char;
    uint32 minucs4Char;
    /* from Unicode 3.1, non-shortest form is illegal */
    static const uint32 minucs4Table[] = {
        0x00000080, 0x00000800, 0x0001000, 0x0020000, 0x0400000
    };

    if (utf8Length == 1) {
        ucs4Char = *utf8Buffer;
    } else {
        ucs4Char = *utf8Buffer++ & ((1<<(7-utf8Length))-1);
        minucs4Char = minucs4Table[utf8Length-2];
        while (--utf8Length) {
            ucs4Char = ucs4Char<<6 | (*utf8Buffer++ & 0x3F);
        }
        if (ucs4Char < minucs4Char ||
            ucs4Char == 0xFFFE || ucs4Char == 0xFFFF) {
            ucs4Char = 0xFFFD;
        }
    }
    return ucs4Char;
}

JSBool
DecodeString(const char *src, size_t srclen, jschar *dst, size_t *dstlenp) {
    uint32 v;
    size_t offset = 0, j, n, dstlen = *dstlenp, origDstlen = dstlen;

    if (!dst)
        dstlen = origDstlen = (size_t) -1;

    while (srclen) {
        v = (uint8) *src;
        n = 1;
        if (v & 0x80) {
            while (v & (0x80 >> n))
                n++;
            if (n > srclen)
                goto bufferTooSmall;
            if (n == 1 || n > 6)
                goto badCharacter;
            for (j = 1; j < n; j++) {
                if ((src[j] & 0xC0) != 0x80)
                    goto badCharacter;
            }
            v = DecodeChar((const uint8 *) src, n);
            if (v >= 0x10000) {
                v -= 0x10000;
                if (v > 0xFFFFF || dstlen < 2) {
                    *dstlenp = (origDstlen - dstlen);
                    return JS_FALSE;
                }
                if (dstlen < 2)
                    goto bufferTooSmall;
                if (dst) {
                    *dst++ = (jschar)((v >> 10) + 0xD800);
                    v = (jschar)((v & 0x3FF) + 0xDC00);
                }
                dstlen--;
            }
        }
        if (!dstlen)
            goto bufferTooSmall;
        if (dst)
            *dst++ = (jschar) v;
        dstlen--;
        offset += n;
        src += n;
        srclen -= n;
    }
    *dstlenp = (origDstlen - dstlen);
    return JS_TRUE;

badCharacter:
    *dstlenp = (origDstlen - dstlen);
    return JS_FALSE;

bufferTooSmall:
    *dstlenp = (origDstlen - dstlen);
    return JS_FALSE;
}

static JSBool
EvalInContext(JSContext *context, JSObject *obj, uintN argc, jsval *argv,
              jsval *rval) {
    JSString *str;
    JSObject *sandbox;
    JSContext *sub_context;
    const jschar *src;
    size_t srclen;
    JSBool ok;
    jsval v;

    sandbox = NULL;
    if (!JS_ConvertArguments(context, argc, argv, "S / o", &str, &sandbox))
        return JS_FALSE;

    sub_context = JS_NewContext(JS_GetRuntime(context), gStackChunkSize);
    if (!sub_context) {
        JS_ReportOutOfMemory(context);
        return JS_FALSE;
    }

    src = JS_GetStringChars(str);
    srclen = JS_GetStringLength(str);

    if (!sandbox) {
        sandbox = JS_NewObject(sub_context, NULL, NULL, NULL);
        if (!sandbox || !JS_InitStandardClasses(sub_context, sandbox)) {
            ok = JS_FALSE;
            goto out;
        }
    }

    if (srclen == 0) {
        *rval = OBJECT_TO_JSVAL(sandbox);
        ok = JS_TRUE;
    } else {
        ok = JS_EvaluateUCScript(sub_context, sandbox, src, srclen, NULL, -1,
                                 rval);
    }

out:
    JS_DestroyContext(sub_context);
    return ok;
}

static JSBool
GC(JSContext *context, JSObject *obj, uintN argc, jsval *argv, jsval *rval) {
    JS_GC(context);
    return JS_TRUE;
}

static JSBool
Print(JSContext *context, JSObject *obj, uintN argc, jsval *argv, jsval *rval) {
    uintN i, n;
    size_t cl, bl;
    JSString *str;
    jschar *chars;
    char *bytes;

    for (i = n = 0; i < argc; i++) {
        str = JS_ValueToString(context, argv[i]);
        if (!str)
            return JS_FALSE;
        chars = JS_GetStringChars(str);
        cl = JS_GetStringLength(str);
        if (!EncodeString(chars, cl, NULL, &bl))
            return JS_FALSE;
        bytes = JS_malloc(context, bl + 1);
        bytes[bl] = '\0';
        if (!EncodeString(chars, cl, bytes, &bl)) {
            JS_free(context, bytes);
            return JS_FALSE;
        }
        fprintf(stdout, "%s%s", i ? " " : "", bytes);
        JS_free(context, bytes);
    }
    n++;
    if (n)
        fputc('\n', stdout);
    fflush(stdout);
    return JS_TRUE;
}

static JSBool
Quit(JSContext *context, JSObject *obj, uintN argc, jsval *argv, jsval *rval) {
    JS_ConvertArguments(context, argc, argv, "/ i", &gExitCode);
    return JS_FALSE;
}

static JSBool
ReadLine(JSContext *context, JSObject *obj, uintN argc, jsval *argv, jsval *rval) {
    char *bytes, *tmp;
    jschar *chars;
    size_t bufsize, byteslen, charslen, readlen;
    JSString *str;

    JS_MaybeGC(context);

    byteslen = 0;
    bufsize = 256;
    bytes = JS_malloc(context, bufsize);
    if (!bytes)
        return JS_FALSE;

    while ((readlen = js_fgets(bytes + byteslen, bufsize - byteslen, stdin)) > 0) {
        byteslen += readlen;

        /* Are we done? */
        if (bytes[byteslen - 1] == '\n') {
            bytes[byteslen - 1] = '\0';
            break;
        }

        /* Else, grow our buffer for another pass */
        tmp = JS_realloc(context, bytes, bufsize * 2);
        if (!tmp) {
            JS_free(context, bytes);
            return JS_FALSE;
        }

        bufsize *= 2;
        bytes = tmp;
    }

    /* Treat the empty string specially */
    if (byteslen == 0) {
        *rval = JS_GetEmptyStringValue(context);
        JS_free(context, bytes);
        return JS_TRUE;
    }

    /* Shrink the buffer to the real size */
    tmp = JS_realloc(context, bytes, byteslen);
    if (!tmp) {
        JS_free(context, bytes);
        return JS_FALSE;
    }
    bytes = tmp;

    /* Decode the string from UTF-8 */
    if (!DecodeString(bytes, byteslen, NULL, &charslen)) {
        JS_free(context, bytes);
        return JS_FALSE;
    }
    chars = JS_malloc(context, (charslen + 1) * sizeof(jschar));
    if (!DecodeString(bytes, byteslen, chars, &charslen)) {
        JS_free(context, bytes);
        JS_free(context, chars);
        return JS_FALSE;
    }
    JS_free(context, bytes);
    chars[charslen] = '\0';

    /* Initialize a JSString object */
    str = JS_NewUCString(context, chars, charslen - 1);
    if (!str) {
        JS_free(context, chars);
        return JS_FALSE;
    }

    *rval = STRING_TO_JSVAL(str);
    return JS_TRUE;
}

static JSBool
Seal(JSContext *context, JSObject *obj, uintN argc, jsval *argv, jsval *rval) {
    JSObject *target;
    JSBool deep = JS_FALSE;

    if (!JS_ConvertArguments(context, argc, argv, "o/b", &target, &deep))
        return JS_FALSE;
    if (!target)
        return JS_TRUE;
    return JS_SealObject(context, target, deep);
}

static void
ExecuteScript(JSContext *context, JSObject *obj, const char *filename) {
    FILE *file;
    JSScript *script;
    jsval result;

    if (!filename || strcmp(filename, "-") == 0) {
        file = stdin;
    } else {
        file = fopen(filename, "r");
        if (!file) {
            fprintf(stderr, "could not open script file %s\n", filename);
            gExitCode = 1;
            return;
        }
    }

    script = JS_CompileFileHandle(context, obj, filename, file);
    if (script) {
        JS_ExecuteScript(context, obj, script, &result);
        JS_DestroyScript(context, script);
    }
}

static uint32 gBranchCount = 0;

static JSBool
BranchCallback(JSContext *context, JSScript *script) {
    if ((++gBranchCount & 0x3fff) == 1) {
        JS_MaybeGC(context);
    }
    return JS_TRUE;
}

static void
PrintError(JSContext *context, const char *message, JSErrorReport *report) {
    if (!report || !JSREPORT_IS_WARNING(report->flags))
        fprintf(stderr, "%s\n", message);
}

int
main(int argc, const char * argv[]) {
    JSRuntime *runtime;
    JSContext *context;
    JSObject *global;

    runtime = JS_NewRuntime(64L * 1024L * 1024L);
    if (!runtime)
        return 1;
    context = JS_NewContext(runtime, gStackChunkSize);
    if (!context)
        return 1;
    JS_SetErrorReporter(context, PrintError);
    JS_SetBranchCallback(context, BranchCallback);
    JS_ToggleOptions(context, JSOPTION_NATIVE_BRANCH_CALLBACK);
    JS_ToggleOptions(context, JSOPTION_XML);

    global = JS_NewObject(context, NULL, NULL, NULL);
    if (!global)
        return 1;
    if (!JS_InitStandardClasses(context, global))
        return 1;
    if (!JS_DefineFunction(context, global, "evalcx", EvalInContext, 0, 0)
     || !JS_DefineFunction(context, global, "gc", GC, 0, 0)
     || !JS_DefineFunction(context, global, "print", Print, 0, 0)
     || !JS_DefineFunction(context, global, "quit", Quit, 0, 0)
     || !JS_DefineFunction(context, global, "readline", ReadLine, 0, 0)
     || !JS_DefineFunction(context, global, "seal", Seal, 0, 0))
        return 1;

    if (argc != 2) {
        fprintf(stderr, "incorrect number of arguments\n\n");
        fprintf(stderr, "usage: %s <scriptfile>\n", argv[0]);
        return 2;
    }

    ExecuteScript(context, global, argv[1]);

    JS_DestroyContext(context);
    JS_DestroyRuntime(runtime);
    JS_ShutDown();

    return gExitCode;
}
