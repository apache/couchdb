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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "curlhelper.h"
#include <jsapi.h>
#include <curl/curl.h>

#ifndef CURLOPT_COPYPOSTFIELDS
   #define CURLOPT_COPYPOSTFIELDS 10165
#endif

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

JSBool ThrowError(JSContext *cx, const char *message)
{
    void *mark;
    jsval *args;
    jsval exc;

    printf("%s\n",message);

    args = JS_PushArguments(cx, &mark, "s", message);
    if (args) {
        if (JS_CallFunctionName(cx, JS_GetGlobalObject(cx),
                                "Error", 1, args, &exc))
            JS_SetPendingException(cx, exc);
        JS_PopArguments(cx, mark);
    }

    return JS_FALSE;
}

typedef struct buffer_counter {
 Buffer buffer;
 int pos;
}* BufferCount;

size_t curl_read(void *ptr, size_t size, size_t nmemb, void *stream) {
  if( size == 0 || nmemb == 0) {
    return 0;
  }

  char* databuffer = (char*)ptr;
  Buffer b = ((BufferCount)stream)->buffer;
  int* pos = &(((BufferCount)stream)->pos);

  if((b->count - *pos) == 0) {
    return 0;
  }

  int readlength = size*nmemb;
  int spaceleft = b->count - *pos;
  int i;

  if(readlength < spaceleft) {
    copy_Buffer(b,databuffer,*pos,readlength);
    *(pos) += readlength;
    return readlength;
  } else {
    copy_Buffer(b,databuffer,*pos,spaceleft);
    *(pos) += spaceleft;
    return spaceleft;
  }
}

size_t curl_write(void *ptr, size_t size, size_t nmemb, void *stream) {
  if( size == 0 || nmemb == 0 )
    return 0;

  char *data, *tmp;
  Buffer b;

  data = (char *)ptr;
  b = (Buffer)stream;

  append_Buffer(b,data,size*nmemb);

  return size*nmemb;
}

// This uses MALLOC dont forget to free
char* JSValToChar(JSContext* context, jsval* arg) {
  if(!JSVAL_IS_STRING(*arg)) {
    return NULL;
  }

  char *c, *tmp;
  JSString *jsmsg;
  size_t len;

  jsmsg = JS_ValueToString(context,*arg);
  len = JS_GetStringLength(jsmsg);
  tmp = JS_GetStringBytes(jsmsg);
      
  c = (char*)malloc(len+1);
  c[len] = '\0';

  int i;
 
  for(i = 0;i < len;i++) {
    c[i] = tmp[i];
  }

  return c;
}

JSBool BufferToJSVal(JSContext *context, Buffer b, jsval *rval) {
  char* c;
  JSString *str;

  // Important for char* to be JS_malloced, otherwise js wont let you use it in the NewString method
  c = JS_malloc(context, b->count * sizeof(char));
  copy_Buffer(b,c,0,b->count);


  /* Initialize a JSString object */
  str = JS_NewString(context, c, b->count);

  if (!str) {
    JS_free(context, c);
    return JS_FALSE;
  }

  // Set Return Value
  *rval = STRING_TO_JSVAL(str);
  if(rval == NULL) {
    return JS_FALSE;
  }
  return JS_TRUE;
}

struct curl_slist* generateCurlHeaders(JSContext* context,jsval* arg) {
  // If arg is an object then we go the header-hash route else return NULL

  if(!JSVAL_IS_NULL(*arg)) {

    struct curl_slist *slist = NULL;
    JSObject* header_obj;

    // If we fail to convert arg2 to an object. Error!
    if(!JS_ValueToObject(context,*arg,&header_obj)) {
      return NULL;
    }

    JSObject* iterator = JS_NewPropertyIterator(context,header_obj);
    
    jsval *jsProperty = JS_malloc(context,sizeof(jsval));
    jsval *jsValue = JS_malloc(context,sizeof(jsval));
    jsid *jsId = JS_malloc(context,sizeof(jsid));
    
    while(JS_NextProperty(context,iterator,jsId) == JS_TRUE) {

      if(*jsId == JSVAL_VOID) {
	break;
      }

      // TODO: Refactor this maybe make a JSValAppendBuffer method b/c that is what you really want to do.

      Buffer bTmp = init_Buffer();
      JS_IdToValue(context,*jsId,jsProperty);
      char* jsPropertyName = JSValToChar(context,jsProperty);

      // TODO: Remove strlen =/
      append_Buffer(bTmp,jsPropertyName,strlen(jsPropertyName));
      append_Buffer(bTmp,": ",2);

      JS_GetProperty(context,header_obj,jsPropertyName,jsValue);
      char* jsPropertyValue = JSValToChar(context,jsValue);
      // TODO: Remove strlen =/
      append_Buffer(bTmp,jsPropertyValue,strlen(jsPropertyValue));
      append_Buffer(bTmp,"",1);

      slist = curl_slist_append(slist,bTmp->data);
      
      free_Buffer(bTmp);
      free(jsPropertyValue);
      free(jsPropertyName);
    }

    JS_free(context,jsProperty);
    JS_free(context,jsValue);
    JS_free(context,jsId);

    return slist;

  } else {
    return NULL;
  }
}

static JSBool
GetHttp(JSContext *context, JSObject *obj, uintN argc, jsval *argv, jsval *rval) {
  CURL* handle;
  Buffer b;
  char *url;
  size_t charslen, readlen;

  // Run GC
  JS_MaybeGC(context);
  
  // Init Curl
  if((handle = curl_easy_init()) == NULL) {
    return JS_FALSE;
  }

  // Get URL
  url = JSValToChar(context,argv);
  if( url == NULL ) {
    return ThrowError(context,"Unable to convert url (argument 0) to a string");
  }

  b = init_Buffer(); // Allocate buffer that will store the get resultant

  // Configuration
  curl_easy_setopt(handle,CURLOPT_WRITEFUNCTION,curl_write);
  curl_easy_setopt(handle,CURLOPT_WRITEDATA,b);
  curl_easy_setopt(handle,CURLOPT_HEADERFUNCTION,curl_write);
  curl_easy_setopt(handle,CURLOPT_WRITEHEADER,b);
  curl_easy_setopt(handle,CURLOPT_URL,url);
  curl_easy_setopt(handle,CURLOPT_HTTPGET,1);
  curl_easy_setopt(handle,CURLOPT_NOPROGRESS,1);
  curl_easy_setopt(handle,CURLOPT_IPRESOLVE,CURL_IPRESOLVE_V4);

  struct curl_slist *slist = generateCurlHeaders(context,argv+1);
  if(slist != NULL) {
    curl_easy_setopt(handle,CURLOPT_HTTPHEADER,slist);
  }

  // Perform
  int exitcode;

  if((exitcode = curl_easy_perform(handle)) != 0) {
    if(slist != NULL) {
      curl_slist_free_all(slist);
    }
    curl_easy_cleanup(handle);
    free(url);
    free_Buffer(b);
    return JS_FALSE;
  }

  free(url);
  if(slist != NULL) {
    curl_slist_free_all(slist);
  }

  /* Treat the empty string specially */
  if (b->count == 0) {
    free_Buffer(b);
    *rval = JS_GetEmptyStringValue(context);
    curl_easy_cleanup(handle);
    return JS_TRUE;
  }

  /* Shrink the buffer to the real size  and store its value in rval */
  shrink_Buffer(b);
  BufferToJSVal(context,b,rval);
  
  // Free Buffer
  free_Buffer(b);

  if(rval == NULL) {
    curl_easy_cleanup(handle);
    return JS_FALSE;
  }

  JS_MaybeGC(context);

  curl_easy_cleanup(handle);

  return JS_TRUE;
}

static JSBool
HeadHttp(JSContext *context, JSObject *obj, uintN argc, jsval *argv, jsval *rval) {
  CURL* handle;
  Buffer b;
  char *url;
  size_t charslen, readlen;

  // Run GC
  JS_MaybeGC(context);
  
  // Init Curl
  if((handle = curl_easy_init()) == NULL) {
    return JS_FALSE;
  }

  // Get URL
  url = JSValToChar(context,argv);
  if( url == NULL ) {
    return ThrowError(context,"Unable to convert url (argument 0) to a string");
  }

  b = init_Buffer(); // Allocate buffer that will store the get resultant

  // Configuration
  // curl_easy_setopt(handle,CURLOPT_WRITEFUNCTION,curl_write);
  // curl_easy_setopt(handle,CURLOPT_WRITEDATA,b);
  curl_easy_setopt(handle,CURLOPT_HEADERFUNCTION,curl_write);
  curl_easy_setopt(handle,CURLOPT_WRITEHEADER,b);
  curl_easy_setopt(handle,CURLOPT_URL,url);
  curl_easy_setopt(handle,CURLOPT_HTTPGET,0);
  curl_easy_setopt(handle,CURLOPT_NOBODY,1);
  curl_easy_setopt(handle,CURLOPT_NOPROGRESS,1);
  curl_easy_setopt(handle,CURLOPT_IPRESOLVE,CURL_IPRESOLVE_V4);

  struct curl_slist *slist = generateCurlHeaders(context,argv+1);
  if(slist != NULL) {
    curl_easy_setopt(handle,CURLOPT_HTTPHEADER,slist);
  }

  // fprintf(stderr, "about to run HEAD request\n");

  // Perform
  int exitcode;

  if((exitcode = curl_easy_perform(handle)) != 0) {
    if(slist != NULL) {
      curl_slist_free_all(slist);
    }
    curl_easy_cleanup(handle);
    free(url);
    free_Buffer(b);
    return JS_FALSE;
  }
  // fprintf(stderr, "ran ok HEAD request\n");

  free(url);
  if(slist != NULL) {
    curl_slist_free_all(slist);
  }

  /* Treat the empty string specially */
  if (b->count == 0) {
    free_Buffer(b);
    *rval = JS_GetEmptyStringValue(context);
    curl_easy_cleanup(handle);
    return JS_TRUE;
  }

  /* Shrink the buffer to the real size  and store its value in rval */
  shrink_Buffer(b);
  BufferToJSVal(context,b,rval);
  
  // Free Buffer
  free_Buffer(b);

  if(rval == NULL) {
    curl_easy_cleanup(handle);
    return JS_FALSE;
  }

  JS_MaybeGC(context);

  curl_easy_cleanup(handle);

  return JS_TRUE;
}


static JSBool
PostHttp(JSContext *context, JSObject *obj, uintN argc, jsval *argv, jsval *rval) {
  CURL* handle;
  Buffer b;
  char *url, *body;
  size_t charslen, readlen;

  // Run GC
  JS_MaybeGC(context);

  // Init Curl
  if((handle = curl_easy_init()) == NULL) {
    return JS_FALSE;
  }

  // Get URL
  if((url = JSValToChar(context,argv)) == NULL) {
    curl_easy_cleanup(handle);
    return JS_FALSE;
  }

  // Initialize buffer
  b = init_Buffer();

  curl_easy_setopt(handle,CURLOPT_WRITEFUNCTION,curl_write);    // function that recieves data
  curl_easy_setopt(handle,CURLOPT_WRITEDATA,b);                 // buffer to write the data to
  curl_easy_setopt(handle,CURLOPT_HEADERFUNCTION,curl_write);
  curl_easy_setopt(handle,CURLOPT_WRITEHEADER,b);
  curl_easy_setopt(handle,CURLOPT_URL,url);                     // url
  curl_easy_setopt(handle,CURLOPT_POST,1);                  // Set Op. to post
  curl_easy_setopt(handle,CURLOPT_NOPROGRESS,1);                // No Progress Meter
  curl_easy_setopt(handle,CURLOPT_IPRESOLVE,CURL_IPRESOLVE_V4); // only ipv4

  if((body = JSValToChar(context,argv+1)) == NULL) {            // Convert arg1 to a string
    free(url);
    free_Buffer(b);
    curl_easy_cleanup(handle);
    return JS_FALSE;
  }

  curl_easy_setopt(handle,CURLOPT_POSTFIELDSIZE,strlen(body));
  curl_easy_setopt(handle,CURLOPT_POSTFIELDS,body);         // Curl wants '\0' terminated, we oblige

  struct curl_slist *slist = generateCurlHeaders(context,argv+2); // Initialize Headers
  if(slist != NULL) {
    curl_easy_setopt(handle,CURLOPT_HTTPHEADER,slist);            
  }

  int exitcode;

  if((exitcode = curl_easy_perform(handle)) != 0) {             // Perform
    curl_slist_free_all(slist);
    free(body);
    free(url);
    free_Buffer(b);
    curl_easy_cleanup(handle);
    return JS_FALSE;
  }

  free(body);
  free(url);
  curl_slist_free_all(slist);

  // Convert response back to javascript value and then clean
  BufferToJSVal(context,b,rval);
  free_Buffer(b);
  curl_easy_cleanup(handle);

  JS_MaybeGC(context);

  if( rval == NULL ) {
    return JS_FALSE;
  }

  return JS_TRUE;
}

#define CLEAN \
  free_Buffer(b); \
  free_Buffer(b_data->buffer); \
  free(b_data);		       \
  free(url)

static JSBool
PutHttp(JSContext *context, JSObject *obj, uintN argc, jsval *argv, jsval *rval){

  Buffer b;
  BufferCount b_data;
  char *url, *data;
  size_t charslen, readlen;
  JSObject* header_obj;

  // Run GC
  JS_MaybeGC(context);

  // Get URL
  url = JSValToChar(context,argv);

  // Allocate buffer that will store the get resultant
  b = init_Buffer();
  
  // Allocate data buffer and move data into them
  b_data = (BufferCount)malloc(sizeof(Buffer) + sizeof(int));
  b_data->buffer = init_Buffer();
  b_data->pos = 0;
  
  data = JSValToChar(context,(argv+1));
  // TODO: remove strlen
  append_Buffer(b_data->buffer,data,strlen(data));

  free(data);

  CURL* handle;

  // Init Curl

  if((handle = curl_easy_init()) == NULL) {
    CLEAN;
    return JS_FALSE;
  }

  // Configuration
  curl_easy_setopt(handle,CURLOPT_WRITEFUNCTION,curl_write);
  curl_easy_setopt(handle,CURLOPT_WRITEDATA,b);
  curl_easy_setopt(handle,CURLOPT_HEADERFUNCTION,curl_write);
  curl_easy_setopt(handle,CURLOPT_WRITEHEADER,b);
  curl_easy_setopt(handle,CURLOPT_READFUNCTION,curl_read);
  curl_easy_setopt(handle,CURLOPT_READDATA,b_data);
  curl_easy_setopt(handle,CURLOPT_URL,url);
  curl_easy_setopt(handle,CURLOPT_UPLOAD,1);

  // Curl structure
  struct curl_slist *slist = generateCurlHeaders(context,argv+2);
  if(slist != NULL) {
    curl_easy_setopt(handle,CURLOPT_HTTPHEADER,slist);
  }

  // Little Things
  // No progress meter
  curl_easy_setopt(handle,CURLOPT_NOPROGRESS,1);
  // Use only ipv4
  curl_easy_setopt(handle,CURLOPT_IPRESOLVE,CURL_IPRESOLVE_V4);

  // Perform
  int exitcode;
  
  if((exitcode = curl_easy_perform(handle)) != 0) {
    if(slist != NULL)
      curl_slist_free_all(slist);
    curl_easy_cleanup(handle);
    CLEAN;
    return JS_FALSE;
  }

  if(slist != NULL)
    curl_slist_free_all(slist);
  free_Buffer(b_data->buffer);
  free(b_data);
  free(url);

  /* Treat the empty string specially */
  if (b->count == 0) {
    *rval = JS_GetEmptyStringValue(context);
    curl_easy_cleanup(handle);
    free_Buffer(b);
    return JS_TRUE;
  }

  /* Shrink the buffer to the real size */
  shrink_Buffer(b);

  BufferToJSVal(context,b,rval);
  
  free_Buffer(b);

  if(rval == NULL) {
    curl_easy_cleanup(handle);
    return JS_FALSE;
  }

  JS_MaybeGC(context);

  curl_easy_cleanup(handle);

  return JS_TRUE;
}

static JSBool
DelHttp(JSContext *context, JSObject *obj, uintN argc, jsval *argv, jsval *rval) {
  Buffer b;
  char *url;
  size_t charslen, readlen;
  char header_name[7];
  strcpy(header_name,"DELETE");

  // Run GC
  JS_MaybeGC(context);

  // Get URL
  url = JSValToChar(context,argv);

  // Allocate buffer that will store the del resultant
  b = init_Buffer();

  CURL* handle;

  // Init Curl
  if((handle = curl_easy_init()) == NULL) {
    free_Buffer(b);
    return JS_FALSE;
  }

  // Configuration
  curl_easy_setopt(handle,CURLOPT_WRITEFUNCTION,curl_write);
  curl_easy_setopt(handle,CURLOPT_WRITEDATA,b);
  curl_easy_setopt(handle,CURLOPT_HEADERFUNCTION,curl_write);
  curl_easy_setopt(handle,CURLOPT_WRITEHEADER,b);
  curl_easy_setopt(handle,CURLOPT_URL,url);
  curl_easy_setopt(handle,CURLOPT_CUSTOMREQUEST,header_name);
  curl_easy_setopt(handle,CURLOPT_NOPROGRESS,1);
  curl_easy_setopt(handle,CURLOPT_IPRESOLVE,CURL_IPRESOLVE_V4);

  // Curl structure
  struct curl_slist *slist = NULL;
  if((slist = generateCurlHeaders(context,argv+1)) != NULL) {
    curl_easy_setopt(handle,CURLOPT_HTTPHEADER,slist);
  }

  // Perform
  int exitcode;

  if((exitcode = curl_easy_perform(handle)) != 0) {
    if(slist != NULL)
      curl_slist_free_all(slist);
    curl_easy_cleanup(handle);
    free(url);
    free_Buffer(b);
    return JS_FALSE;
  }

  if(slist != NULL)
    curl_slist_free_all(slist);
  free(url);

  /* Treat the empty string specially */
  if (b->count == 0) {
    *rval = JS_GetEmptyStringValue(context);
    curl_easy_cleanup(handle);
    free_Buffer(b);
    return JS_TRUE;
  }

  /* Shrink the buffer to the real size */
  shrink_Buffer(b);

  BufferToJSVal(context,b,rval);
  
  if(rval == NULL) {
    curl_easy_cleanup(handle);
    return JS_FALSE;
  }

  JS_MaybeGC(context);

  curl_easy_cleanup(handle);

  return JS_TRUE;
}

static JSBool
CopyHttp(JSContext *context, JSObject *obj, uintN argc, jsval *argv, jsval *rval) {
  Buffer b;
  char *url;
  size_t charslen, readlen;
  char header_name[5];
  strcpy(header_name,"COPY");

  // Run GC
  JS_MaybeGC(context);

  // Get URL
  url = JSValToChar(context,argv);

  // Allocate buffer that will store the del resultant
  b = init_Buffer();

  CURL* handle;

  // Init Curl
  if((handle = curl_easy_init()) == NULL) {
    free_Buffer(b);
    return JS_FALSE;
  }

  // Configuration
  curl_easy_setopt(handle,CURLOPT_WRITEFUNCTION,curl_write);
  curl_easy_setopt(handle,CURLOPT_WRITEDATA,b);
  curl_easy_setopt(handle,CURLOPT_HEADERFUNCTION,curl_write);
  curl_easy_setopt(handle,CURLOPT_WRITEHEADER,b);
  curl_easy_setopt(handle,CURLOPT_URL,url);
  curl_easy_setopt(handle,CURLOPT_CUSTOMREQUEST,header_name);
  curl_easy_setopt(handle,CURLOPT_NOPROGRESS,1);
  curl_easy_setopt(handle,CURLOPT_IPRESOLVE,CURL_IPRESOLVE_V4);

  // Curl structure
  struct curl_slist *slist = NULL;
  if((slist = generateCurlHeaders(context,argv+1)) != NULL) {
    curl_easy_setopt(handle,CURLOPT_HTTPHEADER,slist);
  }

  // Perform
  int exitcode;

  if((exitcode = curl_easy_perform(handle)) != 0) {
    if(slist != NULL)
      curl_slist_free_all(slist);
    curl_easy_cleanup(handle);
    free(url);
    free_Buffer(b);
    return JS_FALSE;
  }

  if(slist != NULL)
    curl_slist_free_all(slist);
  free(url);

  /* Treat the empty string specially */
  if (b->count == 0) {
    *rval = JS_GetEmptyStringValue(context);
    curl_easy_cleanup(handle);
    free_Buffer(b);
    return JS_TRUE;
  }

  /* Shrink the buffer to the real size */
  shrink_Buffer(b);

  BufferToJSVal(context,b,rval);
  
  if(rval == NULL) {
    curl_easy_cleanup(handle);
    return JS_FALSE;
  }

  JS_MaybeGC(context);

  curl_easy_cleanup(handle);

  return JS_TRUE;
}

static JSBool
MoveHttp(JSContext *context, JSObject *obj, uintN argc, jsval *argv, jsval *rval) {
  Buffer b;
  char *url;
  size_t charslen, readlen;
  char header_name[5];
  strcpy(header_name,"MOVE");

  // Run GC
  JS_MaybeGC(context);

  // Get URL
  url = JSValToChar(context,argv);

  // Allocate buffer that will store the del resultant
  b = init_Buffer();

  CURL* handle;

  // Init Curl
  if((handle = curl_easy_init()) == NULL) {
    free_Buffer(b);
    return JS_FALSE;
  }

  // Configuration
  curl_easy_setopt(handle,CURLOPT_WRITEFUNCTION,curl_write);
  curl_easy_setopt(handle,CURLOPT_WRITEDATA,b);
  curl_easy_setopt(handle,CURLOPT_HEADERFUNCTION,curl_write);
  curl_easy_setopt(handle,CURLOPT_WRITEHEADER,b);
  curl_easy_setopt(handle,CURLOPT_URL,url);
  curl_easy_setopt(handle,CURLOPT_CUSTOMREQUEST,header_name);
  curl_easy_setopt(handle,CURLOPT_NOPROGRESS,1);
  curl_easy_setopt(handle,CURLOPT_IPRESOLVE,CURL_IPRESOLVE_V4);

  // Curl structure
  struct curl_slist *slist = NULL;
  if((slist = generateCurlHeaders(context,argv+1)) != NULL) {
    curl_easy_setopt(handle,CURLOPT_HTTPHEADER,slist);
  }

  // Perform
  int exitcode;

  if((exitcode = curl_easy_perform(handle)) != 0) {
    if(slist != NULL)
      curl_slist_free_all(slist);
    curl_easy_cleanup(handle);
    free(url);
    free_Buffer(b);
    return JS_FALSE;
  }

  if(slist != NULL)
    curl_slist_free_all(slist);
  free(url);

  /* Treat the empty string specially */
  if (b->count == 0) {
    *rval = JS_GetEmptyStringValue(context);
    curl_easy_cleanup(handle);
    free_Buffer(b);
    return JS_TRUE;
  }

  /* Shrink the buffer to the real size */
  shrink_Buffer(b);

  BufferToJSVal(context,b,rval);
  
  if(rval == NULL) {
    curl_easy_cleanup(handle);
    return JS_FALSE;
  }

  JS_MaybeGC(context);

  curl_easy_cleanup(handle);

  return JS_TRUE;
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
     || !JS_DefineFunction(context, global, "seal", Seal, 0, 0)
     || !JS_DefineFunction(context, global, "gethttp", GetHttp, 1, 0)
     || !JS_DefineFunction(context, global, "headhttp", HeadHttp, 1, 0)
     || !JS_DefineFunction(context, global, "posthttp", PostHttp, 2, 0)
     || !JS_DefineFunction(context, global, "puthttp", PutHttp, 2, 0)
     || !JS_DefineFunction(context, global, "delhttp", DelHttp, 1, 0)
     || !JS_DefineFunction(context, global, "movehttp", MoveHttp, 1, 0)
     || !JS_DefineFunction(context, global, "copyhttp", CopyHttp, 1, 0))
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
