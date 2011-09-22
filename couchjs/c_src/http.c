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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "config.h"
#include "sm.h"
#include "utf8.h"

// Soft dependency on cURL bindings because they're
// only used when running the JS tests from the
// command line which is rare.
#ifndef HAVE_LIBCURL

void
http_check_enabled()
{
    fprintf(stderr, "HTTP API was disabled at compile time.\n");
    exit(3);
}


JSBool
http_ctor(JSContext* cx, JSObject* req)
{
    return JS_FALSE;
}


JSBool
http_dtor(JSContext* cx, JSObject* req)
{
    return JS_FALSE;
}


JSBool
http_open(JSContext* cx, JSObject* req, jsval mth, jsval url, jsval snc)
{
    return JS_FALSE;
}


JSBool
http_set_hdr(JSContext* cx, JSObject* req, jsval name, jsval val)
{
    return JS_FALSE;
}


JSBool
http_send(JSContext* cx, JSObject* req, jsval body)
{
    return JS_FALSE;
}


int
http_status(JSContext* cx, JSObject* req)
{
    return -1;
}


#else
#include <curl/curl.h>


void
http_check_enabled()
{
    return;
}


// Map some of the string function names to things which exist on Windows
#ifdef XP_WIN
#define strcasecmp _strcmpi
#define strncasecmp _strnicmp
#define snprintf _snprintf
#endif


typedef struct curl_slist CurlHeaders;


typedef struct {
    int             method;
    char*           url;
    CurlHeaders*    req_headers;
    jsint           last_status;
} HTTPData;


const char* METHODS[] = {"GET", "HEAD", "POST", "PUT", "DELETE", "COPY", NULL};


#define GET     0
#define HEAD    1
#define POST    2
#define PUT     3
#define DELETE  4
#define COPY    5


static JSBool
go(JSContext* cx, JSObject* obj, HTTPData* http, char* body, size_t blen);


static JSString*
str_from_binary(JSContext* cx, char* data, size_t length);


JSBool
http_ctor(JSContext* cx, JSObject* req)
{
    HTTPData* http = NULL;
    JSBool ret = JS_FALSE;

    http = (HTTPData*) malloc(sizeof(HTTPData));
    if(!http)
    {
        JS_ReportError(cx, "Failed to create CouchHTTP instance.");
        goto error;
    }

    http->method = -1;
    http->url = NULL;
    http->req_headers = NULL;
    http->last_status = -1;

    if(!JS_SetPrivate(cx, req, http))
    {
        JS_ReportError(cx, "Failed to set private CouchHTTP data.");
        goto error;
    }

    ret = JS_TRUE;
    goto success;

error:
    if(http) free(http);

success:
    return ret;
}


void
http_dtor(JSContext* cx, JSObject* obj)
{
    HTTPData* http = (HTTPData*) JS_GetPrivate(cx, obj);
    if(http) {
        if(http->url) free(http->url);
        if(http->req_headers) curl_slist_free_all(http->req_headers);
        free(http);
    }
}


JSBool
http_open(JSContext* cx, JSObject* req, jsval mth, jsval url, jsval snc)
{
    HTTPData* http = (HTTPData*) JS_GetPrivate(cx, req);
    char* method = NULL;
    int methid;
    JSBool ret = JS_FALSE;

    if(!http) {
        JS_ReportError(cx, "Invalid CouchHTTP instance.");
        goto done;
    }

    if(mth == JSVAL_VOID) {
        JS_ReportError(cx, "You must specify a method.");
        goto done;
    }

    method = enc_string(cx, mth, NULL);
    if(!method) {
        JS_ReportError(cx, "Failed to encode method.");
        goto done;
    }

    for(methid = 0; METHODS[methid] != NULL; methid++) {
        if(strcasecmp(METHODS[methid], method) == 0) break;
    }

    if(methid > COPY) {
        JS_ReportError(cx, "Invalid method specified.");
        goto done;
    }

    http->method = methid;

    if(url == JSVAL_VOID) {
        JS_ReportError(cx, "You must specify a URL.");
        goto done;
    }

    if(http->url != NULL) {
        free(http->url);
        http->url = NULL;
    }

    http->url = enc_string(cx, url, NULL);
    if(http->url == NULL) {
        JS_ReportError(cx, "Failed to encode URL.");
        goto done;
    }

    if(snc != JSVAL_FALSE) {
        JS_ReportError(cx, "Synchronous flag must be false.");
        goto done;
    }

    if(http->req_headers) {
        curl_slist_free_all(http->req_headers);
        http->req_headers = NULL;
    }

    // Disable Expect: 100-continue
    http->req_headers = curl_slist_append(http->req_headers, "Expect:");

    ret = JS_TRUE;

done:
    if(method) free(method);
    return ret;
}


JSBool
http_set_hdr(JSContext* cx, JSObject* req, jsval name, jsval val)
{
    HTTPData* http = (HTTPData*) JS_GetPrivate(cx, req);
    char* keystr = NULL;
    char* valstr = NULL;
    char* hdrbuf = NULL;
    size_t hdrlen = -1;
    JSBool ret = JS_FALSE;

    if(!http) {
        JS_ReportError(cx, "Invalid CouchHTTP instance.");
        goto done;
    }

    if(name == JSVAL_VOID)
    {
        JS_ReportError(cx, "You must speciy a header name.");
        goto done;
    }

    keystr = enc_string(cx, name, NULL);
    if(!keystr)
    {
        JS_ReportError(cx, "Failed to encode header name.");
        goto done;
    }

    if(val == JSVAL_VOID)
    {
        JS_ReportError(cx, "You must specify a header value.");
        goto done;
    }

    valstr = enc_string(cx, val, NULL);
    if(!valstr)
    {
        JS_ReportError(cx, "Failed to encode header value.");
        goto done;
    }

    hdrlen = strlen(keystr) + strlen(valstr) + 3;
    hdrbuf = (char*) malloc(hdrlen * sizeof(char));
    if(!hdrbuf) {
        JS_ReportError(cx, "Failed to allocate header buffer.");
        goto done;
    }

    snprintf(hdrbuf, hdrlen, "%s: %s", keystr, valstr);
    http->req_headers = curl_slist_append(http->req_headers, hdrbuf);

    ret = JS_TRUE;

done:
    if(keystr) free(keystr);
    if(valstr) free(valstr);
    if(hdrbuf) free(hdrbuf);
    return ret;
}

JSBool
http_send(JSContext* cx, JSObject* req, jsval body)
{
    HTTPData* http = (HTTPData*) JS_GetPrivate(cx, req);
    char* bodystr = NULL;
    size_t bodylen = 0;
    JSBool ret = JS_FALSE;

    if(!http) {
        JS_ReportError(cx, "Invalid CouchHTTP instance.");
        goto done;
    }

    if(body != JSVAL_VOID && body != JS_GetEmptyStringValue(cx)) {
        bodystr = enc_string(cx, body, &bodylen);
        if(!bodystr) {
            JS_ReportError(cx, "Failed to encode body.");
            goto done;
        }
    }

    ret = go(cx, req, http, bodystr, bodylen);

done:
    if(bodystr) free(bodystr);
    return ret;
}

int
http_status(JSContext* cx, JSObject* req)
{
    HTTPData* http = (HTTPData*) JS_GetPrivate(cx, req);

    if(!http) {
        JS_ReportError(cx, "Invalid CouchHTTP instance.");
        return JS_FALSE;
    }

    return http->last_status;
}

// Curl Helpers

typedef struct {
    HTTPData*   http;
    JSContext*  cx;
    JSObject*   resp_headers;
    char*       sendbuf;
    size_t      sendlen;
    size_t      sent;
    int         sent_once;
    char*       recvbuf;
    size_t      recvlen;
    size_t      read;
} CurlState;

/*
 * I really hate doing this but this doesn't have to be
 * uber awesome, it just has to work.
 */
CURL*       HTTP_HANDLE = NULL;
char        ERRBUF[CURL_ERROR_SIZE];

static size_t send_body(void *ptr, size_t size, size_t nmem, void *data);
static int seek_body(void *ptr, curl_off_t offset, int origin);
static size_t recv_body(void *ptr, size_t size, size_t nmem, void *data);
static size_t recv_header(void *ptr, size_t size, size_t nmem, void *data);

static JSBool
go(JSContext* cx, JSObject* obj, HTTPData* http, char* body, size_t bodylen)
{
    CurlState state;
    JSString* jsbody;
    JSBool ret = JS_FALSE;
    jsval tmp;

    state.cx = cx;
    state.http = http;

    state.sendbuf = body;
    state.sendlen = bodylen;
    state.sent = 0;
    state.sent_once = 0;

    state.recvbuf = NULL;
    state.recvlen = 0;
    state.read = 0;

    if(HTTP_HANDLE == NULL) {
        HTTP_HANDLE = curl_easy_init();
        curl_easy_setopt(HTTP_HANDLE, CURLOPT_READFUNCTION, send_body);
        curl_easy_setopt(HTTP_HANDLE, CURLOPT_SEEKFUNCTION,
                                        (curl_seek_callback) seek_body);
        curl_easy_setopt(HTTP_HANDLE, CURLOPT_HEADERFUNCTION, recv_header);
        curl_easy_setopt(HTTP_HANDLE, CURLOPT_WRITEFUNCTION, recv_body);
        curl_easy_setopt(HTTP_HANDLE, CURLOPT_NOPROGRESS, 1);
        curl_easy_setopt(HTTP_HANDLE, CURLOPT_IPRESOLVE, CURL_IPRESOLVE_V4);
        curl_easy_setopt(HTTP_HANDLE, CURLOPT_ERRORBUFFER, ERRBUF);
        curl_easy_setopt(HTTP_HANDLE, CURLOPT_COOKIEFILE, "");
        curl_easy_setopt(HTTP_HANDLE, CURLOPT_USERAGENT,
                                            "CouchHTTP Client - Relax");
    }

    if(!HTTP_HANDLE) {
        JS_ReportError(cx, "Failed to initialize cURL handle.");
        goto done;
    }

    if(http->method < 0 || http->method > COPY) {
        JS_ReportError(cx, "INTERNAL: Unknown method.");
        goto done;
    }

    curl_easy_setopt(HTTP_HANDLE, CURLOPT_CUSTOMREQUEST, METHODS[http->method]);
    curl_easy_setopt(HTTP_HANDLE, CURLOPT_NOBODY, 0);
    curl_easy_setopt(HTTP_HANDLE, CURLOPT_FOLLOWLOCATION, 1);
    curl_easy_setopt(HTTP_HANDLE, CURLOPT_UPLOAD, 0);

    if(http->method == HEAD) {
        curl_easy_setopt(HTTP_HANDLE, CURLOPT_NOBODY, 1);
        curl_easy_setopt(HTTP_HANDLE, CURLOPT_FOLLOWLOCATION, 0);
    } else if(http->method == POST || http->method == PUT) {
        curl_easy_setopt(HTTP_HANDLE, CURLOPT_UPLOAD, 1);
        curl_easy_setopt(HTTP_HANDLE, CURLOPT_FOLLOWLOCATION, 0);
    }

    if(body && bodylen) {
        curl_easy_setopt(HTTP_HANDLE, CURLOPT_INFILESIZE, bodylen);
    } else {
        curl_easy_setopt(HTTP_HANDLE, CURLOPT_INFILESIZE, 0);
    }

    // curl_easy_setopt(HTTP_HANDLE, CURLOPT_VERBOSE, 1);

    curl_easy_setopt(HTTP_HANDLE, CURLOPT_URL, http->url);
    curl_easy_setopt(HTTP_HANDLE, CURLOPT_HTTPHEADER, http->req_headers);
    curl_easy_setopt(HTTP_HANDLE, CURLOPT_READDATA, &state);
    curl_easy_setopt(HTTP_HANDLE, CURLOPT_SEEKDATA, &state);
    curl_easy_setopt(HTTP_HANDLE, CURLOPT_WRITEHEADER, &state);
    curl_easy_setopt(HTTP_HANDLE, CURLOPT_WRITEDATA, &state);

    if(curl_easy_perform(HTTP_HANDLE) != 0) {
        JS_ReportError(cx, "Failed to execute HTTP request: %s", ERRBUF);
        goto done;
    }

    if(!state.resp_headers) {
        JS_ReportError(cx, "Failed to recieve HTTP headers.");
        goto done;
    }

    tmp = OBJECT_TO_JSVAL(state.resp_headers);
    if(!JS_DefineProperty(
        cx, obj,
        "_headers",
        tmp,
        NULL, NULL,
        JSPROP_READONLY
    )) {
        JS_ReportError(cx, "INTERNAL: Failed to set response headers.");
        goto done;
    }

    if(state.recvbuf) {
        state.recvbuf[state.read] = '\0';
        jsbody = dec_string(cx, state.recvbuf, state.read+1);
        if(!jsbody) {
            // If we can't decode the body as UTF-8 we forcefully
            // convert it to a string by just forcing each byte
            // to a jschar.
            jsbody = str_from_binary(cx, state.recvbuf, state.read);
            if(!jsbody) {
                if(!JS_IsExceptionPending(cx)) {
                    JS_ReportError(cx, "INTERNAL: Failed to decode body.");
                }
                goto done;
            }
        }
        tmp = STRING_TO_JSVAL(jsbody);
    } else {
        tmp = JS_GetEmptyStringValue(cx);
    }

    if(!JS_DefineProperty(
        cx, obj,
        "responseText",
        tmp,
        NULL, NULL,
        JSPROP_READONLY
    )) {
        JS_ReportError(cx, "INTERNAL: Failed to set responseText.");
        goto done;
    }

    ret = JS_TRUE;

done:
    if(state.recvbuf) JS_free(cx, state.recvbuf);
    return ret;
}

static size_t
send_body(void *ptr, size_t size, size_t nmem, void *data)
{
    CurlState* state = (CurlState*) data;
    size_t length = size * nmem;
    size_t towrite = state->sendlen - state->sent;

    // Assume this is cURL trying to resend a request that
    // failed.
    if(towrite == 0 && state->sent_once == 0) {
        state->sent_once = 1;
        return 0;
    } else if(towrite == 0) {
        state->sent = 0;
        state->sent_once = 0;
        towrite = state->sendlen;
    }

    if(length < towrite) towrite = length;

    memcpy(ptr, state->sendbuf + state->sent, towrite);
    state->sent += towrite;

    return towrite;
}

static int
seek_body(void* ptr, curl_off_t offset, int origin)
{
    CurlState* state = (CurlState*) ptr;
    if(origin != SEEK_SET) return -1;

    state->sent = (size_t) offset;
    return (int) state->sent;
}

static size_t
recv_header(void *ptr, size_t size, size_t nmem, void *data)
{
    CurlState* state = (CurlState*) data;
    char code[4];
    char* header = (char*) ptr;
    size_t length = size * nmem;
    JSString* hdr = NULL;
    jsuint hdrlen;
    jsval hdrval;

    if(length > 7 && strncasecmp(header, "HTTP/1.", 7) == 0) {
        if(length < 12) {
            return CURLE_WRITE_ERROR;
        }

        memcpy(code, header+9, 3*sizeof(char));
        code[3] = '\0';
        state->http->last_status = atoi(code);

        state->resp_headers = JS_NewArrayObject(state->cx, 0, NULL);
        if(!state->resp_headers) {
            return CURLE_WRITE_ERROR;
        }

        return length;
    }

    // We get a notice at the \r\n\r\n after headers.
    if(length <= 2) {
        return length;
    }

    // Append the new header to our array.
    hdr = dec_string(state->cx, header, length);
    if(!hdr) {
        return CURLE_WRITE_ERROR;
    }

    if(!JS_GetArrayLength(state->cx, state->resp_headers, &hdrlen)) {
        return CURLE_WRITE_ERROR;
    }

    hdrval = STRING_TO_JSVAL(hdr);
    if(!JS_SetElement(state->cx, state->resp_headers, hdrlen, &hdrval)) {
        return CURLE_WRITE_ERROR;
    }

    return length;
}

static size_t
recv_body(void *ptr, size_t size, size_t nmem, void *data)
{
    CurlState* state = (CurlState*) data;
    size_t length = size * nmem;
    char* tmp = NULL;

    if(!state->recvbuf) {
        state->recvlen = 4096;
        state->read = 0;
        state->recvbuf = (char*) JS_malloc(state->cx, state->recvlen);
    }

    if(!state->recvbuf) {
        return CURLE_WRITE_ERROR;
    }

    // +1 so we can add '\0' back up in the go function.
    while(length+1 > state->recvlen - state->read) state->recvlen *= 2;
    tmp = (char*) JS_realloc(state->cx, state->recvbuf, state->recvlen);
    if(!tmp) return CURLE_WRITE_ERROR;
    state->recvbuf = tmp;

    memcpy(state->recvbuf + state->read, ptr, length);
    state->read += length;
    return length;
}

JSString*
str_from_binary(JSContext* cx, char* data, size_t length)
{
    jschar* conv = (jschar*) JS_malloc(cx, length * sizeof(jschar));
    JSString* ret = NULL;
    size_t i;

    if(!conv) return NULL;

    for(i = 0; i < length; i++) {
        conv[i] = (jschar) data[i];
    }

    ret = JS_NewUCString(cx, conv, length);
    if(!ret) JS_free(cx, conv);

    return ret;
}

#endif /* HAVE_CURL */
