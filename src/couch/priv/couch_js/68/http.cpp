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
#include <sys/types.h>
#include <sys/stat.h>
#include <jsapi.h>
#include <js/Initialization.h>
#include <js/MemoryFunctions.h>
#include "config.h"
#include "util.h"

// Soft dependency on cURL bindings because they're
// only used when running the JS tests from the
// command line which is rare.
#ifndef HAVE_CURL

void
http_check_enabled()
{
    fprintf(stderr, "HTTP API was disabled at compile time.\n");
    exit(3);
}


bool
http_ctor(JSContext* cx, JSObject* req)
{
    return false;
}


void
http_dtor(JSFreeOp* fop, JSObject* req)
{
    return;
}


bool
http_open(JSContext* cx, JSObject* req, JS::Value mth, JS::Value url, JS::Value snc)
{
    return false;
}


bool
http_set_hdr(JSContext* cx, JSObject* req, JS::Value name, JS::Value val)
{
    return false;
}


bool
http_send(JSContext* cx, JSObject* req, JS::Value body)
{
    return false;
}


int
http_status(JSContext* cx, JSObject* req)
{
    return -1;
}

bool
http_uri(JSContext* cx, JSObject* req, couch_args* args, JS::Value* uri_val)
{
    return false;
}


#else
#include <curl/curl.h>
#ifndef XP_WIN
#include <unistd.h>
#endif


void
http_check_enabled()
{
    return;
}


// Map some of the string function names to things which exist on Windows
#ifdef XP_WIN
#define strcasecmp _strcmpi
#define strncasecmp _strnicmp
#endif


typedef struct curl_slist CurlHeaders;


typedef struct {
    int             method;
    std::string     url;
    CurlHeaders*    req_headers;
    int16_t          last_status;
} HTTPData;


const char* METHODS[] = {"GET", "HEAD", "POST", "PUT", "DELETE", "COPY", "OPTIONS", NULL};


#define GET     0
#define HEAD    1
#define POST    2
#define PUT     3
#define DELETE  4
#define COPY    5
#define OPTIONS 6


static bool go(JSContext* cx, JSObject* obj, HTTPData* http, std::string& body);


bool
http_ctor(JSContext* cx, JSObject* req)
{
    HTTPData* http = new HTTPData();
    bool ret = false;

    if(!http)
    {
        JS_ReportErrorUTF8(cx, "Failed to create CouchHTTP instance.");
        goto error;
    }

    http->method = -1;
    http->req_headers = NULL;
    http->last_status = -1;

    JS_SetPrivate(req, http);

    ret = true;
    goto success;

error:
    if(http) delete http;

success:
    return ret;
}


void
http_dtor(JSFreeOp* fop, JSObject* obj)
{
    HTTPData* http = (HTTPData*) JS_GetPrivate(obj);
    if(http) {
        if(http->req_headers) curl_slist_free_all(http->req_headers);
        delete http;
    }
}


bool
http_open(JSContext* cx, JSObject* req, JS::Value mth, JS::Value url, JS::Value snc)
{
    HTTPData* http = (HTTPData*) JS_GetPrivate(req);
    int methid;

    if(!http) {
        JS_ReportErrorUTF8(cx, "Invalid CouchHTTP instance.");
        return false;
    }

    if(!mth.isString()) {
        JS_ReportErrorUTF8(cx, "Method must be a string.");
        return false;
    }

    std::string method;
    if(!js_to_string(cx, JS::RootedValue(cx, mth), method)) {
        JS_ReportErrorUTF8(cx, "Failed to encode method.");
        return false;
    }

    for(methid = 0; METHODS[methid] != NULL; methid++) {
        if(strcasecmp(METHODS[methid], method.c_str()) == 0) break;
    }

    if(methid > OPTIONS) {
        JS_ReportErrorUTF8(cx, "Invalid method specified.");
        return false;
    }

    http->method = methid;

    if(!url.isString()) {
        JS_ReportErrorUTF8(cx, "URL must be a string");
        return false;
    }

    std::string urlstr;
    if(!js_to_string(cx, JS::RootedValue(cx, url), urlstr)) {
        JS_ReportErrorUTF8(cx, "Failed to encode URL.");
        return false;
    }
    http->url = urlstr;

    if(snc.isBoolean() && snc.isTrue()) {
        JS_ReportErrorUTF8(cx, "Synchronous flag must be false.");
        return false;
    }

    if(http->req_headers) {
        curl_slist_free_all(http->req_headers);
        http->req_headers = NULL;
    }

    // Disable Expect: 100-continue
    http->req_headers = curl_slist_append(http->req_headers, "Expect:");

    return true;
}


bool
http_set_hdr(JSContext* cx, JSObject* req, JS::Value name, JS::Value val)
{
    HTTPData* http = (HTTPData*) JS_GetPrivate(req);

    if(!http) {
        JS_ReportErrorUTF8(cx, "Invalid CouchHTTP instance.");
        return false;
    }

    if(!name.isString())
    {
        JS_ReportErrorUTF8(cx, "Header names must be strings.");
        return false;
    }

    std::string keystr;
    if(!js_to_string(cx, JS::RootedValue(cx, name), keystr))
    {
        JS_ReportErrorUTF8(cx, "Failed to encode header name.");
        return false;
    }

    if(!val.isString())
    {
        JS_ReportErrorUTF8(cx, "Header values must be strings.");
        return false;
    }

    std::string valstr;
    if(!js_to_string(cx, JS::RootedValue(cx, val), valstr)) {
        JS_ReportErrorUTF8(cx, "Failed to encode header value.");
        return false;
    }

    std::string header = keystr + ": " + valstr;
    http->req_headers = curl_slist_append(http->req_headers, header.c_str());

    return true;
}

bool
http_send(JSContext* cx, JSObject* req, JS::Value body)
{
    HTTPData* http = (HTTPData*) JS_GetPrivate(req);

    if(!http) {
        JS_ReportErrorUTF8(cx, "Invalid CouchHTTP instance.");
        return false;
    }

    std::string bodystr;
    if(!js_to_string(cx, JS::RootedValue(cx, body), bodystr)) {
        JS_ReportErrorUTF8(cx, "Failed to encode body.");
        return false;
    }

    return go(cx, req, http, bodystr);
}

int
http_status(JSContext* cx, JSObject* req)
{
    HTTPData* http = (HTTPData*) JS_GetPrivate(req);

    if(!http) {
        JS_ReportErrorUTF8(cx, "Invalid CouchHTTP instance.");
        return false;
    }

    return http->last_status;
}

bool
http_uri(JSContext* cx, JSObject* req, couch_args* args, JS::Value* uri_val)
{
    FILE* uri_fp = NULL;
    JSString* uri_str;

    // Default is http://localhost:15986/ when no uri file is specified
    if (!args->uri_file) {
        uri_str = JS_NewStringCopyZ(cx, "http://localhost:15986/");
        *uri_val = JS::StringValue(uri_str);
        JS_SetReservedSlot(req, 0, *uri_val);
        return true;
    }

    // Else check to see if the base url is cached in a reserved slot
    *uri_val = JS_GetReservedSlot(req, 0);
    if (!(*uri_val).isUndefined()) {
        return true;
    }

    // Read the first line of the couch.uri file.
    if(!((uri_fp = fopen(args->uri_file, "r")) &&
         (uri_str = couch_readline(cx, uri_fp)))) {
        JS_ReportErrorUTF8(cx, "Failed to read couch.uri file.");
        goto error;
    }

    fclose(uri_fp);
    *uri_val = JS::StringValue(uri_str);
    JS_SetReservedSlot(req, 0, *uri_val);
    return true;

error:
    if(uri_fp) fclose(uri_fp);
    return false;
}


// Curl Helpers

typedef struct {
    HTTPData*   http;
    JSContext*  cx;
    JSObject*   resp_headers;
    const char* sendbuf;
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

static bool
go(JSContext* cx, JSObject* obj, HTTPData* http, std::string& body)
{
    CurlState state;
    JSString* jsbody;
    bool ret = false;
    JS::Value tmp;
    JS::RootedObject robj(cx, obj);
    JS::RootedValue vobj(cx);


    state.cx = cx;
    state.http = http;

    state.sendbuf = body.c_str();;
    state.sendlen = body.size();
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
        JS_ReportErrorUTF8(cx, "Failed to initialize cURL handle.");
        if(state.recvbuf) JS_free(cx, state.recvbuf);
        return ret;
    }

    tmp = JS_GetReservedSlot(obj, 0);

    std::string referer;
    if(!js_to_string(cx, JS::RootedValue(cx, tmp), referer)) {
        JS_ReportErrorUTF8(cx, "Failed to encode referer.");
        if(state.recvbuf) JS_free(cx, state.recvbuf);
        return ret;
    }
    curl_easy_setopt(HTTP_HANDLE, CURLOPT_REFERER, referer.c_str());

    if(http->method < 0 || http->method > OPTIONS) {
        JS_ReportErrorUTF8(cx, "INTERNAL: Unknown method.");
        if(state.recvbuf) JS_free(cx, state.recvbuf);
          return ret;
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

    if(body.size() > 0) {
        curl_easy_setopt(HTTP_HANDLE, CURLOPT_INFILESIZE, body.size());
    } else {
        curl_easy_setopt(HTTP_HANDLE, CURLOPT_INFILESIZE, 0);
    }

    // curl_easy_setopt(HTTP_HANDLE, CURLOPT_VERBOSE, 1);

    curl_easy_setopt(HTTP_HANDLE, CURLOPT_URL, http->url.c_str());
    curl_easy_setopt(HTTP_HANDLE, CURLOPT_HTTPHEADER, http->req_headers);
    curl_easy_setopt(HTTP_HANDLE, CURLOPT_READDATA, &state);
    curl_easy_setopt(HTTP_HANDLE, CURLOPT_SEEKDATA, &state);
    curl_easy_setopt(HTTP_HANDLE, CURLOPT_WRITEHEADER, &state);
    curl_easy_setopt(HTTP_HANDLE, CURLOPT_WRITEDATA, &state);

    if(curl_easy_perform(HTTP_HANDLE) != 0) {
        JS_ReportErrorUTF8(cx, "Failed to execute HTTP request: %s", ERRBUF);
        if(state.recvbuf) JS_free(cx, state.recvbuf);
        return ret;
    }

    if(!state.resp_headers) {
        JS_ReportErrorUTF8(cx, "Failed to recieve HTTP headers.");
        if(state.recvbuf) JS_free(cx, state.recvbuf);
        return ret;
    }
    tmp = JS::ObjectValue(*state.resp_headers);
    JS::RootedValue rtmp(cx, tmp);

    if(!JS_DefineProperty(
        cx, robj,
        "_headers",
        rtmp,
        JSPROP_READONLY
    )) {
        JS_ReportErrorUTF8(cx, "INTERNAL: Failed to set response headers.");
        if(state.recvbuf) JS_free(cx, state.recvbuf);
        return ret;;
    }

    if(state.recvbuf) {
        state.recvbuf[state.read] = '\0';
        std::string bodystr(state.recvbuf, state.read);
        jsbody = string_to_js(cx, bodystr);
        if(!jsbody) {
            // If we can't decode the body as UTF-8 we forcefully
            // convert it to a string by just forcing each byte
            // to a char16_t.
            jsbody = JS_NewStringCopyN(cx, state.recvbuf, state.read);
            if(!jsbody) {
                if(!JS_IsExceptionPending(cx)) {
                    JS_ReportErrorUTF8(cx, "INTERNAL: Failed to decode body.");
                }
                if(state.recvbuf) JS_free(cx, state.recvbuf);
                return ret;
            }
        }
        tmp = JS::StringValue(jsbody);
    } else {
        tmp = JS_GetEmptyStringValue(cx);
    }

    JS::RootedValue rtmp2(cx, tmp);

    if(!JS_DefineProperty(
        cx, robj,
        "responseText",
        rtmp2,
        JSPROP_READONLY
    )) {
        JS_ReportErrorUTF8(cx, "INTERNAL: Failed to set responseText.");
        if(state.recvbuf) JS_free(cx, state.recvbuf);
        return ret;
    }

    ret = true;
    if(state.recvbuf) JS_free(cx, state.recvbuf);
    return ret;
}

static size_t
send_body(void *ptr, size_t size, size_t nmem, void *data)
{
    CurlState* state = static_cast<CurlState*>(data);
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
    CurlState* state = static_cast<CurlState*>(ptr);
    if(origin != SEEK_SET) return -1;

    state->sent = static_cast<size_t>(offset);
    return static_cast<int>(state->sent);
}

static size_t
recv_header(void *ptr, size_t size, size_t nmem, void *data)
{
    CurlState* state = static_cast<CurlState*>(data);
    char code[4];
    char* header = static_cast<char*>(ptr);
    size_t length = size * nmem;
    JSString* hdr = NULL;
    uint32_t hdrlen;

    if(length > 7 && strncasecmp(header, "HTTP/1.", 7) == 0) {
        if(length < 12) {
            return CURLE_WRITE_ERROR;
        }

        memcpy(code, header+9, 3*sizeof(char));
        code[3] = '\0';
        state->http->last_status = atoi(code);

        state->resp_headers = JS_NewArrayObject(state->cx, 0);
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
    std::string hdrstr(header, length);
    hdr = string_to_js(state->cx, hdrstr);
    if(!hdr) {
        return CURLE_WRITE_ERROR;
    }

    JS::RootedObject obj(state->cx, state->resp_headers);
    if(!JS_GetArrayLength(state->cx, obj, &hdrlen)) {
        return CURLE_WRITE_ERROR;
    }

    JS::RootedString hdrval(state->cx, hdr);
    if(!JS_SetElement(state->cx, obj, hdrlen, hdrval)) {
        return CURLE_WRITE_ERROR;
    }

    return length;
}

static size_t
recv_body(void *ptr, size_t size, size_t nmem, void *data)
{
    CurlState* state = static_cast<CurlState*>(data);
    size_t length = size * nmem;
    char* tmp = NULL;

    if(!state->recvbuf) {
        state->recvlen = 4096;
        state->read = 0;
        state->recvbuf = static_cast<char*>(JS_malloc(
                state->cx,
                state->recvlen
            ));
    }

    if(!state->recvbuf) {
        return CURLE_WRITE_ERROR;
    }

    // +1 so we can add '\0' back up in the go function.
    size_t oldlen = state->recvlen;
    while(length+1 > state->recvlen - state->read) state->recvlen *= 2;
    tmp = static_cast<char*>(JS_realloc(
            state->cx,
            state->recvbuf,
            oldlen,
            state->recvlen
        ));
    if(!tmp) return CURLE_WRITE_ERROR;
    state->recvbuf = tmp;

    memcpy(state->recvbuf + state->read, ptr, length);
    state->read += length;
    return length;
}

#endif /* HAVE_CURL */
