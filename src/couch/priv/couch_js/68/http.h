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

#ifndef COUCH_JS_HTTP_H
#define COUCH_JS_HTTP_H

#include "util.h"

void http_check_enabled();
bool http_ctor(JSContext* cx, JSObject* req);
void http_dtor(JSFreeOp* fop, JSObject* req);
bool http_open(JSContext* cx, JSObject* req, JS::Value mth, JS::Value url, JS::Value snc);
bool http_set_hdr(JSContext* cx, JSObject* req, JS::Value name, JS::Value val);
bool http_send(JSContext* cx, JSObject* req, JS::Value body);
int http_status(JSContext* cx, JSObject* req);
bool http_uri(JSContext* cx, JSObject *req, couch_args* args, JS::Value* uri);

#endif
