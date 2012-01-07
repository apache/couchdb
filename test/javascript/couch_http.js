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

(function() {
  if(typeof(CouchHTTP) != "undefined") {
    CouchHTTP.prototype.open = function(method, url, async) {
      if(!/^\s*http:\/\//.test(url)) {
        if(/^\//.test(url)) {
          // The couch.uri file (base_url) has a trailing slash
          url = this.base_url + url.slice(1);
        } else {
          url = this.base_url + url;
        }
      }
      
      return this._open(method, url, async);
    };
    
    CouchHTTP.prototype.setRequestHeader = function(name, value) {
      // Drop content-length headers because cURL will set it for us
      // based on body length
      if(name.toLowerCase().replace(/^\s+|\s+$/g, '') != "content-length") {
        this._setRequestHeader(name, value);
      }
    }
    
    CouchHTTP.prototype.send = function(body) {
      this._send(body || "");
      var headers = {};
      this._headers.forEach(function(hdr) {
          var pair = hdr.split(":");
          var name = pair.shift();
          headers[name] = pair.join(":").replace(/^\s+|\s+$/g, "");
      });
      this.headers = headers;
    };

    CouchHTTP.prototype.getResponseHeader = function(name) {
      for(var hdr in this.headers) {
        if(hdr.toLowerCase() == name.toLowerCase()) {
          return this.headers[hdr];
        }
      }
      return null;
    };
  }
})();

CouchDB.urlPrefix = "";
CouchDB.newXhr = function() {
  return new CouchHTTP();
};

CouchDB.xhrheader = function(xhr, header) {
  if(typeof(xhr) == "CouchHTTP") {
    return xhr.getResponseHeader(header);
  } else {
    return xhr.headers[header];
  }
}

CouchDB.xhrbody = function(xhr) {
  return xhr.responseText || xhr.body;
}
