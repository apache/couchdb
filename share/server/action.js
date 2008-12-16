// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License.  You may obtain a copy
// of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
// License for the specific language governing permissions and limitations under
// the License.

// JSON from json2.js
/*
    json2.js
    2008-03-14

    Public Domain

    No warranty expressed or implied. Use at your own risk.

    See http://www.JSON.org/js.html

    This is a reference implementation. You are free to copy, modify, or
    redistribute.

    Use your own copy. It is extremely unwise to load third party
    code into your pages.
*/

var JSON = (function () {

    function f(n) {    // Format integers to have at least two digits.
        return n < 10 ? '0' + n : n;
    }

    Date.prototype.toJSON = function () {
        return this.getUTCFullYear()   + '-' +
             f(this.getUTCMonth() + 1) + '-' +
             f(this.getUTCDate())      + 'T' +
             f(this.getUTCHours())     + ':' +
             f(this.getUTCMinutes())   + ':' +
             f(this.getUTCSeconds())   + 'Z';
    };


    var m = {    // table of character substitutions
        '\b': '\\b',
        '\t': '\\t',
        '\n': '\\n',
        '\f': '\\f',
        '\r': '\\r',
        '"' : '\\"',
        '\\': '\\\\'
    };

    function stringify(value, whitelist) {
        var a,          // The array holding the partial texts.
            i,          // The loop counter.
            k,          // The member key.
            l,          // Length.
            r = /["\\\x00-\x1f\x7f-\x9f]/g,
            v;          // The member value.

        switch (typeof value) {
        case 'string':

            return r.test(value) ?
                '"' + value.replace(r, function (a) {
                    var c = m[a];
                    if (c) {
                        return c;
                    }
                    c = a.charCodeAt();
                    return '\\u00' + Math.floor(c / 16).toString(16) +
                                               (c % 16).toString(16);
                }) + '"' :
                '"' + value + '"';

        case 'number':

            return isFinite(value) ? String(value) : 'null';

        case 'boolean':
        case 'null':
            return String(value);

        case 'object':

            if (!value) {
                return 'null';
            }

            if (typeof value.toJSON === 'function') {
                return stringify(value.toJSON());
            }
            a = [];
            if (typeof value.length === 'number' &&
                    !(value.propertyIsEnumerable('length'))) {

                l = value.length;
                for (i = 0; i < l; i += 1) {
                    a.push(stringify(value[i], whitelist) || 'null');
                }

                return '[' + a.join(',') + ']';
            }
            if (whitelist) {
                l = whitelist.length;
                for (i = 0; i < l; i += 1) {
                    k = whitelist[i];
                    if (typeof k === 'string') {
                        v = stringify(value[k], whitelist);
                        if (v) {
                            a.push(stringify(k) + ':' + v);
                        }
                    }
                }
            } else {

                for (k in value) {
                    if (typeof k === 'string') {
                        v = stringify(value[k], whitelist);
                        if (v) {
                            a.push(stringify(k) + ':' + v);
                        }
                    }
                }
            }

            return '{' + a.join(',') + '}';
        }
    }

    return {
        stringify: stringify,
        parse: function (text, filter) {
            var j;

            function walk(k, v) {
                var i, n;
                if (v && typeof v === 'object') {
                    for (i in v) {
                        if (Object.prototype.hasOwnProperty.apply(v, [i])) {
                            n = walk(i, v[i]);
                            if (n !== undefined) {
                                v[i] = n;
                            } else {
                                delete v[i];
                            }
                        }
                    }
                }
                return filter(k, v);
            }

            if (/^[\],:{}\s]*$/.test(text.replace(/\\["\\\/bfnrtu]/g, '@').
replace(/"[^"\\\n\r]*"|true|false|null|-?\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?/g, ']').
replace(/(?:^|:|,)(?:\s*\[)+/g, ''))) {

                j = eval('(' + text + ')');

                return typeof filter === 'function' ? walk('', j) : j;
            }

            throw new SyntaxError('parseJSON');
        }
    };
})();

// HTTP object provides access to curl via couchjs.

var HTTP = (function() {
  function parseCurl(string) {
    var parts = string.split(/\r\n\r\n/);
    var body = parts.pop();
    var header = parts.pop();
    var headers = header.split(/\n/);
    
    var status = /HTTP\/1.\d (\d*)/.exec(header)[1];
    return {
      responseText: body,
      status: parseInt(status),
      getResponseHeader: function(key) {
        var keymatcher = new RegExp(RegExp.escape(key), "i");
        for (var i in headers) {
          var h = headers[i];
          if (keymatcher.test(h)) {
            var value = h.substr(key.length+2);
            value = value.slice(0, value.length-1);
            return value;
          }
        }
        return "";
      }
    }
  };
  return {
    GET : function(url, body, headers) {
      var st, urx = url, hx = (headers || null);
      st = gethttp(urx, hx);
      return parseCurl(st);
    },
    HEAD : function(url, body, headers) {
      var st, urx = url, hx = (headers || null);
      st = headhttp(urx, hx);
      return parseCurl(st);      
    },
    DELETE : function(url, body, headers) {
      var st, urx = url, hx = (headers || null);
      st = delhttp(urx, hx);
      return parseCurl(st);
    },
    MOVE : function(url, body, headers) {
      var st, urx = url, hx = (headers || null);
      st = movehttp(urx, hx);
      return parseCurl(st);
    },
    COPY : function(url, body, headers) {
      var st, urx = url, hx = (headers || null);
      st = copyhttp(urx, hx);
      return parseCurl(st);
    },
    POST : function(url, body, headers) {
      var st, urx = url, bx = (body || ""), hx = (headers || {});
      hx['Content-Type'] = hx['Content-Type'] || "application/json";
      st = posthttp(urx, bx, hx);
      return parseCurl(st);
    },
    PUT : function(url, body, headers) {
      var st, urx = url, bx = (body || ""), hx = (headers || {});
      hx['Content-Type'] = hx['Content-Type'] || "application/json";
      st = puthttp(urx, bx, hx);
      return parseCurl(st);
    }
  };
})();


// couch.js

// A simple class to represent a database. Uses XMLHttpRequest to interface with
// the CouchDB server.

function CouchDB(name) {
  this.name = name
  this.uri = "/" + encodeURIComponent(name) + "/";
  request = CouchDB.request;

  // Creates the database on the server
  this.createDb = function() {
    var req = request("PUT", this.uri);
    var result = JSON.parse(req.responseText);
    if (req.status != 201)
      throw result;
    return result;
  }

  // Deletes the database on the server
  this.deleteDb = function() {
    var req = request("DELETE", this.uri);
    if (req.status == 404)
      return false;
    var result = JSON.parse(req.responseText);
    if (req.status != 200)
      throw result;
    return result;
  }

  // Save a document to the database
  this.save = function(doc, options) {
    var req;
    if (doc._id == undefined)
      doc._id = CouchDB.newUuids(1)[0];

    req = request("PUT", this.uri  + encodeURIComponent(doc._id) + encodeOptions(options), {
      body: JSON.stringify(doc)
    });
    var result = JSON.parse(req.responseText);
    if (req.status != 201)
      throw result;
    doc._rev = result.rev;
    return result;
  }

  // Open a document from the database
  this.open = function(docId, options) {
    var req = request("GET", this.uri + encodeURIComponent(docId) + encodeOptions(options));
    if (req.status == 404)
      return null;
    var result = JSON.parse(req.responseText);
    if (req.status != 200)
      throw result;
    return result;
  }

  // Deletes a document from the database
  this.deleteDoc = function(doc) {
    var req = request("DELETE", this.uri + encodeURIComponent(doc._id) + "?rev=" + doc._rev);
    var result = JSON.parse(req.responseText);
    if (req.status != 200)
      throw result;
    doc._rev = result.rev; //record rev in input document
    doc._deleted = true;
    return result;
  }

  // Deletes an attachment from a document
  this.deleteDocAttachment = function(doc, attachment_name) {
    var req = request("DELETE", this.uri + encodeURIComponent(doc._id) + "/" + attachment_name + "?rev=" + doc._rev);
    var result = JSON.parse(req.responseText);
    if (req.status != 200)
      throw result;
    doc._rev = result.rev; //record rev in input document
    return result;
  }
  
  this.bulkSave = function(docs, options) {
    // first prepoulate the UUIDs for new documents
    var newCount = 0
    for (var i=0; i<docs.length; i++) {
      if (docs[i]._id == undefined)
        newCount++;
    }
    var newUuids = CouchDB.newUuids(docs.length);
    var newCount = 0
    for (var i=0; i<docs.length; i++) {
      if (docs[i]._id == undefined)
        docs[i]._id = newUuids.pop();
    }
    var req = request("POST", this.uri + "_bulk_docs" + encodeOptions(options), {
      body: JSON.stringify({"docs": docs})
    });
    var result = JSON.parse(req.responseText);
    if (req.status != 201)
      throw result;
    for (var i = 0; i < docs.length; i++) {
        docs[i]._rev = result.new_revs[i].rev;
    }
    return result;
  }

  // Applies the map function to the contents of database and returns the results.
  this.query = function(mapFun, reduceFun, options, keys) {
    var body = {language: "javascript"};
    if(keys) {
      body.keys = keys ;      
    }
    if (typeof(mapFun) != "string")
      mapFun = mapFun.toSource ? mapFun.toSource() : "(" + mapFun.toString() + ")";
    body.map = mapFun;
    if (reduceFun != null) {
      if (typeof(reduceFun) != "string")
        reduceFun = reduceFun.toSource ? reduceFun.toSource() : "(" + reduceFun.toString() + ")";
      body.reduce = reduceFun;
    }
    var req = request("POST", this.uri + "_temp_view" + encodeOptions(options), {
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify(body)
    });
    var result = JSON.parse(req.responseText);
    if (req.status != 200)
      throw result;
    return result;
  }

  this.view = function(viewname, options, keys) {
    var req = null ;
    if(!keys) {
      req = request("GET", this.uri + "_view/" + viewname + encodeOptions(options));      
    } else {
      req = request("POST", this.uri + "_view/" + viewname + encodeOptions(options), {
        headers: {"Content-Type": "application/json"},
        body: JSON.stringify({keys:keys})
      });      
    }
    if (req.status == 404)
      return null;
    var result = JSON.parse(req.responseText);
    if (req.status != 200)
      throw result;
    return result;
  }

  // gets information about the database
  this.info = function() {
    var req = request("GET", this.uri);
    var result = JSON.parse(req.responseText);
    if (req.status != 200)
      throw result;
    return result;
  }

  this.allDocs = function(options,keys) {
    var req = null;
    if(!keys) {
      req = request("GET", this.uri + "_all_docs" + encodeOptions(options));      
    } else {
      req = request("POST", this.uri + "_all_docs" + encodeOptions(options), {
        headers: {"Content-Type": "application/json"},
        body: JSON.stringify({keys:keys})
      });      
    }
    var result = JSON.parse(req.responseText);
    if (req.status != 200)
      throw result;
    return result;
  }

  this.compact = function() {
    var req = request("POST", this.uri + "_compact");
    var result = JSON.parse(req.responseText);
    if (req.status != 202)
      throw result;
    return result;
  }

  // Convert a options object to an url query string.
  // ex: {key:'value',key2:'value2'} becomes '?key="value"&key2="value2"'
  function encodeOptions(options) {
    var buf = []
    if (typeof(options) == "object" && options !== null) {
      for (var name in options) {
        if (!options.hasOwnProperty(name)) continue;
        var value = options[name];
        if (name == "key" || name == "startkey" || name == "endkey") {
          value = toJSON(value);
        }
        buf.push(encodeURIComponent(name) + "=" + encodeURIComponent(value));
      }
    }
    if (!buf.length) {
      return "";
    }
    return "?" + buf.join("&");
  }

  function toJSON(obj) {
    return obj !== null ? JSON.stringify(obj) : null;
  }
}

CouchDB.allDbs = function() {
  var req = CouchDB.request("GET", "/_all_dbs");
  var result = JSON.parse(req.responseText);
  if (req.status != 200)
    throw result;
  return result;
}

CouchDB.getVersion = function() {
  var req = CouchDB.request("GET", "/");
  var result = JSON.parse(req.responseText);
  if (req.status != 200)
    throw result;
  return result.version;
}

CouchDB.replicate = function(source, target) {
  var req = CouchDB.request("POST", "/_replicate", {
    body: JSON.stringify({source: source, target: target})
  });
  var result = JSON.parse(req.responseText);
  if (req.status != 200)
    throw result;
  return result;
}

CouchDB.uuids_cache = [];

CouchDB.newUuids = function(n) {
  if (CouchDB.uuids_cache.length >= n) {
    var uuids = CouchDB.uuids_cache.slice(CouchDB.uuids_cache.length - n);
    if(CouchDB.uuids_cache.length - n == 0) {
      CouchDB.uuids_cache = [];
    } else {
      CouchDB.uuids_cache =
          CouchDB.uuids_cache.slice(0, CouchDB.uuids_cache.length - n);
    }
    return uuids;
  } else {
    var req = CouchDB.request("POST", "/_uuids?count=" + (100 + n));
    var result = JSON.parse(req.responseText);
    if (req.status != 200)
      throw result;
    CouchDB.uuids_cache =
        CouchDB.uuids_cache.concat(result.uuids.slice(0, 100));
    return result.uuids.slice(100);
  }
}

CouchDB.host = (typeof window == 'undefined' || !window) ? "127.0.0.1:5984" : window;

CouchDB.request = function(method, uri, options) {
  var full_uri = "http://" + CouchDB.host + uri;
  options = options || {};
  var response = HTTP[method](full_uri, options.body, options.headers);
  return response;
}

log = function(message) {
  print(toJSON({log: toJSON(message)}));  
}

var sandbox = null;

try {
  // if possible, use evalcx (not always available)
  sandbox = evalcx('');
  sandbox.JSON = JSON;
  sandbox.HTTP = HTTP;
  sandbox.CouchDB = CouchDB;
  sandbox.log = log;
} catch (e) {}

function error(httpcode, type, msg) {
  return {code: httpcode , json:{error: type, reason: msg}};
}

function process_request(req) {
  // Req is an object that contains information about the HTTP
  // request made to the action server.
  // Something like: {"verb": "GET", "path": ["foo", "bar"], "query": ..., "post":...}
  // Here we use "path" to fetch the action, compile it, and run it.
  if (req.path.length != 2) {
    return error(404, "action_error", "Invalid path: \"" + req.path.join("/") + "\".");
  }

  var desname = "_design%2F" + req.path[0];
  
  // todo: make this work with other ports and addresses...
  // maybe couchdb can send config information over to the _external server on boot
  
  var desdoc = HTTP.GET("http://127.0.0.1:5984/" + req.info.db_name + "/" + desname);

  if (desdoc.status != 200) {
    return error(404, "action_error", "Design document '" + desname + "' not found.");
  }

  var desjson = JSON.parse(desdoc.responseText);

  if (!desjson.actions) {
    return error(500, "action_error", "No actions found in design doc '" + desname + "'.");
  }

  if (!desjson.actions[req.path[1]]) {
    return error(500, "action_error", "No action '" + req.path[1] + "' in design doc '" + desname + "'");
  }

  var func = null;
  try {
    // todo - use compileFunction from main.js?
    func = eval(desjson.actions[req.path[1]]);
  } catch(exception) {
    return error(500, "action_error", "Failed to compile action: " + JSON.stringify(exception));
  }

  var ret = null;
  var db = new CouchDB(req.info.db_name);
  return func(req, db);
}

function respond(obj) {
  print(JSON.stringify(obj));
};

var req;
var resp;
while (req = JSON.parse(readline())) {
  try {
    resp = process_request(req);
  } catch(exception) {
    if (exception.code) {
      resp = exception;
    } else if (exception.error && exception.reason) {
      resp = error(500, exception.error, exception.reason);      
    } else {
      resp = error(500, "action_error", JSON.stringify(exception));      
    }
  }
  
  try {
    respond(resp);
  } catch(e) {
    respond(error(500, "json_error", "Error encoding JSON response. Exception: "+e.toString()));
  }
}