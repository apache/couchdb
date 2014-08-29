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

// A simple class to represent a database. Uses XMLHttpRequest to interface with
// the CouchDB server.

function CouchDB(name, httpHeaders) {
  this.name = name;
  this.uri = "/" + encodeURIComponent(name) + "/";

  // The XMLHttpRequest object from the most recent request. Callers can
  // use this to check result http status and headers.
  this.last_req = null;

  this.request = function(method, uri, requestOptions) {
    requestOptions = requestOptions || {};
    requestOptions.headers = combine(requestOptions.headers, httpHeaders);
    return CouchDB.request(method, uri, requestOptions);
  };

  // Creates the database on the server
  this.createDb = function() {
    this.last_req = this.request("PUT", this.uri);
    CouchDB.maybeThrowError(this.last_req);
    return JSON.parse(this.last_req.responseText);
  };

  // Deletes the database on the server
  this.deleteDb = function() {
    this.last_req = this.request("DELETE", this.uri + "?sync=true");
    if (this.last_req.status == 404) {
      return false;
    }
    CouchDB.maybeThrowError(this.last_req);
    return JSON.parse(this.last_req.responseText);
  };

  // Save a document to the database
  this.save = function(doc, options, http_headers) {
    if (doc._id == undefined) {
      doc._id = CouchDB.newUuids(1)[0];
    }
    http_headers = http_headers || {};
    this.last_req = this.request("PUT", this.uri  +
        encodeURIComponent(doc._id) + encodeOptions(options),
        {body: JSON.stringify(doc), headers: http_headers});
    CouchDB.maybeThrowError(this.last_req);
    var result = JSON.parse(this.last_req.responseText);
    doc._rev = result.rev;
    return result;
  };

  // Open a document from the database
  this.open = function(docId, url_params, http_headers) {
    this.last_req = this.request("GET", this.uri + encodeURIComponent(docId)
      + encodeOptions(url_params), {headers:http_headers});
    if (this.last_req.status == 404) {
      return null;
    }
    CouchDB.maybeThrowError(this.last_req);
    return JSON.parse(this.last_req.responseText);
  };

  // Deletes a document from the database
  this.deleteDoc = function(doc) {
    this.last_req = this.request("DELETE", this.uri + encodeURIComponent(doc._id)
      + "?rev=" + doc._rev);
    CouchDB.maybeThrowError(this.last_req);
    var result = JSON.parse(this.last_req.responseText);
    doc._rev = result.rev; //record rev in input document
    doc._deleted = true;
    return result;
  };

  // Deletes an attachment from a document
  this.deleteDocAttachment = function(doc, attachment_name) {
    this.last_req = this.request("DELETE", this.uri + encodeURIComponent(doc._id)
      + "/" + attachment_name + "?rev=" + doc._rev);
    CouchDB.maybeThrowError(this.last_req);
    var result = JSON.parse(this.last_req.responseText);
    doc._rev = result.rev; //record rev in input document
    return result;
  };

  this.bulkSave = function(docs, options) {
    // first prepoulate the UUIDs for new documents
    var newCount = 0;
    for (var i=0; i<docs.length; i++) {
      if (docs[i]._id == undefined) {
        newCount++;
      }
    }
    var newUuids = CouchDB.newUuids(newCount);
    var newCount = 0;
    for (var i=0; i<docs.length; i++) {
      if (docs[i]._id == undefined) {
        docs[i]._id = newUuids.pop();
      }
    }
    var json = {"docs": docs};
    // put any options in the json
    for (var option in options) {
      json[option] = options[option];
    }
    this.last_req = this.request("POST", this.uri + "_bulk_docs", {
      body: JSON.stringify(json)
    });
    if (this.last_req.status == 417) {
      return {errors: JSON.parse(this.last_req.responseText)};
    }
    else {
      CouchDB.maybeThrowError(this.last_req);
      var results = JSON.parse(this.last_req.responseText);
      for (var i = 0; i < docs.length; i++) {
        if(results[i] && results[i].rev && results[i].ok) {
          docs[i]._rev = results[i].rev;
        }
      }
      return results;
    }
  };

  this.ensureFullCommit = function() {
    this.last_req = this.request("POST", this.uri + "_ensure_full_commit");
    CouchDB.maybeThrowError(this.last_req);
    return JSON.parse(this.last_req.responseText);
  };

  // Applies the map function to the contents of database and returns the results.
  this.query = function(mapFun, reduceFun, options, keys, language) {
    var body = {language: language || "javascript"};
    if(keys) {
      body.keys = keys ;
    }
    if (typeof(mapFun) != "string") {
      mapFun = mapFun.toSource ? mapFun.toSource() : "(" + mapFun.toString() + ")";
    }
    body.map = mapFun;
    if (reduceFun != null) {
      if (typeof(reduceFun) != "string") {
        reduceFun = reduceFun.toSource ?
          reduceFun.toSource() : "(" + reduceFun.toString() + ")";
      }
      body.reduce = reduceFun;
    }
    if (options && options.options != undefined) {
        body.options = options.options;
        delete options.options;
    }
    this.last_req = this.request("POST", this.uri + "_temp_view"
      + encodeOptions(options), {
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify(body)
    });
    CouchDB.maybeThrowError(this.last_req);
    return JSON.parse(this.last_req.responseText);
  };

  this.view = function(viewname, options, keys) {
    var viewParts = viewname.split('/');
    var viewPath = this.uri + "_design/" + viewParts[0] + "/_view/"
        + viewParts[1] + encodeOptions(options);
    if(!keys) {
      this.last_req = this.request("GET", viewPath);
    } else {
      this.last_req = this.request("POST", viewPath, {
        headers: {"Content-Type": "application/json"},
        body: JSON.stringify({keys:keys})
      });
    }
    if (this.last_req.status == 404) {
      return null;
    }
    CouchDB.maybeThrowError(this.last_req);
    return JSON.parse(this.last_req.responseText);
  };

  // gets information about the database
  this.info = function() {
    this.last_req = this.request("GET", this.uri);
    CouchDB.maybeThrowError(this.last_req);
    return JSON.parse(this.last_req.responseText);
  };

  // gets information about a design doc
  this.designInfo = function(docid) {
    this.last_req = this.request("GET", this.uri + docid + "/_info");
    CouchDB.maybeThrowError(this.last_req);
    return JSON.parse(this.last_req.responseText);
  };

  this.allDocs = function(options,keys) {
    if(!keys) {
      this.last_req = this.request("GET", this.uri + "_all_docs"
        + encodeOptions(options));
    } else {
      this.last_req = this.request("POST", this.uri + "_all_docs"
        + encodeOptions(options), {
        headers: {"Content-Type": "application/json"},
        body: JSON.stringify({keys:keys})
      });
    }
    CouchDB.maybeThrowError(this.last_req);
    return JSON.parse(this.last_req.responseText);
  };

  this.designDocs = function() {
    return this.allDocs({startkey:"_design", endkey:"_design0"});
  };

  this.changes = function(options) {
    this.last_req = this.request("GET", this.uri + "_changes"
      + encodeOptions(options));
    CouchDB.maybeThrowError(this.last_req);
    return JSON.parse(this.last_req.responseText);
  };

  this.compact = function() {
    this.last_req = this.request("POST", this.uri + "_compact");
    CouchDB.maybeThrowError(this.last_req);
    return JSON.parse(this.last_req.responseText);
  };

  this.viewCleanup = function() {
    this.last_req = this.request("POST", this.uri + "_view_cleanup");
    CouchDB.maybeThrowError(this.last_req);
    return JSON.parse(this.last_req.responseText);
  };

  this.setDbProperty = function(propId, propValue) {
    this.last_req = this.request("PUT", this.uri + propId,{
      body:JSON.stringify(propValue)
    });
    CouchDB.maybeThrowError(this.last_req);
    return JSON.parse(this.last_req.responseText);
  };

  this.getDbProperty = function(propId) {
    this.last_req = this.request("GET", this.uri + propId);
    CouchDB.maybeThrowError(this.last_req);
    return JSON.parse(this.last_req.responseText);
  };

  this.setSecObj = function(secObj) {
    this.last_req = this.request("PUT", this.uri + "_security",{
      body:JSON.stringify(secObj)
    });
    CouchDB.maybeThrowError(this.last_req);
    return JSON.parse(this.last_req.responseText);
  };

  this.getSecObj = function() {
    this.last_req = this.request("GET", this.uri + "_security");
    CouchDB.maybeThrowError(this.last_req);
    return JSON.parse(this.last_req.responseText);
  };

  // Convert a options object to an url query string.
  // ex: {key:'value',key2:'value2'} becomes '?key="value"&key2="value2"'
  function encodeOptions(options) {
    var buf = [];
    if (typeof(options) == "object" && options !== null) {
      for (var name in options) {
        if (!options.hasOwnProperty(name)) { continue; };
        var value = options[name];
        if (name == "key" || name == "keys" || name == "startkey" || name == "endkey" || (name == "open_revs" && value !== "all")) {
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

  function combine(object1, object2) {
    if (!object2) {
      return object1;
    }
    if (!object1) {
      return object2;
    }

    for (var name in object2) {
      object1[name] = object2[name];
    }
    return object1;
  }

}

// this is the XMLHttpRequest object from last request made by the following
// CouchDB.* functions (except for calls to request itself).
// Use this from callers to check HTTP status or header values of requests.
CouchDB.last_req = null;
CouchDB.urlPrefix = '';

CouchDB.login = function(name, password) {
  CouchDB.last_req = CouchDB.request("POST", "/_session", {
    headers: {"Content-Type": "application/x-www-form-urlencoded",
      "X-CouchDB-WWW-Authenticate": "Cookie"},
    body: "name=" + encodeURIComponent(name) + "&password="
      + encodeURIComponent(password)
  });
  return JSON.parse(CouchDB.last_req.responseText);
}

CouchDB.logout = function() {
  CouchDB.last_req = CouchDB.request("DELETE", "/_session", {
    headers: {"Content-Type": "application/x-www-form-urlencoded",
      "X-CouchDB-WWW-Authenticate": "Cookie"}
  });
  return JSON.parse(CouchDB.last_req.responseText);
};

CouchDB.session = function(options) {
  options = options || {};
  CouchDB.last_req = CouchDB.request("GET", "/_session", options);
  CouchDB.maybeThrowError(CouchDB.last_req);
  return JSON.parse(CouchDB.last_req.responseText);
};

CouchDB.allDbs = function() {
  CouchDB.last_req = CouchDB.request("GET", "/_all_dbs");
  CouchDB.maybeThrowError(CouchDB.last_req);
  return JSON.parse(CouchDB.last_req.responseText);
};

CouchDB.allDesignDocs = function() {
  var ddocs = {}, dbs = CouchDB.allDbs();
  for (var i=0; i < dbs.length; i++) {
    var db = new CouchDB(dbs[i]);
    ddocs[dbs[i]] = db.designDocs();
  };
  return ddocs;
};

CouchDB.getVersion = function() {
  CouchDB.last_req = CouchDB.request("GET", "/");
  CouchDB.maybeThrowError(CouchDB.last_req);
  return JSON.parse(CouchDB.last_req.responseText).version;
};

CouchDB.reloadConfig = function() {
  CouchDB.last_req = CouchDB.request("POST", "/_config/_reload");
  CouchDB.maybeThrowError(CouchDB.last_req);
  return JSON.parse(CouchDB.last_req.responseText);
};

CouchDB.replicate = function(source, target, rep_options) {
  rep_options = rep_options || {};
  var headers = rep_options.headers || {};
  var body = rep_options.body || {};
  body.source = source;
  body.target = target;
  CouchDB.last_req = CouchDB.request("POST", "/_replicate", {
    headers: headers,
    body: JSON.stringify(body)
  });
  CouchDB.maybeThrowError(CouchDB.last_req);
  return JSON.parse(CouchDB.last_req.responseText);
};

CouchDB.newXhr = function() {
  if (typeof(XMLHttpRequest) != "undefined") {
    return new XMLHttpRequest();
  } else if (typeof(ActiveXObject) != "undefined") {
    return new ActiveXObject("Microsoft.XMLHTTP");
  } else {
    throw new Error("No XMLHTTPRequest support detected");
  }
};

CouchDB.xhrbody = function(xhr) {
  if (xhr.responseText) {
    return xhr.responseText;
  } else if (xhr.body) {
    return xhr.body
  } else {
    throw new Error("No XMLHTTPRequest support detected");
  }
}

CouchDB.xhrheader = function(xhr, header) {
  if(xhr.getResponseHeader) {
    return xhr.getResponseHeader(header);
  } else if(xhr.headers) {
    return xhr.headers[header] || null;
  } else {
    throw new Error("No XMLHTTPRequest support detected");
  }
}

CouchDB.proxyUrl = function(uri) {
  if(uri.substr(0, CouchDB.protocol.length) != CouchDB.protocol) {
    uri = CouchDB.urlPrefix + uri;
  }
  return uri;
}

CouchDB.request = function(method, uri, options) {
  options = typeof(options) == 'object' ? options : {};
  options.headers = typeof(options.headers) == 'object' ? options.headers : {};
  options.headers["Content-Type"] = options.headers["Content-Type"] || options.headers["content-type"] || "application/json";
  options.headers["Accept"] = options.headers["Accept"] || options.headers["accept"] || "application/json";
  var req = CouchDB.newXhr();
  uri = CouchDB.proxyUrl(uri);
  req.open(method, uri, false);
  if (options.headers) {
    var headers = options.headers;
    for (var headerName in headers) {
      if (!headers.hasOwnProperty(headerName)) { continue; }
      req.setRequestHeader(headerName, headers[headerName]);
    }
  }
  req.send(options.body || "");
  return req;
};

CouchDB.requestStats = function(path, test) {
  var query_arg = "";
  if(test !== null) {
    query_arg = "?flush=true";
  }

  var url = "/_stats/" + path.join("/") + query_arg;
  var stat = CouchDB.request("GET", url).responseText;
  return JSON.parse(stat);
};

CouchDB.uuids_cache = [];

CouchDB.newUuids = function(n, buf) {
  buf = buf || 100;
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
    CouchDB.last_req = CouchDB.request("GET", "/_uuids?count=" + (buf + n));
    CouchDB.maybeThrowError(CouchDB.last_req);
    var result = JSON.parse(CouchDB.last_req.responseText);
    CouchDB.uuids_cache =
        CouchDB.uuids_cache.concat(result.uuids.slice(0, buf));
    return result.uuids.slice(buf);
  }
};

CouchDB.maybeThrowError = function(req) {
  if (req.status >= 400) {
    try {
      var result = JSON.parse(req.responseText);
    } catch (ParseError) {
      var result = {error:"unknown", reason:req.responseText};
    }

    throw (new CouchError(result));
  }
}

CouchDB.params = function(options) {
  options = options || {};
  var returnArray = [];
  for(var key in options) {
    var value = options[key];
    returnArray.push(key + "=" + value);
  }
  return returnArray.join("&");
};
// Used by replication test
if (typeof window == 'undefined' || !window) {
  var hostRE = RegExp("https?://([^\/]+)");
  var getter = function () {
    return (new CouchHTTP).base_url.match(hostRE)[1];
  };
  if(Object.defineProperty) {
    Object.defineProperty(CouchDB, "host", {
      get : getter,
      enumerable : true
    });
  } else {
    CouchDB.__defineGetter__("host", getter);
  }
  CouchDB.protocol = "http://";
  CouchDB.inBrowser = false;
} else {
  CouchDB.host = window.location.host;
  CouchDB.inBrowser = true;
  CouchDB.protocol = window.location.protocol + "//";
}

// Turns an {error: ..., reason: ...} response into an Error instance
function CouchError(error) {
  var inst = new Error(error.reason);
  inst.name = 'CouchError';
  inst.error = error.error;
  inst.reason = error.reason;
  return inst;
}
CouchError.prototype.constructor = CouchError;
