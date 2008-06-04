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
      req = request("POST", this.uri + encodeOptions(options), {
        body: JSON.stringify(doc)
      });
    else
      req = request("PUT", this.uri  + encodeURIComponent(doc._id) + encodeOptions(options), {
        body: JSON.stringify(doc)
      });
    var result = JSON.parse(req.responseText);
    if (req.status != 201)
      throw result;
    // set the _id and _rev members on the input object, for caller convenience.
    doc._id = result.id;
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

  this.bulkSave = function(docs, options) {
    var req = request("POST", this.uri + "_bulk_docs" + encodeOptions(options), {
      body: JSON.stringify({"docs": docs})
    });
    var result = JSON.parse(req.responseText);
    if (req.status != 201)
      throw result;
    for (var i = 0; i < docs.length; i++) {
        docs[i]._id = result.new_revs[i].id;
        docs[i]._rev = result.new_revs[i].rev;
    }
    return result;
  }

  // Applies the map function to the contents of database and returns the results.
  this.query = function(mapFun, reduceFun, options) {
    var body = {language: "javascript"};
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

  this.view = function(viewname, options) {
    var req = request("GET", this.uri + "_view/" + viewname + encodeOptions(options));
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

  this.allDocs = function(options) {
    var req = request("GET", this.uri + "_all_docs" + encodeOptions(options));
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

CouchDB.request = function(method, uri, options) {
  options = options || {};
  var req = null;
  if (typeof(XMLHttpRequest) != "undefined") {
    req = new XMLHttpRequest();
  } else if (typeof(ActiveXObject) != "undefined") {
    req = new ActiveXObject("Microsoft.XMLHTTP");
  } else {
    throw new Error("No XMLHTTPRequest support detected");
  }
  req.open(method, uri, false);
  if (options.headers) {
    var headers = options.headers;
    for (var headerName in headers) {
      if (!headers.hasOwnProperty(headerName)) continue;
      req.setRequestHeader(headerName, headers[headerName]);
    }
  }
  req.send(options.body || "");
  return req;
}
