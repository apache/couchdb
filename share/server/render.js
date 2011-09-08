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


var Mime = (function() {
  // registerType(name, mime-type, mime-type, ...)
  // 
  // Available in query server sandbox. TODO: The list is cleared on reset.
  // This registers a particular name with the set of mimetypes it can handle.
  // Whoever registers last wins.
  // 
  // Example: 
  // registerType("html", "text/html; charset=utf-8");

  var mimesByKey = {};
  var keysByMime = {};
  function registerType() {
    var mimes = [], key = arguments[0];
    for (var i=1; i < arguments.length; i++) {
      mimes.push(arguments[i]);
    };
    mimesByKey[key] = mimes;
    for (var i=0; i < mimes.length; i++) {
      keysByMime[mimes[i]] = key;
    };
  }

  // Some default types
  // Ported from Ruby on Rails
  // Build list of Mime types for HTTP responses
  // http://www.iana.org/assignments/media-types/
  // http://dev.rubyonrails.org/svn/rails/trunk/actionpack/lib/action_controller/mime_types.rb

  registerType("all", "*/*");
  registerType("text", "text/plain; charset=utf-8", "txt");
  registerType("html", "text/html; charset=utf-8");
  registerType("xhtml", "application/xhtml+xml", "xhtml");
  registerType("xml", "application/xml", "text/xml", "application/x-xml");
  registerType("js", "text/javascript", "application/javascript", "application/x-javascript");
  registerType("css", "text/css");
  registerType("ics", "text/calendar");
  registerType("csv", "text/csv");
  registerType("rss", "application/rss+xml");
  registerType("atom", "application/atom+xml");
  registerType("yaml", "application/x-yaml", "text/yaml");
  // just like Rails
  registerType("multipart_form", "multipart/form-data");
  registerType("url_encoded_form", "application/x-www-form-urlencoded");
  // http://www.ietf.org/rfc/rfc4627.txt
  registerType("json", "application/json", "text/x-json");
  
  
  var mimeFuns = [];
  function provides(type, fun) {
    Mime.providesUsed = true;
    mimeFuns.push([type, fun]);
  };

  function resetProvides() {
    // set globals
    Mime.providesUsed = false;
    mimeFuns = [];
    Mime.responseContentType = null;  
  };

  function runProvides(req, ddoc) {
    var supportedMimes = [], bestFun, bestKey = null, accept = req.headers["Accept"];
    if (req.query && req.query.format) {
      bestKey = req.query.format;
      Mime.responseContentType = mimesByKey[bestKey][0];
    } else if (accept) {
      // log("using accept header: "+accept);
      mimeFuns.reverse().forEach(function(mimeFun) {
        var mimeKey = mimeFun[0];
        if (mimesByKey[mimeKey]) {
          supportedMimes = supportedMimes.concat(mimesByKey[mimeKey]);
        }
      });
      Mime.responseContentType = Mimeparse.bestMatch(supportedMimes, accept);
      bestKey = keysByMime[Mime.responseContentType];
    } else {
      // just do the first one
      bestKey = mimeFuns[0][0];
      Mime.responseContentType = mimesByKey[bestKey][0];
    }

    if (bestKey) {
      for (var i=0; i < mimeFuns.length; i++) {
        if (mimeFuns[i][0] == bestKey) {
          bestFun = mimeFuns[i][1];
          break;
        }
      };
    };

    if (bestFun) {
      return bestFun.call(ddoc);
    } else {
      var supportedTypes = mimeFuns.map(function(mf) {
        return mimesByKey[mf[0]].join(', ') || mf[0];
      });
      throw(["error","not_acceptable",
        "Content-Type "+(accept||bestKey)+" not supported, try one of: "+supportedTypes.join(', ')]);
    }
  };

  
  return {
    registerType : registerType,
    provides : provides,
    resetProvides : resetProvides,
    runProvides : runProvides
  };
})();




////
////  Render dispatcher
////
////
////
////

var Render = (function() {
  var chunks = [];
  
  
  //  Start chunks
  var startResp = {};
  function start(resp) {
    startResp = resp || {};
  };

  function sendStart() {
    startResp = applyContentType((startResp || {}), Mime.responseContentType);
    respond(["start", chunks, startResp]);
    chunks = [];
    startResp = {};
  }

  function applyContentType(resp, responseContentType) {
    resp["headers"] = resp["headers"] || {};
    if (responseContentType) {
      resp["headers"]["Content-Type"] = resp["headers"]["Content-Type"] || responseContentType;    
    }
    return resp;
  }

  function send(chunk) {
    chunks.push(chunk.toString());
  };

  function blowChunks(label) {
    respond([label||"chunks", chunks]);
    chunks = [];
  };

  var gotRow = false, lastRow = false;
  function getRow() {
    if (lastRow) return null;
    if (!gotRow) {
      gotRow = true;
      sendStart();
    } else {
      blowChunks();
    }
    var json = JSON.parse(readline());
    if (json[0] == "list_end") {
      lastRow = true;
      return null;
    }
    if (json[0] != "list_row") {
      throw(["fatal", "list_error", "not a row '" + json[0] + "'"]);
    }
    return json[1];
  };

  
  function maybeWrapResponse(resp) {
    var type = typeof resp;
    if ((type == "string") || (type == "xml")) {
      return {body:resp};
    } else {
      return resp;
    }
  };

  // from http://javascript.crockford.com/remedial.html
  function typeOf(value) {
    var s = typeof value;
    if (s === 'object') {
      if (value) {
        if (value instanceof Array) {
          s = 'array';
        }
      } else {
        s = 'null';
      }
    }
    return s;
  };

  function isDocRequestPath(info) {
    var path = info.path;
    return path.length > 5;
  };

  function runShow(fun, ddoc, args) {
    try {
      resetList();
      Mime.resetProvides();
      var resp = fun.apply(ddoc, args) || {};
      resp = maybeWrapResponse(resp);

      // handle list() style API
      if (chunks.length && chunks.length > 0) {
        resp.headers = resp.headers || {};
        for(var header in startResp) {
          resp.headers[header] = startResp[header];
        }
        resp.body = chunks.join("") + (resp.body || "");
        resetList();
      }

      if (Mime.providesUsed) {
        var provided_resp = Mime.runProvides(args[1], ddoc) || {};
        provided_resp = maybeWrapResponse(provided_resp);
        resp.body = (resp.body || "") + chunks.join("");
        resp.body += provided_resp.body || "";
        resp = applyContentType(resp, Mime.responseContentType);
        resetList();
      }

      var type = typeOf(resp);
      if (type == 'object' || type == 'string') {
        respond(["resp", maybeWrapResponse(resp)]);
      } else {
        throw(["error", "render_error", "undefined response from show function"]);      
      }
    } catch(e) {
      if (args[0] === null && isDocRequestPath(args[1])) {
        throw(["error", "not_found", "document not found"]);
      } else {
        renderError(e, fun.toSource());
      }
    }
  };

  function runUpdate(fun, ddoc, args) {
    try {
      var method = args[1].method;
      // for analytics logging applications you might want to remove the next line
      if (method == "GET") throw(["error","method_not_allowed","Update functions do not allow GET"]);
      var result = fun.apply(ddoc, args);
      var doc = result[0];
      var resp = result[1];
      var type = typeOf(resp);
      if (type == 'object' || type == 'string') {
        respond(["up", doc, maybeWrapResponse(resp)]);
      } else {
        throw(["error", "render_error", "undefined response from update function"]);      
      }
    } catch(e) {
      renderError(e, fun.toSource());
    }
  };

  function resetList() {
    gotRow = false;
    lastRow = false;
    chunks = [];
    startResp = {};
  };

  function runList(listFun, ddoc, args) {
    try {
      Mime.resetProvides();
      resetList();
      var head = args[0];
      var req = args[1];
      var tail = listFun.apply(ddoc, args);

      if (Mime.providesUsed) {
        tail = Mime.runProvides(req, ddoc);
      }    
      if (!gotRow) getRow();
      if (typeof tail != "undefined") {
        chunks.push(tail);
      }
      blowChunks("end");
    } catch(e) {
      renderError(e, listFun.toSource());
    }
  };

  function renderError(e, funSrc) {
    if (e.error && e.reason || e[0] == "error" || e[0] == "fatal") {
      throw(e);
    } else {
      var logMessage = "function raised error: "+e.toSource()+" \nstacktrace: "+e.stack;
      log(logMessage);
      throw(["error", "render_error", logMessage]);
    }
  };

  function escapeHTML(string) {
    return string && string.replace(/&/g, "&amp;")
                 .replace(/</g, "&lt;")
                 .replace(/>/g, "&gt;");
  };

  
  return {
    start : start,
    send : send,
    getRow : getRow,
    show : function(fun, ddoc, args) {
      // var showFun = Couch.compileFunction(funSrc);
      runShow(fun, ddoc, args);
    },
    update : function(fun, ddoc, args) {
      // var upFun = Couch.compileFunction(funSrc);
      runUpdate(fun, ddoc, args);
    },
    list : function(fun, ddoc, args) {
      runList(fun, ddoc, args);
    }
  };
})();

// send = Render.send;
// getRow = Render.getRow;
// start = Render.start;

// unused. this will be handled in the Erlang side of things.
// function htmlRenderError(e, funSrc) {
//   var msg = ["<html><body><h1>Render Error</h1>",
//     "<p>JavaScript function raised error: ",
//     e.toString(),
//     "</p><h2>Stacktrace:</h2><code><pre>",
//     escapeHTML(e.stack),
//     "</pre></code><h2>Function source:</h2><code><pre>",
//     escapeHTML(funSrc),
//     "</pre></code></body></html>"].join('');
//   return {body:msg};
// };
