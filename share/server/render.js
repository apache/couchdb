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


// registerType(name, mime-type, mime-type, ...)
// 
// Available in query server sandbox. TODO: The list is cleared on reset.
// This registers a particular name with the set of mimetypes it can handle.
// Whoever registers last wins.
// 
// Example: 
// registerType("html", "text/html; charset=utf-8");

mimesByKey = {};
keysByMime = {};
registerType = function() {
  var mimes = [], key = arguments[0];
  for (var i=1; i < arguments.length; i++) {
    mimes.push(arguments[i]);
  };
  mimesByKey[key] = mimes;
  for (var i=0; i < mimes.length; i++) {
    keysByMime[mimes[i]] = key;
  };
};

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

//  Start chunks
var startResp = {};
function start(resp) {
  startResp = resp || {};
};

function sendStart() {
  startResp = applyContentType((startResp || {}), responseContentType);
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

//  Send chunk
var chunks = [];
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
  var line = readline();
  var json = eval(line);
  if (json[0] == "list_end") {
    lastRow = true;
    return null;
  }
  if (json[0] != "list_row") {
    respond({
      error: "query_server_error",
      reason: "not a row '" + json[0] + "'"});
    quit();
  }
  return json[1];
};

var mimeFuns = [], providesUsed, responseContentType;
function provides(type, fun) {
  providesUsed = true;
  mimeFuns.push([type, fun]);
};

function runProvides(req) {
  var supportedMimes = [], bestFun, bestKey = null, accept = req.headers["Accept"];
  if (req.query && req.query.format) {
    bestKey = req.query.format;
    responseContentType = mimesByKey[bestKey][0];
  } else if (accept) {
    // log("using accept header: "+accept);
    mimeFuns.reverse().forEach(function(mimeFun) {
      var mimeKey = mimeFun[0];
      if (mimesByKey[mimeKey]) {
        supportedMimes = supportedMimes.concat(mimesByKey[mimeKey]);
      }
    });
    responseContentType = Mimeparse.bestMatch(supportedMimes, accept);
    bestKey = keysByMime[responseContentType];
  } else {
    // just do the first one
    bestKey = mimeFuns[0][0];
    responseContentType = mimesByKey[bestKey][0];
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
    // log("responding with: "+bestKey);
    return bestFun();
  } else {
    var supportedTypes = mimeFuns.map(function(mf) {return mimesByKey[mf[0]].join(', ') || mf[0]});
    throw({error:"not_acceptable", reason:"Content-Type "+(accept||bestKey)+" not supported, try one of: "+supportedTypes.join(', ')});
  }
};



////
////  Render dispatcher
////
////
////
////

var Render = {
  show : function(funSrc, doc, req) {
    var showFun = compileFunction(funSrc);
    runShow(showFun, doc, req, funSrc);
  },
  update : function(funSrc, doc, req) {
    var upFun = compileFunction(funSrc);
    runUpdate(upFun, doc, req, funSrc);
  },
  list : function(head, req) {
    runList(funs[0], head, req, funsrc[0]);
  }
};

function maybeWrapResponse(resp) {
  var type = typeof resp;
  if ((type == "string") || (type == "xml")) {
    return {body:resp};
  } else {
    return resp;
  }
};

function resetProvides() {
  // set globals
  providesUsed = false;
  mimeFuns = [];
  responseContentType = null;  
};

function runShow(showFun, doc, req, funSrc) {
  try {
    resetProvides();
    var resp = showFun.apply(null, [doc, req]);
    
    if (providesUsed) {
      resp = runProvides(req);
      resp = applyContentType(maybeWrapResponse(resp), responseContentType);
    }
    
    if (resp) {
      respond(["resp", maybeWrapResponse(resp)]);
    } else {
      renderError("undefined response from show function");
    }
  } catch(e) {
    respondError(e, funSrc, true);
  }
};

function runUpdate(renderFun, doc, req, funSrc) {
  try {
    var result = renderFun.apply(null, [doc, req]);
    var doc = result[0];
    var resp = result[1];
    if (resp) {
      respond(["up", doc, maybeWrapResponse(resp)]);
    } else {
      renderError("undefined response from update function");
    }
  } catch(e) {
    respondError(e, funSrc, true);
  }
};

function resetList() {
  gotRow = false;
  lastRow = false;
  chunks = [];
  startResp = {};
};

function runList(listFun, head, req, funSrc) {
  try {
    if (listFun.arity > 2) {
      throw("the list API has changed for CouchDB 0.10, please upgrade your code");
    }
    
    resetProvides();
    resetList();
    
    var tail = listFun.apply(null, [head, req]);
    
    if (providesUsed) {
      tail = runProvides(req);
    }
    
    if (!gotRow) {
      getRow();
    }
    if (typeof tail != "undefined") {
      chunks.push(tail);
    }
    blowChunks("end");
  } catch(e) {
    respondError(e, funSrc, false);
  }
};

function renderError(m) {
  respond({error : "render_error", reason : m});
}

function respondError(e, funSrc, htmlErrors) {
  if (e.error && e.reason) {
    respond(e);
  } else {
    var logMessage = "function raised error: "+e.toString();
    log(logMessage);
    log("stacktrace: "+e.stack);
    var errorMessage = htmlErrors ? htmlRenderError(e, funSrc) : logMessage;
    renderError(errorMessage);
  }
}

function escapeHTML(string) {
  return string.replace(/&/g, "&amp;")
               .replace(/</g, "&lt;")
               .replace(/>/g, "&gt;");
}

function htmlRenderError(e, funSrc) {
  var msg = ["<html><body><h1>Render Error</h1>",
    "<p>JavaScript function raised error: ",
    e.toString(),
    "</p><h2>Stacktrace:</h2><code><pre>",
    escapeHTML(e.stack),
    "</pre></code><h2>Function source:</h2><code><pre>",
    escapeHTML(funSrc),
    "</pre></code></body></html>"].join('');
  return {body:msg};
};
