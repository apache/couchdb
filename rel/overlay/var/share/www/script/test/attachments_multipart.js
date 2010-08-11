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

couchTests.attachments_multipart= function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;
  
  // mime multipart
            
  xhr = CouchDB.request("PUT", "/test_suite_db/multipart", {
    headers: {"Content-Type": "multipart/related;boundary=\"abc123\""},
    body:
      "--abc123\r\n" +
      "content-type: application/json\r\n" +
      "\r\n" +
      JSON.stringify({
        "body":"This is a body.",
        "_attachments":{
          "foo.txt": {
            "follows":true,
            "content_type":"text/plain",
            "length":21
            },
          "bar.txt": {
            "follows":true,
            "content_type":"text/plain",
            "length":20
            },
          "baz.txt": {
            "follows":true,
            "content_type":"text/plain",
            "length":19
            }
          }
        }) +
      "\r\n--abc123\r\n" +
      "\r\n" +
      "this is 21 chars long" +
      "\r\n--abc123\r\n" +
      "\r\n" +
      "this is 20 chars lon" +
      "\r\n--abc123\r\n" +
      "\r\n" +
      "this is 19 chars lo" +
      "\r\n--abc123--"
    });
    
  var result = JSON.parse(xhr.responseText);
  
  T(result.ok)
  
  
    
  TEquals(201, xhr.status, "should send 201 Accepted");
  
  xhr = CouchDB.request("GET", "/test_suite_db/multipart/foo.txt");
  
  T(xhr.responseText == "this is 21 chars long");
  
  xhr = CouchDB.request("GET", "/test_suite_db/multipart/bar.txt");
  
  T(xhr.responseText == "this is 20 chars lon");
  
  xhr = CouchDB.request("GET", "/test_suite_db/multipart/baz.txt");
  
  T(xhr.responseText == "this is 19 chars lo");
  
  // now edit an attachment
  
  var doc = db.open("multipart");
  var firstrev = doc._rev;
  
  T(doc._attachments["foo.txt"].stub == true);
  T(doc._attachments["bar.txt"].stub == true);
  T(doc._attachments["baz.txt"].stub == true);
  
  //lets change attachment bar
  delete doc._attachments["bar.txt"].stub; // remove stub member (or could set to false)
  doc._attachments["bar.txt"].length = 18;
  doc._attachments["bar.txt"].follows = true;
  //lets delete attachment baz:
  delete doc._attachments["baz.txt"];
  
  var xhr = CouchDB.request("PUT", "/test_suite_db/multipart", {
    headers: {"Content-Type": "multipart/related;boundary=\"abc123\""},
    body:
      "--abc123\r\n" +
      "content-type: application/json\r\n" +
      "\r\n" +
      JSON.stringify(doc) +
      "\r\n--abc123\r\n" +
      "\r\n" +
      "this is 18 chars l" +
      "\r\n--abc123--"
    });
  
  xhr = CouchDB.request("GET", "/test_suite_db/multipart/bar.txt");
  
  T(xhr.responseText == "this is 18 chars l");
  
  xhr = CouchDB.request("GET", "/test_suite_db/multipart/baz.txt");
  T(xhr.status == 404);
  
  // now test receiving multipart docs
  
  function getBoundary(xhr) {
    var ctype = xhr.getResponseHeader("Content-Type");
  
    var ctypeArgs = ctype.split("; ").slice(1);
    var boundary = null;
    for(var i=0; i<ctypeArgs.length; i++) {
      if (ctypeArgs[i].indexOf("boundary=") == 0) {
        boundary = ctypeArgs[i].split("=")[1];
        if (boundary.charAt(0) == '"') {
          // stringified boundary, parse as json 
          // (will maybe not if there are escape quotes)
          boundary = JSON.parse(boundary);
        }
      }
    }
    return boundary;
  }
  
  function parseMultipart(xhr) {
    var boundary = getBoundary(xhr);
    var mimetext = xhr.responseText;
    // strip off leading boundary
    var leading = "--" + boundary + "\r\n";
    var last = "\r\n--" + boundary + "--";
    
    // strip off leading and trailing boundary
    var leadingIdx = mimetext.indexOf(leading) + leading.length;
    var trailingIdx = mimetext.indexOf(last);
    mimetext = mimetext.slice(leadingIdx, trailingIdx);
    
    // now split the sections
    var sections = mimetext.split(new RegExp("\\r\\n--" + boundary));
    
    // spilt out the headers for each section
    for(var i=0; i < sections.length; i++) {
      var section = sections[i];
      var headerEndIdx = section.indexOf("\r\n\r\n");
      var headersraw = section.slice(0, headerEndIdx).split(/\r\n/);
      var body = section.slice(headerEndIdx + 4);
      var headers = {};
      for(var j=0; j<headersraw.length; j++) {
        var tmp = headersraw[j].split(": ");
        headers[tmp[0]] = tmp[1]; 
      }
      sections[i] = {"headers":headers, "body":body};
    }
    
    return sections;
  }
  
  
  xhr = CouchDB.request("GET", "/test_suite_db/multipart?attachments=true",
    {headers:{"accept": "multipart/related,*/*;"}});
  
  T(xhr.status == 200);
  
  // parse out the multipart
  
  var sections = parseMultipart(xhr);
  
  T(sections.length == 3);
  
  // The first section is the json doc. Check it's content-type. It contains
  // the metadata for all the following attachments
  
  T(sections[0].headers['content-type'] == "application/json");
  
  var doc = JSON.parse(sections[0].body);
  
  T(doc._attachments['foo.txt'].follows == true);
  T(doc._attachments['bar.txt'].follows == true);
  
  T(sections[1].body == "this is 21 chars long");
  T(sections[2].body == "this is 18 chars l");
  
  // now get attachments incrementally (only the attachments changes since
  // a certain rev).
  
  xhr = CouchDB.request("GET", "/test_suite_db/multipart?atts_since=[\"" + firstrev + "\"]",
    {headers:{"accept": "multipart/related,*/*;"}});
  
  T(xhr.status == 200);
  
  var sections = parseMultipart(xhr);
  
  T(sections.length == 2);
  
  var doc = JSON.parse(sections[0].body);
  
  T(doc._attachments['foo.txt'].stub == true);
  T(doc._attachments['bar.txt'].follows == true);
  
  T(sections[1].body == "this is 18 chars l");
  
  // try it with a rev that doesn't exist (should get all attachments)
  
  xhr = CouchDB.request("GET", "/test_suite_db/multipart?atts_since=[\"1-2897589\"]",
    {headers:{"accept": "multipart/related,*/*;"}});
  
  T(xhr.status == 200);
  
  var sections = parseMultipart(xhr);
  
  T(sections.length == 3);
  
  var doc = JSON.parse(sections[0].body);
  
  T(doc._attachments['foo.txt'].follows == true);
  T(doc._attachments['bar.txt'].follows == true);
  
  T(sections[1].body == "this is 21 chars long");
  T(sections[2].body == "this is 18 chars l");
  
  // try it with a rev that doesn't exist, and one that does
  
  xhr = CouchDB.request("GET", "/test_suite_db/multipart?atts_since=[\"1-2897589\",\"" + firstrev + "\"]",
    {headers:{"accept": "multipart/related,*/*;"}});
  
  T(xhr.status == 200);
  
  var sections = parseMultipart(xhr);
  
  T(sections.length == 2);
  
  var doc = JSON.parse(sections[0].body);
  
  T(doc._attachments['foo.txt'].stub == true);
  T(doc._attachments['bar.txt'].follows == true);
  
  T(sections[1].body == "this is 18 chars l");
  
};
