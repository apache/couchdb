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

// *********************** Test Framework of Sorts ************************* //


function loadScript(url) {
  // disallow loading remote URLs
  var re = /^[a-z0-9_]+(\/[a-z0-9_]+)*\.js#?$/;
  if (!re.test(url)) {
      throw "Not loading remote test scripts";
  }
  if (typeof document != "undefined") document.write('<script src="'+url+'"></script>');
};

function patchTest(fun) {
  var source = fun.toString();
  var output = "";
  var i = 0;
  var testMarker = "T(";
  while (i < source.length) {
    var testStart = source.indexOf(testMarker, i);
    if (testStart == -1) {
      output = output + source.substring(i, source.length);
      break;
    }
    var testEnd = source.indexOf(");", testStart);
    var testCode = source.substring(testStart + testMarker.length, testEnd);
    output += source.substring(i, testStart) + "T(" + testCode + "," + JSON.stringify(testCode);
    i = testEnd;
  }
  try {
    return eval("(" + output + ")");
  } catch (e) {
    return null;
  }
}

function runAllTests() {
  var rows = $("#tests tbody.content tr");
  $("td", rows).text("");
  $("td.status", rows).removeClass("error").removeClass("failure").removeClass("success").text("not run");
  var offset = 0;
  function runNext() {
    if (offset < rows.length) {
      var row = rows.get(offset);
      runTest($("th button", row).get(0), function() {
        offset += 1;
        setTimeout(runNext, 100);
      }, false, true);
    } else {
      saveTestReport();
    }
  }
  runNext();
}

var numFailures = 0;
var currentRow = null;

function runTest(button, callback, debug, noSave) {

  // offer to save admins
  if (currentRow != null) {
    alert("Can not run multiple tests simultaneously.");
    return;
  }
  var row = currentRow = $(button).parents("tr").get(0);
  $("td.status", row).removeClass("error").removeClass("failure").removeClass("success");
  $("td", row).text("");
  $("#toolbar li.current").text("Running: "+row.id);
  var testFun = couchTests[row.id];
  function run() {
    numFailures = 0;
    var start = new Date().getTime();
    try {
      if (debug == undefined || !debug) {
        testFun = patchTest(testFun) || testFun;
      }
      testFun(debug);
      var status = numFailures > 0 ? "failure" : "success";
    } catch (e) {
      var status = "error";
      if ($("td.details ol", row).length == 0) {
        $("<ol></ol>").appendTo($("td.details", row));
      }
      $("<li><b>Exception raised:</b> <code class='error'></code></li>")
        .find("code").text(JSON.stringify(e)).end()
        .appendTo($("td.details ol", row));
      if (debug) {
        currentRow = null;
        throw e;
      }
    }
    if ($("td.details ol", row).length) {
      $("<a href='#'>Run with debugger</a>").click(function() {
        runTest(this, undefined, true);
      }).prependTo($("td.details ol", row));
    }
    var duration = new Date().getTime() - start;
    $("td.status", row).removeClass("running").addClass(status).text(status);
    $("td.duration", row).text(duration + "ms");
    $("#toolbar li.current").text("Finished: "+row.id);
    updateTestsFooter();
    currentRow = null;
    if (callback) callback();
    if (!noSave) saveTestReport();
  }
  $("td.status", row).addClass("running").text("running…");
  setTimeout(run, 100);
}

function showSource(cell) {
  var name = $(cell).text();
  var win = window.open("", name, "width=700,height=500,resizable=yes,scrollbars=yes");
  win.document.location = "script/test/" + name + ".js";
}

var readyToRun;
function setupAdminParty(fun) {
  if (readyToRun) {
    fun();
  } else {
    function removeAdmins(confs, doneFun) {
      // iterate through the config and remove current user last
      // current user is at front of list
      var remove = confs.pop();
      if (remove) {
        $.couch.config({
          success : function() {
            removeAdmins(confs, doneFun);
          }
        }, "admins", remove[0], null);
      } else {
        doneFun();
      }
    };
    $.couch.session({
      success : function(resp) {
        var userCtx = resp.userCtx;
        if (userCtx.name && userCtx.roles.indexOf("_admin") != -1) {
          // admin but not admin party. dialog offering to make admin party
          $.showDialog("dialog/_admin_party.html", {
            submit: function(data, callback) {
              $.couch.config({
                success : function(conf) {
                  var meAdmin, adminConfs = [];
                  for (var name in conf) {
                    if (name == userCtx.name) {
                      meAdmin = [name, conf[name]];
                    } else {
                      adminConfs.push([name, conf[name]]);
                    }
                  }
                  adminConfs.unshift(meAdmin);
                  removeAdmins(adminConfs, function() {
                    callback();
                    $.futon.session.sidebar();
                    readyToRun = true;
                    setTimeout(fun, 500);
                  });
                }
              }, "admins");
            }
          });
        } else if (userCtx.roles.indexOf("_admin") != -1) {
          // admin party!
          readyToRun = true;
          fun();
        } else {
          // not an admin
          alert("Error: You need to be an admin to run the tests.");
        };
      }
    });
  }
};

function updateTestsListing() {
  for (var name in couchTests) {
    var testFunction = couchTests[name];
    var row = $("<tr><th></th><td></td><td></td><td></td></tr>")
      .find("th").text(name).attr("title", "Show source").click(function() {
        showSource(this);
      }).end()
      .find("td:nth(0)").addClass("status").text("not run").end()
      .find("td:nth(1)").addClass("duration").end()
      .find("td:nth(2)").addClass("details").end();
    $("<button type='button' class='run' title='Run test'></button>").click(function() {
      this.blur();
      var self = this;
      // check for admin party
      setupAdminParty(function() {
        runTest(self);
      });
      return false;
    }).prependTo(row.find("th"));
    row.attr("id", name).appendTo("#tests tbody.content");
  }
  $("#tests tr").removeClass("odd").filter(":odd").addClass("odd");
  updateTestsFooter();
}

function updateTestsFooter() {
  var tests = $("#tests tbody.content tr td.status");
  var testsRun = tests.filter(".success, .error, .failure");
  var testsFailed = testsRun.not(".success");
  var totalDuration = 0;
  $("#tests tbody.content tr td.duration:contains('ms')").each(function() {
    var text = $(this).text();
    totalDuration += parseInt(text.substr(0, text.length - 2), 10);
  });
  $("#tests tbody.footer td").html("<span>"+testsRun.length + " of " + tests.length +
    " test(s) run, " + testsFailed.length + " failures (" +
    totalDuration + " ms)</span> ");
}

// make report and save to local db
// display how many reports need replicating to the mothership
// have button to replicate them

function saveTestReport(report) {
  var report = makeTestReport();
  if (report) {
    var db = $.couch.db("test_suite_reports");
    var saveReport = function(db_info) {
      report.db = db_info;
      $.couch.info({success : function(node_info) {
        report.node = node_info;
        db.saveDoc(report);
      }});
    };
    var createDb = function() {
      db.create({success: function() {
        db.info({success:saveReport});
      }});
    };
    db.info({error: createDb, success:saveReport});
  }
};

function makeTestReport() {
  var report = {};
  report.summary = $("#tests tbody.footer td").text();
  report.platform = testPlatform();
  var date = new Date();
  report.timestamp = date.getTime();
  report.timezone = date.getTimezoneOffset();
  report.tests = [];
  $("#tests tbody.content tr").each(function() {
    var status = $("td.status", this).text();
    if (status != "not run") {
      var test = {};
      test.name = this.id;
      test.status = status;
      test.duration = parseInt($("td.duration", this).text());
      test.details = [];
      $("td.details li", this).each(function() {
        test.details.push($(this).text());
      });
      if (test.details.length == 0) {
        delete test.details;
      }
      report.tests.push(test);
    }
  });
  if (report.tests.length > 0) return report;
};

function testPlatform() {
  var b = $.browser;
  var bs = ["mozilla", "msie", "opera", "safari"];
  for (var i=0; i < bs.length; i++) {
    if (b[bs[i]]) {
      return {"browser" : bs[i], "version" : b.version};
    }
  };
  return {"browser" : "undetected"};
}


function reportTests() {
  // replicate the database to couchdb.couchdb.org
}

// Use T to perform a test that returns false on failure and if the test fails,
// display the line that failed.
// Example:
// T(MyValue==1);
function T(arg1, arg2, testName) {
  if (!arg1) {
    if (currentRow) {
      if ($("td.details ol", currentRow).length == 0) {
        $("<ol></ol>").appendTo($("td.details", currentRow));
      }
      var message = (arg2 != null ? arg2 : arg1).toString();
      $("<li><b>Assertion " + (testName ? "'" + testName + "'" : "") + " failed:</b> <code class='failure'></code></li>")
        .find("code").text(message).end()
        .appendTo($("td.details ol", currentRow));
    }
    numFailures += 1;
  }
}

function TIsnull(actual, testName) {
  T(actual === null, "expected 'null', got '"
    + repr(actual) + "'", testName);
}

function TEquals(expected, actual, testName) {
  T(equals(expected, actual), "expected '" + repr(expected) +
    "', got '" + repr(actual) + "'", testName);
}

function TEqualsIgnoreCase(expected, actual, testName) {
  T(equals(expected.toUpperCase(), actual.toUpperCase()), "expected '" + repr(expected) +
    "', got '" + repr(actual) + "'", testName);
}

function equals(a,b) {
  if (a === b) return true;
  try {
    return repr(a) === repr(b);
  } catch (e) {
    return false;
  }
}

function repr(val) {
  if (val === undefined) {
    return null;
  } else if (val === null) {
    return "null";
  } else {
    return JSON.stringify(val);
  }
}

function makeDocs(start, end, templateDoc) {
  var templateDocSrc = templateDoc ? JSON.stringify(templateDoc) : "{}";
  if (end === undefined) {
    end = start;
    start = 0;
  }
  var docs = [];
  for (var i = start; i < end; i++) {
    var newDoc = eval("(" + templateDocSrc + ")");
    newDoc._id = (i).toString();
    newDoc.integer = i;
    newDoc.string = (i).toString();
    docs.push(newDoc);
  }
  return docs;
}

function run_on_modified_server(settings, fun) {
  try {
    // set the settings
    for(var i=0; i < settings.length; i++) {
      var s = settings[i];
      var xhr = CouchDB.request("PUT", "/_config/" + s.section + "/" + s.key, {
        body: JSON.stringify(s.value),
        headers: {"X-Couch-Persist": "false"}
      });
      CouchDB.maybeThrowError(xhr);
      s.oldValue = xhr.responseText;
    }
    // run the thing
    fun();
  } finally {
    // unset the settings
    for(var j=0; j < i; j++) {
      var s = settings[j];
      if(s.oldValue == "\"\"\n") { // unset value
        CouchDB.request("DELETE", "/_config/" + s.section + "/" + s.key, {
          headers: {"X-Couch-Persist": "false"}
        });
      } else {
        CouchDB.request("PUT", "/_config/" + s.section + "/" + s.key, {
          body: s.oldValue,
          headers: {"X-Couch-Persist": "false"}
        });
      }
    }
  }
}

function stringFun(fun) {
  var string = fun.toSource ? fun.toSource() : "(" + fun.toString() + ")";
  return string;
}

function waitForSuccess(fun, tag) {
  var start = new Date();
  while(true) {
    if (new Date() - start > 5000) {
      throw("timeout: "+tag);
    } else {
      try {
        fun();
        break;
      } catch (e) {}
      // sync http req allow async req to happen
      CouchDB.request("GET", "/test_suite_db/?tag="+encodeURIComponent(tag));
    }
  }
}

function waitForRestart() {
  var waiting = true;
  // Wait for the server to go down but don't
  // wait too long because we might miss the
  // the unavailable period.
  var count = 25;
  while (waiting && count > 0) {
    count--;
    try {
      CouchDB.request("GET", "/");
    } catch(e) {
      waiting = false;
    }
  }
  // Wait for it to come back up
  waiting = true;
  while (waiting) {
    try {
      CouchDB.request("GET", "/");
      waiting = false;
    } catch(e) {
      // the request will fail until restart completes
    }
  }
};

function restartServer() {
  var xhr;
  try {
    CouchDB.request("POST", "/_restart");
  } catch(e) {
    // this request may sometimes fail
  }
  waitForRestart();
}

// legacy functions for CouchDB < 1.2.0
// we keep them to make sure we keep BC
CouchDB.user_prefix = "org.couchdb.user:";

CouchDB.prepareUserDoc = function(user_doc, new_password) {
  user_doc._id = user_doc._id || CouchDB.user_prefix + user_doc.name;
  if (new_password) {
    // handle the password crypto
    user_doc.salt = CouchDB.newUuids(1)[0];
    user_doc.password_sha = hex_sha1(new_password + user_doc.salt);
  }
  user_doc.type = "user";
  if (!user_doc.roles) {
    user_doc.roles = [];
  }
  return user_doc;
};
