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
  if (typeof document != "undefined") document.write('<script src="'+url+'"></script>');
};

function patchTest(fun) {
  var source = fun.toString();
  var output = "";
  var i = 0;
  var testMarker = "T("
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
      });
    }
  }
  runNext();
}

var numFailures = 0;
var currentRow = null;

function runTest(button, callback, debug) {
  if (currentRow != null) {
    alert("Can not run multiple tests simultaneously.");
    return;
  }
  var row = currentRow = $(button).parents("tr").get(0);
  $("td.status", row).removeClass("error").removeClass("failure").removeClass("success");
  $("td", row).text("");
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
    updateTestsFooter();
    currentRow = null;
    if (callback) callback();
  }
  $("td.status", row).addClass("running").text("running…");
  setTimeout(run, 100);
}

function showSource(cell) {
  var name = $(cell).text();
  var win = window.open("", name, "width=700,height=500,resizable=yes,scrollbars=yes");
  win.document.location = "script/test/" + name + ".js";
}

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
      runTest(this);
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
  if (testsFailed.length > 0) {
    $("#tests tbody.footer td").append($('<a href="#">Click to Report Test Failures</a>').click(function(e) {
      e.preventDefault();
      reportTests();
    }));
  }
}


function testReport() {
  var report = [];
  report.push(testPlatform()+"\n");
  $("#tests tbody.content tr").each(function() {
    var status = $("td.status", this).text();
    if (status != "not run") {
      var dur = $("td.duration", this).text();
      report.push(this.id+"\n  "+status+" "+dur);
      var details = [];
      $("td.details li", this).each(function() {
        report.push("    "+$(this).text());
      });
    }
  });
  return report.join("\n");
};

function testPlatform() {
  var b = $.browser;
  var bs = ["mozilla", "msie", "opera", "safari"];
  for (var i=0; i < bs.length; i++) {
    if (b[bs[i]]) {
      return "Platform: "+ bs[i] + " " + b.version;
    }
  };
}


function reportTests() {
  var summary = $("#tests tbody.footer td span").text();
  var report = testReport();
  var uri = "http://groups.google.com/group/couchdb-test-report/post"
    + "?subject=" + escape(summary);  

  var d=document;
  var f=d.createElement("form");
  // f.style.display='none';
  f.action=uri;
  f.method="POST";f.target="_blank";
  var t=d.createElement("textarea");
  t.name="body";
  t.value=report;
  f.appendChild(t);
  d.body.appendChild(f);
  f.submit();
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
    numFailures += 1
  }
}

function TEquals(expected, actual, testName) {
  T(equals(expected, actual), "expected '" + expected + "', got '" + actual + "'", testName);
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
