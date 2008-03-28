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

/*
 * Page class for browse/index.html
 */
function CouchIndexPage() {
  page = this;

  this.addDatabase = function() {
    $.showDialog("_create_database.html", {
      submit: function(data) {
        if (!data.name || data.name.length == 0) {
          return {name: "Please enter a name."};
        }
        try {
          new CouchDB(data.name).createDb();
        } catch (e) {
          return {name: e.reason};
        }
        if (window !== parent) parent.setTimeout("updateDatabaseList()", 500);
        window.open("database.html?" + data.name, "content");
      }
    });
    return false;
  }

  this.updateDatabaseListing = function() {
    var allDbs = CouchDB.allDbs();
    for (var i = 0; i < allDbs.length; i++) {
      var dbName = allDbs[i];
      var info = new CouchDB(dbName).info();
      $("#databases tbody.content").append(
        "<tr><th><a href='database.html?" + dbName + "'>" +
        dbName + "</a></th><td>" + info.doc_count +"</td><td>" +
        info.update_seq + "</td></tr>");
      $("#databases tbody tr:odd").addClass("odd");
      $("#databases tbody.footer tr td").text(allDbs.length + " database(s)");
    }
  }

}

/*
 * Page class for browse/database.html
 */
function CouchDatabasePage() {
  var urlParts = location.search.substr(1).split("/");
  var dbName = urlParts.shift();
  var viewName = (urlParts.length > 0) ? urlParts.join("/") : null;
  if (!viewName) {
    viewName = $.cookies.get(dbName + ".view") || "";
  } else {
    $.cookies.set(dbName + ".view", viewName);
  }
  var db = new CouchDB(dbName);

  this.dbName = dbName;
  this.viewName = viewName;
  this.db = db;
  this.isDirty = false;
  page = this;

  this.addDocument = function() {
    $.showDialog("_create_document.html", {
      submit: function(data) {
        try {
          var result = db.save(data.docid ? {_id: data.docid} : {});
        } catch (err) {
          return {docid: err.reason};
        }
        location.href = "document.html?" + dbName + "/" + result.id;
      }
    });
  }

  this.deleteDatabase = function() {
    $.showDialog("_delete_database.html", {
      submit: function() {
        db.deleteDb();
        location.href = "index.html";
        if (window !== null) {
          parent.$("#dbs li").filter(function(index) {
            return $("a", this).text() == dbName;
          }).remove();
        }
      }
    });
  }

  this.populateViewEditor = function() {
    if (viewName.match(/^_design\//)) {
      page.revertViewChanges();
      var dirtyTimeout = null;
      function updateDirtyState() {
        clearTimeout(dirtyTimeout);
        dirtyTimeout = setTimeout(function() {
          var buttons = $("#viewcode button.save, #viewcode button.revert");
          page.isDirty = $("#viewcode textarea").val() != page.storedViewCode;
          if (page.isDirty) {
            buttons.removeAttr("disabled");
          } else {
            buttons.attr("disabled", "disabled");
          }
        }, 100);
      }
      $("#viewcode textarea").bind("input", updateDirtyState);
      if ($.browser.msie) { // sorry, browser detection
        $("#viewcode textarea").get(0).onpropertychange = updateDirtyState
      } else if ($.browser.safari) {
        $("#viewcode textarea").bind("paste", updateDirtyState)
                               .bind("change", updateDirtyState)
                               .bind("keydown", updateDirtyState)
                               .bind("keypress", updateDirtyState)
                               .bind("keyup", updateDirtyState)
                               .bind("textInput", updateDirtyState);
      }
    }
  }

  this.populateViewsMenu = function() {
    var designDocs = db.allDocs({startkey: "_design/", endkey: "_design/ZZZ"});
    $("#switch select").each(function() {
      this.options.length = 3;
      for (var i = 0; i < designDocs.rows.length; i++) {
        var doc = db.open(designDocs.rows[i].id);
        var optGroup = $("<optgroup></optgroup>").attr("label", doc._id.substr(8));
        for (var name in doc.views) {
          if (!doc.views.hasOwnProperty(name)) continue;
          $("<option></option>").attr("value", doc._id + "/" + name).text(name)
            .appendTo(optGroup);
        }
        optGroup.appendTo(this);
      }
      this.autocomplete = false;
    });
  }

  this.revertViewChanges = function() {
    if (!page.storedViewCode) {
      var viewNameParts = viewName.split("/");
      var designDocId = viewNameParts[1];
      var localViewName = viewNameParts[2];
      var designDoc = db.open(["_design", designDocId].join("/"));
      if (designDoc) {
        page.storedViewCode = designDoc.views[localViewName];
      }
    }
    $("#viewcode textarea").val(page.storedViewCode);
    page.isDirty = false;
    $("#viewcode button.revert, #viewcode button.save").attr("disabled", "disabled");
  }

  this.saveViewAs = function() {
    if (viewName && /^_design/.test(viewName)) {
      var viewNameParts = viewName.split("/");
      var designDocId = viewNameParts[1];
      var localViewName = viewNameParts[2];
    } else {
      var designDocId = "", localViewName = ""
    }
    $.showDialog("_save_view_as.html", {
      load: function(elem) {
        $("#input_docid", elem).val(designDocId).suggest(function(text, callback) {
          var matches = [];
          var docs = db.allDocs({
            count: 10, startkey: "_design/" + text,
            endkey: "_design/" + text + "ZZZZ"
          });
          for (var i = 0; i < docs.rows.length; i++) {
            matches[i] = docs.rows[i].id.substr(8);
          }
          callback(matches);
        });
        $("#input_name", elem).val(localViewName).suggest(function(text, callback) {
          var matches = [];
          try {
            var doc = db.open("_design/" + $("#input_docid").val());
          } catch (err) {
            return;
          }
          if (!doc || !doc.views) return;
          for (var viewName in doc.views) {
            if (!doc.views.hasOwnProperty(viewName) || !viewName.match("^" + text)) {
              continue;
            }
            matches.push(viewName);
          }
          callback(matches);
        });
      },
      submit: function(data) {
        if (!data.docid || !data.name) {
          var errors = {};
          if (!data.docid) errors.docid = "Please enter a document ID";
          if (!data.name) errors.name = "Please enter a view name";
          return errors;
        }
        var viewCode = $("#viewcode textarea").val();
        var docId = ["_design", data.docid].join("/");
        var designDoc = db.open(docId);
        if (!designDoc) designDoc = {_id: docId, language: "text/javascript"};
        if (designDoc.views === undefined) designDoc.views = {};
        designDoc.views[data.name] = viewCode;
        db.save(designDoc);
        page.isDirty = false;
        location.href = "database.html?" + dbName + "/" + designDoc._id + "/" + data.name;
      }
    });
  }

  this.saveViewChanges = function() {
    var viewNameParts = viewName.split("/");
    var designDocId = viewNameParts[1];
    var localViewName = viewNameParts[2];
    var designDoc = db.open(["_design", designDocId].join("/"));
    var viewCode = $("#viewcode textarea").val();
    designDoc.views[localViewName] = viewCode;
    db.save(designDoc);
    page.isDirty = false;
    $("#viewcode button.revert, #viewcode button.save").attr("disabled", "disabled");
  }

  this.updateDesignDocLink = function() {
    if (viewName && /^_design/.test(viewName)) {
      var docId = "_design/" + viewName.split("/")[1];
      $("#designdoc-link").attr("href", "document.html?" + dbName + "/" + docId).text(docId);
    } else {
      $("#designdoc-link").removeAttr("href").text("");
    }
  }

  this.updateDocumentListing = function(options) {
    if (options === undefined) options = {};
    if (options.count === undefined) {
      options.count = parseInt($("#perpage").val(), 10);
    }
    if ($("#documents thead th.key").is(".desc")) {
      options.descending = true;
      $.cookies.set(dbName + ".desc", "1");
    } else {
      if (options.descending !== undefined) delete options.descending;
      $.cookies.remove(dbName + ".desc");
    }
    $("#paging a").unbind();
    $("#documents tbody.content").empty();
    this.updateDesignDocLink();

    var result = null;
    if (!viewName) {
      $("#switch select").get(0).selectedIndex = 0;
      result = db.allDocs(options);
    } else {
      $("#switch select").each(function() {
        for (var i = 0; i < this.options.length; i++) {
          if (this.options[i].value == viewName) {
            this.selectedIndex = i;
            break;
          }
        }
      });
      docs = [];
      if (viewName == "_temp_view") {
        $("#viewcode").show().addClass("expanded");
        var query = $("#viewcode textarea").val();
        $.cookies.set(db.name + ".query", query);
        try {
          result = db.query(query, options);
        } catch (e) {
          alert(e.reason ? e.reason : e.message);
          return;
        }
      } else if (viewName == "_design_docs") {
        options.startkey = options.descending ? "_design/ZZZZ" : "_design/";
        options.endkey = options.descending ? "_design/" : "_design/ZZZZ";
        result = db.allDocs(options);
      } else {
        $("#viewcode").show();
        var currentViewCode = $("#viewcode textarea").val();
        if (currentViewCode != page.storedViewCode) {
          result = db.query(currentViewCode, options);
        } else {
          result = db.view(viewName.substr(8), options);
        }
      }
    }
    if (result.offset === undefined) {
      result.offset = 0;
    }
    if (result.offset > 0) {
      $("#paging a.prev").attr("href", "#" + (result.offset - options.count)).click(function() {
        var firstDoc = result.rows[0];
        page.updateDocumentListing({
          startkey: firstDoc.key !== undefined ? firstDoc.key : null,
          startkey_docid: firstDoc.id,
          skip: 1,
          count: -options.count
        });
        return false;
      });
    } else {
      $("#paging a.prev").removeAttr("href");
    }
    if (result.total_rows - result.offset > options.count) {
      $("#paging a.next").attr("href", "#" + (result.offset + options.count)).click(function() {
        var lastDoc = result.rows[result.rows.length - 1];
        page.updateDocumentListing({
          startkey: lastDoc.key !== undefined ? lastDoc.key : null,
          startkey_docid: lastDoc.id,
          skip: 1,
          count: options.count
        });
        return false;
      });
    } else {
      $("#paging a.next").removeAttr("href");
    }

    for (var i = 0; i < result.rows.length; i++) {
      var row = result.rows[i];
      var tr = $("<tr></tr>");
      var key = row.key;
      $("<td class='key'><a href='document.html?" + db.name + "/" + row.id + "'>" +
        "<em></em><br><span class='docid'>ID:&nbsp;" + row.id +
        "</span></a></td>").find("em").text(
        key !== null ? prettyPrintJSON(key, 0, "") : "null"
      ).end().appendTo(tr);
      var value = row.value;
      $("<td class='value'></td>").text(
        value !== null ? prettyPrintJSON(value, 0, "") : "null"
      ).appendTo(tr).dblclick(function() {
        location.href = this.previousSibling.firstChild.href;
      });
      tr.appendTo("#documents tbody.content");
    }

    $("#documents tbody tr:odd").addClass("odd");
    $("#documents tbody.footer td span").text(
      "Showing " + Math.min(result.total_rows, result.offset + 1) + "-" +
      (result.offset + result.rows.length) + " of " + result.total_rows +
      " document" + (result.total_rows != 1 ? "s" : ""));
  }

  window.onbeforeunload = function() {
    $("#switch select").val(viewName);
    if (page.isDirty) {
      return "You've made changes to the view code that have not been " +
             "saved yet.";
    }
  }

}

/*
 * Page class for browse/database.html
 */
function CouchDocumentPage() {
  var urlParts = location.search.substr(1).split("/");
  var dbName = urlParts.shift();
  var idParts = urlParts.join("/").split("@", 2);
  var docId = idParts[0];
  var docRev = (idParts.length > 1) ? idParts[1] : null;
  var db = new CouchDB(dbName);
  var doc = db.open(docId, {revs_info: true});
  var revs = doc._revs_info;
  delete doc._revs_info;
  if (docRev != null) {
    try {
      doc = db.open(docId, {rev: docRev});
    } catch (e) {
      alert("The requested revision was not found. " +
            "You will be redirected back to the latest revision.");
      location.href = "?" + dbName + "/" + docId;
      return;
    }
  }

  this.dbName = dbName;
  this.db = db;
  this.doc = doc;
  this.isDirty = false;
  page = this;

  this.addField = function() {
    var fieldName = "unnamed";
    var fieldIdx = 1;
    while (doc.hasOwnProperty(fieldName)) {
      fieldName = "unnamed " + fieldIdx++;
    }
    doc[fieldName] = null;
    var row = _addRowForField(fieldName);
    page.isDirty = true;
    _editKey(row.find("th"), fieldName);
  }

  this.updateFieldListing = function() {
    $("#fields tbody.content").empty();
    var propNames = [];
    for (var prop in doc) {
      if (!doc.hasOwnProperty(prop)) continue;
      propNames.push(prop);
    }
    // Order properties alphabetically, but put internal fields first
    propNames.sort(function(a, b) {
      var a0 = a.charAt(0), b0 = b.charAt(0);
      if (a0 == "_" && b0 != "_") {
        return -1;
      } else if (a0 != "_" && b0 == "_") {
        return 1;
      } else {
        return a < b ? -1 : a != b ? 1 : 0;
      }
    });
    for (var pi = 0; pi < propNames.length; pi++) {
      _addRowForField(propNames[pi]);
    }
    if (revs.length > 1) {
      var currentIndex = 0;
      for (var i = 0; i < revs.length; i++) {
        if (revs[i].rev == doc._rev) {
          currentIndex = i;
          break;
        }
      }
      if (currentIndex < revs.length - 1) {
        var prevRev = revs[currentIndex + 1].rev;
        $("#paging a.prev").attr("href", "?" + dbName + "/" + docId + "@" + prevRev);
      }
      if (currentIndex > 0) {
        var nextRev = revs[currentIndex - 1].rev;
        $("#paging a.next").attr("href", "?" + dbName + "/" + docId + "@" + nextRev);
      }
      $("#fields tbody.footer td span").text("Showing revision " +
        (revs.length - currentIndex) + " of " + revs.length);
    }
  }

  this.deleteDocument = function() {
    $.showDialog("_delete_document.html", {
      submit: function() {
        db.deleteDoc(doc);
        location.href = "database.html?" + dbName;
      }
    });
  }

  this.saveDocument = function() {
    try {
      db.save(doc);
    } catch (e) {
      alert(e.reason);
      return;
    }
    page.isDirty = false;
    location.href = "?" + dbName + "/" + docId;
  }

  window.onbeforeunload = function() {
    if (page.isDirty) {
      return "You've made changes to this document that have not been " +
             "saved yet.";
    }
  }

  function _addRowForField(fieldName) {
    var value = _renderValue(doc[fieldName]);
    var row = $("<tr><th></th><td></td></tr>")
      .find("th").append($("<b></b>").text(fieldName)).dblclick(function() {
        _editKey(this, $(this).text());
      }).end()
      .find("td").append(value).dblclick(function() {
        _editValue(this, $(this).prev("th").text());
      }).end()
      .appendTo("#fields tbody.content");
    if (fieldName != "_id" && fieldName != "_rev") {
      row.find("th, td").attr("title", "Double click to edit");
      _initKey(row, fieldName);
      _initValue(value);
    }
    $("#fields tbody tr").removeClass("odd").filter(":odd").addClass("odd");
    return row;
  }

  function _editKey(cell, fieldName) {
    if (fieldName == "_id" || fieldName == "_rev") return;
    var th = $(cell);
    th.empty();
    var input = $("<input type='text' spellcheck='false'>");
    input.dblclick(function() { return false; }).keydown(function(evt) {
      switch (evt.keyCode) {
        case 13: applyChange(); break;
        case 27: cancelChange(); break;
      }
    });
    var tools = $("<div class='tools'></div>");
    function applyChange() {
      input.nextAll().remove();
      var newName = input.val();
      if (!newName.length || newName == fieldName) {
        cancelChange();
        return;
      }
      doc[newName] = doc[fieldName];
      delete doc[fieldName];
      th.children().remove();
      th.append($("<b></b>").text(newName));
      _initKey(th.parent("tr"), fieldName);
      page.isDirty = true;
    }
    function cancelChange() {
      th.children().remove();
      th.append($("<b></b>").text(fieldName));
      _initKey(th.parent("tr"), fieldName);
    }

    $("<button type='button' class='apply'></button>").click(function() {
      applyChange();
    }).appendTo(tools);
    $("<button type='button' class='cancel'></button>").click(function() {
      cancelChange();
    }).appendTo(tools);
    tools.appendTo(th);
    input.val(fieldName).appendTo(th);
    input.each(function() { this.focus(); this.select(); });
  }

  function _editValue(cell, fieldName) {
    if (fieldName == "_id" || fieldName == "_rev") return;
    var td = $(cell);
    var value = doc[fieldName];
    var needsTextarea = $("dl", td).length > 0 || $("code", td).text().length > 60;
    td.empty();
    if (needsTextarea) {
      var input = $("<textarea rows='8' cols='40' spellcheck='false'></textarea>");
    } else {
      var input = $("<input type='text' spellcheck='false'>");
    }
    input.dblclick(function() { return false; }).keydown(function(evt) {
      switch (evt.keyCode) {
        case 13: if (!needsTextarea) applyChange(); break;
        case 27: cancelChange(); break;
      }
    });
    var tools = $("<div class='tools'></div>");
    function applyChange() {
      input.nextAll().remove();
      try {
        var newValue = input.val() || "null";
        if (newValue == doc[fieldName]) {
          cancelChange();
          return;
        }
        doc[fieldName] = JSON.parse(newValue);
        td.children().remove();
        page.isDirty = true;
        var value = _renderValue(doc[fieldName]);
        td.append(value);
        _initValue(value);
      } catch (err) {
        input.addClass("invalid");
        var msg = err.message;
        if (msg == "parseJSON") {
          msg = "Please enter a valid JSON value (for example, \"string\").";
        }
        $("<div class='error'></div>").text(msg).insertAfter(input);
      }
    }
    function cancelChange() {
      td.children().remove();
      var value = _renderValue(doc[fieldName]);
      td.append(value);
      _initValue(value);
    }

    $("<button type='button' class='apply'></button>").click(function() {
      applyChange();
    }).appendTo(tools);
    $("<button type='button' class='cancel'></button>").click(function() {
      cancelChange();
    }).appendTo(tools);
    tools.appendTo(td);
    input.val(prettyPrintJSON(value)).appendTo(td);
    input.each(function() { this.focus(); this.select(); });
    if (needsTextarea) input.resizable();
  }

  function _initKey(row, fieldName) {
    if (fieldName != "_id" && fieldName != "_rev") {
      $("<button type='button' class='delete' title='Delete field'></button>").click(function() {
        delete doc[fieldName];
        row.remove();
        page.isDirty = true;
        $("#fields tbody tr").removeClass("odd").filter(":odd").addClass("odd");
      }).prependTo(row.find("th"));
    }
  }

  function _initValue(value) {
    value.find("dd").filter(":has(dl)").hide().prev("dt").addClass("collapsed");
    value.find("dd").not(":has(dl)").addClass("inline").prev().addClass("inline");
    value.find("dt.collapsed").click(function() {
      $(this).toggleClass("collapsed").next().toggle();
    });
  }

  function _renderValue(value) {
    var type = typeof(value);
    if (type == "object" && value !== null) {
      var list = $("<dl></dl>");
      for (var i in value) {
        if (!value.hasOwnProperty(i)) continue;
        $("<dt></dt>").text(i).appendTo(list);
        $("<dd></dd>").append(_renderValue(value[i])).appendTo(list);
      }
      return list;
    } else {
      return $("<code></code>").addClass(type).text(
        value !== null ? JSON.stringify(value) : "null"
      );
    }
  }

}
