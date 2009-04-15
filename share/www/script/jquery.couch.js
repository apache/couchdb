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

(function($) {
  $.couch = $.couch || {};
  $.extend($.couch, {

    activeTasks: function(options) {
      options = options || {};
      $.ajax({
        type: "GET", url: "/_active_tasks", dataType: "json",
        complete: function(req) {
          var resp = $.httpData(req, "json");
          if (req.status == 200) {
            if (options.success) options.success(resp);
          } else  if (options.error) {
            options.error(req.status, resp.error, resp.reason);
          } else {
            alert("Active task status could not be retrieved: " +
              resp.reason);
          }
        }
      });
    },

    allDbs: function(options) {
      options = options || {};
      $.ajax({
        type: "GET", url: "/_all_dbs",
        complete: function(req) {
          var resp = $.httpData(req, "json");
          if (req.status == 200) {
            if (options.success) options.success(resp);
          } else if (options.error) {
            options.error(req.status, resp.error, resp.reason);
          } else {
            alert("An error occurred retrieving the list of all databases: " +
              resp.reason);
          }
        }
      });
    },

    config: function(options, section, option, value) {
      options = options || {};
      var url = "/_config/";
      if (section) {
        url += encodeURIComponent(section) + "/";
        if (option) {
          url += encodeURIComponent(option);
        }
      }
      if (value === undefined) {
        var method = "GET";
      } else {
        var method = "PUT";
        var data = toJSON(value);
      }
      $.ajax({
        type: method, url: url, contentType: "application/json",
        dataType: "json", data: toJSON(value), processData: false,
        complete: function(req) {
          var resp = $.httpData(req, "json");
          if (req.status == 200) {
            if (options.success) options.success(resp);
          } else if (options.error) {
            options.error(req.status, resp.error, resp.reason);
          } else {
            alert("An error occurred retrieving/updating the server " +
              "configuration: " + resp.reason);
          }
        }
      });
    },

    db: function(name) {
      return {
        name: name,
        uri: "/" + encodeURIComponent(name) + "/",

        compact: function(options) {
          options = options || {};
          $.ajax({
            type: "POST", url: this.uri + "_compact",
            contentType: "application/json",
            dataType: "json", data: "", processData: false, 
            complete: function(req) {
              var resp = $.httpData(req, "json");
              if (req.status == 202) {
                if (options.success) options.success(resp);
              } else if (options.error) {
                options.error(req.status, resp.error, resp.reason);
              } else {
                alert("The database could not be compacted: " + resp.reason);
              }
            }
          });
        },
        create: function(options) {
          options = options || {};
          $.ajax({
            type: "PUT", url: this.uri, contentType: "application/json",
            dataType: "json", data: "", processData: false,
            complete: function(req) {
              var resp = $.httpData(req, "json");
              if (req.status == 201) {
                if (options.success) options.success(resp);
              } else if (options.error) {
                options.error(req.status, resp.error, resp.reason);
              } else {
                alert("The database could not be created: " + resp.reason);
              }
            }
          });
        },
        drop: function(options) {
          options = options || {};
          $.ajax({
            type: "DELETE", url: this.uri, dataType: "json",
            complete: function(req) {
              var resp = $.httpData(req, "json");
              if (req.status == 200) {
                if (options.success) options.success(resp);
              } else if (options.error) {
                options.error(req.status, resp.error, resp.reason);
              } else {
                alert("The database could not be deleted: " + resp.reason);
              }
            }
          });
        },
        info: function(options) {
          options = options || {};
          $.ajax({
            type: "GET", url: this.uri, dataType: "json",
            complete: function(req) {
              var resp = $.httpData(req, "json");
              if (req.status == 200) {
                if (options.success) options.success(resp);
              } else  if (options.error) {
                options.error(req.status, resp.error, resp.reason);
              } else {
                alert("Database information could not be retrieved: " +
                  resp.reason);
              }
            }
          });
        },
        allDocs: function(options) {
          options = options || {};
          $.ajax({
            type: "GET", url: this.uri + "_all_docs" + encodeOptions(options),
            dataType: "json",
            complete: function(req) {
              var resp = $.httpData(req, "json");
              if (req.status == 200) {
                if (options.success) options.success(resp);
              } else if (options.error) {
                options.error(req.status, resp.error, resp.reason);
              } else {
                alert("An error occurred retrieving a list of all documents: " +
                  resp.reason);
              }
            }
          });
        },
        allDesignDocs: function(options) {
          options = options || {};
          this.allDocs($.extend({startkey:"_design", endkey:"_design0"}, options));
        },
        allApps: function(options) {
          options = options || {};
          var self = this;
          if (options.eachApp) {
            this.allDesignDocs({
              success: function(resp) {
                $.each(resp.rows, function() {
                  self.openDoc(this.id, {
                    success: function(ddoc) {
                      var index, appPath, appName = ddoc._id.split('/');
                      appName.shift();
                      appName = appName.join('/');
                      index = ddoc.couchapp && ddoc.couchapp.index;
                      if (index) {
                        appPath = ['', name, ddoc._id, index].join('/');
                      } else if (ddoc._attachments && ddoc._attachments["index.html"]) {
                        appPath = ['', name, ddoc._id, "index.html"].join('/');
                      }
                      if (appPath) options.eachApp(appName, appPath, ddoc);
                    }
                  });
                });
              }
            });            
          } else {
            alert("please provide an eachApp function for allApps()");
          }
        },
        openDoc: function(docId, options, ajaxOptions) {
          options = options || {};
          ajaxOptions = ajaxOptions || {};
          $.ajax($.extend({
            type: "GET",
            url: this.uri + encodeURIComponent(docId) + encodeOptions(options),
            dataType: "json",
            complete: function(req) {
              var resp = $.httpData(req, "json");
              if (req.status == 200) {
                if (options.success) options.success(resp);
              } else if (options.error) {
                options.error(req.status, resp.error, resp.reason);
              } else {
                alert("The document could not be retrieved: " + resp.reason);
              }
            }
          }, ajaxOptions));
        },
        saveDoc: function(doc, options) {
          options = options || {};
          if (doc._id === undefined) {
            var method = "POST";
            var uri = this.uri;
          } else {
            var method = "PUT";
            var uri = this.uri  + encodeURIComponent(doc._id);
          }
          $.ajax({
            type: method, url: uri + encodeOptions(options),
            contentType: "application/json",
            dataType: "json", data: toJSON(doc),
            complete: function(req) {
              var resp = $.httpData(req, "json");
              doc._id = resp.id;
              doc._rev = resp.rev;
              if (req.status == 201) {
                if (options.success) options.success(resp);
              } else if (options.error) {
                options.error(req.status, resp.error, resp.reason);
              } else {
                alert("The document could not be saved: " + resp.reason);
              }
            }
          });
        },
        bulkSave: function(docs, options) {
          options = options || {};
          $.ajax({
            type: 'POST', url: this.uri + "_bulk_docs" + encodeOptions(options),
            contentType: "application/json",
            dataType: "json", data: toJSON({docs: docs}),
            complete: function(req) {
              var resp = $.httpData(req, "json");
              if (req.status == 201) {
                if (options.success) options.success(resp);
              } else if (options.error) {
                options.error(req.status, resp.error, resp.reason);
              } else {
                alert("The documents could not be saved: " + resp.reason);
              }
            }
          });
        },
        removeDoc: function(doc, options) {
          options = options || {};
          $.ajax({
            type: "DELETE",
            url: this.uri + encodeURIComponent(doc._id) + encodeOptions({rev: doc._rev}),
            dataType: "json",
            complete: function(req) {
              var resp = $.httpData(req, "json");
              if (req.status == 200) {
                if (options.success) options.success(resp);
              } else if (options.error) {
                options.error(req.status, resp.error, resp.reason);
              } else {
                alert("The document could not be deleted: " + resp.reason);
              }
            }
          });
        },
        query: function(mapFun, reduceFun, language, options) {
          options = options || {};
          language = language || "javascript"
          if (typeof(mapFun) != "string") {
            mapFun = mapFun.toSource ? mapFun.toSource() : "(" + mapFun.toString() + ")";
          }
          var body = {language: language, map: mapFun};
          if (reduceFun != null) {
            if (typeof(reduceFun) != "string")
              reduceFun = reduceFun.toSource ? reduceFun.toSource() : "(" + reduceFun.toString() + ")";
            body.reduce = reduceFun;
          }
          $.ajax({
            type: "POST", url: this.uri + "_temp_view" + encodeOptions(options),
            contentType: "application/json",
            data: toJSON(body), dataType: "json",
            complete: function(req) {
              var resp = $.httpData(req, "json");
              if (req.status == 200) {
                if (options.success) options.success(resp);
              } else if (options.error) {
                options.error(req.status, resp.error, resp.reason);
              } else {
                alert("An error occurred querying the database: " + resp.reason);
              }
            }
          });
        },
        view: function(name, options) {
          options = options || {};
          name = name.split('/');
          $.ajax({
            type: "GET", url: this.uri + "_design/" + name[0] + "/_view/" + name[1] + encodeOptions(options),
            dataType: "json",
            complete: function(req) {
              var resp = $.httpData(req, "json");
              if (req.status == 200) {
                if (options.success) options.success(resp);
              } else if (options.error) {
                options.error(req.status, resp.error, resp.reason);
              } else {
                alert("An error occurred accessing the view: " + resp.reason);
              }
            }
          });
        }
      };
    },

    info: function(options) {
      options = options || {};
      $.ajax({
        type: "GET", url: "/", dataType: "json",
        complete: function(req) {
          var resp = $.httpData(req, "json");
          if (req.status == 200) {
            if (options.success) options.success(resp);
          } else if (options.error) {
            options.error(req.status, resp.error, resp.reason);
          } else {
            alert("Server information could not be retrieved: " + resp.reason);
          }
        }
      });
    },

    replicate: function(source, target, options) {
      options = options || {};
      $.ajax({
        type: "POST", url: "/_replicate", dataType: "json",
        data: JSON.stringify({source: source, target: target}),
        contentType: "application/json",
        complete: function(req) {
          var resp = $.httpData(req, "json");
          if (req.status == 200) {
            if (options.success) options.success(resp);
          } else if (options.error) {
            options.error(req.status, resp.error, resp.reason);
          } else {
            alert("Replication failed: " + resp.reason);
          }
        }
      });
    }

  });

  // Convert a options object to an url query string.
  // ex: {key:'value',key2:'value2'} becomes '?key="value"&key2="value2"'
  function encodeOptions(options) {
    var buf = []
    if (typeof(options) == "object" && options !== null) {
      for (var name in options) {
        if (name == "error" || name == "success") continue;
        var value = options[name];
        if (name == "key" || name == "startkey" || name == "endkey") {
          value = toJSON(value);
        }
        buf.push(encodeURIComponent(name) + "=" + encodeURIComponent(value));
      }
    }
    return buf.length ? "?" + buf.join("&") : "";
  }

  function toJSON(obj) {
    return obj !== null ? JSON.stringify(obj) : null;
  }

})(jQuery);
