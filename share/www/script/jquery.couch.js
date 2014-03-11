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

/**
 * @namespace
 * $.couch is used to communicate with a CouchDB server, the server methods can
 * be called directly without creating an instance. Typically all methods are
 * passed an <code>options</code> object which defines a success callback which
 * is called with the data returned from the http request to CouchDB, you can
 * find the other settings that can be used in the <code>options</code> object
 * from <a href="http://api.jquery.com/jQuery.ajax/#jQuery-ajax-settings">
 * jQuery.ajax settings</a>
 * <pre><code>$.couch.activeTasks({
 *   success: function (data) {
 *     console.log(data);
 *   }
 * });</code></pre>
 * Outputs (for example):
 * <pre><code>[
 *  {
 *   "pid" : "<0.11599.0>",
 *   "status" : "Copied 0 of 18369 changes (0%)",
 *   "task" : "recipes",
 *   "type" : "Database Compaction"
 *  }
 *]</code></pre>
 */
(function($) {

  $.couch = $.couch || {};
  /** @lends $.couch */

  /**
   * @private
   */
  function encodeDocId(docID) {
    var parts = docID.split("/");
    if (parts[0] == "_design") {
      parts.shift();
      return "_design/" + encodeURIComponent(parts.join('/'));
    }
    return encodeURIComponent(docID);
  }

  /**
   * @private
   */

  var uuidCache = [];

  $.extend($.couch, {
    urlPrefix: '',

    /**
     * You can obtain a list of active tasks by using the /_active_tasks URL.
     * The result is a JSON array of the currently running tasks, with each task
     * being described with a single object.
     * @see <a href="http://docs.couchdb.org/en/latest/api/server/common.html#
     * active-tasks">docs for /_active_tasks</a>
     * @param {ajaxSettings} options <a href="http://api.jquery.com/jQuery.ajax
     * /#jQuery-ajax-settings">jQuery ajax settings</a>
     */
    activeTasks: function(options) {
      return ajax(
        {url: this.urlPrefix + "/_active_tasks"},
        options,
        "Active task status could not be retrieved"
      );
    },

    /**
     * Returns a list of all the databases in the CouchDB instance
     * @see <a href="http://docs.couchdb.org/en/latest/api/server/common.html
     * #all-dbs">docs for /_all_dbs</a>
     * @param {ajaxSettings} options <a href="http://api.jquery.com/jQuery.ajax
     * /#jQuery-ajax-settings">jQuery ajax settings</a>
     */
    allDbs: function(options) {
      return ajax(
        {url: this.urlPrefix + "/_all_dbs"},
        options,
        "An error occurred retrieving the list of all databases"
      );
    },

    /**
     * View and edit the CouchDB configuration, called with just the options
     * parameter the entire config is returned, you can be more specific by
     * passing the section and option parameters, if you specify a value that
     * value will be stored in the configuration.
     * @see <a href="http://docs.couchdb.org/en/latest/api/server
     * /configuration.html#config-section-key">docs for /_config</a>
     * @param {ajaxSettings} options
     * <a href="http://api.jquery.com/jQuery.ajax/#jQuery-ajax-settings">
     * jQuery ajax settings</a>
     * @param {String} [section] the section of the config
     * @param {String} [option] the particular config option
     * @param {String} [value] value to be set
     */
    config: function(options, section, option, value) {
      var req = {url: this.urlPrefix + "/_config/"};
      if (section) {
        req.url += encodeURIComponent(section) + "/";
        if (option) {
          req.url += encodeURIComponent(option);
        }
      }
      if (value === null) {
        req.type = "DELETE";
      } else if (value !== undefined) {
        req.type = "PUT";
        req.data = toJSON(value);
        req.contentType = "application/json";
        req.processData = false
      }

      return ajax(req, options,
        "An error occurred retrieving/updating the server configuration"
      );
    },

    /**
     * Returns the session information for the currently logged in user.
     * @see <a href="http://docs.couchdb.org/en/latest/api/server/authn.html
     * #get--_session">docs for GET /_session</a>
     * @param {ajaxSettings} options
     * <a href="http://api.jquery.com/jQuery.ajax/#jQuery-ajax-settings">
     * jQuery ajax settings</a>
     */
    session: function(options) {
      options = options || {};
      return ajax({
        type: "GET", url: this.urlPrefix + "/_session",
        beforeSend: function(xhr) {
            xhr.setRequestHeader('Accept', 'application/json');
        },
        complete: function(req) {
          var resp = $.parseJSON(req.responseText);
          if (req.status == 200) {
            if (options.success) options.success(resp);
          } else if (options.error) {
            options.error(req.status, resp.error, resp.reason);
          } else {
            throw "An error occurred getting session info: " + resp.reason;
          }
        }
      });
    },

    /**
     * @private
     */
    userDb : function(callback) {
      return $.couch.session({
        success : function(resp) {
          var userDb = $.couch.db(resp.info.authentication_db);
          callback(userDb);
        }
      });
    },

    /**
     * Create a new user on the CouchDB server, <code>user_doc</code> is an
     * object with a <code>name</code> field and other information you want
     * to store relating to that user, for example
     * <code>{"name": "daleharvey"}</code>
     * @param {Object} user_doc Users details
     * @param {String} password Users password
     * @param {ajaxSettings} options
     * <a href="http://api.jquery.com/jQuery.ajax/#jQuery-ajax-settings">
      * jQuery ajax settings</a>
     */
    signup: function(user_doc, password, options) {
      options = options || {};
      user_doc.password = password;
      user_doc.roles = user_doc.roles || [];
      user_doc.type = "user";
      var user_prefix = "org.couchdb.user:";
      user_doc._id = user_doc._id || user_prefix + user_doc.name;

      return $.couch.userDb(function(db) {
        db.saveDoc(user_doc, options);
      });
    },

    /**
     * Authenticate against CouchDB, the <code>options</code> parameter is
      *expected to have <code>name</code> and <code>password</code> fields.
     * @see <a href="http://docs.couchdb.org/en/latest/api/server/authn.html
     * #post--_session">docs for POST /_session</a>
     * @param {ajaxSettings} options
     * <a href="http://api.jquery.com/jQuery.ajax/#jQuery-ajax-settings">
     * jQuery ajax settings</a>
     */
    login: function(options) {
      options = options || {};
      return $.ajax({
        type: "POST", url: this.urlPrefix + "/_session", dataType: "json",
        data: {name: options.name, password: options.password},
        beforeSend: function(xhr) {
            xhr.setRequestHeader('Accept', 'application/json');
        },
        complete: function(req) {
          var resp = $.parseJSON(req.responseText);
          if (req.status == 200) {
            if (options.success) options.success(resp);
          } else if (options.error) {
            options.error(req.status, resp.error, resp.reason);
          } else {
            throw 'An error occurred logging in: ' + resp.reason;
          }
        }
      });
    },


    /**
     * Delete your current CouchDB user session
     * @see <a href="http://docs.couchdb.org/en/latest/api/server/authn.html
     * #delete--_session">docs for DELETE /_session</a>
     * @param {ajaxSettings} options
     * <a href="http://api.jquery.com/jQuery.ajax/#jQuery-ajax-settings">
     * jQuery ajax settings</a>
     */
    logout: function(options) {
      options = options || {};
      return $.ajax({
        type: "DELETE", url: this.urlPrefix + "/_session", dataType: "json",
        username : "_", password : "_",
        beforeSend: function(xhr) {
            xhr.setRequestHeader('Accept', 'application/json');
        },
        complete: function(req) {
          var resp = $.parseJSON(req.responseText);
          if (req.status == 200) {
            if (options.success) options.success(resp);
          } else if (options.error) {
            options.error(req.status, resp.error, resp.reason);
          } else {
            throw 'An error occurred logging out: ' + resp.reason;
          }
        }
      });
    },

    /**
     * @namespace
     * $.couch.db is used to communicate with a specific CouchDB database
     * <pre><code>var $db = $.couch.db("mydatabase");
     *$db.allApps({
     *  success: function (data) {
     *    ... process data ...
     *  }
     *});
     * </code></pre>
     */
    db: function(name, db_opts) {
      db_opts = db_opts || {};
      var rawDocs = {};
      function maybeApplyVersion(doc) {
        if (doc._id && doc._rev && rawDocs[doc._id] &&
            rawDocs[doc._id].rev == doc._rev) {
          // todo: can we use commonjs require here?
          if (typeof Base64 == "undefined") {
            throw 'Base64 support not found.';
          } else {
            doc._attachments = doc._attachments || {};
            doc._attachments["rev-"+doc._rev.split("-")[0]] = {
              content_type :"application/json",
              data : Base64.encode(rawDocs[doc._id].raw)
            };
            return true;
          }
        }
      }
      return /** @lends $.couch.db */{
        name: name,
        uri: this.urlPrefix + "/" + encodeURIComponent(name) + "/",

        /**
         * Request compaction of the specified database.
         * @see <a href="http://docs.couchdb.org/en/latest/api/database
         * /compact.html#db-compact">docs for /db/_compact</a>
         * @param {ajaxSettings} options
         * <a href="http://api.jquery.com/jQuery.ajax/#jQuery-ajax-settings">
         * jQuery ajax settings</a>
         */
        compact: function(options) {
          $.extend(options, {successStatus: 202});
          return ajax({
              type: "POST", url: this.uri + "_compact",
              data: "", processData: false
            },
            options,
            "The database could not be compacted"
          );
        },

        /**
         * Cleans up the cached view output on disk for a given view.
         * @see <a href="http://docs.couchdb.org/en/latest/api/database
         * /compact.html#db-view-cleanup">docs for /db/_view_cleanup</a>
         * @param {ajaxSettings} options <a href="http://api.jquery.com/
         * jQuery.ajax/#jQuery-ajax-settings">jQuery ajax settings</a>
         */
        viewCleanup: function(options) {
          $.extend(options, {successStatus: 202});
          return ajax({
              type: "POST", url: this.uri + "_view_cleanup",
              data: "", processData: false
            },
            options,
            "The views could not be cleaned up"
          );
        },

        /**
         * Compacts the view indexes associated with the specified design
         * document. You can use this in place of the full database compaction
         * if you know a specific set of view indexes have been affected by a
         * recent database change.
         * @see <a href="http://docs.couchdb.org/en/latest/api/database
         * /compact.html#db-compact-design-doc">
         * docs for /db/_compact/design-doc</a>
         * @param {String} groupname Name of design-doc to compact
         * @param {ajaxSettings} options <a href="http://api.jquery.com/
         * jQuery.ajax/#jQuery-ajax-settings">jQuery ajax settings</a>
         */
        compactView: function(groupname, options) {
          $.extend(options, {successStatus: 202});
          return ajax({
              type: "POST", url: this.uri + "_compact/" + groupname,
              data: "", processData: false
            },
            options,
            "The view could not be compacted"
          );
        },

        /**
         * Create a new database
         * @see <a href="http://docs.couchdb.org/en/latest/api/database
         * /common.html#put--db">docs for PUT /db/</a>
         * @param {ajaxSettings} options <a href="http://api.jquery.com/
         * jQuery.ajax/#jQuery-ajax-settings">jQuery ajax settings</a>
         */
        create: function(options) {
          $.extend(options, {successStatus: 201});
          return ajax({
              type: "PUT", url: this.uri, contentType: "application/json",
              data: "", processData: false
            },
            options,
            "The database could not be created"
          );
        },

        /**
         * Deletes the specified database, and all the documents and
         * attachments contained within it.
         * @see <a href="http://docs.couchdb.org/en/latest/api/database
         * /common.html#delete--db">docs for DELETE /db/</a>
         * @param {ajaxSettings} options <a href="http://api.jquery.com/
         * jQuery.ajax/#jQuery-ajax-settings">jQuery ajax settings</a>
         */
        drop: function(options) {
          return ajax(
            {type: "DELETE", url: this.uri},
            options,
            "The database could not be deleted"
          );
        },

        /**
         * Gets information about the specified database.
         * @see <a href="http://docs.couchdb.org/en/latest/api/database
         * /common.html#get--db">docs for GET /db/</a>
         * @param {ajaxSettings} options <a href="http://api.jquery.com/
         * jQuery.ajax/#jQuery-ajax-settings">jQuery ajax settings</a>
         */
        info: function(options) {
          return ajax(
            {url: this.uri},
            options,
            "Database information could not be retrieved"
          );
        },

        /**
         * @namespace
         * $.couch.db.changes provides an API for subscribing to the changes
         * feed
         * <pre><code>var $changes = $.couch.db("mydatabase").changes();
         *$changes.onChange = function (data) {
         *    ... process data ...
         * }
         * $changes.stop();
         * </code></pre>
         */
        changes: function(since, options) {

          options = options || {};
          // set up the promise object within a closure for this handler
          var timeout = 100, db = this, active = true,
            listeners = [],
            xhr = null,
            promise = /** @lends $.couch.db.changes */ {
              /**
               * Add a listener callback
               * @see <a href="http://docs.couchdb.org/en/latest/api/database
               * /changes.html#db-changes">docs for /db/_changes</a>
               * @param {Function} fun Callback function to run when
               * notified of changes.
               */
            onChange : function(fun) {
              listeners.push(fun);
            },
              /**
               * Stop subscribing to the changes feed
               */
            stop : function() {
              active = false;
              if (xhr){
                xhr.abort();
              }
            }
          };
          // call each listener when there is a change
          function triggerListeners(resp) {
            $.each(listeners, function() {
              this(resp);
            });
          }
          // when there is a change, call any listeners, then check for
          // another change
          options.success = function(resp) {
            timeout = 100;
            if (active) {
              since = resp.last_seq;
              triggerListeners(resp);
              getChangesSince();
            }
          };
          options.error = function() {
            if (active) {
              setTimeout(getChangesSince, timeout);
              timeout = timeout * 2;
            }
          };
          // actually make the changes request
          function getChangesSince() {
            var opts = $.extend({heartbeat : 10 * 1000}, options, {
              feed : "longpoll",
              since : since
            });
            xhr = ajax(
              {url: db.uri + "_changes"+encodeOptions(opts)},
              options,
              "Error connecting to "+db.uri+"/_changes."
            );
          }
          // start the first request
          if (since) {
            getChangesSince();
          } else {
            db.info({
              success : function(info) {
                since = info.update_seq;
                getChangesSince();
              }
            });
          }
          return promise;
        },

        /**
         * Fetch all the docs in this db, you can specify an array of keys to
         * fetch by passing the <code>keys</code> field in the
         * <code>options</code>
         * parameter.
         * @see <a href="http://docs.couchdb.org/en/latest/api/database
         * /bulk-api.html#db-all-docs">docs for /db/all_docs/</a>
         * @param {ajaxSettings} options <a href="http://api.jquery.com/
         * jQuery.ajax/#jQuery-ajax-settings">jQuery ajax settings</a>
         */
        allDocs: function(options) {
          var type = "GET";
          var data = null;
          if (options["keys"]) {
            type = "POST";
            var keys = options["keys"];
            delete options["keys"];
            data = toJSON({ "keys": keys });
          }
          return ajax({
              type: type,
              data: data,
              url: this.uri + "_all_docs" + encodeOptions(options)
            },
            options,
            "An error occurred retrieving a list of all documents"
          );
        },

        /**
         * Fetch all the design docs in this db
         * @param {ajaxSettings} options <a href="http://api.jquery.com/
         * jQuery.ajax/#jQuery-ajax-settings">jQuery ajax settings</a>
         */
        allDesignDocs: function(options) {
          return this.allDocs($.extend(
            {startkey:"_design", endkey:"_design0"}, options));
        },

        /**
         * Fetch all the design docs with an index.html, <code>options</code>
         * parameter expects an <code>eachApp</code> field which is a callback
         * called on each app found.
         * @param {ajaxSettings} options <a href="http://api.jquery.com/
         * jQuery.ajax/#jQuery-ajax-settings">jQuery ajax settings</a>
         */
        allApps: function(options) {
          options = options || {};
          var self = this;
          if (options.eachApp) {
            return this.allDesignDocs({
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
                      } else if (ddoc._attachments &&
                                 ddoc._attachments["index.html"]) {
                        appPath = ['', name, ddoc._id, "index.html"].join('/');
                      }
                      if (appPath) options.eachApp(appName, appPath, ddoc);
                    }
                  });
                });
              }
            });
          } else {
            throw 'Please provide an eachApp function for allApps()';
          }
        },

        /**
         * Returns the specified doc from the specified db.
         * @see <a href="http://docs.couchdb.org/en/latest/api/document
         * /common.html#get--db-docid">docs for GET /db/doc</a>
         * @param {String} docId id of document to fetch
         * @param {ajaxSettings} options <a href="http://api.jquery.com/
         * jQuery.ajax/#jQuery-ajax-settings">jQuery ajax settings</a>
         * @param {ajaxSettings} ajaxOptions <a href="http://api.jquery.com/
         * jQuery.ajax/#jQuery-ajax-settings">jQuery ajax settings</a>
         */
        openDoc: function(docId, options, ajaxOptions) {
          options = options || {};
          if (db_opts.attachPrevRev || options.attachPrevRev) {
            $.extend(options, {
              beforeSuccess : function(req, doc) {
                rawDocs[doc._id] = {
                  rev : doc._rev,
                  raw : req.responseText
                };
              }
            });
          } else {
            $.extend(options, {
              beforeSuccess : function(req, doc) {
                if (doc["jquery.couch.attachPrevRev"]) {
                  rawDocs[doc._id] = {
                    rev : doc._rev,
                    raw : req.responseText
                  };
                }
              }
            });
          }
          return ajax(
            {url: this.uri + encodeDocId(docId) + encodeOptions(options)},
            options,
            "The document could not be retrieved",
            ajaxOptions
          );
        },

        /**
         * Create a new document in the specified database, using the supplied
         * JSON document structure. If the JSON structure includes the _id
         * field, then the document will be created with the specified document
         * ID. If the _id field is not specified, a new unique ID will be
         * generated.
         * @see <a href="http://docs.couchdb.org/en/latest/api/document
         * /common.html#put--db-docid">docs for PUT /db/doc</a>
         * @param {String} doc document to save
         * @param {ajaxSettings} options <a href="http://api.jquery.com/
         * jQuery.ajax/#jQuery-ajax-settings">jQuery ajax settings</a>
         */
        saveDoc: function(doc, options) {
          options = options || {};
          var db = this;
          var beforeSend = fullCommit(options);
          if (doc._id === undefined) {
            var method = "POST";
            var uri = this.uri;
          } else {
            var method = "PUT";
            var uri = this.uri + encodeDocId(doc._id);
          }
          var versioned = maybeApplyVersion(doc);
          return $.ajax({
            type: method, url: uri + encodeOptions(options),
            contentType: "application/json",
            dataType: "json", data: toJSON(doc),
            beforeSend : beforeSend,
            complete: function(req) {
              var resp = $.parseJSON(req.responseText);
              if (req.status == 200 || req.status == 201 || req.status == 202) {
                doc._id = resp.id;
                doc._rev = resp.rev;
                if (versioned) {
                  db.openDoc(doc._id, {
                    attachPrevRev : true,
                    success : function(d) {
                      doc._attachments = d._attachments;
                      if (options.success) options.success(resp);
                    }
                  });
                } else {
                  if (options.success) options.success(resp);
                }
              } else if (options.error) {
                options.error(req.status, resp.error, resp.reason);
              } else {
                throw "The document could not be saved: " + resp.reason;
              }
            }
          });
        },

        /**
         * Save a list of documents
         * @see <a href="http://docs.couchdb.org/en/latest/api/database
         * /bulk-api.html#db-bulk-docs">docs for /db/_bulk_docs</a>
         * @param {Object[]} docs List of documents to save
         * @param {ajaxSettings} options <a href="http://api.jquery.com/
         * jQuery.ajax/#jQuery-ajax-settings">jQuery ajax settings</a>
         */
        bulkSave: function(docs, options) {
          var beforeSend = fullCommit(options);
          $.extend(options, {successStatus: 201, beforeSend : beforeSend});
          return ajax({
              type: "POST",
              url: this.uri + "_bulk_docs" + encodeOptions(options),
              contentType: "application/json", data: toJSON(docs)
            },
            options,
            "The documents could not be saved"
          );
        },

        /**
         * Deletes the specified document from the database. You must supply
         * the current (latest) revision and <code>id</code> of the document
         * to delete eg <code>removeDoc({_id:"mydoc", _rev: "1-2345"})</code>
         * @see <a href="http://docs.couchdb.org/en/latest/api/document
         * /common.html#delete--db-docid">docs for DELETE /db/doc</a>
         * @param {Object} doc Document to delete
         * @param {ajaxSettings} options <a href="http://api.jquery.com/
         * jQuery.ajax/#jQuery-ajax-settings">jQuery ajax settings</a>
         */
        removeDoc: function(doc, options) {
          return ajax({
              type: "DELETE",
              url: this.uri +
                   encodeDocId(doc._id) +
                   encodeOptions({rev: doc._rev})
            },
            options,
            "The document could not be deleted"
          );
        },

        /**
         * Remove a set of documents
         * @see <a href="http://docs.couchdb.org/en/latest/api/database
         * /bulk-api.html#db-bulk-docs">docs for /db/_bulk_docs</a>
         * @param {String[]} docs List of document id's to remove
         * @param {ajaxSettings} options <a href="http://api.jquery.com/
         * jQuery.ajax/#jQuery-ajax-settings">jQuery ajax settings</a>
         */
        bulkRemove: function(docs, options){
          docs.docs = $.each(
            docs.docs, function(i, doc){
              doc._deleted = true;
            }
          );
          $.extend(options, {successStatus: 201});
          return ajax({
              type: "POST",
              url: this.uri + "_bulk_docs" + encodeOptions(options),
              data: toJSON(docs)
            },
            options,
            "The documents could not be deleted"
          );
        },

        /**
         * The COPY command (which is non-standard HTTP) copies an existing
         * document to a new or existing document.
         * @see <a href="http://docs.couchdb.org/en/latest/api/document
         * /common.html#copy--db-docid">docs for COPY /db/doc</a>
         * @param {String[]} docId document id to copy
         * @param {ajaxSettings} options <a href="http://api.jquery.com/
         * jQuery.ajax/#jQuery-ajax-settings">jQuery ajax settings</a>
         * @param {ajaxSettings} ajaxOptions <a href="http://api.jquery.com/
         * jQuery.ajax/#jQuery-ajax-settings">jQuery ajax settings</a>
         */
        copyDoc: function(docId, options, ajaxOptions) {
          ajaxOptions = $.extend(ajaxOptions, {
            complete: function(req) {
              var resp = $.parseJSON(req.responseText);
              if (req.status == 201) {
                if (options.success) options.success(resp);
              } else if (options.error) {
                options.error(req.status, resp.error, resp.reason);
              } else {
                throw "The document could not be copied: " + resp.reason;
              }
            }
          });
          return ajax({
              type: "COPY",
              url: this.uri + encodeDocId(docId)
            },
            options,
            "The document could not be copied",
            ajaxOptions
          );
        },

        /**
         * Creates (and executes) a temporary view based on the view function
         * supplied in the JSON request.
         * @see <a href="http://docs.couchdb.org/en/latest/api/database
         * /temp-views.html#db-temp-view">docs for /db/_temp_view</a>
         * @param {Function} mapFun Map function
         * @param {Function} reduceFun Reduce function
         * @param {String} language Language the map / reduce funs are
         * implemented in
         * @param {ajaxSettings} options <a href="http://api.jquery.com/
         * jQuery.ajax/#jQuery-ajax-settings">jQuery ajax settings</a>
         */
        query: function(mapFun, reduceFun, language, options) {
          language = language || "javascript";
          if (typeof(mapFun) !== "string") {
            mapFun = mapFun.toSource ? mapFun.toSource()
              : "(" + mapFun.toString() + ")";
          }
          var body = {language: language, map: mapFun};
          if (reduceFun != null) {
            if (typeof(reduceFun) !== "string")
              reduceFun = reduceFun.toSource ? reduceFun.toSource()
                : "(" + reduceFun.toString() + ")";
            body.reduce = reduceFun;
          }
          return ajax({
              type: "POST",
              url: this.uri + "_temp_view" + encodeOptions(options),
              contentType: "application/json", data: toJSON(body)
            },
            options,
            "An error occurred querying the database"
          );
        },

        /**
         * Fetch a _list view output, you can specify a list of
         * <code>keys</code> in the options object to receive only those keys.
         * @see <a href="http://docs.couchdb.org/en/latest/api/ddoc/render.html
         * #db-design-design-doc-list-list-name-view-name">
         * docs for /db/_design/design-doc/_list/list/view</a>
         * @param {String} list Listname in the form of ddoc/listname
         * @param {String} view View to run list against
         * @param {Object} options CouchDB <a href="http://docs.couchdb.org/en
         * /latest/api/ddoc/views.html#get--db-_design-ddoc-_view-view">
         * View Options</a>
         * @param {ajaxSettings} ajaxOptions <a href="http://api.jquery.com/
         * jQuery.ajax/#jQuery-ajax-settings">jQuery ajax settings</a>
         */
        list: function(list, view, options, ajaxOptions) {
          var list = list.split('/');
          var options = options || {};
          var type = 'GET';
          var data = null;
          if (options['keys']) {
            type = 'POST';
            var keys = options['keys'];
            delete options['keys'];
            data = toJSON({'keys': keys });
          }
          return ajax({
              type: type,
              data: data,
              url: this.uri + '_design/' + list[0] +
                   '/_list/' + list[1] + '/' + view + encodeOptions(options)
              },
              ajaxOptions, 'An error occurred accessing the list'
          );
        },

        /**
         * Executes the specified view-name from the specified design-doc
         * design document, you can specify a list of <code>keys</code>
         * in the options object to receive only those keys.
         * @see <a href="http://docs.couchdb.org/en/latest/api/ddoc/views.html
         * #db-design-design-doc-view-view-name">docs for /db/
         * _design/design-doc/_view/name</a>
         * @param {String} name View to run list against (string should have
         * the design-doc name followed by a slash and the view name)
         * @param {ajaxSettings} options <a href="http://api.jquery.com/
         * jQuery.ajax/#jQuery-ajax-settings">jQuery ajax settings</a>
         */
        view: function(name, options) {
          var name = name.split('/');
          var options = options || {};
          var type = "GET";
          var data= null;
          if (options["keys"]) {
            type = "POST";
            var keys = options["keys"];
            delete options["keys"];
            data = toJSON({ "keys": keys });
          }
          return ajax({
              type: type,
              data: data,
              url: this.uri + "_design/" + name[0] +
                   "/_view/" + name[1] + encodeOptions(options)
            },
            options, "An error occurred accessing the view"
          );
        },

        /**
         * Fetch an arbitrary CouchDB database property
         * @param {String} propName Property name to fetch
         * @param {ajaxSettings} options <a href="http://api.jquery.com/
         * jQuery.ajax/#jQuery-ajax-settings">jQuery ajax settings</a>
         * @param {ajaxSettings} ajaxOptions <a href="http://api.jquery.com/
         * jQuery.ajax/#jQuery-ajax-settings">jQuery ajax settings</a>
         */
        getDbProperty: function(propName, options, ajaxOptions) {
          return ajax({url: this.uri + propName + encodeOptions(options)},
            options,
            "The property could not be retrieved",
            ajaxOptions
          );
        },

        /**
         * Set an arbitrary CouchDB database property
         * @param {String} propName Property name to fetch
         * @param {String} propValue Property value to set
         * @param {ajaxSettings} options <a href="http://api.jquery.com/
         * jQuery.ajax/#jQuery-ajax-settings">jQuery ajax settings</a>
         * @param {ajaxSettings} ajaxOptions <a href="http://api.jquery.com/
         * jQuery.ajax/#jQuery-ajax-settings">jQuery ajax settings</a>
         */
        setDbProperty: function(propName, propValue, options, ajaxOptions) {
          return ajax({
            type: "PUT",
            url: this.uri + propName + encodeOptions(options),
            data : JSON.stringify(propValue)
          },
            options,
            "The property could not be updated",
            ajaxOptions
          );
        }
      };
    },

    encodeDocId: encodeDocId,

    /**
     * Accessing the root of a CouchDB instance returns meta information about
     * the instance. The response is a JSON structure containing information
     * about the server, including a welcome message and the version of the
     * server.
     * @see <a href="http://docs.couchdb.org/en/latest/api/server/common.html
     * #api-server-root">
     * docs for GET /</a>
     * @param {ajaxSettings} options <a href="http://api.jquery.com/
     * jQuery.ajax/#jQuery-ajax-settings">jQuery ajax settings</a>
     */
    info: function(options) {
      return ajax(
        {url: this.urlPrefix + "/"},
        options,
        "Server information could not be retrieved"
      );
    },

    /**
     * Request, configure, or stop, a replication operation.
     * @see <a href="http://docs.couchdb.org/en/latest/api/server/common.html
     * #replicate">docs for POST /_replicate</a>
     * @param {String} source Path or url to source database
     * @param {String} target Path or url to target database
     * @param {ajaxSettings} ajaxOptions <a href="http://api.jquery.com/
     * jQuery.ajax/#jQuery-ajax-settings">jQuery ajax settings</a>
     * @param {Object} repOpts Additional replication options
     */
    replicate: function(source, target, ajaxOptions, repOpts) {
      repOpts = $.extend({source: source, target: target}, repOpts);
      if (repOpts.continuous && !repOpts.cancel) {
        ajaxOptions.successStatus = 202;
      }
      return ajax({
          type: "POST", url: this.urlPrefix + "/_replicate",
          data: JSON.stringify(repOpts),
          contentType: "application/json"
        },
        ajaxOptions,
        "Replication failed"
      );
    },

    /**
     * Fetch a new UUID
     * @see <a href="http://docs.couchdb.org/en/latest/api/server/common.html
     * #uuids">docs for /_uuids</a>
     * @param {Integer} cacheNum Number of uuids to keep cached for future use
     */
    newUUID: function(cacheNum) {
      if (cacheNum === undefined) {
        cacheNum = 1;
      }
      if (!uuidCache.length) {
        ajax({url: this.urlPrefix + "/_uuids", data: {count: cacheNum}, async:
              false}, {
            success: function(resp) {
              uuidCache = resp.uuids;
            }
          },
          "Failed to retrieve UUID batch."
        );
      }
      return uuidCache.shift();
    }
  });

  /**
   * @private
   */
  function ajax(obj, options, errorMessage, ajaxOptions) {
    var timeStart;
    var defaultAjaxOpts = {
      contentType: "application/json",
      headers:{"Accept": "application/json"}
    };

    options = $.extend({successStatus: 200}, options);
    ajaxOptions = $.extend(defaultAjaxOpts, ajaxOptions);
    errorMessage = errorMessage || "Unknown error";
    timeStart = (new Date()).getTime();
    return $.ajax($.extend($.extend({
      type: "GET", dataType: "json", cache : maybeUseCache(),
      beforeSend: function(xhr){
        if(ajaxOptions && ajaxOptions.headers){
          for (var header in ajaxOptions.headers){
            xhr.setRequestHeader(header, ajaxOptions.headers[header]);
          }
        }
      },
      complete: function(req) {
        var reqDuration = (new Date()).getTime() - timeStart;
        try {
          var resp = $.parseJSON(req.responseText);
        } catch(e) {
          if (options.error) {
            options.error(req.status, req, e);
          } else {
            throw errorMessage + ': ' + e;
          }
          return;
        }
        if (options.ajaxStart) {
          options.ajaxStart(resp);
        }
        if (req.status == options.successStatus) {
          if (options.beforeSuccess) options.beforeSuccess(req, resp, reqDuration);
          if (options.success) options.success(resp, reqDuration);
        } else if (options.error) {
          options.error(req.status, resp && resp.error ||
                        errorMessage, resp && resp.reason || "no response",
                        reqDuration);
        } else {
          throw errorMessage + ": " + resp.reason;
        }
      }
    }, obj), ajaxOptions));
  }

  /**
   * @private
   */
  function fullCommit(options) {
    var options = options || {};
    if (typeof options.ensure_full_commit !== "undefined") {
      var commit = options.ensure_full_commit;
      delete options.ensure_full_commit;
      return function(xhr) {
        xhr.setRequestHeader('Accept', 'application/json');
        xhr.setRequestHeader("X-Couch-Full-Commit", commit.toString());
      };
    }
  }

  /**
   * @private
   */
  // Convert a options object to an url query string.
  // ex: {key:'value',key2:'value2'} becomes '?key="value"&key2="value2"'
  function encodeOptions(options) {
    var buf = [];
    if (typeof(options) === "object" && options !== null) {
      for (var name in options) {
        if ($.inArray(name,
                      ["error", "success", "beforeSuccess", "ajaxStart"]) >= 0)
          continue;
        var value = options[name];
        if ($.inArray(name, ["key", "startkey", "endkey"]) >= 0) {
          value = toJSON(value);
        }
        buf.push(encodeURIComponent(name) + "=" + encodeURIComponent(value));
      }
    }
    return buf.length ? "?" + buf.join("&") : "";
  }

  /**
   * @private
   */
  function toJSON(obj) {
    return obj !== null ? JSON.stringify(obj) : null;
  }

  /**
   * @private
   */
  function maybeUseCache() {
    if (!navigator){
      return true;
    }
    else if (/(MSIE|Trident)/.test(navigator.userAgent)){
      return false;
    }
    return true;
  }

})(jQuery);
