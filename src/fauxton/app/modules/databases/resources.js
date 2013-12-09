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

define([
  "app",

  "api",

  // Modules
  "modules/documents/resources"
],

function(app, FauxtonAPI, Documents) {
  var Databases = FauxtonAPI.module();

  Databases.DocLimit = 20;

  Databases.Model = Backbone.Model.extend({
    initialize: function(options) {
      this.status = new Databases.Status({
        database: this
      });
    },

    documentation: function(){
      return "all_dbs";
    },
    
    buildAllDocs: function(params) {
      this.allDocs = new Documents.AllDocs(null, {
        database: this,
        params: params
      });

      return this.allDocs;
    },

    isNew: function(){
      // Databases are never new, to make Backbone do a PUT
      return false;
    },

    url: function(context) {
      if (context === "index") {
        return "/database/" + this.id + "/_all_docs";
      } else if (context === "web-index") {
        return "#/database/"+ encodeURIComponent(this.get("name"))  + "/_all_docs?limit=" + Databases.DocLimit;
      } else if (context === "changes") {
        return "/database/" + this.id + "/_changes?descending=true&limit=100&include_docs=true";
      } else if (context === "app") {
        return "/database/" + this.id;
      } else {
        return app.host + "/" + this.id;
      }
    },

    buildChanges: function (params) {
      this.changes = new Databases.Changes({
        database: this,
        params: params
      });

      return this.changes;
    }
  });

  Databases.Changes = Backbone.Collection.extend({

    initialize: function(options) {
      this.database = options.database;
      this.params = options.params;
    },

    url: function () {
      var query = "";
      if (this.params) {
        query = "?" + $.param(this.params);
      }

      return app.host + '/' + this.database.id + '/_changes' + query;
    },

    parse: function (resp) {
      this.last_seq = resp.last_seq;
      return resp.results;
    }
  });

  Databases.Status = Backbone.Model.extend({
    url: function() {
      return app.host + "/" + this.database.id;
    },

    initialize: function(options) {
      this.database = options.database;
    },

    numDocs: function() {
      return this.get("doc_count");
    },

    updateSeq: function(full) {
      var updateSeq = this.get("update_seq");
      if (full || (typeof(updateSeq) === 'number')) {
        return updateSeq;
      } else if (updateSeq) {
        return updateSeq.split('-')[0];
      } else {
        return 0;
      }
    },

    humanSize: function() {
      // cribbed from http://stackoverflow.com/questions/10420352/converting-file-size-in-bytes-to-human-readable
      var i = -1;
      var byteUnits = [' kB', ' MB', ' GB', ' TB', 'PB', 'EB', 'ZB', 'YB'];
      var fileSizeInBytes = this.diskSize();

      if (!fileSizeInBytes) {
        return 0;
      }

      do {
          fileSizeInBytes = fileSizeInBytes / 1024;
          i++;
      } while (fileSizeInBytes > 1024);

      return Math.max(fileSizeInBytes, 0.1).toFixed(1) + byteUnits[i];
    },

    diskSize: function () {
      return this.get("disk_size");
    }
  });

  // TODO: shared databases - read from the user doc
  Databases.List = Backbone.Collection.extend({
    model: Databases.Model,
    documentation: function(){
      return "all_dbs";
    },
    url: function() {
      return app.host + "/_all_dbs";
    },

    parse: function(resp) {
      // TODO: pagination!
      return _.map(resp, function(database) {
        return {
          id: encodeURIComponent(database),
          name: database
        };
      });
    }
  });

  return Databases;
});
