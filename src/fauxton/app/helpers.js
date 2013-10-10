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


// This file creates a set of helper functions that will be loaded for all html
// templates. These functions should be self contained and not rely on any 
// external dependencies as they are loaded prior to the application. We may
// want to change this later, but for now this should be thought of as a
// "purely functional" helper system.


define([
  "d3"
],

function() {

  var Helpers = {};

  Helpers.imageUrl = function(path) {
    // TODO: add dynamic path for different deploy targets
    return path;
  };


  // Get the URL for documentation, wiki, wherever we store it.
  // update the URLs in documentation_urls.js 
  Helpers.docs =  {
    "docs": "http://docs.couchdb.org/en/latest/intro/api.html#documents",
    "all_dbs": "http://docs.couchdb.org/en/latest/api/server/common.html?highlight=all_dbs#get--_all_dbs",
    "replication_doc": "http://docs.couchdb.org/en/latest/replication/replicator.html#basics",
    "design_doc": "http://docs.couchdb.org/en/latest/couchapp/ddocs.html#design-docs",
    "view_functions": "http://docs.couchdb.org/en/latest/couchapp/ddocs.html#view-functions",
    "map_functions": "http://docs.couchdb.org/en/latest/couchapp/ddocs.html#map-functions",
    "reduce_functions": "http://docs.couchdb.org/en/latest/couchapp/ddocs.html#reduce-and-rereduce-functions",
    "api_reference": "http://docs.couchdb.org/en/latest/http-api.html",
    "database_permission": "http://docs.couchdb.org/en/latest/api/database/security.html#db-security",
    "stats": "http://docs.couchdb.org/en/latest/api/server/common.html?highlight=stats#get--_stats",
    "_active_tasks": "http://docs.couchdb.org/en/latest/api/server/common.html?highlight=stats#active-tasks",
    "log": "http://docs.couchdb.org/en/latest/api/server/common.html?highlight=stats#log",
    "config": "http://docs.couchdb.org/en/latest/config/index.html",
    "views": "http://docs.couchdb.org/en/latest/intro/overview.html#views"
  }; 
  
  Helpers.getDocUrl = function(docKey){
    return Helpers.docs[docKey] || '#';
  };

  // File size pretty printing, taken from futon.format.js
  Helpers.formatSize = function(size) {
      var jump = 512;
      if (size < jump) return size + " bytes";
      var units = ["KB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB"];
      var i = 0;
      while (size >= jump && i < units.length) {
        i += 1;
        size /= 1024;
      }
      return size.toFixed(1) + ' ' + units[i - 1];
    };

  Helpers.formatDate = function(timestamp){
    format = d3.time.format("%b. %e at %H:%M%p"); 
    return format(new Date(timestamp*1000));
  };

  return Helpers;
});

