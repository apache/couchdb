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
  "modules/databases/routes",
  // Views
  "modules/databases/views"

],

function(app, FauxtonAPI, Databases, Views) {
  Databases.Views = Views;

  // Utility functions
  Databases.databaseUrl = function(database) {
    var name = _.isObject(database) ? database.id : database,
        dbname = app.mixins.safeURLName(name);

    return ["/database/", dbname, "/_all_docs?limit=" + Databases.DocLimit].join('');
  };

  return Databases;
});
