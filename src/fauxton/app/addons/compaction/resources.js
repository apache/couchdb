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
  "api"
],

function (app, FauxtonAPI) {
  var Compaction = FauxtonAPI.addon();

  Compaction.compactDB = function (db) {
    return $.ajax({
      url: db.url() + '/_compact',
      contentType: 'application/json',
      type: 'POST'
    });
  };

  Compaction.cleanupViews = function (db) {
    return $.ajax({
      url: db.url() + '/_view_cleanup',
      contentType: 'application/json',
      type: 'POST'
    });
  };


  Compaction.compactView = function (db, designDoc) {
    // /some_database/_compact/designname
    return $.ajax({
      url: db.url() + '/_compact/' + designDoc.replace('_design/','') ,
      contentType: 'application/json',
      type: 'POST'
    });
  };

  return Compaction;
});
