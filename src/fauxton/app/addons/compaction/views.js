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
       "addons/compaction/resources"
],
function (app, FauxtonAPI, Compaction) {

  Compaction.Layout = FauxtonAPI.View.extend({
    template: 'addons/compaction/templates/layout',

    events: {
      "click compact-db": "compactDB"
    },

    compactDB: function (event) {
      event.preventDefault();
    }


  });



  return Compaction;
});
