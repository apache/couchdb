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

var couchapp = require('couchapp'),
    path = require('path'),
    ddoc;

ddoc = {
  _id: '_design/fauxton',
  rewrites: [
    { "from": "_db" ,    "to"  : "../.." },
    { "from": "_db/*" ,  "to"  : "../../*" },
    { "from": "_ddoc" ,  "to"  : "" },
    { "from": "_ddoc/*", "to"  : "*"},
    {from: '/', to: 'index.html'},
    {from: '/*', to: '*'}
  ],
  views: {},
  shows: {},
  lists: {},
  validate_doc_update: function(newDoc, oldDoc, userCtx) {
    /*if (newDoc._deleted === true && userCtx.roles.indexOf('_admin') === -1) {
      throw "Only admin can delete documents on this database.";
    }*/
  }
};


couchapp.loadAttachments(ddoc, path.join(__dirname, 'dist', 'release'));
module.exports = ddoc;
