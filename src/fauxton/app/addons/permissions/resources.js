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
function (app, FauxtonAPI ) {
  var Permissions = FauxtonAPI.addon();

  Permissions.Security = Backbone.Model.extend({
    defaults: {
      admins: {
        names: [],
        roles: []
      },

      members: {
        names: [],
        roles: []
      }
    },

    isNew: function () {
      return false;
    },

    initialize: function (attrs, options) {
      this.database = options.database;
    },

    url: function () {
      return this.database.id + '/_security';
    },

    addItem: function (value, type, section) {
      var sectionValues = this.get(section);

      if (!sectionValues || !sectionValues[type]) { 
        return {
          error: true,
          msg: 'Section ' + section + 'does not exist'
        };
      }

      if (sectionValues[type].indexOf(value) > -1) { 
        return {
          error: true,
          msg: 'Role/Name has already been added'
        }; 
      }

      sectionValues[type].push(value);
      return this.set(section, sectionValues);
    }

  });

  return Permissions;
});

