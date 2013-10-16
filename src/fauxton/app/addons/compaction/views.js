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

    initialize: function () {
      _.bindAll(this);
    },

    events: {
      "click #compact-db": "compactDB",
      "click #compact-view": "compactDB",
      "click #cleanup-views": "cleanupViews"
    },

    disableButton: function (selector, text) {
      this.$(selector).attr('disabled', 'disabled').text(text);
    },

    enableButton: function (selector, text) {
      this.$(selector).removeAttr('disabled').text(text);
    },

    compactDB: function (event) {
      var enableButton = this.enableButton;
      event.preventDefault();

      this.disableButton('#compact-db', 'Compacting...');

      Compaction.compactDB(this.model).then(function () {
        FauxtonAPI.addNotification({
          type: 'success',
          msg: 'Database compaction has started.'
        });
      }, function (xhr, error, reason) {
        console.log(arguments);
        FauxtonAPI.addNotification({
          type: 'error',
          msg: 'Error: ' + JSON.parse(xhr.responseText).reason
        });
      }).always(function () {
        enableButton('#compact-db', 'Run');
      });
    },

    cleanupViews: function (event) {
      var enableButton = this.enableButton;
      event.preventDefault();

      this.disableButton('#cleanup-view', 'Cleaning...');

      Compaction.cleanupViews(this.model).then(function () {
        FauxtonAPI.addNotification({
          type: 'success',
          msg: 'View cleanup has started.'
        });
      }, function (xhr, error, reason) {
        console.log(arguments);
        FauxtonAPI.addNotification({
          type: 'error',
          msg: 'Error: ' + JSON.parse(xhr.responseText).reason
        });
      }).always(function () {
        enableButton('#cleanup-views', 'Run');
      });
    }
  });

  return Compaction;
});
