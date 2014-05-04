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
          msg: 'Database compaction has started. Visit <a href="#activetasks">Active Tasks</a> to view the compaction progress.',
          escape: false // beware of possible XSS when the message changes
        });
      }, function (xhr, error, reason) {
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
          msg: 'View cleanup has started. Visit <a href="#activetasks">Active Tasks</a> to view progress.',
          escape: false // beware of possible XSS when the message changes
        });
      }, function (xhr, error, reason) {
        FauxtonAPI.addNotification({
          type: 'error',
          msg: 'Error: ' + JSON.parse(xhr.responseText).reason
        });
      }).always(function () {
        enableButton('#cleanup-views', 'Run');
      });
    }
  });

  Compaction.CompactView = FauxtonAPI.View.extend({
    template: 'addons/compaction/templates/compact_view',
    className: 'btn btn-info pull-right',
    tagName: 'button',

    initialize: function () {
      _.bindAll(this);
    },

    events: {
      "click": "compact"
    },

    disableButton: function () {
      this.$el.attr('disabled', 'disabled').text('Compacting...');
    },

    enableButton: function () {
      this.$el.removeAttr('disabled').text('Compact View');
    },


    update: function (database, designDoc, viewName) {
      this.database = database;
      this.designDoc = designDoc;
      this.viewName = viewName;
    },

    compact: function (event) {
      event.preventDefault();
      var enableButton = this.enableButton;

      this.disableButton();

      Compaction.compactView(this.database, this.designDoc).then(function () {
        FauxtonAPI.addNotification({
          type: 'success',
          msg: 'View compaction has started. Visit <a href="#activetasks">Active Tasks</a> to view progress.',
          escape: false // beware of possible XSS when the message changes
        });
      }, function (xhr, error, reason) {
        FauxtonAPI.addNotification({
          type: 'error',
          msg: 'Error: ' + JSON.parse(xhr.responseText).reason
        });
      }).always(function () {
        enableButton();
      });

    }

  });

  return Compaction;
});
