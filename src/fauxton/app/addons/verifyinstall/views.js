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
  "addons/verifyinstall/resources",
],
function(app, FauxtonAPI, VerifyInstall) {

  VerifyInstall.Main = FauxtonAPI.View.extend({
    template: 'addons/verifyinstall/templates/main',

    events: {
      "click #start": "startTest"
    },

    initialize: function (options) {
      _.bindAll(this);
    },

    setPass: function (id) {
      this.$('#' + id).html('&#10003;');
    },

    setError: function (id, msg) {
      this.$('#' + id).html('&#x2717;');
      FauxtonAPI.addNotification({
        msg: 'Error: ' + msg,
        type: 'error',
        selector: '#error'
      });
    },

    complete: function () {
      FauxtonAPI.addNotification({
        msg: 'Success! You Couchdb install is working. Time to Relax',
        type: 'success',
        selector: '#error'
      });
    },

    enableButton: function () {
      this.$('#start').removeAttr('disabled').text('Verify Installation');
    },

    disableButton: function () {
      this.$('#start').attr('disabled','disabled').text('Verifying');
    },

    formatError: function (id) {
      var enableButton = this.enableButton,
          setError = this.setError;

      return function (xhr, error, reason) {
        enableButton();

        if (!xhr) { return; }

        setError(id, JSON.parse(xhr.responseText).reason);
      };
    },

    
    startTest: function () {
      this.disableButton();
      this.$('.status').text('');

      var testProcess = VerifyInstall.testProcess,
          setPass = this.setPass,
          complete = this.complete,
          setError = this.setError,
          formatError = this.formatError;

      testProcess.setup()
      .then(function () {
        return testProcess.saveDB();
      }, formatError('create-database'))
      .then(function () {
        setPass('create-database');
        return testProcess.saveDoc();
      }, formatError('create-document'))
      .then(function () {
        setPass('create-document');
        return testProcess.updateDoc();
      }, formatError('update-document'))
      .then(function () {
        setPass('update-document');
        return testProcess.destroyDoc();
      }, formatError('delete-document'))
      .then(function () {
        setPass('delete-document');
        return testProcess.setupView();
      }, formatError('create-view'))
      .then(function () {
        return testProcess.testView();
      }, formatError('create-view'))
      .then(function () {
        setPass('create-view');
        return testProcess.setupReplicate();
      }, formatError('create-view'))
      .then(function () {
        return testProcess.testReplicate();
      }, formatError('replicate'))
      .then(function () {
          setPass('replicate');
          complete();
          testProcess.removeDBs();
      }, formatError('replicate'));

      this.enableButton();
    }
  });


  return VerifyInstall;

});
