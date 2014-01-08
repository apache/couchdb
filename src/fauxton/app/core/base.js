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
],

function() {
  var FauxtonAPI = {};

  FauxtonAPI.Deferred = function() {
    return $.Deferred();
  };

  FauxtonAPI.when = function (deferreds) {
    if (deferreds instanceof Array) {
      return $.when.apply(null, deferreds);
    }

    return $.when(deferreds);
  };

  FauxtonAPI.View = Backbone.View.extend({
    // This should return an array of promises, an empty array, or null
    establish: function() {
      return null;
    },

    loaderClassname: 'loader',

    disableLoader: false,

    forceRender: function () {
      this.hasRendered = false;
    }
  });

  FauxtonAPI.Model = Backbone.Model.extend({
    //add fetchOnce
  });

  return FauxtonAPI;
});

