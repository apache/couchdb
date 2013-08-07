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
  // Application.
  "app",

  // Libraries
  "lodash",
  "bootstrap"
],

function(app, _, Bootstrap) {

  // Provide a global location to place configuration settings and module
  // creation.
  _.extend(app, {
    // The root path to run the application through.
    // TODO: pick this up wither at build time or from the browser
    root: "/_utils/fauxton/",


    renderView: function(baseView, selector, view, options, callback) {
      baseView.setView(selector, new view(options)).render().then(callback);
    },

    // Thanks to: http://stackoverflow.com/a/2880929
    getParams: function(queryString) {
      if (queryString) {
        // I think this could be combined into one if
        if (queryString.substring(0,1) === "?") {
          queryString = queryString.substring(1);
        } else if (queryString.indexOf('?') > -1) {
          queryString = queryString.split('?')[1];
        }
      }
      var hash = window.location.hash.split('?')[1];
      queryString = queryString || hash || window.location.search.substring(1);
      var match,
      urlParams = {},
      pl     = /\+/g,  // Regex for replacing addition symbol with a space
      search = /([^&=]+)=?([^&]*)/g,
      decode = function (s) { return decodeURIComponent(s.replace(pl, " ")); },
      query  = queryString;

      if (queryString) {
        while ((match = search.exec(query))) {
          urlParams[decode(match[1])] = decode(match[2]);
        }
      }

      return urlParams;
    }
  });

});
