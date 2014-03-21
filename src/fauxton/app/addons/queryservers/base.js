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

  "addons/config/resources",

  "addons/queryservers/javascript",
  "addons/queryservers/coffeescript"
],

function(app, FauxtonAPI, Config, javascript, coffeescript) {
  // action happens in the inclues
  var QueryServers = FauxtonAPI.addon();
  var other_langs = {
    "coffeescript": coffeescript
  };
  QueryServers.initialize = function () {
    javascript.register();

    $.getJSON('/_config/query_servers').done(function(config) {
      _.each(_.keys(config), function(lang) {
        if (lang in other_langs) {
          other_langs[lang].register();
        }
      });
    });
  };
  return QueryServers;
});
