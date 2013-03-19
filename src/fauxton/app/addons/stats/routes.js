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
  "addons/stats/resources",
  "addons/stats/views"
],

function(app, FauxtonAPI, Stats, Views) {
  var data = {
    stats: new Stats.Collection()
  };

  var deferred = FauxtonAPI.Deferred();

  var routeCallback = function() {
    return {
      layout: "with_sidebar",

      data: data,

      crumbs: [],

      views: {
        "#sidebar-content": new Views.StatSelect({
          collection: data.stats
        }),

        "#dashboard-content": new Views.Statistics({
          collection: data.stats
        })
      },

      establish: function() {
        return [data.stats.fetch()];
      },

      apiUrl: "_stats"
    };
  };

  Routes = {
    "stats": routeCallback,
    "_stats": routeCallback
  };

  return Routes;
});
