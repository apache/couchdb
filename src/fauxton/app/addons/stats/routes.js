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
       "addons/stats/views"
],

function(app, FauxtonAPI, Stats) {

  var StatsRouteObject = FauxtonAPI.RouteObject.extend({
    layout: "with_sidebar",

    routes: {
      "stats":"showStats",
      "_stats": "showStats"
    },


    crumbs: [
      {"name": "Statistics", "link": "_stats"}
    ],

    selectedHeader: "Statistics",

    initialize: function () {
      this.stats = new Stats.Collection();
      this.setView("#sidebar-content", new Stats.Views.StatSelect({
        collection: this.stats
      }));

    },

    showStats: function () {
      this.setView("#dashboard-content", new Stats.Views.Statistics({
        collection: this.stats
      }));
    },

    establish: function() {
      return [this.stats.fetch()];
    },

    apiUrl: function(){
      return [ this.stats.url, this.stats.documentation];
    }
  });

  Stats.RouteObjects = [StatsRouteObject];

  return Stats;
});
