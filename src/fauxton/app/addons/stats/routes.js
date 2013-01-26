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
