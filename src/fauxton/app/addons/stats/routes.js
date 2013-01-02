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
      layout: "two_pane",

      data: data,

      crumbs: [],

      views: {
        "#left-content": new Views.Pie({
          collection: data.stats,
          template: "by_method",
          datatype: "httpd_request_methods"
        }),
        "#right-content": new Views.Pie({
          collection: data.stats,
          template: "by_code",
          datatype: "httpd_status_codes"
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
