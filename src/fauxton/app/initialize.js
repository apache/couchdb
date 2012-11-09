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
    root: "/_utils/fauxton/",
    host: "http://localhost:5984",

    renderView: function(baseView, selector, view, options, callback) {
      baseView.setView(selector, new view(options)).render().then(callback);
    },

    // Thanks to: http://stackoverflow.com/a/2880929
    getParams: function(queryString) {
      if (typeof queryString !== "undefined") {
        if (queryString.substring(0,1) === "?")
          queryString = queryString.substring(1);
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
