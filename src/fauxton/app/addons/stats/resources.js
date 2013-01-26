define([
       "app",
       "api",
       "backbone",
       "lodash",
       "modules/fauxton/base"
],

function (app, FauxtonAPI, backbone, _, Fauxton) {
  var Stats = {};

  Stats.Collection = Backbone.Collection.extend({
    model: Backbone.Model,
    url: "/_stats",
    parse: function(resp) {
      return _.flatten(_.map(resp, function(doc, key) {
        return _.map(doc, function(v, k){
          return _.extend({id: k, type: key}, v);
        });
      }), true);
    }
  });

  return Stats;
});
