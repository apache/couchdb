define([
  "app",
  "api",
  "addons/stats/routes"
],

function(app, FauxtonAPI, AddonRoutes) {
  var Stats = new FauxtonAPI.addon();

  Stats.initialize = function() {
    FauxtonAPI.addHeaderLink({title: "Statistics", href: "#stats"});
  };

  Stats.Routes = AddonRoutes;
  return Stats;
});
