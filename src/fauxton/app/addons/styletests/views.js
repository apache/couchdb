define([
  "app",
  "api"
],


function (app, FauxtonAPI) {
  var Views= {};

  Views.tests = FauxtonAPI.View.extend({
    template: "addons/styletests/templates/theme"
  });

  return Views;
});


