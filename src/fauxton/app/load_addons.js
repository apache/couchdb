define([
  "addons/demo/init",
  "addons/logs/base",
  "addons/config/base"
],

function() {
  var LoadAddons = {};

  // TODO: template this or build from a file
  LoadAddons.addons = [
    "addons/demo/init"
  ];

  return LoadAddons;
});
