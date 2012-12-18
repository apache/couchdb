define([
  "addons/demo/init",
  "addons/logs/base",
  "addons/config/base"
],
function() {
  // TODO: figure out a way to have this outside for sharing in the
  // define block above
  var FauxtonLoadAddons = [
    "addons/demo/init",
    "addons/logs/base",
    "addons/config/base"
  ];

  var LoadAddons = {
    addons: FauxtonLoadAddons
  };

  return LoadAddons;
});
