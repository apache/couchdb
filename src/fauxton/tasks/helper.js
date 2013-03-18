var fs = require('fs');

exports.init = function(grunt) {

  return { 
    readSettingsFile: function () {
      if (fs.existsSync("settings.json")) {
        return grunt.file.readJSON("settings.json")
      } else if (fs.existsSync("settings.json.default")) {
        return grunt.file.readJSON("settings.json.default")
      } else {
        return {deps: []};
      }
    },

    processAddons: function (callback) {
      this.readSettingsFile().deps.forEach(callback);
    },
  };
}
