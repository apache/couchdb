// var prompt = require('prompt');

module.exports = function(grunt) {
  var _ = grunt.utils._;
  var prompts = [
    {
      name: "name",
      message: "Add on Name",
      validator: /^[\w\-\.]+$/,
      default: "WickedCool"
    },
    {
      name: "path",
      message: "Location of add ons",
      default: "app/addons"
    }
  ];

  addonTemplates = [
    {
      name: 'base',
      filename: 'base.js',
      template: 'define([\n  "app",\n  "api",\n  "addons/<%= module.toLowerCase() %>/routes"\n],\n\nfunction(app, FauxtonAPI, <%= module %>) {\n  return <%= module %>;\n});\n'
    },
    {
      name: 'resources',
      filename: 'resources.js',
      template: 'define([\n  "app",\n  "backbone",\n  "modules/fauxton/base"\n],\n\nfunction (app, backbone, Fauxton) {\n\n});\n'
    },
    {
      name: 'routes',
      filename: 'routes.js',
      template: 'define([\n  "app",\n  "api",\n  "addons/<%= module.toLowerCase() %>/resources"\n],\n\nfunction(app, FauxtonAPI, <%= module %>) {\n  return <%= module %>;\n});\n'
    }
  ]

  // Create a new task.
  grunt.registerInitTask('addon', 'Generate a skeleton for an addon"', function() {
    var done = this.async()
    grunt.helper('prompt', {}, prompts, function (err, result) {
      if (err) { return onErr(err); }
      var module = result.name
      filepath = result.path + '/' + module.toLowerCase();
      grunt.file.mkdir(filepath);
      _.each(addonTemplates, function(file){
        file.module = module.charAt(0).toUpperCase() + module.substr(1);
        var content = grunt.template.process(file.template, file);
        grunt.file.write(filepath + '/' + file.filename, content)
      });
      grunt.log.writeln('Created addon ' + result.name + ' in ' + result.path);
      done();
    });
    function onErr(err) {
      grunt.log.writeln(err);
      done();
      return 1;
    }
  });

};