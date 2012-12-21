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

  grunt.registerMultiTask('template', 'generates an html file from a specified template', function(){
    var data = this.data;
    var _ = grunt.utils._;
    var tmpl = _.template(grunt.file.read(data.src), null, data.variables);
    grunt.file.write(data.dest, tmpl(data.variables));
  });

  grunt.registerMultiTask('get_deps', 'Fetch external dependencies', function() {
    var child_process = require('child_process');
    var async = require('async');
    var path = require('path');
    var done = this.async();
    var data = this.data;
    var target = data.target || "app/addons/";
    var settingsFile = path.existsSync(data.src) ? data.src : "settings.json.default";
    var settings = grunt.file.readJSON(settingsFile);
    var _ = grunt.utils._;
    var remoteDeps = _.filter(settings.deps, function(dep) { return !! dep.url; });

    async.forEach(remoteDeps, function(dep, cb) {
      var path = target + dep.name;
      var command = "git clone " + dep.url + " " + path;
      console.log("Cloning: " + dep.name + "(" + dep.url + ")");

      child_process.exec(command, function(error, stdout, stderr) {
        console.log(stderr);
        console.log(stdout);

        cb(error);
      });
    }, function(error) {
      if (error) {
        console.log("ERROR: " + error.message);
        done(false);
      } else {
        done();
      }
    });
  });

  grunt.registerMultiTask('gen_load_addons', 'Generate the load_addons.js file', function() {
    var path = require('path');
    var data = this.data;
    var _ = grunt.utils._;
    var settingsFile = path.existsSync(data.src) ? data.src : "settings.json.default";
    var settings = grunt.file.readJSON(settingsFile);
    var template = "app/load_addons.js.underscore";
    var dest = "app/load_addons.js";
    var deps = _.map(settings.deps, function(dep) {
      return "addons/" + dep.name + "/base";
    });
    var tmpl = _.template(grunt.file.read(template));
    grunt.file.write(dest, tmpl({deps: deps}));
  });

};