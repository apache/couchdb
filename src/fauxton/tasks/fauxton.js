// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

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
    },
    {
      name: "assets",
      message: "Do you need an assets folder? (for .less)",
      default: 'y/N'
    }
  ];

  addonTemplates = [
    {
      name: 'base',
      filename: 'base.js',
      template: grunt.file.read('./tasks/templates/base.js.underscore')
    },
    {
      name: 'resources',
      filename: 'resources.js',
      template: grunt.file.read('./tasks/templates/resources.js.underscore')
    },
    {
      name: 'routes',
      filename: 'routes.js',
      template: grunt.file.read('./tasks/templates/route.js.underscore')
    }
  ]

  // Create a new task.
  grunt.registerInitTask('addon', 'Generate a skeleton for an addon"', function() {
    var done = this.async()
    grunt.helper('prompt', {}, prompts, function (err, result) {
      if (err) { return onErr(err); }
      grunt.log.writeln(result.assets);
      var module = result.name,
          assets = result.assets;
      if (assets == 'y') {
        //if you need an assets folder
        filepath = result.path + '/' + module.toLowerCase() + '/assets/less';
        grunt.file.mkdir(filepath);
        lessfile = {
          name: 'less',
          filename: module.toLowerCase()+'.less',
          template: '//<%= module %> styles'
        }
        lessfile.module = module.charAt(0).toUpperCase() + module.substr(1);
        var content = grunt.template.process(lessfile.template, lessfile);
        grunt.file.write(filepath + '/' + lessfile.filename, content);
      }
      filepath = result.path + '/' + module.toLowerCase() + '/templates';
      grunt.file.mkdir(filepath);
      filepath = result.path + '/' + module.toLowerCase();
      _.each(addonTemplates, function(file){
        file.module = module.charAt(0).toUpperCase() + module.substr(1);
        var content = grunt.template.process(file.template, file);
        grunt.file.write(filepath + '/' + file.filename, content);
      });
      grunt.log.writeln('Created addon ' + result.name + ' in ' + result.path);
      grunt.log.writeln('\n\nAdd ' + result.name + ' to settings.json for it to be compiled and deployed');
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
    grunt.log.writeln("Fetching external dependencies");

    var path = require('path');
    var done = this.async();
    var data = this.data;
    var target = data.target || "app/addons/";
    var settingsFile = path.existsSync(data.src) ? data.src : "settings.json.default";
    var settings = grunt.file.readJSON(settingsFile);
    var _ = grunt.utils._;

    // This should probably be a helper, though they seem to have been removed
    var fetch = function(deps, command){
      var child_process = require('child_process');
      var async = require('async');
      async.forEach(deps, function(dep, cb) {
        var path = target + dep.name;
        var location = dep.url || dep.path;
        grunt.log.writeln("Fetching: " + dep.name + " (" + location + ")");

        child_process.exec(command(dep, path), function(error, stdout, stderr) {
          grunt.log.writeln(stderr);
          grunt.log.writeln(stdout);
          cb(error);
        });
      }, function(error) {
        if (error) {
          grunt.log.writeln("ERROR: " + error.message);
          return false;
        } else {
          return true;
        }
      });
    };

    var remoteDeps = _.filter(settings.deps, function(dep) { return !! dep.url; });
    grunt.log.writeln(remoteDeps.length + " remote dependencies");
    var remote = fetch(remoteDeps, function(dep, destination){
      return "git clone " + dep.url + " " + destination;
    });

    var localDeps = _.filter(settings.deps, function(dep) { return !! dep.path; });
    grunt.log.writeln(localDeps.length + " local dependencies");
    var local = fetch(localDeps, function(dep, destination){
      // TODO: Windows
      var command = "cp -r " + dep.path + " " + destination;
      grunt.log.writeln(command);
      return command;
    });

    done(remote && local);

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
