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

module.exports = function(grunt) {
  var _ = grunt.util._;

  grunt.registerMultiTask('template', 'generates an html file from a specified template', function(){
    var data = this.data;
    var _ = grunt.util._;
    var tmpl = _.template(grunt.file.read(data.src), null, data.variables);
    grunt.file.write(data.dest, tmpl(data.variables));
  });

  grunt.registerMultiTask('get_deps', 'Fetch external dependencies', function(version) {
    grunt.log.writeln("Fetching external dependencies");

    var path = require('path');
    var done = this.async();
    var data = this.data;
    var target = data.target || "app/addons/";
    var settingsFile = path.existsSync(data.src) ? data.src : "settings.json.default";
    var settings = grunt.file.readJSON(settingsFile);
    var _ = grunt.util._;

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
    var _ = grunt.util._;
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

  grunt.registerMultiTask('gen_initialize', 'Generate the initialize.js file', function() {
    var path = require('path');
    var data = this.data;
    var _ = grunt.util._;
    var settingsFile = path.existsSync(data.src) ? data.src : "settings.json.default";
    var settings = grunt.file.readJSON(settingsFile);
    var template = "app/initialize.js.underscore";
    var dest = "app/initialize.js";
    var root = settings.root || "/";
    var tmpl = _.template(grunt.file.read(template));
    grunt.file.write(dest, tmpl({root: root}));
  });

  grunt.registerMultiTask('mochaSetup','Generate a config.js and runner.html for tests', function(){
    var data = this.data,
        configInfo,
        _ = grunt.util._,
        configTemplateSrc = data.template,
        testFiles = grunt.file.expand(data.files.src);

    var configTemplate = _.template(grunt.file.read(configTemplateSrc));
    // a bit of a nasty hack to read our current config.js and get the info so we can change it 
    // for our testing setup
    var require = {
      config: function (args) {
        configInfo = args;
        configInfo.paths['chai'] = "../test/mocha/chai";
        configInfo.paths['sinon-chai'] = "../test/mocha/sinon-chai";
        configInfo.paths['testUtils'] = "../test/mocha/testUtils";
        configInfo.baseUrl = '../app';
        delete configInfo.deps;
      }
    };

    eval(grunt.file.read(data.config) +'');

    grunt.file.write('./test/test.config.js', configTemplate({configInfo: configInfo, testFiles: testFiles}));
  });
};
