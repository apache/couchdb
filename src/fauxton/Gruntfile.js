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


// This is the main application configuration file.  It is a Grunt
// configuration file, which you can learn more about here:
// https://github.com/cowboy/grunt/blob/master/docs/configuring.md

module.exports = function(grunt) {
  var helper = require('./tasks/helper').init(grunt),
  _ = grunt.util._,
  path = require('path');

  var couch_config = function () {

    var default_couch_config = {
      fauxton: {
        db: 'http://localhost:5984/fauxton',
        app: './couchapp.js',
        options: {
          okay_if_missing: true
        }
      }
    };

    var settings_couch_config = helper.readSettingsFile().couch_config;
    return settings_couch_config || default_couch_config;
  }();

  var cleanableAddons =  function () {
    var theListToClean = [];
    helper.processAddons(function(addon){
      // Only clean addons that are included from a local dir
      if (addon.path){
        theListToClean.push("app/addons/" + addon.name);
      }
    });

    return theListToClean;
  }();

  var cleanable = function(){
    // Whitelist files and directories to be cleaned
    // You'll always want to clean these two directories
    // Now find the external addons you have and add them for cleaning up
    return _.union(["dist/", "app/load_addons.js"], cleanableAddons);
  }();

  var assets = function(){
    // Base assets
    var theAssets = {
      less:{
        paths: ["assets/less"],
        files: {
          "dist/debug/css/fauxton.css": "assets/less/fauxton.less"
        }
      },
      img: ["assets/img/**"]
    };
    helper.processAddons(function(addon){
      // Less files from addons
      var root = addon.path || "app/addons/" + addon.name;
      var lessPath = root + "/assets/less";
      if(path.existsSync(lessPath)){
        // .less files exist for this addon
        theAssets.less.paths.push(lessPath);
        theAssets.less.files["dist/debug/css/" + addon.name + ".css"] =
          lessPath + "/" + addon.name + ".less";
      }
      // Images
      root = addon.path || "app/addons/" + addon.name;
      var imgPath = root + "/assets/img";
      if(path.existsSync(imgPath)){
        theAssets.img.push(imgPath + "/**");
      }
    });
    return theAssets;
  }();

  var templateSettings = function(){
    var defaultSettings = {
     "development": {
        "src": "assets/index.underscore",
        "dest": "dist/debug/index.html",
        "variables": {
          "requirejs": "/assets/js/libs/require.js",
          "css": "./css/index.css",
          "base": null
        }
      },
      "release": {
        "src": "assets/index.underscore",
        "dest": "dist/debug/index.html",
        "variables": {
          "requirejs": "./js/require.js",
          "css": "./css/index.css",
          "base": null
        }
      }
    };

    var settings = helper.readSettingsFile();
    return settings.template || defaultSettings;
  }();

  grunt.initConfig({

    // The clean task ensures all files are removed from the dist/ directory so
    // that no files linger from previous builds.
    clean: {
      release:  cleanable,
      watch: cleanableAddons
    },

    // The lint task will run the build configuration and the application
    // JavaScript through JSHint and report any errors.  You can change the
    // options for this task, by reading this:
    // https://github.com/cowboy/grunt/blob/master/docs/task_lint.md
    lint: {
      files: [
        "build/config.js", "app/**/*.js" 
      ]
    },

    less: {
      compile: {
        options: {
          paths: assets.less.paths
        },
        files: assets.less.files
      }
    },

    // The jshint option for scripturl is set to lax, because the anchor
    // override inside main.js needs to test for them so as to not accidentally
    // route.
    jshint: {
      all: ['app/**/*.js', 'Gruntfile.js', "test/core/*.js"],
      options: {
        scripturl: true,
        evil: true
      }
    },

    // The jst task compiles all application templates into JavaScript
    // functions with the underscore.js template function from 1.2.4.  You can
    // change the namespace and the template options, by reading this:
    // https://github.com/gruntjs/grunt-contrib/blob/master/docs/jst.md
    //
    // The concat task depends on this file to exist, so if you decide to
    // remove this, ensure concat is updated accordingly.
    jst: {
      "dist/debug/templates.js": [
        "app/templates/**/*.html",
        "app/addons/**/templates/**/*.html"
      ]
    },

    template: templateSettings,

    // The concatenate task is used here to merge the almond require/define
    // shim and the templates into the application code.  It's named
    // dist/debug/require.js, because we want to only load one script file in
    // index.html.
    concat: {
      requirejs: {
        src: [ "assets/js/libs/require.js", "dist/debug/templates.js", "dist/debug/require.js"],
        dest: "dist/debug/js/require.js"
      },

      index_css: {
        src: ["dist/debug/css/*.css", 'assets/css/*.css'],
        dest: 'dist/debug/css/index.css'
      }

    },

    cssmin: {
      compress: {
        files: {
          "dist/release/css/index.css": [
            "dist/debug/css/index.css", 'assets/css/*.css',
            "app/addons/**/assets/css/*.css"
          ]
        },
        options: {
          report: 'min'
        }
      }
    },

    uglify: {
      release: {
        files: {
          "dist/release/js/require.js": [
            "dist/debug/js/require.js"
          ]
        }
      }
    },

    // Runs a proxy server for easier development, no need to keep deploying to couchdb
    couchserver: {
      dist: './dist/debug/',
      port: 8000,
      proxy: {
        target: {
          host: 'localhost',
          port: 5984,
          https: false
        },
        // This sets the Host header in the proxy so that you can use external
        // CouchDB instances and not have the Host set to 'localhost'
        changeOrigin: true
      }
    },

    watch: {
      js: { 
        files: helper.watchFiles(['.js'], ["./app/**/*.js", '!./app/load_addons.js',"./assets/**/*.js", "./test/**/*.js"]),
        tasks: ['watchRun'],
      },
      style: {
        files: helper.watchFiles(['.less','.css'],["./app/**/*.css","./app/**/*.less","./assets/**/*.css", "./assets/**/*.less"]),
        tasks: ['clean:watch', 'dependencies','less', 'concat:index_css'],
      },
      html: {
        // the index.html is added in as a dummy file incase there is no
        // html dependancies this will break. So we need one include pattern
        files: helper.watchFiles(['.html'], ['./index.html']),
        tasks: ['clean:watch', 'dependencies']
      },
      options: {
        nospawn: true,
      }
    },

    requirejs: {
      compile: {
        options: {
          baseUrl: 'app',
          // Include the main configuration file.
          mainConfigFile: "app/config.js",

          // Output file.
          out: "dist/debug/require.js",

          // Root application module.
          name: "config",

          // Do not wrap everything in an IIFE.
          wrap: false,
          optimize: "none",
        }
      }
    },

    // Copy build artifacts and library code into the distribution
    // see - http://gruntjs.com/configuring-tasks#building-the-files-object-dynamically
    copy: {
      couchdb: {
        files: [
          // this gets built in the template task
          {src: "dist/release/index.html", dest: "../../share/www/fauxton/index.html"},
          {src: ["**"], dest: "../../share/www/fauxton/js/", cwd:'dist/release/js/',  expand: true},
          {src: ["**"], dest: "../../share/www/fauxton/img/", cwd:'dist/release/img/', expand: true},
          {src: ["**"], dest: "../../share/www/fauxton/css/", cwd:"dist/release/css/", expand: true}
        ]
      },
      couchdebug: {
        files: [
          // this gets built in the template task
          {src: "dist/debug/index.html", dest: "../../share/www/fauxton/index.html"},
          {src: ["**"], dest: "../../share/www/fauxton/js/", cwd:'dist/debug/js/',  expand: true},
          {src: ["**"], dest: "../../share/www/fauxton/img/", cwd:'dist/debug/img/', expand: true},
          {src: ["**"], dest: "../../share/www/fauxton/css/", cwd:"dist/debug/css/", expand: true}
        ]
      },
      dist:{
        files:[
          {src: "dist/debug/index.html", dest: "dist/release/index.html"},
          {src: assets.img, dest: "dist/release/img/", flatten: true, expand: true}
        ]
      },
      debug:{
        files:[
          {src: assets.img, dest: "dist/debug/img/", flatten: true, expand: true}
        ]
      }
    },

    get_deps: {
      "default": {
        src: "settings.json"
      }
    },

    gen_load_addons: {
      "default": {
        src: "settings.json"
      }
    },

    mkcouchdb: couch_config,
    rmcouchdb: couch_config,
    couchapp: couch_config,

    mochaSetup: {
      default: {
        files: { src: helper.watchFiles(['[Ss]pec.js'], ['./test/core/**/*[Ss]pec.js', './app/**/*[Ss]pec.js'])},
        template: 'test/test.config.underscore',
        config: './app/config.js'
      }
    },

    mocha_phantomjs: {
      all: ['test/runner.html']
    }

  });

  // on watch events configure jshint:all to only run on changed file
  grunt.event.on('watch', function(action, filepath) {
    if (!!filepath.match(/.js$/)) {
      grunt.config(['jshint', 'all'], filepath);
    }

    if (!!filepath.match(/[Ss]pec.js$/)) {
      grunt.task.run(['mochaSetup','mocha_phantomjs']);
    }
  });

  /*
   * Load Grunt plugins
   */
  // Load fauxton specific tasks
  grunt.loadTasks('tasks');
  // Load the couchapp task
  grunt.loadNpmTasks('grunt-couchapp');
  // Load the copy task
  grunt.loadNpmTasks('grunt-contrib-watch');
  // Load the exec task
  grunt.loadNpmTasks('grunt-exec');
  // Load Require.js task
  grunt.loadNpmTasks('grunt-contrib-requirejs');
  // Load Copy task
  grunt.loadNpmTasks('grunt-contrib-copy');
  // Load Clean task
  grunt.loadNpmTasks('grunt-contrib-clean');
  // Load jshint task
  grunt.loadNpmTasks('grunt-contrib-jshint');
  // Load jst task
  grunt.loadNpmTasks('grunt-contrib-jst');
  // Load less task
  grunt.loadNpmTasks('grunt-contrib-less');
  // Load concat task
  grunt.loadNpmTasks('grunt-contrib-concat');
  // Load UglifyJS task
  grunt.loadNpmTasks('grunt-contrib-uglify');
  // Load CSSMin task
  grunt.loadNpmTasks('grunt-contrib-cssmin');
  grunt.loadNpmTasks('grunt-mocha-phantomjs');

  /*
   * Default task
   */
  // defult task - install minified app to local CouchDB
  grunt.registerTask('default', 'couchdb');

  /*
   * Transformation tasks
   */
  // clean out previous build artefactsa and lint
  grunt.registerTask('lint', ['clean', 'jshint']);
  grunt.registerTask('test', ['lint', 'mochaSetup', 'mocha_phantomjs']);
  // Fetch dependencies (from git or local dir), lint them and make load_addons
  grunt.registerTask('dependencies', ['get_deps', 'gen_load_addons:default']);
  // build templates, js and css
  grunt.registerTask('build', ['less', 'concat:index_css', 'jst', 'requirejs', 'concat:requirejs', 'template:release']);
  // minify code and css, ready for release.
  grunt.registerTask('minify', ['uglify', 'cssmin:compress']);

  /*
   * Build the app in either dev, debug, or release mode
   */
  // dev server
  grunt.registerTask('dev', ['debugDev', 'couchserver']);
  // build a debug release
  grunt.registerTask('debug', ['lint', 'dependencies', 'concat:requirejs','less', 'concat:index_css', 'template:development', 'copy:debug']);
  grunt.registerTask('debugDev', ['clean', 'dependencies','jshint','less', 'concat:index_css', 'template:development', 'copy:debug']);

  grunt.registerTask('watchRun', ['clean:watch', 'dependencies', 'jshint']);
  // build a release
  grunt.registerTask('release', ['clean' ,'dependencies','jshint', 'build', 'minify', 'copy:dist']);

  /*
   * Install into CouchDB in either debug, release, or couchapp mode
   */
  // make a development install that is server by mochiweb under _utils
  grunt.registerTask('couchdebug', ['debug', 'copy:couchdebug']);
  // make a minimized install that is server by mochiweb under _utils
  grunt.registerTask('couchdb', ['release', 'copy:couchdb']);
  // make an install that can be deployed as a couchapp
  grunt.registerTask('couchapp_setup', ['release']);
  // install fauxton as couchapp
  grunt.registerTask('couchapp_install', ['rmcouchdb:fauxton', 'mkcouchdb:fauxton', 'couchapp:fauxton']);
  // setup and install fauxton as couchapp
  grunt.registerTask('couchapp_deploy', ['couchapp_setup', 'couchapp_install']);
};
