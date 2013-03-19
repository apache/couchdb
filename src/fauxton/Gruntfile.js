// This is the main application configuration file.  It is a Grunt
// configuration file, which you can learn more about here:
// https://github.com/cowboy/grunt/blob/master/docs/configuring.md


module.exports = function(grunt) {
  var helper = require('./tasks/helper').init(grunt),
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

  var cleanable = function(){
    // Whitelist files and directories to be cleaned

    // You'll always want to clean these two directories
    var theListToClean = ["dist/", "app/load_addons.js"];
    // Now find the external addons you have and add them for cleaning up
    helper.processAddons(function(addon){
      // Only clean addons that are included from a local dir
      if (addon.path){
        theListToClean.push("app/addons/" + addon.name);
      }
    });
    return theListToClean;
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
    grunt.log.write(theAssets.img[0]);
    return theAssets;
  }();

  var templateSettings = function(){
    var defaultSettings = {
      "src": "assets/index.underscore",
      "dest": "dist/debug/index.html",
      "variables": {
        "assets_root": "./",
        "requirejs": "require.js",
        "base": null
      }
    };
    var settings = helper.readSettingsFile();
    return {template: settings.template || defaultSettings};
  }();

  grunt.initConfig({

    // The clean task ensures all files are removed from the dist/ directory so
    // that no files linger from previous builds.
    clean:  cleanable,

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
      all: ['app/**/*.js', 'Gruntfile.js'],
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
        src: ["assets/js/libs/almond.js", "dist/debug/templates.js", "dist/debug/require.js"],
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
        host: 'localhost',
        port: 5984,
        https: false
      }
    },

    watch: {
      files: './app/**/*',
      tasks: ['debug', 'template']
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
          optimize: "none"
      }
      }
    },

    // The headless QUnit testing environment is provided for "free" by Grunt.
    // Simply point the configuration to your test directory.
    qunit: {
      all: ["test/qunit/*.html"]
    },

    // The headless Jasmine testing is provided by grunt-jasmine-task. Simply
    // point the configuration to your test directory.
    jasmine: {
      all: ["test/jasmine/*.html"]
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
    couchapp: couch_config

  });

  /*
   * Load Grunt plugins
   */
  // Load fauxton specific tasks
  grunt.loadTasks('tasks');
  // Load the couchapp task
  grunt.loadNpmTasks('grunt-couchapp');
  // Load the copy task
  grunt.loadNpmTasks('grunt-contrib');
  // Load the exec task
  grunt.loadNpmTasks('grunt-exec');
  // Load Require.js task
  grunt.loadNpmTasks('grunt-requirejs');
  // Load UglifyJS task
  grunt.loadNpmTasks('grunt-contrib-uglify');
  // Load CSSMin task
  grunt.loadNpmTasks('grunt-contrib-cssmin');

  /*
   * Default task
   */
  // defult task - install minified app to local CouchDB
  grunt.registerTask('default', 'couchdb');

  /*
   * Transformation tasks
   */
  // clean out previous build artefacts, lint and unit test
  grunt.registerTask('test', ['clean', 'jshint']); //qunit
  // Fetch dependencies (from git or local dir), lint them and make load_addons
  grunt.registerTask('dependencies', ['get_deps', 'jshint', 'gen_load_addons:default']);
  // build templates, js and css
  grunt.registerTask('build', ['jst', 'requirejs', 'concat:requirejs', 'less', 'concat:index_css', 'template']);
  // minify code and css, ready for release.
  grunt.registerTask('minify', ['uglify', 'cssmin:compress']);

  /*
   * Build the app in either dev, debug, or release mode
   */
  // dev server
  grunt.registerTask('dev', ['debug', 'couchserver']);
  // build a debug release
  grunt.registerTask('debug', ['test', 'dependencies', 'build', 'copy:debug']);
  // build a release
  grunt.registerTask('release', ['test' ,'dependencies', 'build', 'minify', 'copy:dist']);

  /*
   * Install into CouchDB in either debug, release, or couchapp mode
   */
  // make a development install that is server by mochiweb under _utils
  grunt.registerTask('couchdebug', ['debug', 'copy:couchdebug']);
  // make a minimized install that is server by mochiweb under _utils
  grunt.registerTask('couchdb', ['release', 'copy:couchdb']);
  // make an install that can be deployed as a couchapp
  grunt.registerTask('couchapp_setup', ['build', 'minify', 'copy:dist']);
  // install fauxton as couchapp
  grunt.registerTask('couchapp_install', ['rmcouchdb:fauxton', 'mkcouchdb:fauxton', 'couchapp:fauxton']);
  // setup and install fauxton as couchapp
  grunt.registerTask('couchapp_deploy', ['couchapp_setup', 'couchapp_install']);
};
