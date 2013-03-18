// This is the main application configuration file.  It is a Grunt
// configuration file, which you can learn more about here:
// https://github.com/cowboy/grunt/blob/master/docs/configuring.md

module.exports = function(grunt) {
  var path = require('path');
  var couch_config = {
    fauxton: {
              db: 'http://localhost:5984/fauxton',
              app: './couchapp.js',
              options: {
                okay_if_missing: true
              }
            }
  };

  function readSettingsFile () {
    if (path.existsSync("settings.json")) {
      return grunt.file.readJSON("settings.json")
    } else if (path.existsSync("settings.json.default")) {
      return grunt.file.readJSON("settings.json.default")
    } else {
      return {deps: []};
    }
  }

  function processAddons(callback){
    readSettingsFile().deps.forEach(callback);
  }

  var cleanable = function(){
    // Whitelist files and directories to be cleaned

    // You'll always want to clean these two directories
    var theListToClean = ["dist/", "app/load_addons.js"];
    // Now find the external addons you have and add them for cleaning up
    processAddons(function(addon){
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
    processAddons(function(addon){
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
      var root = addon.path || "app/addons/" + addon.name;
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
    var settings = readSettingsFile();
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

      debug: {
        src: ["dist/debug/css/*.css", 'assets/css/*.css'],
        dest: 'dist/debug/css/index.css'
      }

    },

    // This task uses the MinCSS Node.js project to take all your CSS files in
    // order and concatenate them into a single CSS file named index.css.  It
    // also minifies all the CSS as well.  This is named index.css, because we
    // only want to load one stylesheet in index.html.
    mincss: {
      "dist/release/css/index.css": [
        "dist/debug/css/index.css", 'assets/css/*.css',
        "app/addons/**/assets/css/*.css"
      ]
    },

    // Takes the built require.js file and minifies it for filesize benefits.
    min: {
      "dist/release/js/require.min.js": [
        "dist/debug/js/require.js"
      ]
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
          wrap: false
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
          {src: ["**"], dest: "../../share/www/fauxton/js/", cwd:'assets/js/',  expand: true},
          {src: ["**"], dest: "../../share/www/fauxton/img/", cwd:'dist/release/img/', expand: true},
          {src: ["**"], dest: "../../share/www/fauxton/css/", cwd:"dist/release/css/", expand: true},
          // Must be possible to improve this...
          {src: ["**"], dest: "../../share/www/fauxton/app/", cwd:"dist/release/", expand: true}
        ]
      },
      couchdebug: {
        files: [
          // this gets built in the template task
          {src: "dist/debug/index.html", dest: "../../share/www/fauxton/index.html"},
          {src: ["**"], dest: "../../share/www/fauxton/js/", cwd:'assets/js/',  expand: true},
          {src: ["**"], dest: "../../share/www/fauxton/img/", cwd:'dist/debug/img/', expand: true},
          {src: ["**"], dest: "../../share/www/fauxton/css/", cwd:"dist/debug/css/", expand: true},
          // Must be possible to improve this...
          {src: ["**"], dest: "../../share/www/fauxton/app/", cwd:"dist/debug/", expand: true}
        ]
      },
      dist:{
        files:[
          {src: ["**"], dest: "dist/release/js/", cwd:'assets/js/',  expand: true},
          {src: assets.img, dest: "dist/debug/img/", flatten: true, expand: true},
        ]
      },
      debug:{
        files:[
          {src: ["**"], dest: "dist/debug/js/", cwd:'assets/js/',  expand: true},
          {src: assets.img, dest: "dist/debug/img/", flatten: true, expand: true},
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
  // clean out previous build artefacts, lint and unit test
  grunt.registerTask('test',['clean','jshint']); //qunit
  // Fetch dependencies (from git or local dir), lint them and make load_addons
  grunt.registerTask('dependencies', ['get_deps', 'jshint', 'gen_load_addons:default']);
  // build templates, js and css
  grunt.registerTask('build', ['jst', 'requirejs', 'concat:requirejs','less']);
  // minify code and css, ready for release.
  grunt.registerTask('minify', ['min','mincss']);
  // deafult task - push to CouchDB
  grunt.registerTask('default', ['test','dependencies','build','release','install']);
  grunt.registerTask('dev', ['debug','template','couchserver']);
  // make a debug install
  grunt.registerTask('debug', ['test','dependencies', 'build','template','copy:debug', 'concat:debug']);
  // make an install that is server by mochiweb under _utils
  grunt.registerTask('couchdebug', ['debug', 'template', 'copy:couchdebug']);
  // make an install that can be deployed as a couchapp
  grunt.registerTask('couchapp_setup', ['debug', 'template']);
  grunt.registerTask('couchdb', ['test', 'dependencies', 'build', 'minify', 'template', 'copy:couchdb']);
  // build a release
  grunt.registerTask('release', ['test' ,'dependencies', 'build', 'minify','template', 'copy:dist']);
  // install fauxton as couchapp
  grunt.registerTask('couchapp_install', ['rmcouchdb:fauxton', 'mkcouchdb:fauxton', 'couchapp:fauxton']);
  grunt.registerTask('couchapp_deploy', ['couchapp_setup', 'couchapp_install']);

};
