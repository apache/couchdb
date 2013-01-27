// This is the main application configuration file.  It is a Grunt
// configuration file, which you can learn more about here:
// https://github.com/cowboy/grunt/blob/master/docs/configuring.md
module.exports = function(grunt) {

  var couch_config = {
    fauxton: {
              db: 'http://localhost:5984/fauxton',
              app: './couchapp.js',
              options: {
                okay_if_missing: true
              }
            }
  };

  var cleanable = function(){
    // Whitelist files and directories to be cleaned
    var path = require('path');
    // You'll always want to clean these two directories
    var theListToClean = ["dist/", "app/load_addons.js"];
    // Now find the external addons you have and add them for cleaning up
    if (path.existsSync("settings.json")){
      var settings = grunt.file.readJSON("settings.json") || {deps: []};
      settings.deps.forEach(function(addon){
        // Only clean addons that are included from a local dir
        if (addon.path){
          theListToClean.push("app/addons/" + addon.name);
        }
      });
    }
    return theListToClean;
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
          paths: ["assets/less"]
        },
        files: {
          "dist/debug/css/index.css": "assets/less/cloudant.less"
        }
      }
    },

    // The jshint option for scripturl is set to lax, because the anchor
    // override inside main.js needs to test for them so as to not accidentally
    // route.
    jshint: {
      options: {
        scripturl: true
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

    // Create static html files from templates, for managing change of script
    // or css name.
    template: {
      couchdb:{
        src: 'assets/index.underscore',
        dest: '../../share/www/fauxton/index.html',
        variables: {
          assets_root: '/_utils/fauxton/',
          requirejs: 'require.min.js',
          base: '/_utils/fauxton/'
        }
      },
      couchdebug:{
        src: 'assets/index.underscore',
        dest: '../../share/www/fauxton/index.html',
        variables: {
          assets_root: '/_utils/fauxton/',
          requirejs: 'require.js',
          base: '/_utils/fauxton/'
        }
      },
      release: {
        src: 'assets/index.underscore',
        dest: 'dist/release/index.html',
        variables: {
          assets_root: '/',
          requirejs: 'require.min.js',
          base: '/'
        }
      },
      debug: {
        src: 'assets/index.underscore',
        dest: 'dist/debug/index.html',
        variables: {
          assets_root: '/',
          requirejs: 'require.js',
          base: '/'
        }
      },
      couchapp: {
        src: 'assets/index.underscore',
        dest: 'dist/debug/index.html',
        variables: {
          assets_root: '/fauxton/_design/fauxton/',
          requirejs: 'require.js',
          base: '/fauxton/_design/fauxton/index.html'
        }
      }
    },

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
        src: ["dist/debug/css/index.css", 'assets/css/*.css'],
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

    // Running the server without specifying an action will run the defaults,
    // port: 8000 and host: 127.0.0.1.  If you would like to change these
    // defaults, simply add in the properties `port` and `host` respectively.
    // Alternatively you can omit the port and host properties and the server
    // task will instead default to process.env.PORT or process.env.HOST.
    //
    // Changing the defaults might look something like this:
    //
    // server: {
    //   host: "127.0.0.1", port: 9001
    //   debug: { ... can set host and port here too ...
    //  }
    //
    //  To learn more about using the server task, please refer to the code
    //  until documentation has been written.
    server: {
      // Ensure the favicon is mapped correctly.
      files: { "favicon.ico": "favicon.ico" },

      debug: {
        // Ensure the favicon is mapped correctly.
        "index": "./dist/debug/index.html",
        files: { "favicon.ico": "favicon.ico" },

        // Map `server:debug` to `debug` folders.
        folders: {
          "app": "dist/debug",
          "assets/js/libs": "dist/debug",
          "css": "dist/debug/css",
          "js": "dist/debug/js"
        }
      },

      release: {
        // This makes it easier for deploying, by defaulting to any IP.
        host: "0.0.0.0",

        // Ensure the favicon is mapped correctly.
        files: { "favicon.ico": "favicon.ico" },

        // Map `server:release` to `release` folders.
        folders: {
          "app": "dist/release",
          "assets/js/libs": "dist/release",
          "assets/css": "dist/release"
        }
      }
    },

    // This task uses James Burke's excellent r.js AMD build tool.  In the
    // future other builders may be contributed as drop-in alternatives.
    requirejs: {
      // Include the main configuration file.
      mainConfigFile: "app/config.js",

      // Output file.
      out: "dist/debug/require.js",

      // Root application module.
      name: "config",

      // Do not wrap everything in an IIFE.
      wrap: false
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

    copy: {
      couchdb: {
        files: {
          // this gets built in the template task
          //"../../share/www/fauxton/index.html": "dist/release/index.html",
          "../../share/www/fauxton/js/": "dist/release/js/**",
          // no images... yet
          "../../share/www/fauxton/img/": "dist/release/img/**",
          "../../share/www/fauxton/css/": "dist/release/css/**"
        }
      },
      couchdebug: {
        files: {
          // this gets built in the template task
          //"../../share/www/fauxton/index.html": "dist/release/index.html",
          "../../share/www/fauxton/js/": "dist/debug/js/**",
          // no images... yet
          "../../share/www/fauxton/img/": "dist/debug/img/**",
          "../../share/www/fauxton/css/": "dist/debug/css/**",
          // Must be possible to improve this...
          "../../share/www/fauxton/app/": "dist/debug/**"
        }
      },
      dist:{
        files:{
          "dist/release/js/": "assets/js/**",
          //"dist/release/css/**": "assets/css/**"
          "dist/release/img/": "assets/img/**"
        }
      },
      debug:{
        files:{
          "dist/debug/js/": "assets/js/**",
          //"dist/debug/css/": "dist/release/css/**"
          "dist/debug/img/": "assets/img/**"
        }
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
  grunt.loadNpmTasks('grunt-contrib-copy');
  // Load the exec task
  grunt.loadNpmTasks('grunt-exec');

  // clean out previous build artefacts, lint and unit test
  grunt.registerTask('test', 'clean lint'); //qunit
  // Fetch dependencies (from git or local dir), lint them and make load_addons
  grunt.registerTask('dependencies', 'get_deps lint gen_load_addons:default');
  // build templates, js and css
  grunt.registerTask('build', 'jst requirejs concat:requirejs less');
  // minify code and css, ready for release.
  grunt.registerTask("minify", "min mincss");
  // deafult task - push to CouchDB
  grunt.registerTask("default", "test dependencies build release install");
  grunt.registerTask("dev", "debug server:debug");
  // make a debug install
  grunt.registerTask("debug", "test dependencies build template:debug copy:debug concat:debug");
  // make an install that is server by mochiweb under _utils
  grunt.registerTask("couchdebug", "debug template:couchdebug copy:couchdebug");
  // make an install that can be deployed as a couchapp
  grunt.registerTask("couchapp_setup", "debug template:couchapp");
  grunt.registerTask("couchdb", "test build minify template:couchdb copy:couchdb");
  // build a release
  grunt.registerTask("release", "test dependencies build minify template:release copy:dist");
  // install fauxton as couchapp
  grunt.registerTask('couchapp_install', 'rmcouchdb:fauxton mkcouchdb:fauxton couchapp:fauxton');
  grunt.registerTask('couchapp_deploy', 'couchapp_setup couchapp_install');

};
