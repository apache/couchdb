// This is the main application configuration file.  It is a Grunt
// configuration file, which you can learn more about here:
// https://github.com/cowboy/grunt/blob/master/docs/configuring.md
module.exports = function(grunt) {

  grunt.initConfig({

    // The clean task ensures all files are removed from the dist/ directory so
    // that no files linger from previous builds.
    clean: ["dist/"],

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
        "app/templates/**/*.html"
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
          requirejs: 'require.min.js'
        }
      },
      release: {
        src: 'assets/index.underscore',
        dest: 'dist/release/index.html',
        variables: {
          assets_root: '/',
          requirejs: 'require.min.js'
        }
      },
      debug: {
        src: 'assets/index.underscore',
        dest: 'dist/debug/index.html',
        variables: {
          assets_root: '/',
          requirejs: 'require.js'
        }
      }
    },

    // The concatenate task is used here to merge the almond require/define
    // shim and the templates into the application code.  It's named
    // dist/debug/require.js, because we want to only load one script file in
    // index.html.
    concat: {
      "dist/debug/js/require.js": [
        "assets/js/libs/almond.js",
        "dist/debug/templates.js",
        "dist/debug/require.js"
      ]
    },

    // This task uses the MinCSS Node.js project to take all your CSS files in
    // order and concatenate them into a single CSS file named index.css.  It
    // also minifies all the CSS as well.  This is named index.css, because we
    // only want to load one stylesheet in index.html.
    mincss: {
      "dist/release/css/index.css": [
        "dist/debug/css/index.css", 'assets/css/codemirror.css'
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
        files: { "favicon.ico": "favicon.ico" },

        // Map `server:debug` to `debug` folders.
        folders: {
          "app": "dist/debug",
          "assets/js/libs": "dist/debug"
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
      dist: {
        files: {
          // this gets built in the template task
          //"../../share/www/fauxton/index.html": "dist/release/index.html",
          "../../share/www/fauxton/js/": "dist/release/js/**",
          // no images... yet
          //"../../share/www/fauxton/img/": "dist/release/img/**",
          "../../share/www/fauxton/css/": "dist/release/css/**"
        }
      },
      debug:{
        files:{
          "dist/release/js/": "assets/js/**",
          "dist/release/img": "assets/img/**"
        }
      }
    }

  });

  grunt.registerMultiTask('template', 'generates an html file from a specified template', function(){
    var data = this.data;
    var _ = grunt.utils._;
    var tmpl = _.template(grunt.file.read(data.src), null, data.variables);
    grunt.file.write(data.dest, tmpl(data.variables));
  });

  // Load the couchapp task
  grunt.loadNpmTasks('grunt-couchapp');
  // Load the copy task
  grunt.loadNpmTasks('grunt-contrib-copy');
  // clean out previous build artefacts, lint and unit test
  grunt.registerTask('test', 'clean lint qunit');
  // build templates, js and css
  grunt.registerTask('build', 'jst requirejs concat less')
  // minify code and css, ready for release.
  grunt.registerTask("minify", "min mincss");
  // deafult task - push to CouchDB
  grunt.registerTask("default", "test build release install");
  // make a debug install
  grunt.registerTask("debug", "test build template:debug copy:debug install");
  // make an install that is server by mochiweb under _utils
  grunt.registerTask("couchdb", "test build minify template:couchdb copy:dist");
  // build a release
  grunt.registerTask("release", "minify template:release copy:dist");
  // copy build artifacts into release dir, upload into server
  grunt.registerTask('install', 'release mkcouchdb couchapp');
};
