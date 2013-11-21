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

// Set the require.js configuration for your application.
require.config({

  // Initialize the application with the main application file.
  deps: ["main"],

  paths: {
    // JavaScript folders.
    libs: "../assets/js/libs",
    plugins: "../assets/js/plugins",

    // Libraries.
    jquery: "../assets/js/libs/jquery",
    lodash: "../assets/js/libs/lodash",
    backbone: "../assets/js/libs/backbone",
    "backbone.layoutmanger": "../assets/js/plugins/backbone.layoutmanager",
    bootstrap: "../assets/js/libs/bootstrap",
    spin: "../assets/js/libs/spin.min",
    d3: "../assets/js/libs/d3",
    "nv.d3": "../assets/js/libs/nv.d3",
    "ace":"../assets/js/libs/ace"
  },

  baseUrl: '/',

  map: {
    "*": {
      'underscore': 'lodash'
    }
  },

  shim: {
    // Backbone library depends on lodash and jQuery.
    backbone: {
      deps: ["lodash", "jquery"],
      exports: "Backbone"
    },

    bootstrap: {
      deps: ["jquery"],
      exports: "Bootstrap"
    },

    "plugins/prettify": [],

    "plugins/jquery.form": ["jquery"]
  }
});
