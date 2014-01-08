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


// This file creates a set of helper functions that will be loaded for all html
// templates. These functions should be self contained and not rely on any 
// external dependencies as they are loaded prior to the application. We may
// want to change this later, but for now this should be thought of as a
// "purely functional" helper system.


define([
  "jquery",
  "lodash"
],

function($, _ ) {

  var onWindowResize = {};

  var utils = {
    // Thanks to: http://stackoverflow.com/a/2880929
    getParams: function(queryString) {
      if (queryString) {
        // I think this could be combined into one if
        if (queryString.substring(0,1) === "?") {
          queryString = queryString.substring(1);
        } else if (queryString.indexOf('?') > -1) {
          queryString = queryString.split('?')[1];
        }
      }
      var hash = window.location.hash.split('?')[1];
      queryString = queryString || hash || window.location.search.substring(1);
      var match,
      urlParams = {},
      pl     = /\+/g,  // Regex for replacing addition symbol with a space
      search = /([^&=]+)=?([^&]*)/g,
      decode = function (s) { return decodeURIComponent(s.replace(pl, " ")); },
      query  = queryString;

      if (queryString) {
        while ((match = search.exec(query))) {
          urlParams[decode(match[1])] = decode(match[2]);
        }
      }

      return urlParams;
    },

    addWindowResize: function(fun, key){
      onWindowResize[key]=fun;
      // You shouldn't need to call it here. Just define it at startup and each time it will loop 
      // through all the functions in the hash.
      //app.initWindowResize();
    },

    removeWindowResize: function(key){
      delete onWindowResize[key];
      utils.initWindowResize();
    },

    initWindowResize: function(){
      //when calling this it should be overriding what was called previously
      window.onresize = function(e) {
        // could do this instead of the above for loop
        _.each(onWindowResize, function (fn) {
          fn();
        });
      };
    },

    removeSpecialCharacters: function(name){
      return name.replace(/[^\w\s]/gi,"");
    },

    safeURLName: function(name){
      var testName = name || "";
      var checkforBad = testName.match(/[\$\-/_,+-]/g);
      return (checkforBad !== null)?encodeURIComponent(name):name;
    }
  };

  return utils;
});

