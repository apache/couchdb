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

  var mixins = {};

  var onWindowResize = {};
   
  mixins.addWindowResize = function(fun, key){
    onWindowResize[key]=fun;
    // You shouldn't need to call it here. Just define it at startup and each time it will loop 
    // through all the functions in the hash.
    //app.initWindowResize();
  };
   
  mixins.removeWindowResize = function(key){
    delete onWindowResize[key];
    mixins.initWindowResize();
  };
   
  mixins.initWindowResize = function(){
  //when calling this it should be overriding what was called previously
    window.onresize = function(e) {
       // could do this instead of the above for loop
      _.each(onWindowResize, function (fn) {
        fn();
      });
    };
  };

  return mixins;
});

