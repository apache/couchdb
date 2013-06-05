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
],

function() {

  var Helpers = {};

  Helpers.imageUrl = function(path) {
    // TODO: add dynamic path for different deploy targets
    return path;
  };
	Helpers.currentYear = function(){
		return new Date().getFullYear();
	};

	Helpers.ajaxLoader = function(elem, className){
		var loadingClass = className || "loading",
			loader = {
				add: function(){
					$(elem).addClass(loadingClass);
				},
				remove: function(){
					$(elem).removeClass(loadingClass);
				}
			};
		return loader;
	};

  return Helpers;
});

