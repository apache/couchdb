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
  "api"
],

function(FauxtonAPI) {

  var Resize = function(options){
    this.options = options;
    this.options.selectorElements = options.selectorElements || ".window-resizeable";
  };

  Resize.prototype = {
    getPrimaryNavWidth: function(){
      var primaryNavWidth  = $('body').hasClass('closeMenu')? 64:224;
      return primaryNavWidth;
    },
    getPanelWidth: function(){
      var sidebarWidth = $('#sidebar-content').length > 0 ? $('#sidebar-content').width(): 0;
      return (this.getPrimaryNavWidth() + sidebarWidth); 
    },
    initialize: function(){
     // $(window).off('resize');
      var that = this;
      //add throttler :) 
      this.lazyLayout = _.debounce(that.onResizeHandler, 300).bind(this);
      FauxtonAPI.utils.addWindowResize(this.lazyLayout,"animation");
      FauxtonAPI.utils.initWindowResize();
      this.onResizeHandler();
    },
    updateOptions:function(options){
      this.options = {};
      this.options = options;
      this.options.selectorElements = options.selectorElements || ".window-resizeable";
    },
    turnOff:function(){
      FauxtonAPI.utils.removeWindowResize("animation");
    },
    cleanupCallback: function(){
      this.callback = null;
    },
    onResizeHandler: function (){
      //if there is an override, do that instead
      if (this.options.onResizeHandler){
        this.options.onResizeHandler();
      } else {
        var combinedWidth = window.innerWidth - this.getPanelWidth(),
        smallWidthConstraint = ($('#sidebar-content').length > 0)? 470:800,
        panelWidth; 

        if( combinedWidth > smallWidthConstraint  && combinedWidth < 1400){
          panelWidth = window.innerWidth - this.getPanelWidth();
        } else if (combinedWidth < smallWidthConstraint){
          panelWidth = smallWidthConstraint;
        } else if(combinedWidth > 1400){
          panelWidth = 1400;
        }

        $(this.options.selectorElements).innerWidth(panelWidth);
        
      }
      //if there is a callback, run that
      if(this.options.callback) {
        this.options.callback();
      }
      this.trigger('resize');
    } 
  };

  _.extend(Resize.prototype, Backbone.Events);

  return Resize;
});
