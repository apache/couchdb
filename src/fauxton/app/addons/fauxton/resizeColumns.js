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
  };

  Resize.prototype = {
    getPrimaryNavWidth: function(){
      var primaryNavWidth  = $('body').hasClass('closeMenu') ? 64 : 220;
      return primaryNavWidth;
    },
    getPanelWidth: function(){
      var sidebarWidth = $('#sidebar-content').length > 0 ? $('#sidebar-content').outerWidth() : 0,
          borders = parseInt($('#dashboard').css('border-left-width'), 10) +
                    parseInt($('#dashboard-content').css('border-left-width'), 10) +
                    parseInt($('#dashboard-content').css('border-right-width'), 10);

      return (this.getPrimaryNavWidth() + sidebarWidth + borders);
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

        if (combinedWidth > smallWidthConstraint) {
          panelWidth = window.innerWidth - this.getPanelWidth();
        } else if (combinedWidth < smallWidthConstraint){
          panelWidth = smallWidthConstraint;
        }

        $('.window-resizeable').innerWidth(panelWidth);

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
