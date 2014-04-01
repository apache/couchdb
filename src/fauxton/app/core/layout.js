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

define([
  "backbone", 
  "plugins/backbone.layoutmanager"
], function(Backbone) {

  // A wrapper of the main Backbone.layoutmanager
  // Allows the main layout of the page to be changed by any plugin.
  var Layout = function () {
    this.layout = new Backbone.Layout({
      template: "templates/layouts/with_sidebar",
    });

    this.layoutViews = {};
    this.el = this.layout.el;
  };

  Layout.configure = function (options) {
    Backbone.Layout.configure(options);
  };

  // creatings the dashboard object same way backbone does
  _.extend(Layout.prototype, {
    render: function () {
      return this.layout.render();
    },

    setTemplate: function(template) {
      if (template.prefix){
        this.layout.template = template.prefix + template.name;
      } else{
        this.layout.template = "templates/layouts/" + template;
      }
      // If we're changing layouts all bets are off, so kill off all the
      // existing views in the layout.
      _.each(this.layoutViews, function(view){view.remove();});
      this.layoutViews = {};
      this.render();
    },

    setView: function(selector, view, keep) {
      this.layout.setView(selector, view, false);

      if (!keep) {
        this.layoutViews[selector] = view;
      }

      return view;
    },

    renderView: function(selector) {
      var view = this.layoutViews[selector];
      if (!view) {
        return false;
      } else {
        return view.render();
      }
    },

    removeView: function (selector) {
      var view = this.layout.getView(selector);

      if (!view) {
        return false;
      }

      view.remove();
      
      if (this.layoutViews[selector]) {
        delete this.layoutViews[selector];
      }

      return true;
    }

  });

  return Layout;

});
