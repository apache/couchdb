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

define(["backbone"],

function(Backbone) {

  // A wrapper of the main Backbone.layoutmanager
  // Allows the main layout of the page to be changed by any plugin.
  // Exposes the different views:
  //    navBar -> the top navigation bar
  //    dashboardContent -> Main display view
  //    breadcrumbs -> Breadcrumbs navigation section
  var Layout = function () {
    this.layout = new Backbone.Layout({
      template: "templates/layouts/with_sidebar",
    });

    this.layoutViews = {};
    this.el = this.layout.el;
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

    setTabs: function(view){
      // TODO: Not sure I like this - seems fragile/repetitive
      this.tabs = this.layout.setView("#tabs", view);
      this.tabs.render();
    },

    setBreadcrumbs: function(view) {
      this.breadcrumbs = this.layout.setView("#breadcrumbs", view);
      this.breadcrumbs.render();
    },

    clearBreadcrumbs: function () {
      if (!this.breadcrumbs) {return ;}

      this.breadcrumbs.remove();
    },

    setView: function(selector, view) {
      this.layoutViews[selector] = this.layout.setView(selector, view, false);
    },

    renderView: function(selector) {
      var view = this.layoutViews[selector];
      if (!view) {
        return false;
      } else {
        return view.render();
      }
    }

  });

  return Layout;

});
