define(["backbone"],

function(Backbone) {

  // A wrapper of the main Backbone.layoutmanager
  // Allows the main layout of the page to be changed by any plugin.
  // Exposes the different views:
  //    navBar -> the top navigation bar
  //    dashboardContent -> Main display view
  //    breadcrumbs -> Breadcrumbs navigation section
  var Layout = function (navBar, apiBar) {
    this.navBar = navBar;
    this.apiBar = apiBar;

    this.layout = new Backbone.Layout({
      template: "templates/layouts/with_sidebar",

      views: {
        "#primary-navbar": this.navBar,
        "#api-navbar": this.apiBar
      }
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
      this.layout.template = "templates/layouts/" + template;
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

    setContent: function (view) {
      this.content = this.layout.setView("#dashboard-content", view);
      this.content.render();
    },

    setSidebarContent: function (view) {
      this.sidebarContent = this.layout.setView("#sidebar-content", view);
      this.sidebarContent.render();
    },

    setView: function(selector, view) {
      this.layoutViews[selector] = this.layout.setView(selector, view);
      //this.renderView(selector);
    },

    renderView: function(selector) {
      var view = this.layoutViews[selector];
      if (!view) {
        return false;
      } else {
        return view.render();
      }
    },

    clearContent: function () {
      if (!this.content) { return ;}

      this.content.remove();
    }

  });

  return Layout;

});
