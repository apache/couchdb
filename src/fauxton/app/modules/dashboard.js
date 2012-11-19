define([
  "app",

  // Modules
  "modules/fauxton"
],

function(app, Fauxton) {

  // A dashboard wrapper of the main Backbone.layoutmanager
  // Allows that dashboard to be changed by any plugin.
  // Exposes the different views:
  //    navBar -> the top navigation bar
  //    dashboardContent -> Main display view
  //    breadcrumbs -> Breadcrumbs navigation section
  var Dashboard = function (navBar, apiBar) {
    this.navBar = navBar;
    this.apiBar = apiBar;

    this.layout = new Backbone.Layout({
      template: "layouts/with_sidebar",

      views: {
        "#primary-navbar": this.navBar,
        "#api-navbar": this.apiBar
      }
    });

    this.el = this.layout.el;
  };

  // creatings the dashboard object same way backbone does
  _.extend(Dashboard.prototype, {

    render: function () {
      return this.layout.render();
    },

    setTemplate: function(template) {
      this.layout.template = "layouts/" + template;
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

    setDashboardContent: function (view) {
      this.dashboardContent = this.layout.setView("#dashboard-content", view);
      this.dashboardContent.render();
    },

    setSidebarContent: function (view) {
      this.sidebarContent = this.layout.setView("#sidebar-content", view);
      this.sidebarContent.render();
    },

    clearDasboadContent: function () {
      if (!this.dashboardContent) { return ;}

      this.dashboardContent.remove();
    }

  });

  return Dashboard;

});
