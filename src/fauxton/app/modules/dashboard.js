define([
  "app",

  // Libs
  "lodash",
  "backbone"

  // Views

  // Plugins
],

function(app, _, Backbone) {

  // A dashboard wrapper of the main Backbone.layoutmanager
  // Allows that dashboard to be changed by any plugin.
  // Exposes the different views:
  //    navBar -> the top navigation bar
  //    dashboardContent -> Main display view
  //    breadcrumbs -> Breadcrumbs navigation section
  var Dashboard = function (navBar) {
    this.navBar = navBar;

    this.layout = new Backbone.Layout({
      template: "layouts/dashboard",

      views: {
        "#primary-navbar": this.navBar
      }
    });

    this.el = this.layout.el;
  };

  // creatings the dashboard object same way backbone does
  _.extend(Dashboard.prototype, {

    render: function () {
      return this.layout.render();
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

    clearDasboadContent: function () {
      if (!this.dashboardContent) { return ;}

      this.dashboardContent.remove();
    }

  });

  return Dashboard;

});
