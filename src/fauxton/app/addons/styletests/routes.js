define([
  "app",
  "api",
  "addons/styletests/views"
],

function(app, FauxtonAPI, Views) {

	var TestRouteObject = FauxtonAPI.RouteObject.extend({
		layout: "one_pane",
		routes: {
			"tests": "initialize"
		},
		selectedHeader: 'theme tests',
		crumbs:[],
    apiUrl: function(){
      return false;
    },
    initialize: function(){
			this.setView("#dashboard-content", new Views.tests({}));
    }
	});

	Views.RouteObjects = [TestRouteObject];

	return Views;
 
});
