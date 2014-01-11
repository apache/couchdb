define([
  "app",
  "api",
  "addons/styletests/routes"
],

function(app, FauxtonAPI, tests) {

	tests.initialize = function() {

		FauxtonAPI.addHeaderLink({
			title: "Tests", 
			href: '#/tests',
			bottomNav: true,
			icon: "fonticon-wrench"
		});
		
	};

	return tests;
});
