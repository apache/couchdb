// vim: set ft=javascript:
// Set the require.js configuration for your test setup.
require.config(
{
	"paths": {
		"libs": "../assets/js/libs",
		"plugins": "../assets/js/plugins",
		"jquery": "../assets/js/libs/jquery",
		"lodash": "../assets/js/libs/lodash",
		"backbone": "../assets/js/libs/backbone",
		"bootstrap": "../assets/js/libs/bootstrap",
		"codemirror": "../assets/js/libs/codemirror",
		"jshint": "../assets/js/libs/jshint",
		"d3": "../assets/js/libs/d3",
		"nv.d3": "../assets/js/libs/nv.d3",
		"chai": "../test/mocha/chai",
		"sinon-chai": "../test/mocha/sinon-chai",
		"testUtils": "../test/mocha/testUtils"
	},
	"baseUrl": "../app",
	"shim": {
		"backbone": {
			"deps": [
				"lodash",
				"jquery"
			],
			"exports": "Backbone"
		},
		"bootstrap": {
			"deps": [
				"jquery"
			],
			"exports": "Bootstrap"
		},
		"codemirror": {
			"deps": [
				"jquery"
			],
			"exports": "CodeMirror"
		},
		"jshint": {
			"deps": [
				"jquery"
			],
			"exports": "JSHINT"
		},
		"plugins/backbone.layoutmanager": [
			"backbone"
		],
		"plugins/codemirror-javascript": [
			"codemirror"
		],
		"plugins/prettify": [],
		"plugins/jquery.form": [
			"jquery"
		]
	}
}
);

require([
        
           '.././test/core/routeObjectSpec.js',
        
           '.././app/addons/logs/tests/logSpec.js',
        
], function() {
  if (window.mochaPhantomJS) { mochaPhantomJS.run(); }
  else { mocha.run(); }
});

