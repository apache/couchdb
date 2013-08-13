# Addons
Addons allow you to extend Fauxton for a specific use case. Addons will usually
have the following structure:

 * templates/
   * my_addon.html - _underscore template fragments_
 * base.js - _entry point to the addon_
 * resources.js - _models and collections of the addon_
 * routes.js - _URL routing for the addon_
 * views.js - _views that the model provides_

 [optional]
 * assets/less
   * my_addon.less

## Generating an addon
We have a `grunt-init` template that lets you create a skeleton addon,
including all the boiler plate code. Run `grunt-init tasks/addon` and answer
the questions it asks to create an addon:

    ± grunt-init tasks/addon
    path.existsSync is now called `fs.existsSync`.
    Running "addon" task

    Please answer the following:
    [?] Add on Name (WickedCool) SuperAddon
    [?] Location of add ons (app/addons)
    [?] Do you need an assets folder?(for .less) (y/N)
    [?] Do you need to make any changes to the above before continuing? (y/N)

    Created addon SuperAddon in app/addons

    Done, without errors.

Once the addon is created add the name to the settings.json file to get it
compiled and added on the next install.

## Routes and hooks
An addon can insert itself into fauxton in two ways; via a route or via a hook.

### Routes
An addon will override an existing route should one exist, but in all other
ways is just a normal backbone route/view. This is how you would add a whole
new feature.

### Hooks
Hooks let you modify/extend an existing feature. They modify a DOM element by
selector for a named set of routes, for example:

    var Search = new FauxtonAPI.addon();
    Search.hooks = {
      // Render additional content into the sidebar
      "#sidebar-content": {
        routes:[
          "database/:database/_design/:ddoc/_search/:search",
          "database/:database/_design/:ddoc/_view/:view",
          "database/:database/_:handler"],
        callback: searchSidebar
      }
    };
    return Search;

adds the `searchSidebar` callback to `#sidebar-content` for three routes.

## Hello world addon
First create the addon skeleton:

    ± bbb addon
    path.existsSync is now called `fs.existsSync`.
    Running "addon" task

    Please answer the following:
    [?] Add on Name (WickedCool) Hello
    [?] Location of add ons (app/addons)
    [?] Do you need to make any changes to the above before continuing? (y/N)

    Created addon Hello in app/addons

    Done, without errors.

In `app/addons/hello/templates/hello.html` place:

    <h1>Hello!</h1>

Next, we'll defined a simple view in `resources.js` (for more complex addons
you may want to have a views.js) that renders that template:

    define([
      "app",
      "api"
    ],

    function (app, FauxtonAPI) {
      var Resources = {};

      Resources.Hello = FauxtonAPI.View.extend({
        template: "addons/hello/templates/hello"
      });

      return Resources;
    });


Then define a route in `routes.js` that the addon is accessible at:

    define([
      "app",
      "api",
      "addons/hello/resources"
    ],

    function(app, FauxtonAPI, Resources) {
      var  HelloRouteObject = FauxtonAPI.RouteObject.extend({
        layout: "one_pane",

        crumbs: [
          {"name": "Hello","link": "_hello"}
        ],

        routes: {
           "_hello": "helloRoute"
        },

        selectedHeader: "Hello",

        roles: ["_admin"],

        apiUrl:'hello',

        initialize: function () {
            //put common views used on all your routes here (eg:  sidebars )
        },

        helloRoute: function () {
          this.setView("#dashboard-content", new Resources.Hello({}));
        }
      });

      Resources.RouteObjects = [HelloRouteObject];

      return Resources;

    });




Then wire it all together in base.js:

    define([
      "app",
      "api",
      "addons/hello/routes"
    ],

    function(app, FauxtonAPI, HelloRoutes) {

      HelloRoutes.initialize = function() {
        FauxtonAPI.addHeaderLink({title: "Hello", href: "#_hello"});
      };

      return HelloRoutes;
    });

Once the code is in place include the add on in your `settings.json` so that it
gets included by the `require` task. Your addon is included in one of three
ways; a local path, a git URL or a name. Named plugins assume the plugin is in
the fauxton base directory, addons with a git URL will be cloned into the
application, local paths will be copied. Addons included from a local path will
be cleaned out by the clean task, others are left alone.

**TODO:** addons via npm module
