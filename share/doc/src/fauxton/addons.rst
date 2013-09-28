.. Licensed under the Apache License, Version 2.0 (the "License"); you may not
.. use this file except in compliance with the License. You may obtain a copy of
.. the License at
..
..   http://www.apache.org/licenses/LICENSE-2.0
..
.. Unless required by applicable law or agreed to in writing, software
.. distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
.. WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
.. License for the specific language governing permissions and limitations under
.. the License.


.. _fauxton/addons:

===============
Writting Addons
===============

Addons allow you to extend Fauxton for a specific use case. Usually, they
have the following structure::

  + my_addon/
  | ---+ assets [optional]
  |    \ ---+ less
  |         \ ---- my_addon.less
  | ---+ templates/
  |    \ ---- my_addon.html - underscore template fragments
  | ---- resources.js - models and collections of the addon
  | ---- routes.js - URL routing for the addon
  | ---- views.js - views that the model provides


Generating an Addon
===================

We have a `grunt-init` template that lets you create a skeleton addon,
including all the boiler plate code. Run ``grunt-init tasks/addon`` and answer
the questions it asks to create an addon::

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

Routes and hooks
================

An addon can insert itself into Fauxton in two ways; via a route or via a hook.

Routes
------

An addon will override an existing route should one exist, but in all other
ways is just a normal backbone `route/view`. This is how you would add a whole
new feature.

Hooks
-----

Hooks let you modify/extend an existing feature. They modify a DOM element by
selector for a named set of routes, for example:

.. code-block:: javascript

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

Hello world Addon
=================

First create the addon skeleton::

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

.. code-block:: html

    <h1>Hello!</h1>

Next, we'll defined a simple view in `resources.js` (for more complex addons
you may want to have a views.js) that renders that template:

.. code-block:: javascript

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

.. code-block:: javascript

    define([
      "app",
      "api",
      "addons/hello/resources"
    ],

    function(app, FauxtonAPI, Resources) {
      var helloRoute = function () {
        console.log('helloRoute callback yo');
        return {
          layout: "one_pane",
          crumbs: [
            {"name": "Hello","link": "_hello"}
          ],
          views: {
            "#dashboard-content": new Resources.Hello({})
          },
          apiUrl: 'hello'
        };
      };

      Routes = {
        "_hello": helloRoute
      };

      return Routes;
    });


Then wire it all together in base.js:

.. code-block:: javascript

    define([
      "app",
      "api",
      "addons/hello/routes"
    ],

    function(app, FauxtonAPI, HelloRoutes) {
      var Hello = new FauxtonAPI.addon();
      console.log('hello from hello');

      Hello.initialize = function() {
        FauxtonAPI.addHeaderLink({title: "Hello", href: "#_hello"});
      };

      Hello.Routes = HelloRoutes;
      console.log(Hello);
      return Hello;
    });

Once the code is in place include the add on in your `settings.json` so that it
gets included by the `require` task. Your addon is included in one of three
ways; a local path, a git URL or a name. Named plugins assume the plugin is in
the Fauxton base directory, addons with a git URL will be cloned into the
application, local paths will be copied. Addons included from a local path will
be cleaned out by the clean task, others are left alone.
