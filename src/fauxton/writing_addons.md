# Addons
Addons allow you to extend Fauxton for a specific use case. Addons will usually
have the following structure:

 * templates/
   * my_addon.html - _underscore template fragments_
 * base.js - _entry point to the addon_
 * resources.js - _models and collections of the addon_
 * routes.js - _URL routing for the addon_
 * views.js - _views that the model provides_

## Generating an addon
We have a grunt task that lets you create a skeleton addon, including all the
boiler plate code. Run `bbb addon` and answer the questions it asks to create
an addon:

    ± bbb addon
    path.existsSync is now called `fs.existsSync`.
    Running "addon" task

    Please answer the following:
    [?] Add on Name (WickedCool) SuperAddon
    [?] Location of add ons (app/addons)
    [?] Do you need to make any changes to the above before continuing? (y/N)

    Created addon SuperAddon in app/addons

    Done, without errors.

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