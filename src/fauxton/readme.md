Fauxton
=======

This is the initial implementation of Fauxton, focused on fleshing out
the various pieces of functionality and as a test bed for new ideas.
Full functionality and design considerations will be added later.

Current items of interest:

  * Live JSON editor with dynamic JS Hinting and error popups
  * Initial plugin system
  * Minimal externally loadable plugin example
  * Data popups for additional db info on \_all_dbs page
  * CouchDB API compliant urls

## Setup Fauxton ##

A recent of [node.js](http://nodejs.org/) and npm is required.

### CouchDB Setup ###

    1. Clone the Couchdb repo: https://github.com/apache/couchdb.git or http://git-wip-us.apache.org/repos/asf/couchdb.git
    cd couchdb
    git checkout fauxton

### Fauxton Setup ###

    cd src/fauxton

    # Install all dependencies
    npm install
    
#### (Optional) To avoid a npm global install
    # Add node_modules/.bin to your path
    # export PATH=./node_modules/.bin:$PATH
		# Or just use the wrappers in ./bin/

    # Development mode, non minified files
    ./bin/bbb couchdebug

    # Or fully compiled install
    # ./bin/bbb couchdb

### To Deploy Fauxton

    bbb couchapp_deploy - to deploy to your local [Couchdb instance] (http://localhost:5984/fauxton/_design/fauxton/index.html)

## Understang Fauxton Code layout

Each bit of functionality is its own seperate module or addon. All core modules are stored under `app/module` and any addons that are optional are under `app/addons`.
We use [backbone.js](http://backbonejs.org/) and [Backbone.layoutmanager](https://github.com/tbranyen/backbone.layoutmanager) quite heavily, so best to get an idea how they work. 
Its best at this point to read through a couple of the modules and addons to get an idea of how they work. Two good starting points are `app/addon/config` and `app/modules/databases`. 
Each module must have a `base.js` file, this is read and compile when Fauxton is deployed. A `resource.js` file is usually for your Backbone.Models and Backbone.Collections, 
`view.js` for your Backbone.Views. The `routes.js` is used to register a url path for your view along with what layout, data, breadcrumbs and api point is required for the view.

## Todo items

Checkout [Jira](https://issues.apache.org/jira/browse/COUCHDB/component/12320406) for a list of items to do. 

