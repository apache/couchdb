// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

define([
  "app",

  "api",

  // Modules
  "modules/documents/resources",
  "modules/databases/base"
],

function(app, FauxtonAPI, Documents, Databases) {
  // TODO: look at using:
  // var Documents = require("modules/documents/models_collections");
  // var Databases = require("modules/databases/module");

  var codeEditorCallback = function(databaseName, docID) {
    var data = {
      database: new Databases.Model({id:databaseName}),
      doc: new Documents.Doc({
        "_id": docID
      }),
      selected: "code_editor"
    };
    data.doc.database = data.database;
    data.designDocs = new Documents.AllDocs(null, {
      database: data.database,
      params: {startkey: '"_design"',
        endkey: '"_design1"',
        include_docs: true}
    });

    var options = app.getParams();
    options.include_docs = true;
    data.database.buildAllDocs(options);

    return {
      layout: "one_pane",

      data: data,

      crumbs: [
        {"name": "Databases", "link": "/_all_dbs"},
        {"name": data.database.id, "link": Databases.databaseUrl(data.database)},
        {"name": docID, "link": "#"}
      ],

      views: {
        "#dashboard-content": new Documents.Views.Doc({
          model: data.doc
        }),

        "#tabs": new Documents.Views.FieldEditorTabs({
          selected: data.selected,
          model: data.doc
        })
      },

      apiUrl: data.doc.url()
    };
  };

  var newViewEditorCallback = function(databaseName) {
    var data = {
      database: new Databases.Model({id:databaseName})
    };
    data.designDocs = new Documents.AllDocs(null, {
      database: data.database,
      params: {startkey: '"_design"',
               endkey: '"_design1"',
               include_docs: true}
    });

    return {
      layout: "with_tabs_sidebar",

      data: data,

      crumbs: [
        {"name": "Databases", "link": "/_all_dbs"},
        {"name": data.database.id, "link": data.database.url('app')}
      ],

      views: {
        "#sidebar-content": new Documents.Views.Sidebar({
          collection: data.designDocs
        }),

        "#tabs": new Documents.Views.Tabs({
          collection: data.designDocs,
          database: data.database
        }),

        "#dashboard-content": new Documents.Views.ViewEditor({
          model: data.database,
          ddocs: data.designDocs
        })
      },

      apiUrl: data.database.url()
    };
  };

  // HACK: this kind of works
  // Basically need a way to share state between different routes, for
  // instance making a new doc won't work for switching back and forth
  // between code and field editors
  var newDocCodeEditorCallback = function(databaseName) {
    var data = {
      database: new Databases.Model({id:databaseName}),
      doc: new Documents.NewDoc(),
      selected: "code_editor"
    };
    data.doc.database = data.database;
    data.designDocs = new Documents.AllDocs(null, {
      database: data.database,
      params: {startkey: '"_design"',
        endkey: '"_design1"',
        include_docs: true}
    });

    var options = app.getParams();
    options.include_docs = true;
    data.database.buildAllDocs(options);

    return {
      layout: "one_pane",

      data: data,

      crumbs: [
        {"name": "Databases", "link": "/_all_dbs"},
        {"name": data.database.id, "link": Databases.databaseUrl(data.database)},
        {"name": "new", "link": "#"}
      ],

      views: {
        "#dashboard-content": new Documents.Views.Doc({
          model: data.doc
        }),

        "#tabs": new Documents.Views.FieldEditorTabs({
          selected: data.selected,
          model: data.doc
        })
      },

      apiUrl: data.doc.url()
    };
  };

  Documents.Routes = {
    "database/:database/:doc/field_editor": function(databaseName, docID) {
      var data = {
        database: new Databases.Model({id:databaseName}),
        doc: new Documents.Doc({
          "_id": docID
        }),
        selected: "field_editor"
      };
      data.doc.database = data.database;
      data.designDocs = new Documents.AllDocs(null, {
        database: data.database,
        params: {startkey: '"_design"',
          endkey: '"_design1"',
          include_docs: true}
      });

      var options = app.getParams();
      options.include_docs = true;
      data.database.buildAllDocs(options);

      return {
        layout: "one_pane",

        data: data,

        crumbs: [
          {"name": "Databases", "link": "/_all_dbs"},
          {"name": data.database.id, "link": Databases.databaseUrl(data.database)},
          {"name": docID, "link": "#"}
        ],

        views: {
          "#dashboard-content": new Documents.Views.DocFieldEditor({
            model: data.doc
          }),

          "#tabs": new Documents.Views.FieldEditorTabs({
            selected: data.selected,
            model: data.doc
          })
        },

        apiUrl: data.doc.url()
      };
    },

    "database/:database/:doc/code_editor": codeEditorCallback,
    "database/:database/:doc": codeEditorCallback,

    // HACK
    // The ordering of routes is different in this object that the
    // routes object in the Backbone.Router. As a result, the
    // declaration order of show doc and _handler methods has been
    // switched. This is a brittle solution that needs to be fixed.
    // Conflicts with route: "database/:database/_:handler"
    //
    // TODO: add support for regex based rotues
    // Javascript does not handle a regex as an object key very well,
    // and it turns it into its string representation when you use in
    // non object literal form, which does get recast back as a regex
    // when we need it.
    // The inability to use regex based routes here is a design flaw
    // and should be rectified.
    "old_database/:database/:doc": function(databaseName, docID) {
      var data = {
        database: new Databases.Model({id:databaseName}),
        doc: new Documents.Doc({
          "_id": docID
        })
      };
      data.doc.database = data.database;
      data.designDocs = new Documents.AllDocs(null, {
        database: data.database,
        params: {startkey: '"_design"',
          endkey: '"_design1"',
          include_docs: true}
      });

      var options = app.getParams();
      options.include_docs = true;
      data.database.buildAllDocs(options);

      return {
        layout: "with_sidebar",

        data: data,

        crumbs: [
          {"name": "Databases", "link": "/_all_dbs"},
          {"name": data.database.id, "link": Databases.databaseUrl(data.database)},
          {"name": docID, "link": "#"}
        ],

        views: {
          "#dashboard-content": new Documents.Views.Doc({
            model: data.doc
          }),

          "#sidebar-content": new Documents.Views.Sidebar({
            collection: data.designDocs
          })
        },

        apiUrl: data.doc.url()
      };
    },

    "database/:database/_:handler": function(databaseName, page) {
      var data = {
        database: new Databases.Model({id:databaseName})
      };
      data.designDocs = new Documents.AllDocs(null, {
        database: data.database,
        params: {startkey: '"_design"',
          endkey: '"_design1"',
          include_docs: true}
      });

      var options = app.getParams();
      options.include_docs = true;
      data.database.buildAllDocs(options);

      return {
        layout: "with_tabs_sidebar",

        data: data,

        crumbs: [
          {"name": "Databases", "link": "/_all_dbs"},
          {"name": data.database.id, "link": Databases.databaseUrl(data.database)}
        ],

        views: {
          "#dashboard-content": new Documents.Views.AllDocsList({
            collection: data.database.allDocs
          }),

          "#sidebar-content": new Documents.Views.Sidebar({
            collection: data.designDocs
          }),

          "#tabs": new Documents.Views.Tabs({
            collection: data.designDocs,
            database: data.database
          })
        },

        apiUrl: data.database.allDocs.url()
      };
    },

    "database/:database/_changes(:params)": function(databaseName, params) {
      var data = {
        database: new Databases.Model({id:databaseName})
      };

      var options = app.getParams();
      data.database.buildChanges(options);

      return {
        layout: "with_tabs",

        data: data,

        crumbs: [
          {"name": "Databases", "link": "/_all_dbs"},
          {"name": data.database.id, "link": Databases.databaseUrl(data.database)},
          {"name": "_changes", "link": "/_changes"}
        ],

        views: {
          "#dashboard-content": new Documents.Views.Changes({
            model: data.database
          }),

          "#tabs": new Documents.Views.Tabs({
            collection: data.designDocs,
            database: data.database,
            active_id: 'changes'
          })
        },

        apiUrl: data.database.changes.url()
      };
    },

    "database/:database/new": newDocCodeEditorCallback,
    "database/:database/new_view": newViewEditorCallback,

    // TODO: fix optional search params
    // Can't get ":view(?*search)" to work
    // However ":view?*search" does work
    //"database/:database/_design/:ddoc/_view/:view(\?*options)": function(databaseName, ddoc, view, options) {
    "database/:database/_design/:ddoc/_view/:view": function(databaseName, ddoc, view, options) {
      // hack around backbone router limitations
      view = view.replace(/\?.*$/,'');
      var params = app.getParams();
      var data = {
        database: new Databases.Model({id:databaseName})
      };

      data.indexedDocs = new Documents.IndexCollection(null, {
        database: data.database,
        design: ddoc,
        view: view,
        params: params
      });

      data.designDocs = new Documents.AllDocs(null, {
        database: data.database,
        params: {startkey: '"_design"',
          endkey: '"_design1"',
          include_docs: true}
      });

      var ddocInfo = {
        id: "_design/" + ddoc,
        currView: view,
        designDocs: data.designDocs
      };

      return {
        layout: "with_tabs_sidebar",

        data: data,
        // TODO: change dashboard-content
        views: {
          "#dashboard-content": new Documents.Views.AllDocsList({
            collection: data.indexedDocs,
            nestedView: Documents.Views.Row,
            viewList: true,
            ddocInfo: ddocInfo,
            params: params
          }),

          "#sidebar-content": new Documents.Views.Sidebar({
            collection: data.designDocs,
            ddocInfo: ddocInfo
          }),

          "#tabs": new Documents.Views.Tabs({
            collection: data.designDocs,
            database: data.database
          })
        },

        crumbs: [
          {"name": "Databases", "link": "/_all_dbs"},
          {"name": data.database.id, "link": Databases.databaseUrl(data.database)},
          {"name": ddoc + "/" + view, "link": data.indexedDocs.url()}
        ],
        // TODO: change to view URL
        apiUrl: data.indexedDocs.url()
      };
    }
  };

  return Documents;
});
