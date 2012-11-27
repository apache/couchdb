define([
  "app",

  "fauxton_api",

  // Modules
  "modules/documents",
  "modules/databases"
],

function(app, FauxtonAPI, Documents, Databases) {
  Documents.Routes = {
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
    "database/:database/:doc": function(databaseName, docID) {
      var data = {
        database: new Databases.Model({id:databaseName}),
        doc: new Documents.Doc({
          "_id": docID
        })
      };
      data.doc.collection = data.database;
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
          {"name": "Dashboard", "link": app.root},
          {"name": "Databases", "link": app.root},
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
          {"name": "Dashboard", "link": app.root},
          {"name": "Databases", "link": app.root},
          {"name": data.database.id, "link": Databases.databaseUrl(data.database)}
        ],

        views: {
          "#dashboard-content": new Documents.Views.AllDocsList({
            model: data.database
          }),

          "#sidebar-content": new Documents.Views.Sidebar({
            collection: data.designDocs
          }),

          "#tabs": new Documents.Views.Tabs({})
        },

        apiUrl: data.database.allDocs.url()
      };
    },

    "database/:database/_design/:ddoc/_view/:view": function(databaseName, ddoc, view) {
      console.log(databaseName, ddoc, view);
    }
  };

  return Documents;
});