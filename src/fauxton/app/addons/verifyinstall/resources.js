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
  "modules/databases/resources",
  "modules/documents/resources"
],

function (app, FauxtonAPI, Databases, Documents) {
  var Verifyinstall = FauxtonAPI.addon();

  var db = new Databases.Model({
    id: 'verifytestdb',
    name: 'verifytestdb'
  });

  var dbReplicate = new Databases.Model({
    id: 'verifytestdb_replicate',
    name: 'verifytestdb_replicate'
  });

  var doc, viewDoc;

  Verifyinstall.testProcess = {

    saveDoc: function () {
      doc = new Documents.Doc({_id: 'test_doc_1', a: 1}, {
        database: db
      });

      return doc.save();
    },

    destroyDoc: function () {
     return doc.destroy();
    },

    updateDoc: function () {
      doc.set({b: "hello"});
      return doc.save(); 
    },

    saveDB: function () {
      return db.save();
    },

    setupDB: function (db) {
      var deferred = FauxtonAPI.Deferred();
      db.fetch()
      .then(function () {
        return db.destroy();
      }, function (xhr) {
        deferred.resolve();
      })
      .then(function () {
        deferred.resolve();
      }, function (xhr, error, reason) {
        if (reason === "Unauthorized") {
          deferred.reject(xhr, error, reason);
        }
      });

      return deferred;
    },

    setup: function () {
      return FauxtonAPI.when([
        this.setupDB(db), 
        this.setupDB(dbReplicate)
      ]);
    },

    testView: function () {
      var deferred = FauxtonAPI.Deferred();
      var promise = $.get(viewDoc.url() + '/_view/testview');

      promise.then(function (resp) { 
        var row = JSON.parse(resp).rows[0];
        if (row.value === 6) {
          return deferred.resolve();
        }
        var reason = {
            reason: 'Values expect 6, got ' + row.value
          };

        deferred.reject({responseText: JSON.stringify(reason)});
      }, deferred.reject);

      return deferred;
    },

    setupView: function () {
      var doc1 = new Documents.Doc({_id: 'test_doc10', a: 1}, {
        database: db
      });

      var doc2 = new Documents.Doc({_id: 'test_doc_20', a: 2}, {
        database: db
      });

      var doc3 = new Documents.Doc({_id: 'test_doc_30', a: 3}, {
        database: db
      });

      viewDoc = new Documents.Doc({
        _id: '_design/view_check',
        views: {
          'testview': { 
            map:'function (doc) { emit(doc._id, doc.a); }',
            reduce: '_sum'
          }
        } 
      },{
        database: db,
      });

      return FauxtonAPI.when([doc1.save(),doc2.save(), doc3.save(), viewDoc.save()]);
    },

    setupReplicate: function () {
      return $.ajax({
        url: '/_replicate',
        contentType: 'application/json',
        type: 'POST',
        dataType: 'json',
        processData: false,
        data: JSON.stringify({
          create_target: true,
          source: 'verifytestdb',
          target: 'verifytestdb_replicate'
        }),
      });
    },

    testReplicate: function () {
      var deferred = FauxtonAPI.Deferred();
      /*var dbReplicate = new Databases.Model({
          id: 'verifytestdb_replicate',
          name: 'verifytestdb_replicate'
        });*/

      var promise = dbReplicate.fetch();

      promise.then(function () {
        var docCount = dbReplicate.get('doc_count');
        if ( docCount === 4) {
          deferred.resolve();
          return;
        }

        var reason = {
          reason: 'Replication Failed, expected 4 docs got ' + docCount
        };

        deferred.reject({responseText: JSON.stringify(reason)});
      }, deferred.reject);

      return deferred;
    }
  };

  return Verifyinstall;
});
