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

couchTests.replicator_db = function(debug) {

  if (debug) debugger;

  var wait_rep_doc = 500; // number of millisecs to wait after saving a Rep Doc
  var host = CouchDB.host;
  var dbA = new CouchDB("test_suite_rep_db_a", {"X-Couch-Full-Commit":"false"});
  var dbB = new CouchDB("test_suite_rep_db_b", {"X-Couch-Full-Commit":"false"});
  var repDb = new CouchDB("test_suite_rep_db", {"X-Couch-Full-Commit":"false"});
  var usersDb = new CouchDB("test_suite_auth", {"X-Couch-Full-Commit":"false"});

  var docs1 = [
    {
      _id: "foo1",
      value: 11
    },
    {
      _id: "foo2",
      value: 22
    },
    {
      _id: "foo3",
      value: 33
    }
  ];

  function waitForRep(repDb, repDoc, state) {
    var newRep,
        t0 = new Date(),
        t1,
        ms = 1000;

    do {
      newRep = repDb.open(repDoc._id);
      t1 = new Date();
    } while (((t1 - t0) <= ms) && newRep._replication_state !== state);
  }

  function waitForSeq(sourceDb, targetDb) {
    var targetSeq,
        sourceSeq = sourceDb.info().update_seq,
        t0 = new Date(),
        t1,
        ms = 1000;

    do {
      targetSeq = targetDb.info().update_seq;
      t1 = new Date();
    } while (((t1 - t0) <= ms) && targetSeq < sourceSeq);
  }

  function wait(ms) {
    var t0 = new Date(), t1;
    do {
      CouchDB.request("GET", "/");
      t1 = new Date();
    } while ((t1 - t0) <= ms);
  }


  function populate_db(db, docs) {
    db.deleteDb();
    db.createDb();
    for (var i = 0; i < docs.length; i++) {
      var d = docs[i];
      delete d._rev;
      T(db.save(d).ok);
    }
  }

  function simple_replication() {
    populate_db(dbA, docs1);
    populate_db(dbB, []);

    var repDoc = {
      _id: "foo_simple_rep",
      source: dbA.name,
      target: dbB.name
    };
    T(repDb.save(repDoc).ok);

    waitForRep(repDb, repDoc, "completed");
    for (var i = 0; i < docs1.length; i++) {
      var doc = docs1[i];
      var copy = dbB.open(doc._id);
      T(copy !== null);
      T(copy.value === doc.value);
    }

    var repDoc1 = repDb.open(repDoc._id);
    T(repDoc1 !== null);
    T(repDoc1.source === repDoc.source);
    T(repDoc1.target === repDoc.target);
    T(repDoc1._replication_state === "completed", "simple");
    T(typeof repDoc1._replication_state_time === "number");
    T(typeof repDoc1._replication_id  === "string");
  }


  function filtered_replication() {
    var docs2 = docs1.concat([
      {
        _id: "_design/mydesign",
        language : "javascript",
        filters : {
          myfilter : (function(doc, req) {
            return (doc.value % 2) !== Number(req.query.myparam);
          }).toString()
        }
      }
    ]);

    populate_db(dbA, docs2);
    populate_db(dbB, []);

    var repDoc = {
      _id: "foo_filt_rep_doc",
      source: "http://" + host + "/" + dbA.name,
      target: dbB.name,
      filter: "mydesign/myfilter",
      query_params: {
        myparam: 1
      }
    };
    T(repDb.save(repDoc).ok);

    waitForRep(repDb, repDoc, "completed");
    for (var i = 0; i < docs2.length; i++) {
      var doc = docs2[i];
      var copy = dbB.open(doc._id);

      if (typeof doc.value === "number") {
        if ((doc.value % 2) !== 1) {
          T(copy !== null);
          T(copy.value === doc.value);
        } else {
          T(copy === null);
        }
      }
    }

    var repDoc1 = repDb.open(repDoc._id);
    T(repDoc1 !== null);
    T(repDoc1.source === repDoc.source);
    T(repDoc1.target === repDoc.target);
    T(repDoc1._replication_state === "completed", "filtered");
    T(typeof repDoc1._replication_state_time === "number");
    T(typeof repDoc1._replication_id  === "string");
  }


  function continuous_replication() {
    populate_db(dbA, docs1);
    populate_db(dbB, []);

    var repDoc = {
      _id: "foo_cont_rep_doc",
      source: "http://" + host + "/" + dbA.name,
      target: dbB.name,
      continuous: true
    };

    T(repDb.save(repDoc).ok);

    waitForSeq(dbA, dbB);
    for (var i = 0; i < docs1.length; i++) {
      var doc = docs1[i];
      var copy = dbB.open(doc._id);
      T(copy !== null);
      T(copy.value === doc.value);
    }

    // add another doc to source, it will be replicated to target
    var docX = {
      _id: "foo1000",
      value: 1001
    };

    T(dbA.save(docX).ok);

    waitForSeq(dbA, dbB);
    var copy = dbB.open("foo1000");
    T(copy !== null);
    T(copy.value === 1001);

    var repDoc1 = repDb.open(repDoc._id);
    T(repDoc1 !== null);
    T(repDoc1.source === repDoc.source);
    T(repDoc1.target === repDoc.target);
    T(repDoc1._replication_state === "triggered");
    T(typeof repDoc1._replication_state_time === "number");
    T(typeof repDoc1._replication_id  === "string");

    // add a design doc to source, it will be replicated to target
    // when the "user_ctx" property is not defined in the replication doc,
    // the replication will be done under an _admin context, therefore
    // design docs will be replicated
    var ddoc = {
      _id: "_design/foobar",
      language: "javascript"
    };

    T(dbA.save(ddoc).ok);

    waitForSeq(dbA, dbB);
    var ddoc_copy = dbB.open("_design/foobar");
    T(ddoc_copy !== null);
    T(ddoc.language === "javascript");

    // update the design doc on source, test that the new revision is replicated
    ddoc.language = "erlang";
    T(dbA.save(ddoc).ok);
    T(ddoc._rev.indexOf("2-") === 0);

    waitForSeq(dbA, dbB);
    ddoc_copy = dbB.open("_design/foobar");
    T(ddoc_copy !== null);
    T(ddoc_copy._rev === ddoc._rev);
    T(ddoc.language === "erlang");

    // stop replication by deleting the replication document
    T(repDb.deleteDoc(repDoc1).ok);

    // add another doc to source, it will NOT be replicated to target
    var docY = {
      _id: "foo666",
      value: 999
    };

    T(dbA.save(docY).ok);

    wait(200); // is there a way to avoid wait here?
    var copy = dbB.open("foo666");
    T(copy === null);
  }


  function by_doc_ids_replication() {
    // to test that we can replicate docs with slashes in their IDs
    var docs2 = docs1.concat([
      {
        _id: "_design/mydesign",
        language : "javascript"
      }
    ]);

    populate_db(dbA, docs2);
    populate_db(dbB, []);

    var repDoc = {
      _id: "foo_cont_rep_doc",
      source: "http://" + host + "/" + dbA.name,
      target: dbB.name,
      doc_ids: ["foo666", "foo3", "_design/mydesign", "foo999", "foo1"]
    };
    T(repDb.save(repDoc).ok);

    waitForRep(repDb, repDoc, "completed");
    var copy = dbB.open("foo1");
    T(copy !== null);
    T(copy.value === 11);

    copy = dbB.open("foo2");
    T(copy === null);

    copy = dbB.open("foo3");
    T(copy !== null);
    T(copy.value === 33);

    copy = dbB.open("foo666");
    T(copy === null);

    copy = dbB.open("foo999");
    T(copy === null);

    copy = dbB.open("_design/mydesign");
    T(copy !== null);
    T(copy.language === "javascript");
  }


  function successive_identical_replications() {
    populate_db(dbA, docs1);
    populate_db(dbB, []);

    var repDoc1 = {
      _id: "foo_ident_rep_1",
      source: dbA.name,
      target: dbB.name
    };
    T(repDb.save(repDoc1).ok);

    waitForRep(repDb, repDoc1, "completed");
    for (var i = 0; i < docs1.length; i++) {
      var doc = docs1[i];
      var copy = dbB.open(doc._id);
      T(copy !== null);
      T(copy.value === doc.value);
    }

    var repDoc1_copy = repDb.open(repDoc1._id);
    T(repDoc1_copy !== null);
    T(repDoc1_copy.source === repDoc1.source);
    T(repDoc1_copy.target === repDoc1.target);
    T(repDoc1_copy._replication_state === "completed");
    T(typeof repDoc1_copy._replication_state_time === "number");
    T(typeof repDoc1_copy._replication_id  === "string");

    var newDoc = {
      _id: "doc666",
      value: 666
    };
    T(dbA.save(newDoc).ok);

    wait(200);
    var newDoc_copy = dbB.open(newDoc._id);
    // not replicated because first replication is complete (not continuous)
    T(newDoc_copy === null);

    var repDoc2 = {
      _id: "foo_ident_rep_2",
      source: dbA.name,
      target: dbB.name
    };
    T(repDb.save(repDoc2).ok);

    waitForRep(repDb, repDoc2, "completed");
    var newDoc_copy = dbB.open(newDoc._id);
    T(newDoc_copy !== null);
    T(newDoc_copy.value === newDoc.value);

    var repDoc2_copy = repDb.open(repDoc2._id);
    T(repDoc2_copy !== null);
    T(repDoc2_copy.source === repDoc1.source);
    T(repDoc2_copy.target === repDoc1.target);
    T(repDoc2_copy._replication_state === "completed");
    T(typeof repDoc2_copy._replication_state_time === "number");
    T(typeof repDoc2_copy._replication_id === "string");
    T(repDoc2_copy._replication_id === repDoc1_copy._replication_id);
  }


  // test the case where multiple replication docs (different IDs)
  // describe in fact the same replication (source, target, etc)
  function identical_rep_docs() {
    populate_db(dbA, docs1);
    populate_db(dbB, []);

    var repDoc1 = {
      _id: "foo_dup_rep_doc_1",
      source: "http://" + host + "/" + dbA.name,
      target: dbB.name
    };
    var repDoc2 = {
      _id: "foo_dup_rep_doc_2",
      source: "http://" + host + "/" + dbA.name,
      target: dbB.name
    };

    T(repDb.save(repDoc1).ok);
    T(repDb.save(repDoc2).ok);

    waitForRep(repDb, repDoc1, "completed");
    for (var i = 0; i < docs1.length; i++) {
      var doc = docs1[i];
      var copy = dbB.open(doc._id);
      T(copy !== null);
      T(copy.value === doc.value);
    }

    repDoc1 = repDb.open("foo_dup_rep_doc_1");
    T(repDoc1 !== null);
    T(repDoc1._replication_state === "completed", "identical");
    T(typeof repDoc1._replication_state_time === "number");
    T(typeof repDoc1._replication_id  === "string");

    repDoc2 = repDb.open("foo_dup_rep_doc_2");
    T(repDoc2 !== null);
    T(typeof repDoc2._replication_state === "undefined");
    T(typeof repDoc2._replication_state_time === "undefined");
    T(repDoc2._replication_id === repDoc1._replication_id);
  }


  // test the case where multiple replication docs (different IDs)
  // describe in fact the same continuous replication (source, target, etc)
  function identical_continuous_rep_docs() {
    populate_db(dbA, docs1);
    populate_db(dbB, []);

    var repDoc1 = {
      _id: "foo_dup_cont_rep_doc_1",
      source: "http://" + host + "/" + dbA.name,
      target: dbB.name,
      continuous: true
    };
    var repDoc2 = {
      _id: "foo_dup_cont_rep_doc_2",
      source: "http://" + host + "/" + dbA.name,
      target: dbB.name,
      continuous: true
    };

    T(repDb.save(repDoc1).ok);
    T(repDb.save(repDoc2).ok);

    waitForSeq(dbA, dbB);
    for (var i = 0; i < docs1.length; i++) {
      var doc = docs1[i];
      var copy = dbB.open(doc._id);
      T(copy !== null);
      T(copy.value === doc.value);
    }

    repDoc1 = repDb.open("foo_dup_cont_rep_doc_1");
    T(repDoc1 !== null);
    T(repDoc1._replication_state === "triggered");
    T(typeof repDoc1._replication_state_time === "number");
    T(typeof repDoc1._replication_id  === "string");

    repDoc2 = repDb.open("foo_dup_cont_rep_doc_2");
    T(repDoc2 !== null);
    T(typeof repDoc2._replication_state === "undefined");
    T(typeof repDoc2._replication_state_time === "undefined");
    T(repDoc2._replication_id === repDoc1._replication_id);

    var newDoc = {
      _id: "foo666",
      value: 999
    };
    T(dbA.save(newDoc).ok);

    waitForSeq(dbA, dbB);
    var copy = dbB.open("foo666");
    T(copy !== null);
    T(copy.value === 999);

    // deleting second replication doc, doesn't affect the 1st one and
    // neither it stops the replication
    T(repDb.deleteDoc(repDoc2).ok);
    repDoc1 = repDb.open("foo_dup_cont_rep_doc_1");
    T(repDoc1 !== null);
    T(repDoc1._replication_state === "triggered");
    T(typeof repDoc1._replication_state_time === "number");

    var newDoc2 = {
        _id: "foo5000",
        value: 5000
    };
    T(dbA.save(newDoc2).ok);

    waitForSeq(dbA, dbB);
    var copy = dbB.open("foo5000");
    T(copy !== null);
    T(copy.value === 5000);

    // deleting the 1st replication document stops the replication
    T(repDb.deleteDoc(repDoc1).ok);
    var newDoc3 = {
        _id: "foo1983",
        value: 1983
    };
    T(dbA.save(newDoc3).ok);

    wait(wait_rep_doc); //how to remove wait?
    var copy = dbB.open("foo1983");
    T(copy === null);
  }


  function test_replication_credentials_delegation() {
    populate_db(usersDb, []);

    var joeUserDoc = CouchDB.prepareUserDoc({
      name: "joe",
      roles: ["god", "erlanger"]
    }, "erly");
    T(usersDb.save(joeUserDoc).ok);

    var ddoc = {
      _id: "_design/beer",
      language: "javascript"
    };
    populate_db(dbA, docs1.concat([ddoc]));
    populate_db(dbB, []);

    T(dbB.setSecObj({
      admins: {
        names: [],
        roles: ["god"]
      }
    }).ok);

    var server_admins_config = [
      {
        section: "admins",
        key: "fdmanana",
        value: "qwerty"
      }
    ];

    run_on_modified_server(server_admins_config, function() {

      T(CouchDB.login("fdmanana", "qwerty").ok);
      T(CouchDB.session().userCtx.name === "fdmanana");
      T(CouchDB.session().userCtx.roles.indexOf("_admin") !== -1);

      var repDoc = {
        _id: "foo_rep_del_doc_1",
        source: dbA.name,
        target: dbB.name,
        user_ctx: {
          name: "joe",
          roles: ["erlanger"]
        }
      };

      T(repDb.save(repDoc).ok);

      waitForRep(repDb, repDoc, "completed");
      for (var i = 0; i < docs1.length; i++) {
        var doc = docs1[i];
        var copy = dbB.open(doc._id);
        T(copy !== null);
        T(copy.value === doc.value);
      }

      // design doc was not replicated, because joe is not an admin of db B
      var doc = dbB.open(ddoc._id);
      T(doc === null);

      // now test the same replication but putting the role "god" in the
      // delegation user context property
      var repDoc2 = {
        _id: "foo_rep_del_doc_2",
        source: dbA.name,
        target: dbB.name,
        user_ctx: {
          name: "joe",
          roles: ["erlanger", "god"]
        }
      };
      T(repDb.save(repDoc2).ok);

      waitForRep(repDb, repDoc2, "completed");
      for (var i = 0; i < docs1.length; i++) {
        var doc = docs1[i];
        var copy = dbB.open(doc._id);
        T(copy !== null);
        T(copy.value === doc.value);
      }

      // because anyone with a 'god' role is an admin of db B, a replication
      // that is delegated to a 'god' role can write design docs to db B
      doc = dbB.open(ddoc._id);
      T(doc !== null);
      T(doc.language === ddoc.language);
    });
  }


  function continuous_replication_survives_restart() {
    var origRepDbName = CouchDB.request(
      "GET", "/_config/replicator/db").responseText;

    repDb.deleteDb();

    var xhr = CouchDB.request("PUT", "/_config/replicator/db", {
      body : JSON.stringify(repDb.name),
      headers: {"X-Couch-Persist": "false"}
    });
    T(xhr.status === 200);

    populate_db(dbA, docs1);
    populate_db(dbB, []);

    var repDoc = {
      _id: "foo_cont_rep_survives_doc",
      source: "http://" + host + "/" + dbA.name,
      target: dbB.name,
      continuous: true
    };

    T(repDb.save(repDoc).ok);

    waitForSeq(dbA, dbB);
    for (var i = 0; i < docs1.length; i++) {
      var doc = docs1[i];
      var copy = dbB.open(doc._id);
      T(copy !== null);
      T(copy.value === doc.value);
    }

    repDb.ensureFullCommit();
    dbA.ensureFullCommit();

    restartServer();

    xhr = CouchDB.request("PUT", "/_config/replicator/db", {
      body : JSON.stringify(repDb.name),
      headers: {"X-Couch-Persist": "false"}
    });

    T(xhr.status === 200);

    // add another doc to source, it will be replicated to target
    var docX = {
      _id: "foo1000",
      value: 1001
    };

    T(dbA.save(docX).ok);

    waitForSeq(dbA, dbB);
    var copy = dbB.open("foo1000");
    T(copy !== null);
    T(copy.value === 1001);

    repDoc = repDb.open("foo_cont_rep_survives_doc");
    T(repDoc !== null);
    T(repDoc.continuous === true);

    // stop replication
    T(repDb.deleteDoc(repDoc).ok);

    xhr = CouchDB.request("PUT", "/_config/replicator/db", {
      body : origRepDbName,
      headers: {"X-Couch-Persist": "false"}
    });
    T(xhr.status === 200);
  }


  function rep_db_write_authorization() {
    populate_db(dbA, docs1);
    populate_db(dbB, []);

    var server_admins_config = [
      {
        section: "admins",
        key: "fdmanana",
        value: "qwerty"
      }
    ];

    run_on_modified_server(server_admins_config, function() {
      var repDoc = {
        _id: "foo_rep_doc",
        source: dbA.name,
        target: dbB.name
      };

      T(CouchDB.login("fdmanana", "qwerty").ok);
      T(CouchDB.session().userCtx.name === "fdmanana");
      T(CouchDB.session().userCtx.roles.indexOf("_admin") !== -1);

      T(repDb.save(repDoc).ok);

      waitForRep(repDb, repDoc, "completed");

      for (var i = 0; i < docs1.length; i++) {
        var doc = docs1[i];
        var copy = dbB.open(doc._id);

        T(copy !== null);
        T(copy.value === doc.value);
      }

      repDoc = repDb.open("foo_rep_doc");
      T(repDoc !== null);
      repDoc.target = "test_suite_foo_db";
      repDoc.create_target = true;

      // Only the replicator can update replication documents.
      // Admins can only add and delete replication documents.
      try {
        repDb.save(repDoc);
        T(false && "Should have thrown an exception");
      } catch (x) {
        T(x["error"] === "forbidden");
      }
    });
  }


  function rep_doc_with_bad_rep_id() {
    populate_db(dbA, docs1);
    populate_db(dbB, []);

    var repDoc = {
      _id: "foo_rep",
      source: dbA.name,
      target: dbB.name,
      replication_id: "1234abc"
    };
    T(repDb.save(repDoc).ok);

    waitForRep(repDb, repDoc, "completed");
    for (var i = 0; i < docs1.length; i++) {
      var doc = docs1[i];
      var copy = dbB.open(doc._id);
      T(copy !== null);
      T(copy.value === doc.value);
    }

    var repDoc1 = repDb.open(repDoc._id);
    T(repDoc1 !== null);
    T(repDoc1.source === repDoc.source);
    T(repDoc1.target === repDoc.target);
    T(repDoc1._replication_state === "completed",
      "replication document with bad replication id failed");
    T(typeof repDoc1._replication_state_time === "number");
    T(typeof repDoc1._replication_id  === "string");
    T(repDoc1._replication_id !== "1234abc");
  }


  function error_state_replication() {
    populate_db(dbA, docs1);

    var repDoc = {
      _id: "foo_error_rep",
      source: dbA.name,
      target: "nonexistent_test_db"
    };
    T(repDb.save(repDoc).ok);

    waitForRep(repDb, repDoc, "error");
    var repDoc1 = repDb.open(repDoc._id);
    T(repDoc1 !== null);
    T(repDoc1._replication_state === "error");
    T(typeof repDoc1._replication_state_time === "number");
    T(typeof repDoc1._replication_id  === "string");
  }


  // run all the tests
  var server_config = [
    {
      section: "replicator",
      key: "db",
      value: repDb.name
    }
  ];

  repDb.deleteDb();
  run_on_modified_server(server_config, simple_replication);

  repDb.deleteDb();
  restartServer();
  run_on_modified_server(server_config, filtered_replication);

  repDb.deleteDb();
  restartServer();
  run_on_modified_server(server_config, continuous_replication);

  repDb.deleteDb();
  restartServer();
  run_on_modified_server(server_config, by_doc_ids_replication);

  repDb.deleteDb();
  restartServer();
  run_on_modified_server(server_config, successive_identical_replications);

  repDb.deleteDb();
  restartServer();
  run_on_modified_server(server_config, identical_rep_docs);

  repDb.deleteDb();
  restartServer();
  run_on_modified_server(server_config, identical_continuous_rep_docs);

  repDb.deleteDb();
  restartServer();
  run_on_modified_server(server_config, rep_db_write_authorization);

  repDb.deleteDb();
  restartServer();
  run_on_modified_server(server_config, rep_doc_with_bad_rep_id);

  var server_config_2 = server_config.concat([
    {
      section: "couch_httpd_auth",
      key: "authentication_db",
      value: usersDb.name
    }
  ]);
  repDb.deleteDb();
  restartServer();
  run_on_modified_server(server_config_2, test_replication_credentials_delegation);

  repDb.deleteDb();
  restartServer();
  continuous_replication_survives_restart();

  repDb.deleteDb();
  restartServer();
  run_on_modified_server(server_config, error_state_replication);


  // cleanup
  repDb.deleteDb();
  usersDb.deleteDb();
  dbA.deleteDb();
  dbB.deleteDb();
};
