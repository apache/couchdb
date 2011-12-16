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
        ms = 3000;

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
        ms = 3000;

    do {
      targetSeq = targetDb.info().update_seq;
      t1 = new Date();
    } while (((t1 - t0) <= ms) && targetSeq < sourceSeq);
  }

  function waitForDocPos(db, docId, pos) {
    var doc, curPos, t0, t1,
        maxWait = 3000;

    doc = db.open(docId);
    curPos = Number(doc._rev.split("-", 1));
    t0 = t1 = new Date();

    while ((curPos < pos) && ((t1 - t0) <= maxWait)) {
       doc = db.open(docId);
       curPos = Number(doc._rev.split("-", 1));
       t1 = new Date();
    }

    return doc;
  }

  function wait(ms) {
    var t0 = new Date(), t1;
    do {
      CouchDB.request("GET", "/");
      t1 = new Date();
    } while ((t1 - t0) <= ms);
  }


  function populate_db(db, docs) {
    if (db.name !== usersDb.name) {
      db.deleteDb();
      db.createDb();
    }
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
    T(typeof repDoc1._replication_state_time === "string");
    T(typeof repDoc1._replication_id  === "string");
    T(typeof repDoc1._replication_stats === "object", "doc has stats");
    var stats = repDoc1._replication_stats;
    TEquals(docs1.length, stats.revisions_checked,
       "right # of revisions_checked");
    TEquals(docs1.length, stats.missing_revisions_found,
      "right # of missing_revisions_found");
    TEquals(docs1.length, stats.docs_read, "right # of docs_read");
    TEquals(docs1.length, stats.docs_written, "right # of docs_written");
    TEquals(0, stats.doc_write_failures, "right # of doc_write_failures");
    TEquals(dbA.info().update_seq, stats.checkpointed_source_seq,
      "right checkpointed_source_seq");
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
    T(typeof repDoc1._replication_state_time === "string");
    T(typeof repDoc1._replication_id  === "string");
    T(typeof repDoc1._replication_stats === "object", "doc has stats");
    var stats = repDoc1._replication_stats;
    TEquals(2, stats.revisions_checked, "right # of revisions_checked");
    TEquals(2, stats.missing_revisions_found, "right # of missing_revisions_found");
    TEquals(2, stats.docs_read, "right # of docs_read");
    TEquals(1, stats.docs_written, "right # of docs_written");
    TEquals(1, stats.doc_write_failures, "right # of doc_write_failures");
    TEquals(dbA.info().update_seq, stats.checkpointed_source_seq,
      "right checkpointed_source_seq");
  }


  function continuous_replication() {
    populate_db(dbA, docs1);
    populate_db(dbB, []);

    var repDoc = {
      _id: "foo_cont_rep_doc",
      source: "http://" + host + "/" + dbA.name,
      target: dbB.name,
      continuous: true,
      user_ctx: {
        roles: ["_admin"]
      }
    };

    T(repDb.save(repDoc).ok);

    waitForSeq(dbA, dbB);
    for (var i = 0; i < docs1.length; i++) {
      var doc = docs1[i];
      var copy = dbB.open(doc._id);
      T(copy !== null);
      T(copy.value === doc.value);
    }

    var tasks = JSON.parse(CouchDB.request("GET", "/_active_tasks").responseText);
    TEquals(1, tasks.length, "1 active task");
    TEquals(repDoc._id, tasks[0].doc_id, "replication doc id in active tasks");

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
    T(typeof repDoc1._replication_state_time === "string");
    T(typeof repDoc1._replication_id  === "string");

    // Design documents are only replicated to local targets if the respective
    // replication document has a user_ctx filed with the "_admin" role in it.
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
    T(copy === null);

    repDoc = repDb.open(repDoc._id);
    T(typeof repDoc._replication_stats === "object", "doc has stats");
    var stats = repDoc._replication_stats;
    TEquals(3, stats.revisions_checked, "right # of revisions_checked");
    TEquals(3, stats.missing_revisions_found, "right # of missing_revisions_found");
    TEquals(3, stats.docs_read, "right # of docs_read");
    TEquals(2, stats.docs_written, "right # of docs_written");
    TEquals(1, stats.doc_write_failures, "right # of doc_write_failures");
    TEquals(dbA.info().update_seq, stats.checkpointed_source_seq,
      "right checkpointed_source_seq");
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
    T(typeof repDoc1_copy._replication_state_time === "string");
    T(typeof repDoc1_copy._replication_id  === "string");
    T(typeof repDoc1_copy._replication_stats === "object", "doc has stats");
    var stats = repDoc1_copy._replication_stats;
    TEquals(docs1.length, stats.revisions_checked,
      "right # of revisions_checked");
    TEquals(docs1.length, stats.missing_revisions_found,
      "right # of missing_revisions_found");
    TEquals(docs1.length, stats.docs_read, "right # of docs_read");
    TEquals(docs1.length, stats.docs_written, "right # of docs_written");
    TEquals(0, stats.doc_write_failures, "right # of doc_write_failures");
    TEquals(dbA.info().update_seq, stats.checkpointed_source_seq,
      "right checkpointed_source_seq");

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
    T(typeof repDoc2_copy._replication_state_time === "string");
    T(typeof repDoc2_copy._replication_id === "string");
    T(repDoc2_copy._replication_id === repDoc1_copy._replication_id);
    T(typeof repDoc2_copy._replication_stats === "object", "doc has stats");
    stats = repDoc2_copy._replication_stats;
    TEquals(1, stats.revisions_checked, "right # of revisions_checked");
    TEquals(1, stats.missing_revisions_found,
      "right # of missing_revisions_found");
    TEquals(1, stats.docs_read, "right # of docs_read");
    TEquals(1, stats.docs_written, "right # of docs_written");
    TEquals(0, stats.doc_write_failures, "right # of doc_write_failures");
    TEquals(dbA.info().update_seq, stats.checkpointed_source_seq,
      "right checkpointed_source_seq");
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
    T(typeof repDoc1._replication_state_time === "string");
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
    T(typeof repDoc1._replication_state_time === "string");
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
    T(typeof repDoc1._replication_state_time === "string");

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

    repDoc = waitForDocPos(repDb, "foo_cont_rep_survives_doc", 3);
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
        target: dbB.name,
        continuous: true
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


  function test_user_ctx_validation() {
    populate_db(dbA, docs1);
    populate_db(dbB, []);
    populate_db(usersDb, []);

    var joeUserDoc = CouchDB.prepareUserDoc({
      name: "joe",
      roles: ["erlanger", "bar"]
    }, "erly");
    var fdmananaUserDoc = CouchDB.prepareUserDoc({
      name: "fdmanana",
      roles: ["a", "b", "c"]
    }, "qwerty");

    TEquals(true, usersDb.save(joeUserDoc).ok);
    TEquals(true, usersDb.save(fdmananaUserDoc).ok);

    T(dbB.setSecObj({
      admins: {
        names: [],
        roles: ["god"]
      },
      readers: {
        names: [],
        roles: ["foo"]
      }
    }).ok);

    TEquals(true, CouchDB.login("joe", "erly").ok);
    TEquals("joe", CouchDB.session().userCtx.name);
    TEquals(-1, CouchDB.session().userCtx.roles.indexOf("_admin"));

    var repDoc = {
      _id: "foo_rep",
      source: CouchDB.protocol + host + "/" + dbA.name,
      target: dbB.name
    };

    try {
      repDb.save(repDoc);
      T(false, "Should have failed, user_ctx missing.");
    } catch (x) {
      TEquals("forbidden", x.error);
    }

    repDoc.user_ctx = {
      name: "john",
      roles: ["erlanger"]
    };

    try {
      repDb.save(repDoc);
      T(false, "Should have failed, wrong user_ctx.name.");
    } catch (x) {
      TEquals("forbidden", x.error);
    }

    repDoc.user_ctx = {
      name: "joe",
      roles: ["bar", "god", "erlanger"]
    };

    try {
      repDb.save(repDoc);
      T(false, "Should have failed, a bad role in user_ctx.roles.");
    } catch (x) {
      TEquals("forbidden", x.error);
    }

    // user_ctx.roles might contain only a subset of the user's roles
    repDoc.user_ctx = {
      name: "joe",
      roles: ["erlanger"]
    };

    TEquals(true, repDb.save(repDoc).ok);
    CouchDB.logout();

    waitForRep(repDb, repDoc, "error");
    var repDoc1 = repDb.open(repDoc._id);
    T(repDoc1 !== null);
    TEquals(repDoc.source, repDoc1.source);
    TEquals(repDoc.target, repDoc1.target);
    TEquals("error", repDoc1._replication_state);
    TEquals("string", typeof repDoc1._replication_id);
    TEquals("string", typeof repDoc1._replication_state_time);

    TEquals(true, CouchDB.login("fdmanana", "qwerty").ok);
    TEquals("fdmanana", CouchDB.session().userCtx.name);
    TEquals(-1, CouchDB.session().userCtx.roles.indexOf("_admin"));

    try {
      T(repDb.deleteDoc(repDoc1).ok);
      T(false, "Shouldn't be able to delete replication document.");
    } catch (x) {
      TEquals("forbidden", x.error);
    }

    CouchDB.logout();
    TEquals(true, CouchDB.login("joe", "erly").ok);
    TEquals("joe", CouchDB.session().userCtx.name);
    TEquals(-1, CouchDB.session().userCtx.roles.indexOf("_admin"));

    T(repDb.deleteDoc(repDoc1).ok);
    CouchDB.logout();

    for (var i = 0; i < docs1.length; i++) {
      var doc = docs1[i];
      var copy = dbB.open(doc._id);

      TEquals(null, copy);
    }

    T(dbB.setSecObj({
      admins: {
        names: [],
        roles: ["god", "erlanger"]
      },
      readers: {
        names: [],
        roles: ["foo"]
      }
    }).ok);

    TEquals(true, CouchDB.login("joe", "erly").ok);
    TEquals("joe", CouchDB.session().userCtx.name);
    TEquals(-1, CouchDB.session().userCtx.roles.indexOf("_admin"));

    repDoc = {
      _id: "foo_rep_2",
      source: CouchDB.protocol + host + "/" + dbA.name,
      target: dbB.name,
      user_ctx: {
        name: "joe",
        roles: ["erlanger"]
      }
    };

    TEquals(true, repDb.save(repDoc).ok);
    CouchDB.logout();

    waitForRep(repDb, repDoc, "complete");
    repDoc1 = repDb.open(repDoc._id);
    T(repDoc1 !== null);
    TEquals(repDoc.source, repDoc1.source);
    TEquals(repDoc.target, repDoc1.target);
    TEquals("completed", repDoc1._replication_state);
    TEquals("string", typeof repDoc1._replication_id);
    TEquals("string", typeof repDoc1._replication_state_time);

    for (var i = 0; i < docs1.length; i++) {
      var doc = docs1[i];
      var copy = dbB.open(doc._id);

      T(copy !== null);
      TEquals(doc.value, copy.value);
    }

    // Admins don't need to supply a user_ctx property in replication docs.
    // If they do not, the implicit user_ctx "user_ctx": {name: null, roles: []}
    // is used, meaning that design documents will not be replicated into
    // local targets
    T(dbB.setSecObj({
      admins: {
        names: [],
        roles: []
      },
      readers: {
        names: [],
        roles: []
      }
    }).ok);

    var ddoc = { _id: "_design/foo" };
    TEquals(true, dbA.save(ddoc).ok);

    repDoc = {
      _id: "foo_rep_3",
      source: CouchDB.protocol + host + "/" + dbA.name,
      target: dbB.name
    };

    TEquals(true, repDb.save(repDoc).ok);
    waitForRep(repDb, repDoc, "complete");
    repDoc1 = repDb.open(repDoc._id);
    T(repDoc1 !== null);
    TEquals(repDoc.source, repDoc1.source);
    TEquals(repDoc.target, repDoc1.target);
    TEquals("completed", repDoc1._replication_state);
    TEquals("string", typeof repDoc1._replication_id);
    TEquals("string", typeof repDoc1._replication_state_time);

    var ddoc_copy = dbB.open(ddoc._id);
    T(ddoc_copy === null);

    repDoc = {
      _id: "foo_rep_4",
      source: CouchDB.protocol + host + "/" + dbA.name,
      target: dbB.name,
      user_ctx: {
        roles: ["_admin"]
      }
    };

    TEquals(true, repDb.save(repDoc).ok);
    waitForRep(repDb, repDoc, "complete");
    repDoc1 = repDb.open(repDoc._id);
    T(repDoc1 !== null);
    TEquals(repDoc.source, repDoc1.source);
    TEquals(repDoc.target, repDoc1.target);
    TEquals("completed", repDoc1._replication_state);
    TEquals("string", typeof repDoc1._replication_id);
    TEquals("string", typeof repDoc1._replication_state_time);

    ddoc_copy = dbB.open(ddoc._id);
    T(ddoc_copy !== null);
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
    T(typeof repDoc1._replication_state_time === "string");
    T(typeof repDoc1._replication_id  === "string");
    T(repDoc1._replication_id !== "1234abc");
  }


  function swap_rep_db() {
    var repDb2 = new CouchDB("test_suite_rep_db_2");
    var dbA = new CouchDB("test_suite_rep_db_a");
    var dbA_copy = new CouchDB("test_suite_rep_db_a_copy");
    var dbB = new CouchDB("test_suite_rep_db_b");
    var dbB_copy = new CouchDB("test_suite_rep_db_b_copy");
    var dbC = new CouchDB("test_suite_rep_db_c");
    var dbC_copy = new CouchDB("test_suite_rep_db_c_copy");
    var repDoc1, repDoc2, repDoc3;
    var xhr, i, doc, copy, new_doc;

    populate_db(dbA, docs1);
    populate_db(dbB, docs1);
    populate_db(dbC, docs1);
    populate_db(dbA_copy, []);
    populate_db(dbB_copy, []);
    populate_db(dbC_copy, []);
    populate_db(repDb2, []);

    repDoc1 = {
      _id: "rep1",
      source: CouchDB.protocol + host + "/" + dbA.name,
      target: dbA_copy.name,
      continuous: true
    };
    repDoc2 = {
      _id: "rep2",
      source: CouchDB.protocol + host + "/" + dbB.name,
      target: dbB_copy.name,
      continuous: true
    };
    repDoc3 = {
      _id: "rep3",
      source: CouchDB.protocol + host + "/" + dbC.name,
      target: dbC_copy.name,
      continuous: true
    };

    TEquals(true, repDb.save(repDoc1).ok);
    TEquals(true, repDb.save(repDoc2).ok);

    waitForSeq(dbA, dbA_copy);
    waitForSeq(dbB, dbB_copy);

    xhr = CouchDB.request("PUT", "/_config/replicator/db",{
      body : JSON.stringify(repDb2.name),
      headers: {"X-Couch-Persist": "false"}
    });
    TEquals(200, xhr.status);

    new_doc = {
      _id: "foo666",
      value: 666
    };

    TEquals(true, dbA.save(new_doc).ok);
    TEquals(true, dbB.save(new_doc).ok);
    waitForSeq(dbA, dbA_copy);
    waitForSeq(dbB, dbB_copy);

    TEquals(true, repDb2.save(repDoc3).ok);
    waitForSeq(dbC, dbC_copy);

    for (i = 0; i < docs1.length; i++) {
      doc = docs1[i];
      copy = dbA_copy.open(doc._id);
      T(copy !== null);
      TEquals(doc.value, copy.value);
      copy = dbB_copy.open(doc._id);
      T(copy !== null);
      TEquals(doc.value, copy.value);
      copy = dbC_copy.open(doc._id);
      T(copy !== null);
      TEquals(doc.value, copy.value);
    }

    // replications rep1 and rep2 should have been stopped when the replicator
    // database was swapped
    copy = dbA_copy.open(new_doc._id);
    TEquals(null, copy);
    copy = dbB_copy.open(new_doc._id);
    TEquals(null, copy);

    xhr = CouchDB.request("PUT", "/_config/replicator/db",{
      body : JSON.stringify(repDb.name),
      headers: {"X-Couch-Persist": "false"}
    });
    TEquals(200, xhr.status);

    // after setting the replicator database to the former, replications rep1
    // and rep2 should have been resumed, while rep3 was stopped
    TEquals(true, dbC.save(new_doc).ok);
    wait(1000);

    waitForSeq(dbA, dbA_copy);
    waitForSeq(dbB, dbB_copy);

    copy = dbA_copy.open(new_doc._id);
    T(copy !== null);
    TEquals(new_doc.value, copy.value);
    copy = dbB_copy.open(new_doc._id);
    T(copy !== null);
    TEquals(new_doc.value, copy.value);
    copy = dbC_copy.open(new_doc._id);
    TEquals(null, copy);
  }


  function compact_rep_db() {
    var dbA_copy = new CouchDB("test_suite_rep_db_a_copy");
    var dbB_copy = new CouchDB("test_suite_rep_db_b_copy");
    var repDoc1, repDoc2;
    var xhr, i, doc, copy, new_doc;
    var docs = makeDocs(1, 50);

    populate_db(dbA, docs);
    populate_db(dbB, docs);
    populate_db(dbA_copy, []);
    populate_db(dbB_copy, []);

    repDoc1 = {
      _id: "rep1",
      source: CouchDB.protocol + host + "/" + dbA.name,
      target: dbA_copy.name,
      continuous: true
    };
    repDoc2 = {
      _id: "rep2",
      source: CouchDB.protocol + host + "/" + dbB.name,
      target: dbB_copy.name,
      continuous: true
    };

    TEquals(true, repDb.save(repDoc1).ok);
    TEquals(true, repDb.save(repDoc2).ok);

    TEquals(true, repDb.compact().ok);
    TEquals(202, repDb.last_req.status);

    waitForSeq(dbA, dbA_copy);
    waitForSeq(dbB, dbB_copy);

    while (repDb.info().compact_running) {};

    for (i = 0; i < docs.length; i++) {
      copy = dbA_copy.open(docs[i]._id);
      T(copy !== null);
      copy = dbB_copy.open(docs[i]._id);
      T(copy !== null);
    }

    new_doc = {
      _id: "foo666",
      value: 666
    };

    TEquals(true, dbA.save(new_doc).ok);
    TEquals(true, dbB.save(new_doc).ok);

    waitForSeq(dbA, dbA_copy);
    waitForSeq(dbB, dbB_copy);

    copy = dbA.open(new_doc._id);
    T(copy !== null);
    TEquals(666, copy.value);
    copy = dbB.open(new_doc._id);
    T(copy !== null);
    TEquals(666, copy.value);
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
    T(typeof repDoc1._replication_state_time === "string");
    T(typeof repDoc1._replication_id  === "string");
  }


  function rep_doc_field_validation() {
    var docs = makeDocs(1, 5);

    populate_db(dbA, docs);
    populate_db(dbB, []);

    var repDoc = {
       _id: "rep1",
       target: dbB.name
    };

    try {
      repDb.save(repDoc);
      T(false, "should have failed because source field is missing");
    } catch (x) {
      TEquals("forbidden", x.error);
    }

    repDoc = {
       _id: "rep1",
       source: 123,
       target: dbB.name
    };

    try {
      repDb.save(repDoc);
      T(false, "should have failed because source field is a number");
    } catch (x) {
      TEquals("forbidden", x.error);
    }

    repDoc = {
       _id: "rep1",
       source: dbA.name
    };

    try {
      repDb.save(repDoc);
      T(false, "should have failed because target field is missing");
    } catch (x) {
      TEquals("forbidden", x.error);
    }

    repDoc = {
       _id: "rep1",
       source: dbA.name,
       target: null
    };

    try {
      repDb.save(repDoc);
      T(false, "should have failed because target field is null");
    } catch (x) {
      TEquals("forbidden", x.error);
    }

    repDoc = {
       _id: "rep1",
       source: dbA.name,
       target: { url: 123 }
    };

    try {
      repDb.save(repDoc);
      T(false, "should have failed because target.url field is not a string");
    } catch (x) {
      TEquals("forbidden", x.error);
    }

    repDoc = {
       _id: "rep1",
       source: dbA.name,
       target: { url: dbB.name, auth: null }
    };

    try {
      repDb.save(repDoc);
      T(false, "should have failed because target.auth field is null");
    } catch (x) {
      TEquals("forbidden", x.error);
    }

    repDoc = {
       _id: "rep1",
       source: dbA.name,
       target: { url: dbB.name, auth: "foo:bar" }
    };

    try {
      repDb.save(repDoc);
      T(false, "should have failed because target.auth field is not an object");
    } catch (x) {
      TEquals("forbidden", x.error);
    }

    repDoc = {
       _id: "rep1",
       source: dbA.name,
       target: dbB.name,
       continuous: "true"
    };

    try {
      repDb.save(repDoc);
      T(false, "should have failed because continuous is not a boolean");
    } catch (x) {
      TEquals("forbidden", x.error);
    }

    repDoc = {
       _id: "rep1",
       source: dbA.name,
       target: dbB.name,
       filter: 123
    };

    try {
      repDb.save(repDoc);
      T(false, "should have failed because filter is not a string");
    } catch (x) {
      TEquals("forbidden", x.error);
    }
  }


  function test_invalid_filter() {
    // COUCHDB-1199 - replication document with a filter field that was invalid
    // crashed the CouchDB server.
    var repDoc1 = {
       _id: "rep1",
       source: "couch_foo_test_db",
       target: "couch_bar_test_db",
       filter: "test/foofilter"
    };

    TEquals(true, repDb.save(repDoc1).ok);

    waitForRep(repDb, repDoc1, "error");
    repDoc1 = repDb.open(repDoc1._id);
    TEquals("undefined", typeof repDoc1._replication_id);
    TEquals("error", repDoc1._replication_state);

    populate_db(dbA, docs1);
    populate_db(dbB, []);

    var repDoc2 = {
       _id: "rep2",
       source: dbA.name,
       target: dbB.name,
       filter: "test/foofilter"
    };

    TEquals(true, repDb.save(repDoc2).ok);

    waitForRep(repDb, repDoc2, "error");
    repDoc2 = repDb.open(repDoc2._id);
    TEquals("undefined", typeof repDoc2._replication_id);
    TEquals("error", repDoc2._replication_state);

    var ddoc = {
      _id: "_design/mydesign",
      language : "javascript",
      filters : {
        myfilter : (function(doc, req) {
          return true;
        }).toString()
      }
    };

    TEquals(true, dbA.save(ddoc).ok);

    var repDoc3 = {
       _id: "rep3",
       source: dbA.name,
       target: dbB.name,
       filter: "mydesign/myfilter"
    };

    TEquals(true, repDb.save(repDoc3).ok);

    waitForRep(repDb, repDoc3, "completed");
    repDoc3 = repDb.open(repDoc3._id);
    TEquals("string", typeof repDoc3._replication_id);
    TEquals("completed", repDoc3._replication_state);
  }


  function test_rep_db_update_security() {
    var dbA_copy = new CouchDB("test_suite_rep_db_a_copy");
    var dbB_copy = new CouchDB("test_suite_rep_db_b_copy");
    var repDoc1, repDoc2;
    var xhr, i, doc, copy, new_doc;
    var docs = makeDocs(1, 3);

    populate_db(dbA, docs);
    populate_db(dbB, docs);
    populate_db(dbA_copy, []);
    populate_db(dbB_copy, []);

    repDoc1 = {
      _id: "rep1",
      source: CouchDB.protocol + host + "/" + dbA.name,
      target: dbA_copy.name
    };
    repDoc2 = {
      _id: "rep2",
      source: CouchDB.protocol + host + "/" + dbB.name,
      target: dbB_copy.name
    };

    TEquals(true, repDb.save(repDoc1).ok);
    waitForRep(repDb, repDoc1, "completed");

    T(repDb.setSecObj({
      readers: {
        names: ["joe"]
      }
    }).ok);

    TEquals(true, repDb.save(repDoc2).ok);
    waitForRep(repDb, repDoc2, "completed");
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
  usersDb.deleteDb();
  restartServer();
  run_on_modified_server(server_config_2, test_user_ctx_validation);

  repDb.deleteDb();
  usersDb.deleteDb();
  restartServer();
  run_on_modified_server(server_config_2, test_replication_credentials_delegation);

  repDb.deleteDb();
  restartServer();
  continuous_replication_survives_restart();

  repDb.deleteDb();
  restartServer();
  run_on_modified_server(server_config, swap_rep_db);

  repDb.deleteDb();
  restartServer();
  run_on_modified_server(server_config, compact_rep_db);

  repDb.deleteDb();
  restartServer();
  run_on_modified_server(server_config, rep_doc_field_validation);


  repDb.deleteDb();
  restartServer();
  run_on_modified_server(server_config, test_invalid_filter);

  repDb.deleteDb();
  restartServer();
  run_on_modified_server(server_config, test_rep_db_update_security);

/*
 * Disabled, since error state would be set on the document only after
 * the exponential backoff retry done by the replicator database listener
 * terminates, which takes too much time for a unit test.
 */
/*
 * repDb.deleteDb();
 * restartServer();
 * run_on_modified_server(server_config, error_state_replication);
 */


  // cleanup
  repDb.deleteDb();
  usersDb.deleteDb();
  dbA.deleteDb();
  dbB.deleteDb();
  (new CouchDB("test_suite_rep_db_2")).deleteDb();
  (new CouchDB("test_suite_rep_db_c")).deleteDb();
  (new CouchDB("test_suite_rep_db_a_copy")).deleteDb();
  (new CouchDB("test_suite_rep_db_b_copy")).deleteDb();
  (new CouchDB("test_suite_rep_db_c_copy")).deleteDb();
};
