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

couchTests.replication = function(debug) {
//  return console.log('TODO');
  if (debug) debugger;

  var host = CouchDB.host;
  // as we change names during execution, do NOT use test_suite_db or a
  // pre-computed value like ''+sourceDb.name (compute only on use)
  var sourceDb;
  var targetDb;

  var dbPairsPrefixes = [
    {
      source: "",
      target: ""
    },
    {
      source: CouchDB.protocol + host + "/",
      target: ""
    },
    {
      source: "",
      target: CouchDB.protocol + host + "/"
    },
    {
      source: CouchDB.protocol + host + "/",
      target: CouchDB.protocol + host + "/"
    }
  ];

  var att1_data = CouchDB.request("GET", "/_utils/script/test/lorem.txt");
  att1_data = att1_data.responseText;

  var att2_data = CouchDB.request("GET", "/_utils/script/test/lorem_b64.txt");
  att2_data = att2_data.responseText;

  var sourceInfo, targetInfo;
  var docs, doc, copy;
  var repResult;
  var i, j, k;


  function makeAttData(minSize) {
    var data = att1_data;

    while (data.length < minSize) {
      data = data + att1_data;
    }
    return data;
  }


  function runAllNodes(callback) {
    // new and fancy: clustered version: pull cluster_members and walk over all of them
    var xhr = CouchDB.request("GET", "/_membership");
    T(xhr.status === 200);
    JSON.parse(xhr.responseText).cluster_nodes.forEach(callback);
  }

  function runFirstNode(callback) {
    // new and fancy: clustered version: pull cluster_members and walk over all of them
    var xhr = CouchDB.request("GET", "/_membership");
    T(xhr.status === 200);
    var node = JSON.parse(xhr.responseText).cluster_nodes[0];
    return callback(node);
  }

  function getCompressionInfo() {
    return runFirstNode(function(node) {
      var xhr = CouchDB.request(
        "GET",
        "_node/" + node + "/_config/attachments"
      );
      T(xhr.status === 200);
      var res = JSON.parse(xhr.responseText);
      return {"level": res.compression_level, "types": res.compressible_types};
    });
  }

  function enableAttCompression(level, types) {
    runAllNodes(function(node) {
      var xhr = CouchDB.request(
        "PUT",
        "_node/" + node + "/_config/attachments/compression_level",
        {
          body: JSON.stringify(level),
          headers: {"X-Couch-Persist": "false"}
        }
      );
      T(xhr.status === 200);
      xhr = CouchDB.request(
        "PUT",
        "_node/" + node + "/_config/attachments/compressible_types",
        {
          body: JSON.stringify(types),
          headers: {"X-Couch-Persist": "false"}
        }
      );
      T(xhr.status === 200);
    });
  }

  function disableAttCompression() {
    runAllNodes(function(node) {
      var xhr = CouchDB.request(
        "PUT",
        "_node/" + node + "/_config/attachments/compression_level",
        {
          body: JSON.stringify("0"),
          headers: {"X-Couch-Persist": "false"}
        }
      );
      T(xhr.status === 200);
    });
  }


  function populateSourceDb(docs, dontRecreateDb) {
    if(dontRecreateDb !== true) {
      if(sourceDb) {
        sourceDb.deleteDb();
      }
      sourceDb = new CouchDB(get_random_db_name() + "_src",{"X-Couch-Full-Commit":"false"});
      sourceDb.createDb();
    }
    for (var i = 0; i < docs.length; i++) {
      var doc = docs[i];
      delete doc._rev;
    }
    if (docs.length > 0) {
      sourceDb.bulkSave(docs);
    }
  }
  function populateTargetDb(docs, dontRecreateDb) {
    if(dontRecreateDb !== true) {
      if(targetDb) {
        targetDb.deleteDb();
      }
      targetDb = new CouchDB(get_random_db_name() + "_tgt",{"X-Couch-Full-Commit":"false"});
      targetDb.createDb();
    }
    for (var i = 0; i < docs.length; i++) {
      var doc = docs[i];
      delete doc._rev;
    }
    if (docs.length > 0) {
      targetDb.bulkSave(docs);
    }
  }


  function addAtt(db, doc, attName, attData, type) {
    var uri = "/" + db.name + "/" + encodeURIComponent(doc._id) + "/" + attName;

    if (doc._rev) {
      uri += "?rev=" + doc._rev;
    }

    var xhr = CouchDB.request("PUT", uri, {
      headers: {
        "Content-Type": type
      },
      body: attData
    });

    T(xhr.status === 201);
    doc._rev = JSON.parse(xhr.responseText).rev;
  }


  function compareObjects(o1, o2) {
    for (var p in o1) {
      if (o1[p] === null && o2[p] !== null) {
        return false;
      } else if (typeof o1[p] === "object") {
        if ((typeof o2[p] !== "object") || o2[p] === null) {
          return false;
        }
        if (!arguments.callee(o1[p], o2[p])) {
          return false;
        }
      } else {
        if (o1[p] !== o2[p]) {
          return false;
        }
      }
    }
    return true;
  }


  function getTask(rep_id, delay) {
    var t0 = new Date();
    var t1;
    do {
      var xhr = CouchDB.request("GET", "/_active_tasks");
      var tasks = JSON.parse(xhr.responseText);
      for(var i = 0; i < tasks.length; i++) {
        if(tasks[i].replication_id == repResult._local_id) {
          return tasks[i];
        }
      }
      sleep(500);
      t1 = new Date();
    } while((t1 - t0) <= delay);

    return null;
  }

  function getSourceLastSeq(sourceDb) {
      return sourceDb.changes({"since":"now"}).last_seq;
  }

  function waitForSeq(sourceDb, targetDb, rep_id) {
    var sourceSeq = getSourceLastSeq(sourceDb),
        t0 = new Date(),
        t1,
        ms = 30000;

    do {
      var task = getTask(rep_id, 0);
      if(task && task["through_seq"] == sourceSeq) {
        return;
      }
      t1 = new Date();
      sleep(500);
    } while (((t1 - t0) <= ms));
    throw(Error('Timeout waiting for replication through_seq = source update seq'));
  }

  function waitReplicationTaskStop(rep_id) {
      var t0 = new Date(),
          t1,
          ms = 30000;
      do {
        var task = getTask(rep_id, 0);
        if(task == null) {
            return;
        }
        t1 = new Date();
        sleep(500);
      } while (((t1 - t0) <= ms));
      throw(Error('Timeout waiting for replication task stop' + rep_id));
  }

  // test simple replications (not continuous, not filtered), including
  // conflict creation
  docs = makeDocs(1, 21);
  docs.push({
    _id: "_design/foo",
    language: "javascript",
    value: "ddoc"
  });

  for (i = 0; i < dbPairsPrefixes.length; i++) {
    populateSourceDb(docs);
    populateTargetDb([]);

    // add some attachments
    for (j = 10; j < 15; j++) {
      addAtt(sourceDb, docs[j], "readme.txt", att1_data, "text/plain");
    }

    repResult = CouchDB.replicate(dbPairsPrefixes[i].source+sourceDb.name, dbPairsPrefixes[i].target+targetDb.name);
    TEquals(true, repResult.ok);

    sourceInfo = sourceDb.info();
    targetInfo = targetDb.info();

    TEquals(sourceInfo.doc_count, targetInfo.doc_count);

    TEquals('string', typeof repResult.session_id);
    // we can't rely on sequences in a cluster
    //TEquals(repResult.source_last_seq, sourceInfo.update_seq);
    TEquals(true, repResult.history instanceof Array);
    TEquals(1, repResult.history.length);
    TEquals(repResult.history[0].session_id, repResult.session_id);
    TEquals('string', typeof repResult.history[0].start_time);
    TEquals('string', typeof repResult.history[0].end_time);
    TEquals(0, repResult.history[0].start_last_seq);
    // we can't rely on sequences in a cluster
    //TEquals(sourceInfo.update_seq, repResult.history[0].end_last_seq);
    //TEquals(sourceInfo.update_seq, repResult.history[0].recorded_seq);
    TEquals(sourceInfo.doc_count, repResult.history[0].missing_checked);
    TEquals(sourceInfo.doc_count, repResult.history[0].missing_found);
    TEquals(sourceInfo.doc_count, repResult.history[0].docs_read);
    TEquals(sourceInfo.doc_count, repResult.history[0].docs_written);
    TEquals(0, repResult.history[0].doc_write_failures);

    for (j = 0; j < docs.length; j++) {
      doc = docs[j];
      copy = targetDb.open(doc._id);

      T(copy !== null);
      TEquals(true, compareObjects(doc, copy));

      if (j >= 10 && j < 15) {
        var atts = copy._attachments;
        TEquals('object', typeof atts);
        TEquals('object', typeof atts["readme.txt"]);
        TEquals(2, atts["readme.txt"].revpos);
        TEquals(0, atts["readme.txt"].content_type.indexOf("text/plain"));
        TEquals(true, atts["readme.txt"].stub);

        var att_copy = CouchDB.request(
          "GET", "/" + targetDb.name + "/" + copy._id + "/readme.txt"
        ).responseText;
        TEquals(att1_data.length, att_copy.length);
        TEquals(att1_data, att_copy);
      }
    }


    // add one more doc to source, more attachments to some existing docs
    // and replicate again
    var newDoc = {
      _id: "foo666",
      value: "d"
    };
    TEquals(true, sourceDb.save(newDoc).ok);

    // add some more attachments
    for (j = 10; j < 15; j++) {
      addAtt(sourceDb, docs[j], "data.dat", att2_data, "application/binary");
    }

    repResult = CouchDB.replicate(dbPairsPrefixes[i].source+sourceDb.name, dbPairsPrefixes[i].target+targetDb.name);
    TEquals(true, repResult.ok);

    sourceInfo = sourceDb.info();
    targetInfo = targetDb.info();

    TEquals(targetInfo.doc_count, sourceInfo.doc_count);

    TEquals('string', typeof repResult.session_id);
    // we can't rely on sequences in a cluster
    //TEquals(sourceInfo.update_seq, repResult.source_last_seq);
    TEquals(true, repResult.history instanceof Array);
    TEquals(2, repResult.history.length);
    TEquals(repResult.history[0].session_id, repResult.session_id);
    TEquals('string', typeof repResult.history[0].start_time);
    TEquals('string', typeof repResult.history[0].end_time);
    // we can't rely on sequences in a cluster
    //TEquals((sourceInfo.update_seq - 6), repResult.history[0].start_last_seq);
    //TEquals(sourceInfo.update_seq, repResult.history[0].end_last_seq);
    //TEquals(sourceInfo.update_seq, repResult.history[0].recorded_seq);
    TEquals(6, repResult.history[0].missing_checked);
    TEquals(6, repResult.history[0].missing_found);
    TEquals(6, repResult.history[0].docs_read);
    TEquals(6, repResult.history[0].docs_written);
    TEquals(0, repResult.history[0].doc_write_failures);

    copy = targetDb.open(newDoc._id);
    T(copy !== null);
    TEquals(newDoc._id, copy._id);
    TEquals(newDoc.value, copy.value);

    for (j = 10; j < 15; j++) {
      doc = docs[j];
      copy = targetDb.open(doc._id);

      T(copy !== null);
      TEquals(true, compareObjects(doc, copy));

      var atts = copy._attachments;
      TEquals('object', typeof atts);
      TEquals('object', typeof atts["readme.txt"]);
      TEquals(2, atts["readme.txt"].revpos);
      TEquals(0, atts["readme.txt"].content_type.indexOf("text/plain"));
      TEquals(true, atts["readme.txt"].stub);

      var att1_copy = CouchDB.request(
        "GET", "/" + targetDb.name + "/" + copy._id + "/readme.txt"
      ).responseText;
      TEquals(att1_data.length, att1_copy.length);
      TEquals(att1_data, att1_copy);

      TEquals('object', typeof atts["data.dat"]);
      TEquals(3, atts["data.dat"].revpos);
      TEquals(0, atts["data.dat"].content_type.indexOf("application/binary"));
      TEquals(true, atts["data.dat"].stub);

      var att2_copy = CouchDB.request(
        "GET", "/" + targetDb.name + "/" + copy._id + "/data.dat"
      ).responseText;
      TEquals(att2_data.length, att2_copy.length);
      TEquals(att2_data, att2_copy);
    }

    // test deletion is replicated
    doc = sourceDb.open(docs[1]._id);
    TEquals(true, sourceDb.deleteDoc(doc).ok);

    repResult = CouchDB.replicate(dbPairsPrefixes[i].source+sourceDb.name, dbPairsPrefixes[i].target+targetDb.name);
    TEquals(true, repResult.ok);

    sourceInfo = sourceDb.info();
    targetInfo = targetDb.info();

    TEquals(targetInfo.doc_count, sourceInfo.doc_count);
    TEquals(targetInfo.doc_del_count, sourceInfo.doc_del_count);
    TEquals(1, targetInfo.doc_del_count);

    TEquals(true, repResult.history instanceof Array);
    TEquals(3, repResult.history.length);
    // we can't rely on sequences in a cluster
    //TEquals((sourceInfo.update_seq - 1), repResult.history[0].start_last_seq);
    //TEquals(sourceInfo.update_seq, repResult.history[0].end_last_seq);
    //TEquals(sourceInfo.update_seq, repResult.history[0].recorded_seq);
    TEquals(1, repResult.history[0].missing_checked);
    TEquals(1, repResult.history[0].missing_found);
    TEquals(1, repResult.history[0].docs_read);
    TEquals(1, repResult.history[0].docs_written);
    TEquals(0, repResult.history[0].doc_write_failures);

    copy = targetDb.open(docs[1]._id);
    TEquals(null, copy);

    var changes = targetDb.changes({since: 0});
    // there is no guarantee of ordering also
    // however: the doc has to appear somewhere
    //var idx = changes.results.length - 1;
    var changesResDoc1 = changes.results.filter(function(c){return c.id == docs[1]._id;});
    TEquals(1, changesResDoc1.length);
    TEquals(docs[1]._id, changesResDoc1[0].id);
    TEquals(true, changesResDoc1[0].deleted);

    // test conflict
    doc = sourceDb.open(docs[0]._id);
    doc.value = "white";
    TEquals(true, sourceDb.save(doc).ok);

    copy = targetDb.open(docs[0]._id);
    copy.value = "black";
    TEquals(true, targetDb.save(copy).ok);

    repResult = CouchDB.replicate(dbPairsPrefixes[i].source+sourceDb.name, dbPairsPrefixes[i].target+targetDb.name);
    TEquals(true, repResult.ok);

    sourceInfo = sourceDb.info();
    targetInfo = targetDb.info();

    TEquals(sourceInfo.doc_count, targetInfo.doc_count);

    TEquals(true, repResult.history instanceof Array);
    TEquals(4, repResult.history.length);
    // we can't rely on sequences in a cluster
    //TEquals((sourceInfo.update_seq - 1), repResult.history[0].start_last_seq);
    //TEquals(sourceInfo.update_seq, repResult.history[0].end_last_seq);
    //TEquals(sourceInfo.update_seq, repResult.history[0].recorded_seq);
    TEquals(1, repResult.history[0].missing_checked);
    TEquals(1, repResult.history[0].missing_found);
    TEquals(1, repResult.history[0].docs_read);
    TEquals(1, repResult.history[0].docs_written);
    TEquals(0, repResult.history[0].doc_write_failures);

    copy = targetDb.open(docs[0]._id, {conflicts: true});

    TEquals(0, copy._rev.indexOf("2-"));
    TEquals(true, copy._conflicts instanceof Array);
    TEquals(1, copy._conflicts.length);
    TEquals(0, copy._conflicts[0].indexOf("2-"));

    // replicate again with conflict
    doc.value = "yellow";
    TEquals(true, sourceDb.save(doc).ok);

    repResult = CouchDB.replicate(dbPairsPrefixes[i].source+sourceDb.name, dbPairsPrefixes[i].target+targetDb.name);
    TEquals(true, repResult.ok);

    sourceInfo = sourceDb.info();
    targetInfo = targetDb.info();

    TEquals(sourceInfo.doc_count, targetInfo.doc_count);

    TEquals(true, repResult.history instanceof Array);
    TEquals(5, repResult.history.length);
    // we can't rely on sequences in a cluster
    //TEquals((sourceInfo.update_seq - 1), repResult.history[0].start_last_seq);
    //TEquals(sourceInfo.update_seq, repResult.history[0].end_last_seq);
    //TEquals(sourceInfo.update_seq, repResult.history[0].recorded_seq);
    TEquals(1, repResult.history[0].missing_checked);
    TEquals(1, repResult.history[0].missing_found);
    TEquals(1, repResult.history[0].docs_read);
    TEquals(1, repResult.history[0].docs_written);
    TEquals(0, repResult.history[0].doc_write_failures);

    copy = targetDb.open(docs[0]._id, {conflicts: true});

    TEquals(0, copy._rev.indexOf("3-"));
    TEquals(true, copy._conflicts instanceof Array);
    TEquals(1, copy._conflicts.length);
    TEquals(0, copy._conflicts[0].indexOf("2-"));

    // resolve the conflict
    TEquals(true, targetDb.deleteDoc({_id: copy._id, _rev: copy._conflicts[0]}).ok);

    // replicate again, check there are no more conflicts
    doc.value = "rainbow";
    TEquals(true, sourceDb.save(doc).ok);

    repResult = CouchDB.replicate(dbPairsPrefixes[i].source+sourceDb.name, dbPairsPrefixes[i].target+targetDb.name);
    TEquals(true, repResult.ok);

    sourceInfo = sourceDb.info();
    targetInfo = targetDb.info();

    TEquals(sourceInfo.doc_count, targetInfo.doc_count);

    TEquals(true, repResult.history instanceof Array);
    TEquals(6, repResult.history.length);
    // we can't rely on sequences in a cluster
    //TEquals((sourceInfo.update_seq - 1), repResult.history[0].start_last_seq);
    //TEquals(sourceInfo.update_seq, repResult.history[0].end_last_seq);
    //TEquals(sourceInfo.update_seq, repResult.history[0].recorded_seq);
    TEquals(1, repResult.history[0].missing_checked);
    TEquals(1, repResult.history[0].missing_found);
    TEquals(1, repResult.history[0].docs_read);
    TEquals(1, repResult.history[0].docs_written);
    TEquals(0, repResult.history[0].doc_write_failures);

    copy = targetDb.open(docs[0]._id, {conflicts: true});

    TEquals(0, copy._rev.indexOf("4-"));
    TEquals('undefined', typeof copy._conflicts);

    // test that revisions already in a target are not copied
    TEquals(true, sourceDb.save({_id: "foo1", value: 111}).ok);
    TEquals(true, targetDb.save({_id: "foo1", value: 111}).ok);
    TEquals(true, sourceDb.save({_id: "foo2", value: 222}).ok);
    TEquals(true, sourceDb.save({_id: "foo3", value: 333}).ok);
    TEquals(true, targetDb.save({_id: "foo3", value: 333}).ok);

    repResult = CouchDB.replicate(dbPairsPrefixes[i].source+sourceDb.name, dbPairsPrefixes[i].target+targetDb.name);
    TEquals(true, repResult.ok);

    sourceInfo = sourceDb.info();
    // we can't rely on sequences in a cluster
    //TEquals(sourceInfo.update_seq, repResult.source_last_seq);
    //TEquals(sourceInfo.update_seq - 3, repResult.history[0].start_last_seq);
    //TEquals(sourceInfo.update_seq, repResult.history[0].end_last_seq);
    //TEquals(sourceInfo.update_seq, repResult.history[0].recorded_seq);
    TEquals(3, repResult.history[0].missing_checked);
    TEquals(1, repResult.history[0].missing_found);
    TEquals(1, repResult.history[0].docs_read);
    TEquals(1, repResult.history[0].docs_written);
    TEquals(0, repResult.history[0].doc_write_failures);

    TEquals(true, sourceDb.save({_id: "foo4", value: 444}).ok);
    TEquals(true, targetDb.save({_id: "foo4", value: 444}).ok);
    TEquals(true, sourceDb.save({_id: "foo5", value: 555}).ok);
    TEquals(true, targetDb.save({_id: "foo5", value: 555}).ok);

    repResult = CouchDB.replicate(dbPairsPrefixes[i].source+sourceDb.name, dbPairsPrefixes[i].target+targetDb.name);
    TEquals(true, repResult.ok);

    sourceInfo = sourceDb.info();
    // we can't rely on sequences in a cluster
    //TEquals(sourceInfo.update_seq, repResult.source_last_seq);
    //TEquals(sourceInfo.update_seq - 2, repResult.history[0].start_last_seq);
    //TEquals(sourceInfo.update_seq, repResult.history[0].end_last_seq);
    //TEquals(sourceInfo.update_seq, repResult.history[0].recorded_seq);
    TEquals(2, repResult.history[0].missing_checked);
    TEquals(0, repResult.history[0].missing_found);
    TEquals(0, repResult.history[0].docs_read);
    TEquals(0, repResult.history[0].docs_written);
    TEquals(0, repResult.history[0].doc_write_failures);

    repResult = CouchDB.replicate(dbPairsPrefixes[i].source+sourceDb.name, dbPairsPrefixes[i].target+targetDb.name);
    TEquals(true, repResult.ok);
    TEquals(true, repResult.no_changes);
    sourceInfo = sourceDb.info();
    // we can't rely on sequences in a cluster
    //TEquals(sourceInfo.update_seq, repResult.source_last_seq);
  }


  // test error when source database does not exist
  try {
    CouchDB.replicate("foobar", "test_suite_db");
    T(false, "should have failed with db_not_found error");
  } catch (x) {
    TEquals("db_not_found", x.error);
  }

  // validate COUCHDB-317
  try {
    CouchDB.replicate("/foobar", "test_suite_db");
    T(false, "should have failed with db_not_found error");
  } catch (x) {
    TEquals("db_not_found", x.error);
  }

  try {
    CouchDB.replicate(CouchDB.protocol + host + "/foobar", "test_suite_db");
    T(false, "should have failed with db_not_found error");
  } catch (x) {
    TEquals("db_not_found", x.error);
  }


  // test since_seq parameter
  docs = makeDocs(1, 6);

  for (i = 0; i < dbPairsPrefixes.length; i++) {
    populateSourceDb(docs);
    populateTargetDb([]);
    // sequences are no longer simple numbers - so pull #3 from a feed
    var since_seq = sourceDb.changes().results[2].seq;

    var expected_ids = [];
    var changes = sourceDb.changes({since: JSON.stringify(since_seq)});
    for (j = 0; j < changes.results.length; j++) {
      expected_ids.push(changes.results[j].id);
    }
    TEquals(2, expected_ids.length, "2 documents since since_seq");

    // For OTP < R14B03, temporary child specs are kept in the supervisor
    // after the child terminates, so cancel the replication to delete the
    // child spec in those OTP releases, otherwise since_seq will have no
    // effect.
    try {
      CouchDB.replicate(
        dbPairsPrefixes[i].source+sourceDb.name,
        dbPairsPrefixes[i].target+targetDb.name,
        {body: {cancel: true}}
      );
    } catch (x) {
      // OTP R14B03 onwards
      TEquals("not_found", x.error);
    }
    repResult = CouchDB.replicate(
      dbPairsPrefixes[i].source+sourceDb.name,
      dbPairsPrefixes[i].target+targetDb.name,
      {body: {since_seq: since_seq}}
    );
    // Same reason as before. But here we don't want since_seq to affect
    // subsequent replications, so we need to delete the child spec from the
    // supervisor (since_seq is not used to calculate the replication ID).
    try {
      CouchDB.replicate(
        dbPairsPrefixes[i].source+sourceDb.name,
        dbPairsPrefixes[i].target+targetDb.name,
        {body: {cancel: true}}
      );
    } catch (x) {
      // OTP R14B03 onwards
      TEquals("not_found", x.error);
    }
    TEquals(true, repResult.ok);
    TEquals(2, repResult.history[0].missing_checked);
    TEquals(2, repResult.history[0].missing_found);
    TEquals(2, repResult.history[0].docs_read);
    TEquals(2, repResult.history[0].docs_written);
    TEquals(0, repResult.history[0].doc_write_failures);

    for (j = 0; j < docs.length; j++) {
      doc = docs[j];
      copy = targetDb.open(doc._id);

      if (expected_ids.indexOf(doc._id) === -1) {
        T(copy === null);
      } else {
        T(copy !== null);
        TEquals(true, compareObjects(doc, copy));
      }
    }
  }


  // test errors due to doc validate_doc_update functions in the target endpoint
  docs = makeDocs(1, 8);
  docs[2]["_attachments"] = {
    "hello.txt": {
      "content_type": "text/plain",
      "data": "aGVsbG8gd29ybGQ="  // base64:encode("hello world")
    }
  };
  var ddoc = {
    _id: "_design/test",
    language: "javascript",
    validate_doc_update: (function(newDoc, oldDoc, userCtx, secObj) {
      if ((newDoc.integer % 2) !== 0) {
        throw {forbidden: "I only like multiples of 2."};
      }
    }).toString()
  };

  for (i = 0; i < dbPairsPrefixes.length; i++) {
    populateSourceDb(docs);
    populateTargetDb([ddoc]);

    repResult = CouchDB.replicate(
      dbPairsPrefixes[i].source+sourceDb.name,
      dbPairsPrefixes[i].target+targetDb.name
    );
    TEquals(true, repResult.ok);
    TEquals(7, repResult.history[0].missing_checked);
    TEquals(7, repResult.history[0].missing_found);
    TEquals(7, repResult.history[0].docs_read);
    TEquals(3, repResult.history[0].docs_written);
    TEquals(4, repResult.history[0].doc_write_failures);

    for (j = 0; j < docs.length; j++) {
      doc = docs[j];
      copy = targetDb.open(doc._id);

      if (doc.integer % 2 === 0) {
        T(copy !== null);
        TEquals(copy.integer, doc.integer);
      } else {
        T(copy === null);
      }
    }
  }


  // test create_target option
  docs = makeDocs(1, 2);

  for (i = 0; i < dbPairsPrefixes.length; i++) {
    populateSourceDb(docs);
    targetDb.deleteDb();

    repResult = CouchDB.replicate(
      dbPairsPrefixes[i].source+sourceDb.name,
      dbPairsPrefixes[i].target+targetDb.name,
      {body: {create_target: true}}
    );
    TEquals(true, repResult.ok);

    sourceInfo = sourceDb.info();
    targetInfo = targetDb.info();

    TEquals(sourceInfo.doc_count, targetInfo.doc_count);
    TEquals(sourceInfo.update_seq, targetInfo.update_seq);
  }


  // test filtered replication
  docs = makeDocs(1, 31);
  docs.push({
    _id: "_design/mydesign",
    language: "javascript",
    filters: {
      myfilter: (function(doc, req) {
        var modulus = Number(req.query.modulus);
        var special = req.query.special;
        return (doc.integer % modulus === 0) || (doc.string === special);
      }).toString()
    }
  });

  for (i = 0; i < dbPairsPrefixes.length; i++) {
    populateSourceDb(docs);
    populateTargetDb([]);

    repResult = CouchDB.replicate(
      dbPairsPrefixes[i].source+sourceDb.name,
      dbPairsPrefixes[i].target+targetDb.name,
      {
        body: {
          filter: "mydesign/myfilter",
          query_params: {
            modulus: "2",
            special: "7"
          }
        }
      }
    );

    TEquals(true, repResult.ok);

    for (j = 0; j < docs.length; j++) {
      doc = docs[j];
      copy = targetDb.open(doc._id);

      if ((doc.integer && (doc.integer % 2 === 0)) || (doc.string === "7")) {

        T(copy !== null);
        TEquals(true, compareObjects(doc, copy));
      } else {
        TEquals(null, copy);
      }
    }

    TEquals(true, repResult.history instanceof Array);
    TEquals(1, repResult.history.length);
    // We (incorrectly) don't record update sequences for things
    // that don't pass the changse feed filter. Historically the
    // last document to pass was the second to last doc which has
    // an update sequence of 30. Work that has been applied to avoid
    // conflicts from duplicate IDs breaking _bulk_docs updates added
    // a sort to the logic which changes this. Now the last document
    // to pass has an doc id of "8" and is at update_seq 29 (because only
    // "9" and the design doc are after it).
    //
    // In the future the fix ought to be that we record that update
    // sequence of the database. BigCouch has some existing work on
    // this in the clustered case because if you have very few documents
    // that pass the filter then (given single node's behavior) you end
    // up having to rescan a large portion of the database.
    // we can't rely on sequences in a cluster
    // not only can one figure appear twice (at least for n>1), there's also hashes involved now - so comparing seq==29 is lottery (= cutting off hashes is nonsense)
    // above, there was brute-force comparing all attrs of all docs - now we did check if excluded docs did NOT make it
    // in any way, we can't rely on sequences in a cluster (so leave out)
    //TEquals(29, repResult.source_last_seq);
    //TEquals(0, repResult.history[0].start_last_seq);
    //TEquals(29, repResult.history[0].end_last_seq);
    //TEquals(29, repResult.history[0].recorded_seq);
    // 16 => 15 docs with even integer field  + 1 doc with string field "7"
    TEquals(16, repResult.history[0].missing_checked);
    TEquals(16, repResult.history[0].missing_found);
    TEquals(16, repResult.history[0].docs_read);
    TEquals(16, repResult.history[0].docs_written);
    TEquals(0, repResult.history[0].doc_write_failures);


    // add new docs to source and resume the same replication
    var newDocs = makeDocs(50, 56);
    populateSourceDb(newDocs, true);

    repResult = CouchDB.replicate(
      dbPairsPrefixes[i].source+sourceDb.name,
      dbPairsPrefixes[i].target+targetDb.name,
      {
        body: {
          filter: "mydesign/myfilter",
          query_params: {
            modulus: "2",
            special: "7"
          }
        }
      }
    );

    TEquals(true, repResult.ok);

    for (j = 0; j < newDocs.length; j++) {
      doc = newDocs[j];
      copy = targetDb.open(doc._id);

      if (doc.integer && (doc.integer % 2 === 0)) {

        T(copy !== null);
        TEquals(true, compareObjects(doc, copy));
      } else {
        TEquals(null, copy);
      }
    }

    // last doc has even integer field, so last replicated seq is 36
    // cluster - so no seq (ditto above)
    //TEquals(36, repResult.source_last_seq);
    TEquals(true, repResult.history instanceof Array);
    TEquals(2, repResult.history.length);
    //TEquals(29, repResult.history[0].start_last_seq);
    //TEquals(36, repResult.history[0].end_last_seq);
    //TEquals(36, repResult.history[0].recorded_seq);
    TEquals(3, repResult.history[0].missing_checked);
    TEquals(3, repResult.history[0].missing_found);
    TEquals(3, repResult.history[0].docs_read);
    TEquals(3, repResult.history[0].docs_written);
    TEquals(0, repResult.history[0].doc_write_failures);
  }


  // test filtered replication works as expected after changing the filter's
  // code (ticket COUCHDB-892)
  var filterFun1 = (function(doc, req) {
    if (doc.value < Number(req.query.maxvalue)) {
      return true;
    } else {
      return false;
    }
  }).toString();

  var filterFun2 = (function(doc, req) {
    return true;
  }).toString();

  for (i = 0; i < dbPairsPrefixes.length; i++) {
    populateTargetDb([]);
    populateSourceDb([]);

    TEquals(true, sourceDb.save({_id: "foo1", value: 1}).ok);
    TEquals(true, sourceDb.save({_id: "foo2", value: 2}).ok);
    TEquals(true, sourceDb.save({_id: "foo3", value: 3}).ok);
    TEquals(true, sourceDb.save({_id: "foo4", value: 4}).ok);

    var ddoc = {
      "_id": "_design/mydesign",
      "language": "javascript",
      "filters": {
        "myfilter": filterFun1
      }
    };

    TEquals(true, sourceDb.save(ddoc).ok);

    repResult = CouchDB.replicate(
      dbPairsPrefixes[i].source+sourceDb.name,
      dbPairsPrefixes[i].target+targetDb.name,
      {
        body: {
          filter: "mydesign/myfilter",
          query_params: {
            maxvalue: "3"
          }
        }
      }
    );

    TEquals(true, repResult.ok);
    TEquals(true, repResult.history instanceof Array);
    TEquals(1, repResult.history.length);
    TEquals(2, repResult.history[0].docs_written);
    TEquals(2, repResult.history[0].docs_read);
    TEquals(0, repResult.history[0].doc_write_failures);

    var docFoo1 = targetDb.open("foo1");
    T(docFoo1 !== null);
    TEquals(1, docFoo1.value);

    var docFoo2 = targetDb.open("foo2");
    T(docFoo2 !== null);
    TEquals(2, docFoo2.value);

    var docFoo3 = targetDb.open("foo3");
    TEquals(null, docFoo3);

    var docFoo4 = targetDb.open("foo4");
    TEquals(null, docFoo4);

    // replication should start from scratch after the filter's code changed

    ddoc.filters.myfilter = filterFun2;
    TEquals(true, sourceDb.save(ddoc).ok);

    repResult = CouchDB.replicate(
      dbPairsPrefixes[i].source+sourceDb.name,
      dbPairsPrefixes[i].target+targetDb.name,
      {
        body: {
          filter: "mydesign/myfilter",
          query_params : {
            maxvalue: "3"
          }
        }
      }
    );

    TEquals(true, repResult.ok);
    TEquals(true, repResult.history instanceof Array);
    TEquals(1, repResult.history.length);
    TEquals(3, repResult.history[0].docs_written);
    TEquals(3, repResult.history[0].docs_read);
    TEquals(0, repResult.history[0].doc_write_failures);

    docFoo1 = targetDb.open("foo1");
    T(docFoo1 !== null);
    TEquals(1, docFoo1.value);

    docFoo2 = targetDb.open("foo2");
    T(docFoo2 !== null);
    TEquals(2, docFoo2.value);

    docFoo3 = targetDb.open("foo3");
    T(docFoo3 !== null);
    TEquals(3, docFoo3.value);

    docFoo4 = targetDb.open("foo4");
    T(docFoo4 !== null);
    TEquals(4, docFoo4.value);

    T(targetDb.open("_design/mydesign") !== null);
  }


  // test replication by doc IDs
  docs = makeDocs(1, 11);
  docs.push({
    _id: "_design/foo",
    language: "javascript",
    integer: 1
  });

  var target_doc_ids = [
    { initial: ["1", "2", "10"], after: [], conflict_id: "2" },
    { initial: ["1", "2"], after: ["7"], conflict_id: "1" },
    { initial: ["1", "foo_666", "10"], after: ["7"], conflict_id: "10" },
    { initial: ["_design/foo", "8"], after: ["foo_5"], conflict_id: "8" },
    { initial: ["_design%2Ffoo", "8"], after: ["foo_5"], conflict_id: "8" },
    { initial: [], after: ["foo_1000", "_design/foo", "1"], conflict_id: "1" }
  ];
  var doc_ids, after_doc_ids;
  var id, num_inexistent_docs, after_num_inexistent_docs;
  var total, after_total;

  for (i = 0; i < dbPairsPrefixes.length; i++) {

    for (j = 0; j < target_doc_ids.length; j++) {
      doc_ids = target_doc_ids[j].initial;
      num_inexistent_docs = 0;

      for (k = 0; k < doc_ids.length; k++) {
        id = doc_ids[k];
        if (id.indexOf("foo_") === 0) {
          num_inexistent_docs += 1;
        }
      }

      populateSourceDb(docs);
      populateTargetDb([]);

      repResult = CouchDB.replicate(
        dbPairsPrefixes[i].source+sourceDb.name,
        dbPairsPrefixes[i].target+targetDb.name,
        {
          body: {
            doc_ids: doc_ids
          }
        }
      );

      total = doc_ids.length - num_inexistent_docs;
      TEquals(true, repResult.ok);
      if (total === 0) {
        TEquals(true, repResult.no_changes);
      } else {
        TEquals('string', typeof repResult.start_time);
        TEquals('string', typeof repResult.end_time);
        TEquals(total, repResult.docs_read);
        TEquals(total, repResult.docs_written);
        TEquals(0, repResult.doc_write_failures);
      }

      for (k = 0; k < doc_ids.length; k++) {
        id = decodeURIComponent(doc_ids[k]);
        doc = sourceDb.open(id);
        copy = targetDb.open(id);

        if (id.indexOf("foo_") === 0) {
          TEquals(null, doc);
          TEquals(null, copy);
        } else {
          T(doc !== null);
          T(copy !== null);
          TEquals(true, compareObjects(doc, copy));
        }
      }

      // be absolutely sure that other docs were not replicated
      for (k = 0; k < docs.length; k++) {
        var base_id = docs[k]._id;
        id = encodeURIComponent(base_id);
        doc = targetDb.open(base_id);

        if ((doc_ids.indexOf(id) >= 0) || (doc_ids.indexOf(base_id) >= 0)) {
            T(doc !== null);
        } else {
            TEquals(null, doc);
        }
      }

      targetInfo = targetDb.info();
      TEquals(total, targetInfo.doc_count);


      // add more docs throught replication by doc IDs
      after_doc_ids = target_doc_ids[j].after;
      after_num_inexistent_docs = 0;

      for (k = 0; k < after_doc_ids.length; k++) {
        id = after_doc_ids[k];
        if (id.indexOf("foo_") === 0) {
          after_num_inexistent_docs += 1;
        }
      }

      repResult = CouchDB.replicate(
        dbPairsPrefixes[i].source+sourceDb.name,
        dbPairsPrefixes[i].target+targetDb.name,
        {
          body: {
            doc_ids: after_doc_ids
          }
        }
      );

      after_total = after_doc_ids.length - after_num_inexistent_docs;
      TEquals(true, repResult.ok);
      if (after_total === 0) {
        TEquals(true, repResult.no_changes);
      } else {
        TEquals('string', typeof repResult.start_time);
        TEquals('string', typeof repResult.end_time);
        TEquals(after_total, repResult.docs_read);
        TEquals(after_total, repResult.docs_written);
        TEquals(0, repResult.doc_write_failures);
      }

      for (k = 0; k < after_doc_ids.length; k++) {
        id = after_doc_ids[k];
        doc = sourceDb.open(id);
        copy = targetDb.open(id);

        if (id.indexOf("foo_") === 0) {
          TEquals(null, doc);
          TEquals(null, copy);
        } else {
          T(doc !== null);
          T(copy !== null);
          TEquals(true, compareObjects(doc, copy));
        }
      }

      // be absolutely sure that other docs were not replicated
      for (k = 0; k < docs.length; k++) {
        var base_id = docs[k]._id;
        id = encodeURIComponent(base_id);
        doc = targetDb.open(base_id);

        if ((doc_ids.indexOf(id) >= 0) || (after_doc_ids.indexOf(id) >= 0) ||
            (doc_ids.indexOf(base_id) >= 0) ||
            (after_doc_ids.indexOf(base_id) >= 0)) {
            T(doc !== null);
        } else {
            TEquals(null, doc);
        }
      }

      targetInfo = targetDb.info();
      TEquals((total + after_total), targetInfo.doc_count);


      // replicate again the same doc after updated on source (no conflict)
      id = target_doc_ids[j].conflict_id;
      doc = sourceDb.open(id);
      T(doc !== null);
      doc.integer = 666;
      TEquals(true, sourceDb.save(doc).ok);
      addAtt(sourceDb, doc, "readme.txt", att1_data, "text/plain");
      addAtt(sourceDb, doc, "data.dat", att2_data, "application/binary");

      repResult = CouchDB.replicate(
        dbPairsPrefixes[i].source+sourceDb.name,
        dbPairsPrefixes[i].target+targetDb.name,
        {
          body: {
            doc_ids: [id]
          }
        }
      );

      TEquals(true, repResult.ok);
      TEquals(1, repResult.docs_read);
      TEquals(1, repResult.docs_written);
      TEquals(0, repResult.doc_write_failures);

      copy = targetDb.open(id, {conflicts: true});

      TEquals(666, copy.integer);
      TEquals(0, copy._rev.indexOf("4-"));
      TEquals('undefined', typeof copy._conflicts);

      var atts = copy._attachments;
      TEquals('object', typeof atts);
      TEquals('object', typeof atts["readme.txt"]);
      TEquals(3, atts["readme.txt"].revpos);
      TEquals(0, atts["readme.txt"].content_type.indexOf("text/plain"));
      TEquals(true, atts["readme.txt"].stub);

      var att1_copy = CouchDB.request(
        "GET", "/" + targetDb.name + "/" + copy._id + "/readme.txt"
      ).responseText;
      TEquals(att1_data.length, att1_copy.length);
      TEquals(att1_data, att1_copy);

      TEquals('object', typeof atts["data.dat"]);
      TEquals(4, atts["data.dat"].revpos);
      TEquals(0, atts["data.dat"].content_type.indexOf("application/binary"));
      TEquals(true, atts["data.dat"].stub);

      var att2_copy = CouchDB.request(
        "GET", "/" + targetDb.name + "/" + copy._id + "/data.dat"
      ).responseText;
      TEquals(att2_data.length, att2_copy.length);
      TEquals(att2_data, att2_copy);


      // generate a conflict throught replication by doc IDs
      id = target_doc_ids[j].conflict_id;
      doc = sourceDb.open(id);
      copy = targetDb.open(id);
      T(doc !== null);
      T(copy !== null);
      doc.integer += 100;
      copy.integer += 1;
      TEquals(true, sourceDb.save(doc).ok);
      TEquals(true, targetDb.save(copy).ok);

      repResult = CouchDB.replicate(
        dbPairsPrefixes[i].source+sourceDb.name,
        dbPairsPrefixes[i].target+targetDb.name,
        {
          body: {
            doc_ids: [id]
          }
        }
      );

      TEquals(true, repResult.ok);
      TEquals(1, repResult.docs_read);
      TEquals(1, repResult.docs_written);
      TEquals(0, repResult.doc_write_failures);

      copy = targetDb.open(id, {conflicts: true});

      TEquals(0, copy._rev.indexOf("5-"));
      TEquals(true, copy._conflicts instanceof Array);
      TEquals(1, copy._conflicts.length);
      TEquals(0, copy._conflicts[0].indexOf("5-"));
    }
  }


  docs = makeDocs(1, 25);
  docs.push({
    _id: "_design/foo",
    language: "javascript",
    filters: {
      myfilter: (function(doc, req) { return true; }).toString()
    }
  });

  for (i = 0; i < dbPairsPrefixes.length; i++) {
    populateSourceDb(docs);
    populateTargetDb([]);

    // add some attachments
    for (j = 10; j < 15; j++) {
      addAtt(sourceDb, docs[j], "readme.txt", att1_data, "text/plain");
    }

    repResult = CouchDB.replicate(
      dbPairsPrefixes[i].source+sourceDb.name,
      dbPairsPrefixes[i].target+targetDb.name,
      {
        body: {
          continuous: true
        }
      }
    );
    TEquals(true, repResult.ok);
    TEquals('string', typeof repResult._local_id);

    var rep_id = repResult._local_id;

    waitForSeq(sourceDb, targetDb, rep_id);

    for (j = 0; j < docs.length; j++) {
      doc = docs[j];
      copy = targetDb.open(doc._id);

      T(copy !== null);
      TEquals(true, compareObjects(doc, copy));

      if (j >= 10 && j < 15) {
        var atts = copy._attachments;
        TEquals('object', typeof atts);
        TEquals('object', typeof atts["readme.txt"]);
        TEquals(2, atts["readme.txt"].revpos);
        TEquals(0, atts["readme.txt"].content_type.indexOf("text/plain"));
        TEquals(true, atts["readme.txt"].stub);

        var att_copy = CouchDB.request(
          "GET", "/" + targetDb.name + "/" + copy._id + "/readme.txt"
        ).responseText;
        TEquals(att1_data.length, att_copy.length);
        TEquals(att1_data, att_copy);
      }
    }

    sourceInfo = sourceDb.info();
    targetInfo = targetDb.info();

    TEquals(sourceInfo.doc_count, targetInfo.doc_count);

    // add attachments to docs in source
    for (j = 10; j < 15; j++) {
      addAtt(sourceDb, docs[j], "data.dat", att2_data, "application/binary");
    }

    var ddoc = docs[docs.length - 1]; // design doc
    addAtt(sourceDb, ddoc, "readme.txt", att1_data, "text/plain");

    waitForSeq(sourceDb, targetDb, rep_id);

    var modifDocs = docs.slice(10, 15).concat([ddoc]);
    for (j = 0; j < modifDocs.length; j++) {
      doc = modifDocs[j];
      copy = targetDb.open(doc._id);

      T(copy !== null);
      TEquals(true, compareObjects(doc, copy));

      var atts = copy._attachments;
      TEquals('object', typeof atts);
      TEquals('object', typeof atts["readme.txt"]);
      TEquals(2, atts["readme.txt"].revpos);
      TEquals(0, atts["readme.txt"].content_type.indexOf("text/plain"));
      TEquals(true, atts["readme.txt"].stub);

      var att1_copy = CouchDB.request(
        "GET", "/" + targetDb.name + "/" + copy._id + "/readme.txt"
      ).responseText;
      TEquals(att1_data.length, att1_copy.length);
      TEquals(att1_data, att1_copy);

      if (doc._id.indexOf("_design/") === -1) {
        TEquals('object', typeof atts["data.dat"]);
        TEquals(3, atts["data.dat"].revpos);
        TEquals(0, atts["data.dat"].content_type.indexOf("application/binary"));
        TEquals(true, atts["data.dat"].stub);

        var att2_copy = CouchDB.request(
          "GET", "/" + targetDb.name + "/" + copy._id + "/data.dat"
        ).responseText;
        TEquals(att2_data.length, att2_copy.length);
        TEquals(att2_data, att2_copy);
      }
    }

    sourceInfo = sourceDb.info();
    targetInfo = targetDb.info();

    TEquals(sourceInfo.doc_count, targetInfo.doc_count);

    // add another attachment to the ddoc on source
    addAtt(sourceDb, ddoc, "data.dat", att2_data, "application/binary");

    waitForSeq(sourceDb, targetDb, rep_id);

    copy = targetDb.open(ddoc._id);
    var atts = copy._attachments;
    TEquals('object', typeof atts);
    TEquals('object', typeof atts["readme.txt"]);
    TEquals(2, atts["readme.txt"].revpos);
    TEquals(0, atts["readme.txt"].content_type.indexOf("text/plain"));
    TEquals(true, atts["readme.txt"].stub);

    var att1_copy = CouchDB.request(
      "GET", "/" + targetDb.name + "/" + copy._id + "/readme.txt"
    ).responseText;
    TEquals(att1_data.length, att1_copy.length);
    TEquals(att1_data, att1_copy);

    TEquals('object', typeof atts["data.dat"]);
    TEquals(3, atts["data.dat"].revpos);
    TEquals(0, atts["data.dat"].content_type.indexOf("application/binary"));
    TEquals(true, atts["data.dat"].stub);

    var att2_copy = CouchDB.request(
      "GET", "/" + targetDb.name + "/" + copy._id + "/data.dat"
    ).responseText;
    TEquals(att2_data.length, att2_copy.length);
    TEquals(att2_data, att2_copy);

    sourceInfo = sourceDb.info();
    targetInfo = targetDb.info();

    TEquals(sourceInfo.doc_count, targetInfo.doc_count);


    // add more docs to source
    var newDocs = makeDocs(25, 35);
    populateSourceDb(newDocs, true);

    waitForSeq(sourceDb, targetDb, rep_id);

    for (j = 0; j < newDocs.length; j++) {
      doc = newDocs[j];
      copy = targetDb.open(doc._id);

      T(copy !== null);
      TEquals(true, compareObjects(doc, copy));
    }

    sourceInfo = sourceDb.info();
    targetInfo = targetDb.info();

    TEquals(sourceInfo.doc_count, targetInfo.doc_count);

    // delete docs from source
    TEquals(true, sourceDb.deleteDoc(newDocs[0]).ok);
    TEquals(true, sourceDb.deleteDoc(newDocs[6]).ok);

    waitForSeq(sourceDb, targetDb, rep_id);

    copy = targetDb.open(newDocs[0]._id);
    TEquals(null, copy);
    copy = targetDb.open(newDocs[6]._id);
    TEquals(null, copy);

    var changes = targetDb.changes({since: targetInfo.update_seq});
    // quite unfortunately, there is no way on relying on ordering in a cluster
    // but we can assume a length of 2
    var line1 = changes.results[changes.results.length - 2];
    var line2 = changes.results[changes.results.length - 1];
    T(newDocs[0]._id == line1.id || newDocs[0]._id == line2.id);
    T(newDocs[6]._id == line1.id || newDocs[6]._id == line2.id);
    T(line1.deleted && line2.deleted);

    // cancel the replication
    repResult = CouchDB.replicate(
      dbPairsPrefixes[i].source+sourceDb.name,
      dbPairsPrefixes[i].target+targetDb.name,
      {
        body: {
          continuous: true,
          cancel: true
        }
      }
    );
    TEquals(true, repResult.ok);
    TEquals(rep_id, repResult._local_id);

    doc = {
      _id: 'foobar',
      value: 666
    };
    TEquals(true, sourceDb.save(doc).ok);

    waitReplicationTaskStop(rep_id);

    copy = targetDb.open(doc._id);
    TEquals(null, copy);
  }

  // COUCHDB-1093 - filtered and continuous _changes feed dies when the
  // database is compacted
  // no more relevant when clustering, you can't compact (per se at least)
  /*
  docs = makeDocs(1, 10);
  docs.push({
    _id: "_design/foo",
    language: "javascript",
    filters: {
      myfilter: (function(doc, req) { return true; }).toString()
    }
  });
  populateSourceDb(docs);
  populateTargetDb([]);

  repResult = CouchDB.replicate(
    CouchDB.protocol + host + "/" + sourceDb.name,
    targetDb.name,
    {
      body: {
        continuous: true,
        filter: "foo/myfilter"
      }
    }
  );
  TEquals(true, repResult.ok);
  TEquals('string', typeof repResult._local_id);

  TEquals(true, sourceDb.compact().ok);
  while (sourceDb.info().compact_running) {};

  TEquals(true, sourceDb.save(makeDocs(30, 31)[0]).ok);

  var task = getTask(repResult._local_id, 1000);
  T(task != null);

  waitForSeq(sourceDb, targetDb, repResult._local_id);
  T(sourceDb.open("30") !== null);

  // cancel replication
  repResult = CouchDB.replicate(
    CouchDB.protocol + host + "/" + sourceDb.name,
    targetDb.name,
    {
      body: {
        continuous: true,
        filter: "foo/myfilter",
        cancel: true
      }
    }
  );
  TEquals(true, repResult.ok);
  TEquals('string', typeof repResult._local_id);
  */

  //
  // test replication of compressed attachments
  //
  doc = {
    _id: "foobar"
  };
  var bigTextAtt = makeAttData(128 * 1024);
  var attName = "readme.txt";
  var oldSettings = getCompressionInfo();
  var compressionLevel = oldSettings.level;
  var compressibleTypes = oldSettings.types;

  for (i = 0; i < dbPairsPrefixes.length; i++) {
    populateSourceDb([doc]);
    populateTargetDb([]);

    // enable compression of text types
    enableAttCompression("8", "text/*");

    // add text attachment to foobar doc
    xhr = CouchDB.request(
      "PUT",
      "/" + sourceDb.name + "/" + doc._id + "/" + attName + "?rev=" + doc._rev,
      {
        body: bigTextAtt,
        headers: {"Content-Type": "text/plain"}
      }
    );
    TEquals(201, xhr.status);

    // disable compression and replicate
    disableAttCompression();

    repResult = CouchDB.replicate(dbPairsPrefixes[i].source+sourceDb.name, dbPairsPrefixes[i].target+targetDb.name);
    TEquals(true, repResult.ok);
    TEquals(true, repResult.history instanceof Array);
    TEquals(1, repResult.history.length);
    TEquals(1, repResult.history[0].missing_checked);
    TEquals(1, repResult.history[0].missing_found);
    TEquals(1, repResult.history[0].docs_read);
    TEquals(1, repResult.history[0].docs_written);
    TEquals(0, repResult.history[0].doc_write_failures);

    copy = targetDb.open(
      doc._id,
      {att_encoding_info: true, bypass_cache: Math.round(Math.random() * 1000)}
    );
    T(copy !== null);
    T(attName in copy._attachments);
    TEquals("gzip", copy._attachments[attName].encoding);
    TEquals("number", typeof copy._attachments[attName].length);
    TEquals("number", typeof copy._attachments[attName].encoded_length);
    T(copy._attachments[attName].encoded_length < copy._attachments[attName].length);
  }

  delete bigTextAtt;
  // restore original settings
  enableAttCompression(compressionLevel, compressibleTypes);

  //
  // test replication triggered by non admins
  //

  // case 1) user triggering the replication is not a DB admin of the target DB
  var joeUserDoc = CouchDB.prepareUserDoc({
    name: "joe",
    roles: ["erlanger"]
  }, "erly");
  var defaultUsersDb = new CouchDB("_users", {"X-Couch-Full-Commit":"false"});
  try { defaultUsersDb.createDb(); } catch (e) { /* ignore if exists*/ }
  //var usersDb = new CouchDB("test_suite_auth", {"X-Couch-Full-Commit":"false"});
  /*var server_config = [
    {
      section: "couch_httpd_auth",
      key: "authentication_db",
      value: usersDb.name
    }
  ];*/

  docs = makeDocs(1, 6);
  docs.push({
    _id: "_design/foo",
    language: "javascript"
  });

  dbPairsPrefixes = [
    {
      source: "",
      target: ""
    },
    {
      source: CouchDB.protocol + host + "/",
      target: ""
    },
    {
      source: "",
      target: CouchDB.protocol + "joe:erly@" + host + "/"
    },
    {
      source: CouchDB.protocol + host + "/",
      target: CouchDB.protocol + "joe:erly@" + host + "/"
    }
  ];

  for (i = 0; i < dbPairsPrefixes.length; i++) {
    //usersDb.deleteDb();
    populateSourceDb(docs);
    populateTargetDb([]);

    TEquals(true, targetDb.setSecObj({
      admins: {
        names: ["superman"],
        roles: ["god"]
      }
    }).ok);

    // do NOT run on modified server b/c we use the default DB
    //run_on_modified_server(server_config, function() {
      delete joeUserDoc._rev;
      var prevJoeUserDoc = defaultUsersDb.open(joeUserDoc._id);
      if (prevJoeUserDoc) {
        joeUserDoc._rev = prevJoeUserDoc._rev;
      }
      if(i == 0) {
        TEquals(true, defaultUsersDb.save(joeUserDoc).ok);
        wait(5000);
      }
      TEquals(true, CouchDB.login("joe", "erly").ok);
      TEquals('joe', CouchDB.session().userCtx.name);

      repResult = CouchDB.replicate(dbPairsPrefixes[i].source+sourceDb.name, dbPairsPrefixes[i].target+targetDb.name);

      TEquals(true, CouchDB.logout().ok);

      TEquals(true, repResult.ok);
      TEquals(docs.length, repResult.history[0].docs_read);
      TEquals((docs.length - 1), repResult.history[0].docs_written); // 1 ddoc
      TEquals(1, repResult.history[0].doc_write_failures);
    //});

    for (j = 0; j < docs.length; j++) {
      doc = docs[j];
      copy = targetDb.open(doc._id);

      if (doc._id.indexOf("_design/") === 0) {
        TEquals(null, copy);
      } else {
        T(copy !== null);
        TEquals(true, compareObjects(doc, copy));
      }
    }
  }

  // case 2) user triggering the replication is not a reader (nor admin) of the source DB
  dbPairsPrefixes = [
    {
      source: "",
      target: ""
    },
    {
      source: CouchDB.protocol + "joe:erly@" + host + "/",
      target: ""
    },
    {
      source: "",
      target: CouchDB.protocol + host + "/"
    },
    {
      source: CouchDB.protocol + "joe:erly@" + host + "/",
      target: CouchDB.protocol + host + "/"
    }
  ];

  for (i = 0; i < dbPairsPrefixes.length; i++) {
    //usersDb.deleteDb();
    populateSourceDb(docs);
    populateTargetDb([]);

    TEquals(true, sourceDb.setSecObj({
      admins: {
        names: ["superman"],
        roles: ["god"]
      },
      readers: {
        names: ["john"],
        roles: ["secret"]
      }
    }).ok);
    // check that we start OK (plus give time for sec object apply 2 avoid Heisenbugs)
    for (j = 0; j < docs.length; j++) {
      doc = docs[j];
      copy = targetDb.open(doc._id);
      TEquals(null, copy);
    }

    // do NOT run on modified server b/c we use the default DB
    //run_on_modified_server(server_config, function() {
      delete joeUserDoc._rev;
      var prevJoeUserDoc = defaultUsersDb.open(joeUserDoc._id);
      if (prevJoeUserDoc) {
        joeUserDoc._rev = prevJoeUserDoc._rev;
      }
      if(i == 0) {
        TEquals(true, defaultUsersDb.save(joeUserDoc).ok);
        wait(5000);
      }

      TEquals(true, CouchDB.login("joe", "erly").ok);
      TEquals('joe', CouchDB.session().userCtx.name);

      try {
        CouchDB.replicate(dbPairsPrefixes[i].source+sourceDb.name, dbPairsPrefixes[i].target+targetDb.name);
        T(false, "should have raised an exception");
      } catch (x) {
        // TODO: small thing: DB exists but is no more found - at least we have an exception, so it's rather minor
        //TEquals("unauthorized", x.error);
        T(!!x);
      }

      TEquals(true, CouchDB.logout().ok);
    //});

    for (j = 0; j < docs.length; j++) {
      doc = docs[j];
      copy = targetDb.open(doc._id);
      TEquals(null, copy);
    }
  }


  // COUCHDB-885 - push replication of a doc with attachment causes a
  //               conflict in the target.
  populateSourceDb([]);
  populateTargetDb([]);

  doc = {
    _id: "doc1"
  };
  TEquals(true, sourceDb.save(doc).ok);

  repResult = CouchDB.replicate(
    sourceDb.name,
    CouchDB.protocol + host + "/" + targetDb.name
  );
  TEquals(true, repResult.ok);
  TEquals(true, repResult.history instanceof Array);
  TEquals(1, repResult.history.length);
  TEquals(1, repResult.history[0].docs_written);
  TEquals(1, repResult.history[0].docs_read);
  TEquals(0, repResult.history[0].doc_write_failures);

  doc["_attachments"] = {
    "hello.txt": {
      "content_type": "text/plain",
      "data": "aGVsbG8gd29ybGQ="  // base64:encode("hello world")
    },
    "foo.dat": {
      "content_type": "not/compressible",
      "data": "aSBhbSBub3QgZ3ppcGVk"  // base64:encode("i am not gziped")
    }
  };

  TEquals(true, sourceDb.save(doc).ok);
  repResult = CouchDB.replicate(
    sourceDb.name,
    CouchDB.protocol + host + "/" + targetDb.name
  );
  TEquals(true, repResult.ok);
  TEquals(true, repResult.history instanceof Array);
  TEquals(2, repResult.history.length);
  TEquals(1, repResult.history[0].docs_written);
  TEquals(1, repResult.history[0].docs_read);
  TEquals(0, repResult.history[0].doc_write_failures);

  copy = targetDb.open(doc._id, {
    conflicts: true, deleted_conflicts: true,
    attachments: true, att_encoding_info: true});
  T(copy !== null);
  TEquals("undefined", typeof copy._conflicts);
  TEquals("undefined", typeof copy._deleted_conflicts);
  TEquals("text/plain", copy._attachments["hello.txt"]["content_type"]);
  TEquals("aGVsbG8gd29ybGQ=", copy._attachments["hello.txt"]["data"]);
  TEquals("gzip", copy._attachments["hello.txt"]["encoding"]);
  TEquals("not/compressible", copy._attachments["foo.dat"]["content_type"]);
  TEquals("aSBhbSBub3QgZ3ppcGVk", copy._attachments["foo.dat"]["data"]);
  TEquals("undefined", typeof copy._attachments["foo.dat"]["encoding"]);
  // end of test for COUCHDB-885

  // Test for COUCHDB-1242 (reject non-string query_params)
  // TODO: non-String params crash CouchDB alltogether
  /*
  try {
    CouchDB.replicate(sourceDb, targetDb, {
      body: {
        filter : "mydesign/myfilter",
        query_params : {
          "maxvalue": 4
        }
      }
    });
  } catch (e) {
    TEquals("bad_request", e.error);
  }
  */


  // Test that we can cancel a replication just by POSTing an object
  // like  {"replication_id": Id, "cancel": true}. The replication ID
  // can be obtained from a continuous replication request response
  // (_local_id field), from _active_tasks or from the log
  populateSourceDb(makeDocs(1, 6));
  populateTargetDb([]);

  repResult = CouchDB.replicate(
    CouchDB.protocol + host + "/" + sourceDb.name,
    targetDb.name,
    {
      body: {
        continuous: true,
        create_target: true
      }
    }
  );
  TEquals(true, repResult.ok);
  TEquals('string', typeof repResult._local_id);
  var repId = repResult._local_id;

  var task = getTask(repId, 3000);
  T(task != null);

  TEquals(task["replication_id"], repId, "Replication found in _active_tasks");
  xhr = CouchDB.request(
    "POST", "/_replicate", {
      body: JSON.stringify({"replication_id": repId, "cancel": true}),
      headers: {"Content-Type": "application/json"}
  });
  TEquals(200, xhr.status, "Replication cancel request success");
  waitReplicationTaskStop(repId);
  task = getTask(repId);
  TEquals(null, task, "Replication was canceled");

  xhr = CouchDB.request(
    "POST", "/_replicate", {
      body: JSON.stringify({"replication_id": repId, "cancel": true}),
      headers: {"Content-Type": "application/json"}
  });
  TEquals(404, xhr.status, "2nd replication cancel failed");

  // Non-admin user can not cancel replications triggered by other users
  var userDoc = CouchDB.prepareUserDoc({
    name: "tony",
    roles: ["mafia"]
  }, "soprano");
  // again, due doe _security not there, we use the default users DB
  defaultUsersDb = new CouchDB("_users", {"X-Couch-Full-Commit":"false"});
  //usersDb = new CouchDB("test_suite_auth", {"X-Couch-Full-Commit":"false"});
  // (and leave the server alone)
  /*server_config = [
    {
      section: "couch_httpd_auth",
      key: "authentication_db",
      value: usersDb.name
    }
  ];*/

  //run_on_modified_server(server_config, function() {
    populateSourceDb(makeDocs(1, 6));
    populateTargetDb([]);
    var prevUserDoc = defaultUsersDb.open(userDoc._id);
    if(prevUserDoc) {
      userDoc._rev = prevUserDoc._rev;
    }
    TEquals(true, defaultUsersDb.save(userDoc).ok);

    repResult = CouchDB.replicate(
      CouchDB.protocol + host + "/" + sourceDb.name,
      targetDb.name,
      {
        body: {
          continuous: true
        }
      }
    );
    TEquals(true, repResult.ok);
    TEquals('string', typeof repResult._local_id);

    TEquals(true, CouchDB.login("tony", "soprano").ok);
    TEquals('tony', CouchDB.session().userCtx.name);

    xhr = CouchDB.request(
      "POST", "/_replicate", {
        body: JSON.stringify({"replication_id": repResult._local_id, "cancel": true}),
        headers: {"Content-Type": "application/json"}
    });
    TEquals(401, xhr.status, "Unauthorized to cancel replication");
    TEquals("unauthorized", JSON.parse(xhr.responseText).error);

    TEquals(true, CouchDB.logout().ok);

    xhr = CouchDB.request(
      "POST", "/_replicate", {
        body: JSON.stringify({"replication_id": repResult._local_id, "cancel": true}),
        headers: {"Content-Type": "application/json"}
    });
    TEquals(200, xhr.status, "Authorized to cancel replication");
  //});

  // cleanup
  //usersDb.deleteDb();
  sourceDb.deleteDb();
  targetDb.deleteDb();
  // (not sure what this is - cleanup after 'file not found tests' poss. - not harmful anyway)
  (new CouchDB("test_suite_db")).deleteDb();
};
