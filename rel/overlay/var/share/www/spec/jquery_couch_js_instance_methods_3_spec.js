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

// Specs for jquery_couch.js lines 300-411

describe 'jQuery couchdb db'
  before
    stubAlert();
  end
  
  after
    destubAlert();
  end
  
  before_each
    db = $.couch.db('spec_db');
    db.create();
  end
  
  after_each
    db.drop();
  end

  describe 'removeDoc'
    before_each
      doc = {"Name" : "Louanne Katraine", "Callsign" : "Kat", "_id" : "345"};
      saved_doc = {};
      db.saveDoc(doc, {
        success: function(resp){
          saved_doc = resp;
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should result in a deleted document'
      db.removeDoc({_id : "345", _rev : saved_doc.rev}, {
        success: function(resp){
          db.openDoc("345", {
            error: function(status, error, reason){
              status.should.eql 404
              error.should.eql "not_found"
              reason.should.eql "deleted"
            },
            success: function(resp){successCallback(resp)}
          });
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
  
    it 'should return ok true, the ID and the revision of the deleted document'
      db.removeDoc({_id : "345", _rev : saved_doc.rev}, {
        success: function(resp){
          resp.ok.should.be_true
          resp.id.should.eql "345"
          resp.rev.should.be_a String
          resp.rev.length.should.be_at_least 30
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
      
    it 'should record the revision in the deleted document'
      db.removeDoc({_id : "345", _rev : saved_doc.rev}, {
        success: function(resp){
          db.openDoc("345", {
            rev: resp.rev,
            success: function(resp2){
              resp2._rev.should.eql resp.rev
              resp2._id.should.eql resp.id
              resp2._deleted.should.be_true
            },
            error: function(status, error, reason){errorCallback(status, error, reason)}
          });
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should alert with an error message prefix'
      db.removeDoc({_id: "asdf"});
      alert_msg.should.match /The document could not be deleted/
    end
  end
  
  describe 'bulkRemove'
    before_each
      doc  = {"Name" : "Kara Thrace", "Callsign" : "Starbuck", "_id" : "123"};
      doc2 = {"Name" : "Karl C. Agathon", "Callsign" : "Helo", "_id" : "456"};
      doc3 = {"Name" : "Sharon Valerii", "Callsign" : "Boomer", "_id" : "789"};
      docs = [doc, doc2, doc3];
      
      db.bulkSave({"docs": docs}, {
        success: function(resp){
          for (var i = 0; i < docs.length; i++) {
            docs[i]._rev = resp[i].rev;
          }
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should remove all documents specified'
      db.bulkRemove({"docs": docs});
      db.allDocs({
        success: function(resp) {
          resp.total_rows.should.eql 0
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should not remove documents that should not have been deleted'
      db.bulkRemove({"docs": [doc3]});
      db.allDocs({
        success: function(resp) {
          resp.total_rows.should.eql 2
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should result in deleted documents'
      db.bulkRemove({"docs": docs}, {
        success: function(resp){
          db.openDoc("123", {
            error: function(status, error, reason){
              status.should.eql 404
              error.should.eql "not_found"
              reason.should.eql "deleted"
            },
            success: function(resp){successCallback(resp)}
          });
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
  
    it 'should return the ID and the revision of the deleted documents'
      db.bulkRemove({"docs": docs}, {
        success: function(resp){
          resp[0].id.should.eql "123"
          resp[0].rev.should.be_a String
          resp[0].rev.length.should.be_at_least 30
          resp[1].id.should.eql "456"
          resp[1].rev.should.be_a String
          resp[1].rev.length.should.be_at_least 30
          resp[2].id.should.eql "789"
          resp[2].rev.should.be_a String
          resp[2].rev.length.should.be_at_least 30
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
  
    it 'should record the revision in the deleted documents'
      db.bulkRemove({"docs": docs}, {
        success: function(resp){
          db.openDoc("123", {
            rev: resp[0].rev,
            success: function(resp2){
              resp2._rev.should.eql resp[0].rev
              resp2._id.should.eql resp[0].id
              resp2._deleted.should.be_true
            },
            error: function(status, error, reason){errorCallback(status, error, reason)}
          });
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should alert with an error message prefix'
      db.bulkRemove({docs: ["asdf"]});
      alert_msg.should.match /The documents could not be deleted/
    end
  end
  
  describe 'copyDoc'
    before_each
      doc = {"Name" : "Sharon Agathon", "Callsign" : "Athena", "_id" : "123"};
      db.saveDoc(doc);
    end
    
    it 'should result in another document with same data and new id'
      db.copyDoc("123", {
        success: function(resp){
          resp.id.should.eql "456"
          resp.rev.length.should.be_at_least 30
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      }, {
        headers: {"Destination":"456"}
      });
      
      db.openDoc("456", {
        success: function(resp){
          resp.Name.should.eql "Sharon Agathon"
          resp.Callsign.should.eql "Athena"
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should throw an error when trying to overwrite a document without providing a revision'
      doc2 = {"Name" : "Louanne Katraine", "Callsign" : "Kat", "_id" : "456"};
      db.saveDoc(doc2);
      
      db.copyDoc("123", {
        error: function(status, error, reason){
          status.should.eql 409
          error.should.eql "conflict"
          reason.should.eql "Document update conflict."
        },
        success: function(resp){successCallback(resp)}
      }, {
        headers: {"Destination":"456"}
      });
    end
    
    it 'should overwrite a document with the correct revision'
      doc2 = {"Name" : "Louanne Katraine", "Callsign" : "Kat", "_id" : "456"};
      var doc2_rev;
      db.saveDoc(doc2, {
        success: function(resp){
          doc2_rev = resp.rev;
        }
      });
      
      db.copyDoc("123", {
        success: function(resp){
          resp.id.should.eql "456"
          resp.rev.length.should.be_at_least 30
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      }, {
        headers: {"Destination":"456?rev=" + doc2_rev}
      });
      
      db.openDoc("456", {
        success: function(resp){
          resp.Name.should.eql "Sharon Agathon"
          resp.Callsign.should.eql "Athena"
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should alert with an error message prefix'
      db.copyDoc("asdf", {}, {});
      alert_msg.should.match /The document could not be copied/
    end
  end
  
  describe 'query'
    before_each
      db.saveDoc({"Name" : "Cally Tyrol",      "job" : "deckhand", "_id" : "789"});
      db.saveDoc({"Name" : "Felix Gaeta",      "job" : "officer",  "_id" : "123"});
      db.saveDoc({"Name" : "Samuel T. Anders", "job" : "pilot",    "_id" : "456"});
      map_function    = "function(doc) { emit(doc._id, 1); }";
      reduce_function = "function(key, values, rereduce) { return sum(values); }";
    end
    
    it 'should apply the map function'
      db.query(map_function, null, null, {
        success: function(resp){
          resp.rows.should.have_length 3
          resp.rows[0].id.should.eql "123"
          resp.rows[0].key.should.eql "123"
          resp.rows[0].value.should.eql 1
          resp.rows[1].id.should.eql "456"
          resp.rows[1].key.should.eql "456"
          resp.rows[1].value.should.eql 1
          resp.rows[2].id.should.eql "789"
          resp.rows[2].key.should.eql "789"
          resp.rows[2].value.should.eql 1
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should apply the reduce function'
      db.query(map_function, reduce_function, null, {
        success: function(resp){
          resp.rows.should.have_length 1
          resp.rows[0].key.should.be_null
          resp.rows[0].value.should_eql 3
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should pass through the options'
      db.query(map_function, null, null, {
        "startkey": "456",
        success: function(resp){
          resp.rows.should.have_length 2
          resp.rows[0].id.should.eql "456"
          resp.rows[0].key.should.eql "456"
          resp.rows[0].value.should.eql 1
          resp.rows[1].id.should.eql "789"
          resp.rows[1].key.should.eql "789"
          resp.rows[1].value.should.eql 1
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should pass through the keys'
      //shouldn't this better work? TODO: implement in jquery.couch.js
      console.log("shouldn't this better work? TODO: implement in jquery.couch.js")
      db.query(map_function, null, null, {
        "keys": ["456", "123"],
        success: function(resp){
          resp.rows.should.have_length 2
          resp.rows[0].id.should.eql "456"
          resp.rows[0].key.should.eql "456"
          resp.rows[0].value.should.eql 1
          resp.rows[1].id.should.eql "123"
          resp.rows[1].key.should.eql "123"
          resp.rows[1].value.should.eql 1
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
      
    it 'should pass through the options and the keys'
      //shouldn't this better work? TODO: implement in jquery.couch.js
      console.log("shouldn't this better work? TODO: implement in jquery.couch.js")
      db.query(map_function, null, null, {
        "include_docs":"true",
        "keys": ["456"],
        success: function(resp){
          resp.rows.should.have_length 1
          resp.rows[0].id.should.eql "456"
          resp.rows[0].key.should.eql "456"
          resp.rows[0].value.should.eql 1
          resp.rows[0].doc["job"].should.eql "pilot"
          resp.rows[0].doc["_rev"].length.should.be_at_least 30
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
      
    it 'should apply a query in erlang also'
      // when this test fails, read this: http://wiki.apache.org/couchdb/EnableErlangViews
      var erlang_map = 'fun({Doc}) -> ' +
                       'ID = proplists:get_value(<<"_id">>, Doc, null), ' +
                       'Emit(ID, 1) ' +
                       'end.';
      db.query(erlang_map, null, "erlang", {
        success: function(resp){
          resp.rows.should.have_length 3
          resp.rows[0].id.should.eql "123"
          resp.rows[0].key.should.eql "123"
          resp.rows[0].value.should.eql 1
          resp.rows[1].id.should.eql "456"
          resp.rows[1].key.should.eql "456"
          resp.rows[1].value.should.eql 1
          resp.rows[2].id.should.eql "789"
          resp.rows[2].key.should.eql "789"
          resp.rows[2].value.should.eql 1
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should alert with an error message prefix'
      db.query("asdf");
      alert_msg.should.match /An error occurred querying the database/
    end
  end
  
  describe 'view'
    before_each
      db.saveDoc({"Name" : "Cally Tyrol",      "job" : "deckhand", "_id" : "789"});
      db.saveDoc({"Name" : "Felix Gaeta",      "job" : "officer",  "_id" : "123"});
      db.saveDoc({"Name" : "Samuel T. Anders", "job" : "pilot",    "_id" : "456"});
      view = {
        "views" : {
          "people" : {
            "map" : "function(doc) { emit(doc._id, doc.Name); }"
          }
        },
        "_id" : "_design/spec_db"
      };
      db.saveDoc(view);
    end
    
    it 'should apply the view'
      db.view('spec_db/people', {
        success: function(resp){
          resp.rows.should.have_length 3
          resp.rows[0].id.should.eql "123"
          resp.rows[0].key.should.eql "123"
          resp.rows[0].value.should.eql "Felix Gaeta"
          resp.rows[1].id.should.eql "456"
          resp.rows[1].key.should.eql "456"
          resp.rows[1].value.should.eql "Samuel T. Anders"
          resp.rows[2].id.should.eql "789"
          resp.rows[2].key.should.eql "789"
          resp.rows[2].value.should.eql "Cally Tyrol"
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should pass through the options'
      db.view('spec_db/people', {
        "skip":"2",
        success: function(resp){
          resp.rows.should.have_length 1
          resp.rows[0].id.should.eql "789"
          resp.rows[0].key.should.eql "789"
          resp.rows[0].value.should.eql "Cally Tyrol"
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should pass through the keys'
      db.view('spec_db/people', {
        "keys":["456", "123"],
        success: function(resp){
          resp.rows.should.have_length 2
          resp.rows[0].id.should.eql "456"
          resp.rows[0].key.should.eql "456"
          resp.rows[0].value.should.eql "Samuel T. Anders"
          resp.rows[1].id.should.eql "123"
          resp.rows[1].key.should.eql "123"
          resp.rows[1].value.should.eql "Felix Gaeta"
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should pass through the options and the keys'
      db.view('spec_db/people', {
        "include_docs":"true",
        "keys":["456"],
        success: function(resp){
          resp.rows.should.have_length 1
          resp.rows[0].id.should.eql "456"
          resp.rows[0].key.should.eql "456"
          resp.rows[0].value.should.eql "Samuel T. Anders"
          resp.rows[0].doc["job"].should.eql "pilot"
          resp.rows[0].doc["_rev"].length.should.be_at_least 30
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should throw a 404 when the view doesnt exist'
      db.view('spec_db/non_existing_view', {
        error: function(status, error, reason){
          status.should.eql 404
          error.should.eql "not_found"
          reason.should.eql "missing_named_view"
        },
        success: function(resp){successCallback(resp)}
      });
    end
    
    it 'should alert with an error message prefix'
      db.view("asdf");
      alert_msg.should.match /An error occurred accessing the view/
    end
  end
  
  describe 'setDbProperty'
    it 'should return ok true'
      db.setDbProperty("_revs_limit", 1500, {
        success: function(resp){
          resp.ok.should.be_true
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should set a db property'
      db.setDbProperty("_revs_limit", 1500);
      db.getDbProperty("_revs_limit", {
        success: function(resp){
          resp.should.eql 1500
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
      db.setDbProperty("_revs_limit", 1200);
      db.getDbProperty("_revs_limit", {
        success: function(resp){
          resp.should.eql 1200
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should alert with an error message prefix'
      db.setDbProperty("asdf");
      alert_msg.should.match /The property could not be updated/
    end
  end
  
  describe 'getDbProperty'
    it 'should get a db property'
      db.setDbProperty("_revs_limit", 1200);
      db.getDbProperty("_revs_limit", {
        success: function(resp){
          resp.should.eql 1200
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
   
    it 'should throw a 404 when the property doesnt exist'
      db.getDbProperty("_doesnt_exist", {
        error: function(status, error, reason){
          status.should.eql 404
          error.should.eql "not_found"
          reason.should.eql "missing"
        },
        success: function(resp){successCallback(resp)}
      });
    end
    
    it 'should alert with an error message prefix'
      db.getDbProperty("asdf");
      alert_msg.should.match /The property could not be retrieved/
    end
  end
end