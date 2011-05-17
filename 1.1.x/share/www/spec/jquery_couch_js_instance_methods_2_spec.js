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

// Specs for jquery_couch.js lines 210-299

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

  describe 'info'
    before_each
      result = {};
      db.info({
        success: function(resp) { result = resp; },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should return the name of the database'
      result.db_name.should.eql "spec_db"
    end
    
    it 'should return the number of documents'
      result.doc_count.should.eql 0
    end
    
    it 'should return the start time of the db instance'
      result.instance_start_time.should.have_length 16
    end
  end
  
  describe 'allDocs'
    it 'should return no docs when there arent any'
      db.allDocs({
        success: function(resp) {
          resp.total_rows.should.eql 0
          resp.rows.should.eql []
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    describe 'with docs'
      before_each
        db.saveDoc({"Name" : "Felix Gaeta",      "_id" : "123"});
        db.saveDoc({"Name" : "Samuel T. Anders", "_id" : "456"});
      end
    
      it 'should return all docs'
        db.allDocs({
          success: function(resp) {
            resp.total_rows.should.eql 2
            resp.rows.should.have_length 2
            resp.rows[0].id.should.eql "123"
            resp.rows[0].key.should.eql "123"
            resp.rows[0].value.rev.length.should.be_at_least 30
            resp.rows[1].id.should.eql "456"
          },
          error: function(status, error, reason){errorCallback(status, error, reason)}
        });
      end
    
      it 'should pass through the options'
        db.allDocs({
          "startkey": "123",
          "limit": "1",
          success: function(resp) {
            resp.rows.should.have_length 1
            resp.rows[0].id.should.eql "123"
          },
          error: function(status, error, reason){errorCallback(status, error, reason)}
        });
      end
    end
  end
  
  describe 'allDesignDocs'
    it 'should return nothing when there arent any design docs'
      db.saveDoc({"Name" : "Felix Gaeta", "_id" : "123"});
      db.allDesignDocs({
        success: function(resp) {
          resp.rows.should.eql []
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should return all design docs'
      var designDoc = {
        "views" : {
          "people" : {
            "map" : "function(doc) { emit(doc._id, doc); }"
          }
        },
        "_id" : "_design/spec_db"
      };
      db.saveDoc(designDoc);
      db.saveDoc({"Name" : "Felix Gaeta", "_id" : "123"});
      
      db.allDesignDocs({
        success: function(resp) {
          resp.total_rows.should.eql 2
          resp.rows.should.have_length 1
          resp.rows[0].id.should.eql "_design/spec_db"
          resp.rows[0].key.should.eql "_design/spec_db"
          resp.rows[0].value.rev.length.should.be_at_least 30
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
  end
  
  describe 'allApps'
    it 'should provide a custom function with appName, appPath and design document when there is an attachment with index.html'
      var designDoc = {"_id" : "_design/with_attachments"};
    
      designDoc._attachments = {
        "index.html" : {
          "content_type": "text\/html",
          // this is "<html><p>Hi, here is index!</p></html>", base64 encoded
          "data": "PGh0bWw+PHA+SGksIGhlcmUgaXMgaW5kZXghPC9wPjwvaHRtbD4="
        }
      };
      db.saveDoc(designDoc);
      
      db.allApps({
        eachApp: function(appName, appPath, ddoc) {
          appName.should.eql "with_attachments"
          appPath.should.eql "/spec_db/_design/with_attachments/index.html"
          ddoc._id.should.eql "_design/with_attachments"
          ddoc._attachments["index.html"].content_type.should.eql "text/html"
          ddoc._attachments["index.html"].length.should.eql "<html><p>Hi, here is index!</p></html>".length
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should provide a custom function with appName, appPath and design document when there is a couchapp with index file'
      var designDoc = {"_id" : "_design/with_index"};
      designDoc.couchapp = {
        "index" : "cylon"
      };
      db.saveDoc(designDoc);
      
      db.allApps({
        eachApp: function(appName, appPath, ddoc) {
          appName.should.eql "with_index"
          appPath.should.eql "/spec_db/_design/with_index/cylon"
          ddoc._id.should.eql "_design/with_index"
          ddoc.couchapp.index.should.eql "cylon"
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should not call the eachApp function when there is neither index.html in _attachments nor a couchapp index file'
      var designDoc = {"_id" : "_design/nothing"};
      db.saveDoc(designDoc);
      
      var eachApp_called = false;
      db.allApps({
        eachApp: function(appName, appPath, ddoc) {
          eachApp_called = true;
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
      
      eachApp_called.should.be_false
    end
    
    it 'should alert with an error message prefix'
      db.allApps();
      alert_msg.should.match /Please provide an eachApp function for allApps()/
    end
  end
  
  describe 'openDoc'
    before_each
      doc = {"Name" : "Louanne Katraine", "Callsign" : "Kat", "_id" : "123"};
      db.saveDoc(doc);
    end
     
    it 'should open the document'
      db.openDoc("123", {
        success: function(resp){
          resp.should.eql doc
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should raise a 404 error when there is no document with the given ID'
      db.openDoc("non_existing", {
        error: function(status, error, reason){
          status.should.eql 404
          error.should.eql "not_found"
          reason.should.eql "missing"
        },
        success: function(resp){successCallback(resp)}
      });
    end
   
    it 'should pass through the options'
      doc.Name = "Sasha";
      db.saveDoc(doc);
      db.openDoc("123", {
        revs: true,
        success: function(resp){
          resp._revisions.start.should.eql 2
          resp._revisions.ids.should.have_length 2
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
  
    it 'should alert with an error message prefix'
      db.openDoc("asdf");
      alert_msg.should.match /The document could not be retrieved/
    end
  end
   
  describe 'saveDoc'
    before_each
      doc = {"Name" : "Kara Thrace", "Callsign" : "Starbuck"};
    end
     
    it 'should save the document'
      db.saveDoc(doc, {
        success: function(resp, status){
          status.should.eql 201
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
      
    it 'should return ok true'
      db.saveDoc(doc, {
        success: function(resp, status){
          resp.ok.should.be_true
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should return ID and revision of the document'
      db.saveDoc(doc, {
        success: function(resp, status){
          resp.id.should.be_a String
          resp.id.should.have_length 32
          resp.rev.should.be_a String
          resp.rev.length.should.be_at_least 30
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should result in a saved document with generated ID'
      db.saveDoc(doc, {
        success: function(resp, status){
          db.openDoc(resp.id, {
            success: function(resp2){
              resp2.Name.should.eql "Kara Thrace"
              resp2.Callsign.should.eql "Starbuck"
            },
            error: function(status, error, reason){errorCallback(status, error, reason)}
          });
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should save the document with the specified ID'
      doc._id = "123";
      db.saveDoc(doc, {
        success: function(resp, status){
          resp.id.should.eql "123"
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should pass through the options'
      db.saveDoc(doc, {
        "batch" : "ok",
        success: function(resp, status){
          // when using batch ok, couch sends a 202 status immediately
          status.should.eql 202
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should alert with an error message prefix'
      db.saveDoc("asdf");
      alert_msg.should.match /The document could not be saved/
    end
  end
   
  describe 'bulkSave'
    before_each
      doc  = {"Name" : "Kara Thrace", "Callsign" : "Starbuck"};
      doc2 = {"Name" : "Karl C. Agathon", "Callsign" : "Helo"};
      doc3 = {"Name" : "Sharon Valerii", "Callsign" : "Boomer"};
      docs = [doc, doc2, doc3];
    end
    
    it 'should save all documents'
      db.bulkSave({"docs": docs});
      db.allDocs({
        success: function(resp) {
          resp.total_rows.should.eql 3
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should result in saved documents'
      doc3._id = "789";
      db.bulkSave({"docs": [doc3]});
      
      db.openDoc("789", {
        success: function(resp){
          resp.Name.should.eql "Sharon Valerii"
          resp.Callsign.should.eql "Boomer"
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should return ID and revision of the documents'
      db.bulkSave({"docs": docs},{
        success: function(resp){
          resp[0].id.should.be_a String
          resp[0].id.should.have_length 32
          resp[0].rev.should.be_a String
          resp[0].rev.length.should.be_at_least 30
          resp[1].id.should.be_a String
          resp[1].id.should.have_length 32
          resp[1].rev.should.be_a String
          resp[1].rev.length.should.be_at_least 30
          resp[2].id.should.be_a String
          resp[2].id.should.have_length 32
          resp[2].rev.should.be_a String
          resp[2].rev.length.should.be_at_least 30
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
      
    it 'should save the document with specified IDs'
      doc._id  = "123";
      doc2._id = "456";
      docs = [doc, doc2, doc3];
      
      db.bulkSave({"docs": docs},{
        success: function(resp){
          resp[0].id.should.eql "123"
          resp[1].id.should.eql "456"
          resp[2].id.should.have_length 32
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should pass through the options'
      // a lengthy way to test that a conflict can't be created with the
      // all_or_nothing option set to false, but can be when it's true.
    
      var old_doc = {"Name" : "Louanne Katraine", "Callsign" : "Kat", "_id" : "123"};
      db.saveDoc(old_doc, {
        success: function(resp){
          old_doc._rev = resp.rev;
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
      
      var new_doc = {"Name" : "Sasha", "Callsign" : "Kat", "_id" : "123"};
      
      db.bulkSave({"docs": [new_doc], "all_or_nothing": false}, {
        success: function(resp){
          resp[0].id.should.eql "123"
          resp[0].error.should.eql "conflict"
          resp[0].reason.should.eql "Document update conflict."
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
      
      db.bulkSave({"docs": [new_doc], "all_or_nothing": true}, {
        success: function(resp){
          resp[0].id.should.eql "123"
          resp[0].rev.should.not.eql old_doc._rev
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
      
      db.openDoc("123", {
        "conflicts": true,
        success: function(resp){
          resp._conflicts[0].should.eql old_doc._rev
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
     
    it 'should alert with an error message prefix'
      db.bulkSave("asdf");
      alert_msg.should.match /The documents could not be saved/
    end
  end
end