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

// Specs for couch.js lines 1-130

describe 'CouchDB instance'
  before_each
    db = new CouchDB("spec_db", {"X-Couch-Full-Commit":"false"});
  end
  
  describe '.request'
    before_each
      db.createDb();
    end

    after_each
      db.deleteDb();
    end

    it 'should return a XMLHttpRequest'
      var req = db.request("GET", "/spec_db");
      req.should.include "readyState"
      req.should.include "responseText"
      req.should.include "statusText"
      // in Safari a XMLHttpRequest is actually a XMLHttpRequestConstructor,
      // otherwise we could just do:
      // req.should.be_a XMLHttpRequest
    end
    
    it 'should set the options the CouchDB instance has got as httpHeaders'
      CouchDB.should.receive("request", "once").with_args("GET", "/spec_db", {headers: {"X-Couch-Full-Commit": "false"}})
      db.request("GET", "/spec_db");
    end
    
    it 'should pass through the options'
      CouchDB.should.receive("request", "once").with_args("GET", "/spec_db", {"X-Couch-Persist": "true", headers: {"X-Couch-Full-Commit": "false"}})
      db.request("GET", "/spec_db", {"X-Couch-Persist":"true"});
    end
  end
    
  describe '.createDb'
    after_each
      db.deleteDb();
    end
     
    it 'should create the db'
      db.createDb();
      db.last_req.status.should.eql 201
    end
    
    it 'should return the ok true'
      db.createDb().should.eql {"ok" : true}
    end
    
    it 'should result in a created db'
      db.createDb();
      try{
        db.createDb();
      } catch(e) {
        e.error.should.eql "file_exists"
      }
    end
    
    it 'should have create a db with update sequence 0'
      db.createDb();
      db.info().update_seq.should.eql 0
    end
  end
   
  describe '.deleteDb'
    before_each
      db.createDb();
    end
  
    it 'should delete the db'
      db.deleteDb();
      db.last_req.status.should.eql 200
    end
    
    it 'should return the responseText of the request'
      db.deleteDb().should.eql {"ok" : true}
    end
    
    it 'should result in a deleted db'
      db.deleteDb();
      db.deleteDb();
      db.last_req.status.should.eql 404
    end
  end
  
  describe 'document methods'
    before_each
      doc = {"Name" : "Kara Thrace", "Callsign" : "Starbuck"};
      db.createDb();
    end
  
    after_each
      db.deleteDb();
    end
    
    describe '.save'
      it 'should save the document'
        db.save(doc);
        db.last_req.status.should.eql 201
      end
    
      it 'should return ok true'
        db.save(doc).ok.should.be_true
      end
      
      it 'should return ID and revision of the document'
        var response = db.save(doc);
        response.id.should.be_a String
        response.id.should.have_length 32
        response.rev.should.be_a String
        response.rev.length.should.be_at_least 30
      end
      
      it 'should result in a saved document with generated ID'
        var response  = db.save(doc);
        var saved_doc = db.open(response.id);
        saved_doc.Name.should.eql "Kara Thrace"
        saved_doc.Callsign.should.eql "Starbuck"
      end
  
      it 'should save the document with the specified ID'
        doc._id = "123";
        var response = db.save(doc);
        response.id.should.eql "123"
      end
    
      it 'should pass through the options'
        doc._id = "123";
        CouchDB.should.receive("request", "once").with_args("PUT", "/spec_db/123?batch=ok")
        db.save(doc, {"batch" : "ok"});
      end
    end
      
    describe '.open'
      before_each
        doc._id = "123";
        db.save(doc);
      end
      
      it 'should open the document'
        db.open("123").should.eql doc
      end
    
      it 'should return null when there is no document with the given ID'
        db.open("non_existing").should.be_null
      end
    
      it 'should pass through the options'
        CouchDB.should.receive("request", "once").with_args("GET", "/spec_db/123?revs=true")
        db.open("123", {"revs" : "true"});
      end
    end
      
    describe '.deleteDoc'
      before_each
        doc._id = "123";
        saved_doc = db.save(doc);
        delete_response = db.deleteDoc({_id : "123", _rev : saved_doc.rev});
        delete_last_req = db.last_req;
        db.open("123");
      end
      
      it 'should send a successful request'
        delete_last_req.status.should.eql 200
      end
    
      it 'should result in a deleted document'
        db.open("123").should.be_null
      end
    
      it 'should return ok true, the ID and the revision of the deleted document'
        delete_response.ok.should.be_true
        delete_response.id.should.eql "123"
        delete_response.rev.should.be_a String
        delete_response.rev.length.should.be_at_least 30
      end
      
      it 'should mark the document as deleted'
        var responseText = db.request("GET", "/spec_db/123").responseText;
        JSON.parse(responseText).should.eql {"error":"not_found","reason":"deleted"}
      end
    
      it 'should record the revision in the deleted document'
        var responseText = db.request("GET", "/spec_db/123?rev=" + delete_response.rev).responseText;
        var deleted_doc = JSON.parse(responseText);
        deleted_doc._rev.should.eql delete_response.rev
        deleted_doc._id.should.eql delete_response.id
        deleted_doc._deleted.should.be_true
      end
    end
      
    describe '.deleteDocAttachment'
      before_each
        doc._id = "123";
        doc._attachments = {
          "friend.txt" : {
            "content_type": "text\/plain",
            // base64 encoded
            "data": "TGVlIEFkYW1hIGlzIGEgZm9ybWVyIENvbG9uaWFsIEZsZWV0IFJlc2VydmUgb2ZmaWNlci4="
          }
        };
        saved_doc = db.save(doc);
      end
     
      it 'should be executed on a document with attachment'
        db.open("123")._attachments.should.include "friend.txt"
        db.open("123")._attachments["friend.txt"].stub.should.be_true
      end
     
      describe 'after delete'
        before_each
          delete_response = db.deleteDocAttachment({_id : "123", _rev : saved_doc.rev}, "friend.txt");
          db.open("123");
        end
        
        it 'should send a successful request'
          db.last_req.status.should.eql 200
        end
     
        it 'should leave the document untouched'
          db.open("123").Callsign.should.eql "Starbuck"
        end
     
        it 'should result in a deleted document attachment'
          db.open("123").should.not.include "_attachments"
        end
     
        it 'should record the revision in the document whose attachment has been deleted'
          var responseText = db.request("GET", "/spec_db/123?rev=" + delete_response.rev).responseText;
          var deleted_doc = JSON.parse(responseText);
          deleted_doc._rev.should.eql delete_response.rev
          deleted_doc._id.should.eql delete_response.id
        end
     
        it 'should return ok true, the ID and the revision of the document whose attachment has been deleted'
          delete_response.ok.should.be_true
          delete_response.id.should.eql "123"
          delete_response.should.have_property 'rev'
        end
      end
    end
          
    describe '.bulkSave'
      before_each
        doc  = {"Name" : "Kara Thrace", "Callsign" : "Starbuck"};
        doc2 = {"Name" : "Karl C. Agathon", "Callsign" : "Helo"};
        doc3 = {"Name" : "Sharon Valerii", "Callsign" : "Boomer"};
        docs = [doc, doc2, doc3];
      end
      
      it 'should save the documents'
        db.bulkSave(docs);
        db.last_req.status.should.eql 201
      end
      
      it 'should return ID and revision of the documents'
        var response = db.bulkSave(docs);
        response[0].id.should.be_a String
        response[0].id.should.have_length 32
        response[0].rev.should.be_a String
        response[0].rev.length.should.be_at_least 30
        response[1].id.should.be_a String
        response[1].id.should.have_length 32
        response[1].rev.should.be_a String
        response[1].rev.length.should.be_at_least 30
        response[2].id.should.be_a String
        response[2].id.should.have_length 32
        response[2].rev.should.be_a String
        response[2].rev.length.should.be_at_least 30
      end
      
      it 'should result in saved documents'
        var response  = db.bulkSave(docs);
        db.open(response[0].id).Name.should.eql "Kara Thrace"
        db.open(response[1].id).Name.should.eql "Karl C. Agathon"
        db.open(response[2].id).Name.should.eql "Sharon Valerii"
      end
    
      it 'should save the document with specified IDs'
        doc._id  = "123";
        doc2._id = "456";
        docs = [doc, doc2, doc3];
        var response = db.bulkSave(docs);
        response[0].id.should.eql "123"
        response[1].id.should.eql "456"
        response[2].id.should.have_length 32
      end
      
      it 'should pass through the options'
        doc._id  = "123";
        docs = [doc];
        CouchDB.should.receive("request", "once").with_args("POST", "/spec_db/_bulk_docs", {body: '{"docs":[{"Name":"Kara Thrace","Callsign":"Starbuck","_id":"123"}],"batch":"ok"}'})
        db.bulkSave(docs, {"batch" : "ok"});
      end
    end
  end
end