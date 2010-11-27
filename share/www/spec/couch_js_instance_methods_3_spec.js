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

// Specs for couch.js lines 201-265

describe 'CouchDB instance'
  before_each
    db = new CouchDB("spec_db", {"X-Couch-Full-Commit":"false"});
    db.createDb();
  end
  
  after_each
    db.deleteDb();
  end
  
  describe '.allDocs'
    it 'should return no docs when there arent any'
      db.allDocs().total_rows.should.eql 0
      db.allDocs().rows.should.eql []
    end
    
    describe 'with docs'
      before_each
        db.save({"Name" : "Felix Gaeta",      "_id" : "123"});
        db.save({"Name" : "Samuel T. Anders", "_id" : "456"});
      end
    
      it 'should return all docs'
        var result = db.allDocs();
        
        result.total_rows.should.eql 2
        result.rows.should.have_length 2
        result.rows[0].id.should.eql "123"
        result.rows[0].key.should.eql "123"
        result.rows[0].value.rev.length.should.be_at_least 30
        result.rows[1].id.should.eql "456"
      end
    
      it 'should pass through the options'
        var result = db.allDocs({"startkey": "123", "limit": "1"});
      
        result.rows.should.have_length 1
        result.rows[0].id.should.eql "123"
      end
      
      it 'should pass through the keys'
        var result = db.allDocs({}, ["456"]);

        result.rows.should.have_length 1
        result.rows[0].id.should.eql "456"
        result.rows[0].key.should.eql "456"
        result.rows[0].value.rev.length.should.be_at_least 30
      end

      it 'should pass through the options and the keys'
        var result = db.allDocs({"include_docs":"true"}, ["456"]);

        result.rows.should.have_length 1
        result.rows[0].id.should.eql "456"
        result.rows[0].key.should.eql "456"
        result.rows[0].value.rev.length.should.be_at_least 30
        result.rows[0].doc["Name"].should.eql "Samuel T. Anders"
        result.rows[0].doc["_rev"].length.should.be_at_least 30
      end

    end
  end
  
  describe '.designDocs'
    it 'should return nothing when there arent any design docs'
      db.save({"Name" : "Felix Gaeta", "_id" : "123"});
      db.designDocs().rows.should.eql []
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
      db.save(designDoc);
      db.save({"Name" : "Felix Gaeta", "_id" : "123"});
      
      var result = db.designDocs();
      
      result.total_rows.should.eql 2
      result.rows.should.have_length 1
      result.rows[0].id.should.eql "_design/spec_db"
      result.rows[0].key.should.eql "_design/spec_db"
      result.rows[0].value.rev.length.should.be_at_least 30
    end
  end
  
  describe '.changes'
    it 'should return no changes when there arent any'
      db.changes().last_seq.should.eql 0
      db.changes().results.should.eql []
    end
    
    describe 'with changes'
      before_each
        db.save({"Name" : "Felix Gaeta",      "_id" : "123"});
        db.save({"Name" : "Samuel T. Anders", "_id" : "456"});
      end
    
      it 'should return changes'
        var result = db.changes();
    
        result.last_seq.should.eql 2
        result.results[0].id.should.eql "123"
        result.results[0].seq.should.eql 1
        result.results[0].changes[0].rev.length.should.be_at_least 30
        result.results[1].id.should.eql "456"
        result.results[1].seq.should.eql 2
        result.results[1].changes[0].rev.length.should.be_at_least 30
      end
    
      it 'should pass through the options'
        var result = db.changes({"since":"1"});
      
        result.last_seq.should.eql 2
        result.results[0].id.should.eql "456"
      end
    end
  end
  
  describe '.compact'
    it 'should return ok true'
      db.compact().ok.should.be_true
    end
    
    it 'should post _compact to the db'
      db.should.receive("request", "once").with_args("POST", "/spec_db/_compact")
      db.compact();
    end
  end
  
  describe '.viewCleanup'
    it 'should return ok true'
      db.viewCleanup().ok.should.be_true
    end
    
    it 'should post _view_cleanup to the db'
      db.should.receive("request", "once").with_args("POST", "/spec_db/_view_cleanup")
      db.viewCleanup();
    end
  end
  
  describe '.setDbProperty'
    it 'should return ok true'
      db.setDbProperty("_revs_limit", 1500).ok.should.be_true
    end
    
    it 'should set a db property'
      db.setDbProperty("_revs_limit", 1500);
      db.getDbProperty("_revs_limit").should.eql 1500
      db.setDbProperty("_revs_limit", 1200);
      db.getDbProperty("_revs_limit").should.eql 1200
    end
  end
  
  describe '.getDbProperty'
    it 'should get a db property'
      db.setDbProperty("_revs_limit", 1200);
      db.getDbProperty("_revs_limit").should.eql 1200
    end
   
    it 'should throw an error when the property doesnt exist'
      function(){ db.getDbProperty("_doesnt_exist")}.should.throw_error
    end
  end
  
  describe '.setSecObj'
    it 'should return ok true'
      db.setSecObj({"members":{"names":["laura"],"roles":["president"]}}).ok.should.be_true
    end
      
    it 'should save a well formed object into the _security object '
      db.should.receive("request", "once").with_args("PUT", "/spec_db/_security", {body: '{"members":{"names":["laura"],"roles":["president"]}}'})
      db.setSecObj({"members": {"names" : ["laura"], "roles" : ["president"]}})
    end
    
    it 'should throw an error when the members or admins object is malformed'
      -{ db.setSecObj({"admins":["cylon"]}) }.should.throw_error
    end
    
    it 'should save any other object into the _security object'
      db.setSecObj({"something" : "anything"})
      db.getSecObj().should.eql {"something" : "anything"}
    end
  end
  
  describe '.getSecObj'
    it 'should get the security object'
      db.setSecObj({"admins" : {"names" : ["bill"], "roles" : ["admiral"]}})
      db.getSecObj().should.eql {"admins" : {"names": ["bill"], "roles": ["admiral"]}}
    end
    
    it 'should return an empty object when there is no security object'
      db.getSecObj().should.eql {}
    end
  end
end