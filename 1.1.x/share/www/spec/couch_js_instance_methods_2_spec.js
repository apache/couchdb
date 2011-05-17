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

// Specs for couch.js lines 132-199

describe 'CouchDB instance'
  before_each
    db = new CouchDB("spec_db", {"X-Couch-Full-Commit":"false"});
    db.createDb();
  end
  
  after_each
    db.deleteDb();
  end
  
  describe '.ensureFullCommit'
    it 'should return ok true'
      db.ensureFullCommit().ok.should.be_true
    end
    
    it 'should return the instance start time'
      db.ensureFullCommit().instance_start_time.should.have_length 16
    end
    
    it 'should post _ensure_full_commit to the db'
      db.should.receive("request", "once").with_args("POST", "/spec_db/_ensure_full_commit")
      db.ensureFullCommit();
    end
  end
  
  describe '.query'
    before_each
      db.save({"Name" : "Cally Tyrol",      "job" : "deckhand", "_id" : "789"});
      db.save({"Name" : "Felix Gaeta",      "job" : "officer",  "_id" : "123"});
      db.save({"Name" : "Samuel T. Anders", "job" : "pilot",    "_id" : "456"});
      map_function    = "function(doc) { emit(doc._id, 1); }";
      reduce_function = "function(key, values, rereduce) { return sum(values); }";
    end
    
    it 'should apply the map function'
      var result = db.query(map_function);
      
      result.rows.should.have_length 3
      result.rows[0].id.should.eql "123"
      result.rows[0].key.should.eql "123"
      result.rows[0].value.should.eql 1
      result.rows[1].id.should.eql "456"
      result.rows[1].key.should.eql "456"
      result.rows[1].value.should.eql 1
      result.rows[2].id.should.eql "789"
      result.rows[2].key.should.eql "789"
      result.rows[2].value.should.eql 1
    end
    
    it 'should apply the reduce function'
      var result = db.query(map_function, reduce_function);
    
      result.rows.should.have_length 1
      result.rows[0].key.should.be_null
      result.rows[0].value.should_eql 3
    end
    
    it 'should pass through the options'
      var result = db.query(map_function, null, {"startkey":"456"});
      
      result.rows.should.have_length 2
      result.rows[0].id.should.eql "456"
      result.rows[0].key.should.eql "456"
      result.rows[0].value.should.eql 1
      result.rows[1].id.should.eql "789"
      result.rows[1].key.should.eql "789"
      result.rows[1].value.should.eql 1
    end
    
    it 'should pass through the keys'
      var result = db.query(map_function, null, {}, ["456", "123"]);

      result.rows.should.have_length 2
      result.rows[0].id.should.eql "456"
      result.rows[0].key.should.eql "456"
      result.rows[0].value.should.eql 1
      result.rows[1].id.should.eql "123"
      result.rows[1].key.should.eql "123"
      result.rows[1].value.should.eql 1
    end
      
    it 'should pass through the options and the keys'
      var result = db.query(map_function, null, {"include_docs":"true"}, ["456"]);
    
      result.rows.should.have_length 1
      result.rows[0].id.should.eql "456"
      result.rows[0].key.should.eql "456"
      result.rows[0].value.should.eql 1
      result.rows[0].doc["job"].should.eql "pilot"
      result.rows[0].doc["_rev"].length.should.be_at_least 30
    end
  
    it 'should apply a view in erlang also'
      // when this test fails, read this: http://wiki.apache.org/couchdb/EnableErlangViews
      var erlang_map = 'fun({Doc}) -> ' +
                       'ID = proplists:get_value(<<"_id">>, Doc, null), ' +
                       'Emit(ID, 1) ' +
                       'end.';
      var result = db.query(erlang_map, null, null, null, "erlang");
  
      result.rows.should.have_length 3
      result.rows[0].id.should.eql "123"
      result.rows[0].key.should.eql "123"
      result.rows[0].value.should.eql 1
      result.rows[1].id.should.eql "456"
      result.rows[1].key.should.eql "456"
      result.rows[1].value.should.eql 1
      result.rows[2].id.should.eql "789"
      result.rows[2].key.should.eql "789"
      result.rows[2].value.should.eql 1
    end
  end
  
  describe '.view'
    before_each
      db.save({"Name" : "Cally Tyrol",      "job" : "deckhand", "_id" : "789"});
      db.save({"Name" : "Felix Gaeta",      "job" : "officer",  "_id" : "123"});
      db.save({"Name" : "Samuel T. Anders", "job" : "pilot",    "_id" : "456"});
      view = {
        "views" : {
          "people" : {
            "map" : "function(doc) { emit(doc._id, doc.Name); }"
          }
        },
        "_id" : "_design/spec_db"
      };
      db.save(view);
    end
    
    it 'should apply the view'
      var result = db.view('spec_db/people');
      
      result.rows.should.have_length 3
      result.rows[0].id.should.eql "123"
      result.rows[0].key.should.eql "123"
      result.rows[0].value.should.eql "Felix Gaeta"
      result.rows[1].id.should.eql "456"
      result.rows[1].key.should.eql "456"
      result.rows[1].value.should.eql "Samuel T. Anders"
      result.rows[2].id.should.eql "789"
      result.rows[2].key.should.eql "789"
      result.rows[2].value.should.eql "Cally Tyrol"
    end
    
    it 'should pass through the options'
      var result = db.view('spec_db/people', {"skip":"2"});
    
      result.rows.should.have_length 1
      result.rows[0].id.should.eql "789"
      result.rows[0].key.should.eql "789"
      result.rows[0].value.should.eql "Cally Tyrol"
    end
    
    it 'should pass through the keys'
      var result = db.view('spec_db/people', {}, ["456", "123"]);

      result.rows.should.have_length 2
      result.rows[0].id.should.eql "456"
      result.rows[0].key.should.eql "456"
      result.rows[0].value.should.eql "Samuel T. Anders"
      result.rows[1].id.should.eql "123"
      result.rows[1].key.should.eql "123"
      result.rows[1].value.should.eql "Felix Gaeta"
    end
    
    it 'should pass through the options and the keys'
      var result = db.view('spec_db/people', {"include_docs":"true"}, ["456"]);
      
      result.rows.should.have_length 1
      result.rows[0].id.should.eql "456"
      result.rows[0].key.should.eql "456"
      result.rows[0].value.should.eql "Samuel T. Anders"
      result.rows[0].doc["job"].should.eql "pilot"
      result.rows[0].doc["_rev"].length.should.be_at_least 30
    end
    
    it 'should return null when the view doesnt exist'
      var result = db.view('spec_db/non_existing_view');
    
      result.should.be_null
    end
  end
  
  describe '.info'
    before_each
      result = db.info();
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
  
  describe '.designInfo'
    before_each
      designDoc = {
        "views" : {
          "people" : {
            "map" : "function(doc) { emit(doc._id, doc); }"
          }
        },
        "_id" : "_design/spec_db"
      };
      db.save(designDoc);
      result = db.designInfo("_design/spec_db");
    end
    
    it 'should return the database name'
      result.name.should.eql "spec_db"
    end
        
    it 'should return a views language'
      result.view_index.language.should.eql "javascript"
    end
  
    it 'should return a views update sequence'
      result.view_index.update_seq.should.eql 0
    end
  
    it 'should return a views signature'
      result.view_index.signature.should.have_length 32
    end
  end
end