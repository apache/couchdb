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

// Specs for jquery_couch.js lines 163-209

describe 'jQuery couchdb db'
  before
    stubAlert();
  end
  
  after
    destubAlert();
  end
  
  before_each
    db = $.couch.db('spec_db');
  end

  describe 'constructor'
    it 'should set the name'
      db.name.should.eql 'spec_db'
    end
    
    it 'should set the uri'
      db.uri.should.eql '/spec_db/'
    end
  end
  
  describe 'triggering db functions'
    before_each
      db.create();
    end

    after_each
      db.drop();
    end
    
    describe 'compact'
      it 'should return ok true'
        db.compact({
          success: function(resp) {
            resp.ok.should.be_true
          },
          error: function(status, error, reason){errorCallback(status, error, reason)}
        });
      end
    
      it 'should trigger _compact'
        db.compact({
          success: function(resp, obj) {
            obj.url.should.eql "/spec_db/_compact"
          },
          error: function(status, error, reason){errorCallback(status, error, reason)}
        });
      end
    end
    
    describe 'viewCleanup'
       it 'should return ok true'
         db.viewCleanup({
           success: function(resp) {
             resp.ok.should.be_true
           },
           error: function(status, error, reason){errorCallback(status, error, reason)}
         });
       end
   
       it 'should trigger _view_cleanup'
         db.viewCleanup({
           success: function(resp, obj) {
             obj.url.should.eql "/spec_db/_view_cleanup"
           },
           error: function(status, error, reason){errorCallback(status, error, reason)}
         });
       end
     end
   
    describe 'compactView'
      before_each
        var designDoc = {
          "views" : {
            "people" : {
              "map" : "function(doc) { emit(doc._id, doc); }"
            }
          },
          "_id" : "_design/myview"
        };
        db.saveDoc(designDoc);
        db.saveDoc({"Name" : "Felix Gaeta", "_id" : "123"});
      end
      
      it 'should return ok true'
        db.compactView("myview", {
          success: function(resp) {
            resp.ok.should.be_true
          },
          error: function(status, error, reason){errorCallback(status, error, reason)}
        });
      end
  
      it 'should trigger _compact_view with the groupname'
        db.compactView("myview", {
          success: function(resp, obj) {
            obj.url.should.eql "/spec_db/_compact/myview"
          },
          error: function(status, error, reason){errorCallback(status, error, reason)}
        });
      end
      
      it 'should return raise a 404 error when the design name doesnt exist'
        db.compactView("non_existing_design_name", {
          error: function(status, error, reason){
            status.should.eql 404
            error.should.eql "not_found"
            reason.should.eql "missing"
          },
          success: function(resp){successCallback(resp)}
        });
      end
      
      it 'should alert with an error message prefix'
        db.compactView("asdf");
        alert_msg.should.match /The view could not be compacted/
      end
    end
  end
   
  describe 'create'
    after_each
      db.drop();
    end
  
    it 'should return ok true'
      db.create({
        success: function(resp) {
          resp.ok.should.be_true
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should result in a created db'
      db.create();
      db.create({
        error: function(status, error, reason){
          status.should.eql 412
          error.should.eql "file_exists"
          reason.should.eql "The database could not be created, the file already exists."
        },
        success: function(resp){successCallback(resp)}
      });
    end
    
    it 'should alert with an error message prefix'
      db.create();
      db.create();
      alert_msg.should.match /The database could not be created/
    end
  end
  
  describe 'drop'
    before_each
      db.create();
    end
    
    it 'should return ok true'
      db.drop({
        success: function(resp) {
          resp.ok.should.be_true
        },
        error: function(status, error, reason){errorCallback(status, error, reason)}
      });
    end
    
    it 'should result in a deleted db'
      db.drop();
      db.drop({
        error: function(status, error, reason){
          status.should.eql 404
          error.should.eql "not_found"
          reason.should.eql "missing"
        },
        success: function(resp){successCallback(resp)}
      });
    end
    
    it 'should alert with an error message prefix'
      db.drop();
      db.drop();
      alert_msg.should.match /The database could not be deleted/
    end
  end
end