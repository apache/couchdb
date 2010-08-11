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

// Specs for couch.js lines 313-470

describe 'CouchDB class'
  describe 'session stuff'
    before
      useTestUserDb();
    end
  
    after
      useOldUserDb();
    end
    
    before_each
      userDoc = users_db.save(CouchDB.prepareUserDoc({name: "Gaius Baltar", roles: ["president"]}, "secretpass"));
    end
  
    after_each
      users_db.deleteDoc({_id : userDoc.id, _rev : userDoc.rev})
    end
    
    describe '.login'
      it 'should return ok true'
        CouchDB.login("Gaius Baltar", "secretpass").ok.should.be_true
      end
          
      it 'should return the name of the logged in user'
        CouchDB.login("Gaius Baltar", "secretpass").name.should.eql "Gaius Baltar"
      end
          
      it 'should return the roles of the logged in user'
        CouchDB.login("Gaius Baltar", "secretpass").roles.should.eql ["president"]
      end
      
      it 'should post _session'
        CouchDB.should.receive("request", "once").with_args("POST", "/_session")
        CouchDB.login("Gaius Baltar", "secretpass");
      end
      
      it 'should create a session'
        CouchDB.login("Gaius Baltar", "secretpass");
        CouchDB.session().userCtx.name.should.eql "Gaius Baltar"
      end
    end
      
    describe '.logout'
      before_each
        CouchDB.login("Gaius Baltar", "secretpass");
      end
    
      it 'should return ok true'
        CouchDB.logout().ok.should.be_true
      end
    
      it 'should delete _session'
        CouchDB.should.receive("request", "once").with_args("DELETE", "/_session")
        CouchDB.logout();
      end
      
      it 'should result in an invalid session'
        CouchDB.logout();
        CouchDB.session().name.should.be_null
      end
    end
  
    describe '.session'
      before_each
        CouchDB.login("Gaius Baltar", "secretpass");
      end
    
      it 'should return ok true'
        CouchDB.session().ok.should.be_true
      end
    
      it 'should return the users name'
        CouchDB.session().userCtx.name.should.eql "Gaius Baltar"
      end
    
      it 'should return the users roles'
        CouchDB.session().userCtx.roles.should.eql ["president"]
      end
    
      it 'should return the name of the authentication db'
        CouchDB.session().info.authentication_db.should.eql "spec_users_db"
      end
    
      it 'should return the active authentication handler'
        CouchDB.session().info.authenticated.should.eql "cookie"
      end
    end
  end
  
  describe 'db stuff'
    before_each
      db = new CouchDB("spec_db", {"X-Couch-Full-Commit":"false"});
      db.createDb();
    end
  
    after_each
      db.deleteDb();
    end
  
    describe '.prepareUserDoc'
      before_each
        userDoc = CouchDB.prepareUserDoc({name: "Laura Roslin"}, "secretpass");
      end
      
      it 'should return the users name'
        userDoc.name.should.eql "Laura Roslin"
      end
      
      it 'should prefix the id with the CouchDB user_prefix'
        userDoc._id.should.eql "org.couchdb.user:Laura Roslin"
      end
      
      it 'should return the users roles'
        var userDocWithRoles = CouchDB.prepareUserDoc({name: "William Adama", roles: ["admiral", "commander"]}, "secretpass")
        userDocWithRoles.roles.should.eql ["admiral", "commander"]
      end
      
      it 'should return the hashed password'
        userDoc.password_sha.length.should.be_at_least 30
        userDoc.password_sha.should.be_a String
      end
    end
      
    describe '.allDbs'
      it 'should get _all_dbs'
        CouchDB.should.receive("request", "once").with_args("GET", "/_all_dbs");
        CouchDB.allDbs();
      end
      
      it 'should return an array that includes a created database'
        temp_db = new CouchDB("temp_spec_db", {"X-Couch-Full-Commit":"false"});
        temp_db.createDb();
        CouchDB.allDbs().should.include("temp_spec_db");
        temp_db.deleteDb();
      end
      
      it 'should return an array that does not include a database that does not exist'
        CouchDB.allDbs().should.not.include("not_existing_temp_spec_db");
      end
    end
    
    describe '.allDesignDocs'
      it 'should return the total number of documents'
        CouchDB.allDesignDocs().spec_db.total_rows.should.eql 0
        db.save({'type':'battlestar', 'name':'galactica'});
        CouchDB.allDesignDocs().spec_db.total_rows.should.eql 1
      end
      
      it 'should return undefined when the db does not exist'
        CouchDB.allDesignDocs().non_existing_db.should.be_undefined
      end
      
      it 'should return no documents when there are no design documents'
        CouchDB.allDesignDocs().spec_db.rows.should.eql []
      end
      
      it 'should return all design documents'
        var designDoc = {
          "views" : {
            "people" : {
              "map" : "function(doc) { emit(doc._id, doc); }"
            }
          },
          "_id" : "_design/spec_db"
        };
        db.save(designDoc);
        
        var allDesignDocs = CouchDB.allDesignDocs();
        allDesignDocs.spec_db.rows[0].id.should.eql "_design/spec_db"
        allDesignDocs.spec_db.rows[0].key.should.eql "_design/spec_db"
        allDesignDocs.spec_db.rows[0].value.rev.length.should.be_at_least 30
      end
    end
    
    describe '.getVersion'
      it 'should get the CouchDB version'
        CouchDB.should.receive("request", "once").with_args("GET", "/")
        CouchDB.getVersion();
      end
      
      it 'should return the CouchDB version'
        CouchDB.getVersion().should_match /^\d\d?\.\d\d?\.\d\d?.*/
      end
    end
    
    describe '.replicate'
      before_each
        db2 = new CouchDB("spec_db_2", {"X-Couch-Full-Commit":"false"});
        db2.createDb();
        host = window.location.protocol + "//" + window.location.host ;
      end
      
      after_each
        db2.deleteDb();
      end
      
      it 'should return no_changes true when there are no changes between the dbs'
        CouchDB.replicate(host + db.uri, host + db2.uri).no_changes.should.be_true
      end
      
      it 'should return the session ID'
        db.save({'type':'battlestar', 'name':'galactica'});
        CouchDB.replicate(host + db.uri, host + db2.uri).session_id.length.should.be_at_least 30
      end
      
      it 'should return source_last_seq'
        db.save({'type':'battlestar', 'name':'galactica'});
        db.save({'type':'battlestar', 'name':'pegasus'});
        
        CouchDB.replicate(host + db.uri, host + db2.uri).source_last_seq.should.eql 2
      end
      
      it 'should return the replication history'
        db.save({'type':'battlestar', 'name':'galactica'});
        db.save({'type':'battlestar', 'name':'pegasus'});
        
        var result = CouchDB.replicate(host + db.uri, host + db2.uri);
        result.history[0].docs_written.should.eql 2
        result.history[0].start_last_seq.should.eql 0
      end
      
      it 'should pass through replication options'
        db.save({'type':'battlestar', 'name':'galactica'});
        db2.deleteDb();
        -{CouchDB.replicate(host + db.uri, host + db2.uri)}.should.throw_error
        var result = CouchDB.replicate(host + db.uri, host + db2.uri, {"body" : {"create_target":true}});
    
        result.ok.should.eql true
        result.history[0].docs_written.should.eql 1
        db2.info().db_name.should.eql "spec_db_2"
      end
    end
    
    describe '.newXhr'
      it 'should return a XMLHTTPRequest'
        CouchDB.newXhr().should.have_prop 'readyState'
        CouchDB.newXhr().should.have_prop 'responseText'
        CouchDB.newXhr().should.have_prop 'status'
      end
    end
    
    describe '.request'
      it 'should return a XMLHttpRequest'
        var req = CouchDB.request("GET", '/');
        req.should.include "readyState"
        req.should.include "responseText"
        req.should.include "statusText"
      end
      
      it 'should pass through the options headers'
        var xhr = CouchDB.newXhr();
        stub(CouchDB, 'newXhr').and_return(xhr);
        
        xhr.should.receive("setRequestHeader", "once").with_args("X-Couch-Full-Commit", "true")
        CouchDB.request("GET", "/", {'headers': {"X-Couch-Full-Commit":"true"}});
      end
      
      it 'should pass through the options body'
        var xhr = CouchDB.newXhr();
        stub(CouchDB, 'newXhr').and_return(xhr);
       
        xhr.should.receive("send", "once").with_args({"body_key":"body_value"})
        CouchDB.request("GET", "/", {'body': {"body_key":"body_value"}});
      end
      
      it 'should prepend the urlPrefix to the uri'
        var oldPrefix = CouchDB.urlPrefix;
        CouchDB.urlPrefix = "/_utils";
       
        var xhr = CouchDB.newXhr();
        stub(CouchDB, 'newXhr').and_return(xhr);
        
        xhr.should.receive("open", "once").with_args("GET", "/_utils/", false)
        CouchDB.request("GET", "/", {'headers': {"X-Couch-Full-Commit":"true"}});
        
        CouchDB.urlPrefix = oldPrefix;
      end
    end
    
    describe '.requestStats'
      it 'should get the stats for specified module and key'
        var stats = CouchDB.requestStats('couchdb', 'open_databases', null);
        stats.description.should.eql 'number of open databases'
        stats.current.should.be_a Number
      end
      
      it 'should add flush true to the request when there is a test argument'
        CouchDB.should.receive("request", "once").with_args("GET", "/_stats/httpd/requests?flush=true")
        CouchDB.requestStats('httpd', 'requests', 'test');
      end
      
      it 'should still work when there is a test argument'
        var stats = CouchDB.requestStats('httpd_status_codes', '200', 'test');
        stats.description.should.eql 'number of HTTP 200 OK responses'
        stats.sum.should.be_a Number
      end
    end
    
    describe '.newUuids'
      after_each
        CouchDB.uuids_cache = [];
      end
      
      it 'should return the specified amount of uuids'
        var uuids = CouchDB.newUuids(45);
        uuids.should.have_length 45
      end
          
      it 'should return an array with uuids'
        var uuids = CouchDB.newUuids(1);
        uuids[0].should.be_a String
        uuids[0].should.have_length 32
      end
      
      it 'should leave the uuids_cache with 100 uuids when theres no buffer size specified'
        CouchDB.newUuids(23);
        CouchDB.uuids_cache.should.have_length 100
      end
      
      it 'should leave the uuids_cache with the specified buffer size'
        CouchDB.newUuids(23, 150);
        CouchDB.uuids_cache.should.have_length 150
      end
      
      it 'should get the uuids from the uuids_cache when there are enough uuids in there'
        CouchDB.newUuids(10);
        CouchDB.newUuids(25);
        CouchDB.uuids_cache.should.have_length 75
      end
      
      it 'should create new uuids and add as many as specified to the uuids_cache when there are not enough uuids in the cache'
        CouchDB.newUuids(10);
        CouchDB.newUuids(125, 60);
        CouchDB.uuids_cache.should.have_length 160
      end
    end
    
    describe '.maybeThrowError'
      it 'should throw an error when the request has status 404'
        var req = CouchDB.request("GET", "/nonexisting_db");
        -{CouchDB.maybeThrowError(req)}.should.throw_error
      end
    
      it 'should throw an error when the request has status 412'
        var req = CouchDB.request("PUT", "/spec_db");
        -{CouchDB.maybeThrowError(req)}.should.throw_error
      end
    
      it 'should throw an error when the request has status 405'
        var req = CouchDB.request("DELETE", "/_utils");
        -{CouchDB.maybeThrowError(req)}.should.throw_error
      end
    
      it 'should throw the responseText of the request'
        var req = CouchDB.request("GET", "/nonexisting_db");
        try {
          CouchDB.maybeThrowError(req)
        } catch(e) {
          e.error.should.eql JSON.parse(req.responseText).error
          e.reason.should.eql JSON.parse(req.responseText).reason
        }
      end
    
      it 'should throw an unknown error when the responseText is invalid json'
        mock_request().and_return("invalid json...", "application/json", 404, {})
        try {
          CouchDB.maybeThrowError(CouchDB.newXhr())
        } catch(e) {
          e.error.should.eql "unknown"
          e.reason.should.eql "invalid json..."
        }
      end
    end
    
    describe '.params'
      it 'should turn a json object into a http params string'
        var params = CouchDB.params({"president":"laura", "cag":"lee"})
        params.should.eql "president=laura&cag=lee"
      end
    
      it 'should return a blank string when the object is empty'
        var params = CouchDB.params({})
        params.should.eql ""
      end
    end
  end
end