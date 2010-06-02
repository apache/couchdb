function stubAlert(){
  if(typeof(old_alert) == 'undefined'){
    old_alert = alert;
  }
  alert = function(msg){
    alert_msg = msg;
  };
}

function destubAlert(){
  alert = old_alert;
}

function errorCallback(status, error, reason){
  console.log("Unexpected " + status + " error: " + error + " - " + reason)
  throw("Unexpected " + status + " error: " + error + " - " + reason);
}

function successCallback(resp){
  console.log("No error message here unexpectedly, successful response instead.")
  throw("No error message here unexpectedly, successful response instead.");
}

function useTestUserDb(){
  users_db = new CouchDB("spec_users_db");
  var xhr = CouchDB.request("PUT", "/_config/couch_httpd_auth/authentication_db", {
    body: JSON.stringify("spec_users_db")
  });
  if(typeof(old_auth_db) == 'undefined'){
    old_auth_db = xhr.responseText.replace(/\n/,'').replace(/"/g,'');
  }
}

function useOldUserDb(){
  CouchDB.request("PUT", "/_config/couch_httpd_auth/authentication_db", {
    body: JSON.stringify(old_auth_db)
  });
  users_db.deleteDb();
}