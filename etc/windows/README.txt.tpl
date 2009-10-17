This is the README for the %package_name% binary distribution for 
Windows, version %version%.

* Although CouchDB defaults to installing into your "Program Files" directory,
  the permissions on the 'var' and 'etc' sub-directories have been adjusted 
  to allow modification by any authorized user so the couchdb databases, logs 
  and .ini files can be written.  You may like to further restrict these 
  permissions to only the user who will be running couchdb.

* To start couchdb execute couchdb.bat in the 'bin' directory.  A shortcut
  to this batch file should have been installed.  There is currently no 
  support for Windows Services etc, but you are encouraged to look at the
  various tools which allow arbitrary programs to be run as services.

* The Futon application which comes with CouchDB does not work with
  Internet Explorer - Mozilla Firefox is generally recommended.

* The test suite is known to fail on Windows due to what appear to be
  permissions errors; this is due to couch being unable to delete a 
  file while it is in use on Windows.
  See also https://issues.apache.org/jira/browse/COUCHDB-326

* Additional help with the Windows support is needed - please contact the
  couchdb-dev list if you can help.
