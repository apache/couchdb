This is the README for the %package_name% binary distribution for 
Windows, version %version%.

* Although CouchDB defaults to installing into your "Program Files" directory,
  the permissions on the 'var' and 'etc' sub-directories have been adjusted 
  to allow modification by any authorized user so the couchdb databases, logs 
  and .ini files can be written.  You may like to further restrict these 
  permissions to only the user who will be running couchdb.

* The installer offers to install CouchDB as a Windows Service.  If you select
  this option, the service will run as the "LocalSystem" user and be 
  configured to start automatically.  You can configure the service properties
  via the Windows 'Services' applet.

* To start CouchDB in a "console" environment, execute couchdb.bat in the 
  'bin' directory.  A shortcut to this batch file should have been installed.
  Don't try and start couchdb this way if the service is running - it will 
  fail.

* The Futon application which comes with CouchDB does not work with
  Internet Explorer - Mozilla Firefox is generally recommended.

* The test suite is known to fail on Windows due to what appear to be
  permissions errors; this is due to couch being unable to delete a 
  file while it is in use on Windows.
  See also https://issues.apache.org/jira/browse/COUCHDB-326

* Additional help with the Windows support is needed - please contact the
  couchdb-dev list if you can help.
