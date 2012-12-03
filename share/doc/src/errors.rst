.. Licensed under the Apache License, Version 2.0 (the "License"); you may not
.. use this file except in compliance with the License. You may obtain a copy of
.. the License at
..
..   http://www.apache.org/licenses/LICENSE-2.0
..
.. Unless required by applicable law or agreed to in writing, software
.. distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
.. WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
.. License for the specific language governing permissions and limitations under
.. the License.

Error Messages
==============

The errors reported when CouchDB is unable to read a required file have
been updated so that explicit information about the files and problem
can now be identified from the error message. The errors report file
permission access either when reading or writing to configuration and
database files.

The error is raised both through the log file and the error message
returned through the API call as a JSON error message. For example, when
setting configuration values:

.. code-block:: bash

    shell> curl -X PUT http://couchdb:5984/_config/couchdb/delayed_commits \
                -H 'X-Couch-Persist: true' -d '"false"'
    {"error":"file_permission_error","reason":"/etc/couchdb/local.ini"}

Errors will always be reported using the ``file_permission_error`` error
type.

During startup permissions errors on key files are also reported in the
log with a descriptive error message and file location so that
permissions can be fixed before restart.
