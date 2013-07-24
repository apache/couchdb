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


.. _api/server:

==============
Server Methods
==============

The CouchDB server interface provides the basic interface to a
CouchDB server for obtaining CouchDB information and getting and setting
configuration information.

A list of the available methods and URL paths are provided below:

+--------+-------------------------+-------------------------------------------+
| Method | Path                    | Description                               |
+========+=========================+===========================================+
| GET    | /                       |  Get the welcome message and version      |
|        |                         |  information                              |
+--------+-------------------------+-------------------------------------------+
| GET    | /_active_tasks          |  Obtain a list of the tasks running in the|
|        |                         |  server                                   |
+--------+-------------------------+-------------------------------------------+
| GET    | /_all_dbs               |  Get a list of all the DBs                |
+--------+-------------------------+-------------------------------------------+
| GET    | /_config                |  Obtain a list of the entire server       |
|        |                         |  configuration                            |
+--------+-------------------------+-------------------------------------------+
| GET    | /_config/section        |  Get all the configuration values for the |
|        |                         |  specified section                        |
+--------+-------------------------+-------------------------------------------+
| GET    | /_config/section/key    |  Get a specific section/config value      |
+--------+-------------------------+-------------------------------------------+
| PUT    | /_config/section/key    |  Set the specified configuration value    |
+--------+-------------------------+-------------------------------------------+
| DELETE | /_config/section/key    |  Delete the current setting               |
+--------+-------------------------+-------------------------------------------+
| GET    | /_log                   |  Return the server log file               |
+--------+-------------------------+-------------------------------------------+
| POST   | /_replicate             |  Set or cancel replication                |
+--------+-------------------------+-------------------------------------------+
| POST   | /_restart               |  Restart the server                       |
+--------+-------------------------+-------------------------------------------+
| GET    | /_session               |  Returns cookie based login user          |
|        |                         |  information                              |
+--------+-------------------------+-------------------------------------------+
| POST   | /_session               |  Do cookie based user login               |
+--------+-------------------------+-------------------------------------------+
| DELETE | /_session               |  Logout cookie based user                 |
+--------+-------------------------+-------------------------------------------+
| GET    | /_stats                 |  Return server statistics                 |
+--------+-------------------------+-------------------------------------------+
| GET    | /_utils                 |  CouchDB administration interface (Futon) |
+--------+-------------------------+-------------------------------------------+
| GET    | /_uuids                 |  Get generated UUIDs from the server      |
+--------+-------------------------+-------------------------------------------+
| GET    | /favicon.ico            |  Get the site icon                        |
+--------+-------------------------+-------------------------------------------+

.. toctree::

   common
   authn
   configuration
