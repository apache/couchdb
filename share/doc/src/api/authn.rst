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

.. _api/auth:

======================
Authentication Methods
======================

.. todo:: Authentication Methods

The CouchDB Authentication methods provide an interface for obtaining
session and authorization data.

A list of the available methods and URL paths are provided below:

+--------+-------------------------+-------------------------------------------+
| Method | Path                    | Description                               |
+========+=========================+===========================================+
| GET    | /_oauth/access_token    | TBC                                       |
+--------+-------------------------+-------------------------------------------+
| GET    | /_oauth/authorize       | TBC                                       |
+--------+-------------------------+-------------------------------------------+
| POST   | /_oauth/authorize       | TBC                                       |
+--------+-------------------------+-------------------------------------------+
| GET    | /_oauth/request_token   | TBC                                       |
+--------+-------------------------+-------------------------------------------+
| GET    | /_session               | Returns cookie based login user           |
|        |                         | information                               |
+--------+-------------------------+-------------------------------------------+
| POST   | /_session               | Do cookie based user login                |
+--------+-------------------------+-------------------------------------------+
| DELETE | /_session               | Logout cookie based user                  |
+--------+-------------------------+-------------------------------------------+
