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

.. default-domain:: config
.. highlight:: ini

=================
Database Per User
=================

.. _config/couch_peruser:

Database Per User Options
=========================

.. config:section:: couch_peruser :: Database Per User Options

    .. config:option:: enable :: Enable private per-user database pattern

    If set to ``true``, couch_peruser ensures that a private per-user
    database exists for each document in ``_users``. These databases are
    writable only by the corresponding user. Database names are in the following
    form: ``userdb-{UTF-8 hex encoded username}``. ::

        [couch_peruser]
        enable = false

    .. note::
        The ``_users`` database must exist before couch_peruser can be enabled.

    .. tip::
        Under NodeJS, user names can be converted to and from database names thusly:

    .. code:: javascript

        function dbNameToUsername(prefixedHexName) {
          return Buffer.from(prefixedHexName.replace('userdb-', ''), 'hex').toString('utf8');
        }

        function usernameToDbName(name) {
          return 'userdb-' + Buffer.from(name).toString('hex');
        }

    .. config:option:: delete_dbs :: Enable deleting user-db after user delete

    If set to ``true`` and a user is deleted, the respective database gets
    deleted as well. ::

        [couch_peruser]
        delete_dbs = false

    Note: When using JWT authorization, the provided token must include a custom
    ``_couchdb.roles=['_admin']`` claim to for the peruser database to be properly
    created and accessible for the user provided in the ``sub=`` claim.

    .. config:option:: q :: Sharding value for per-user databases

    If set, specify the sharding value for per-user databases. If unset, the
    cluster default value will be used.

        [couch_peruser]
        q = 1
