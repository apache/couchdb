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

.. _intro/security:

========
Security
========

In this document, we'll look at the basic security mechanisms in CouchDB:
`Basic Authentication` and `Cookie Authentication`. This is how CouchDB
handles users and protects their credentials.

Authentication
==============

CouchDB has the idea of an *admin user* (e.g. an administrator, a super user,
or root) that is allowed to do anything to a CouchDB installation. By default,
one admin user **must** be created for CouchDB to start up successfully.

CouchDB also defines a set of requests that only admin users are allowed to
do. If you have defined one or more specific admin users, CouchDB will ask for
identification for certain requests:

- Creating a database (:put:`PUT /database </{db}>`)
- Deleting a database (:put:`DELETE /database </{db}>`)
- Setup a database security (:put:`PUT /database/_security
  </{db}/_security>`)
- Creating a design document (:put:`PUT /database/_design/app
  </{db}/_design/{ddoc}>`)
- Updating a design document (:put:`PUT /database/_design/app?rev=1-4E2
  </{db}/_design/{ddoc}>`)
- Deleting a design document (:delete:`DELETE /database/_design/app?rev=2-6A7
  </{db}/_design/{ddoc}>`)
- Triggering compaction (:post:`POST /database/_compact </{db}/_compact>`)
- Reading the task status list (:get:`GET /_active_tasks </_active_tasks>`)
- Restarting the server on a given node
  (:post:`POST /_node/{node-name}/_restart </_restart>`)
- Reading the active configuration
  (:get:`GET /_node/{node-name}/_config </_config>`)
- Updating the active configuration
  (:put:`PUT /_node/{node-name}/_config/section/key </_config/{section}/{key}>`)

Creating a New Admin User
-------------------------

If your installation process did not set up an admin user, you will have to add
one to the configuration file by hand and restart CouchDB first. For the purposes of
this example, we'll create a default ``admin`` user with the password ``password``.

.. warning::
    Don't just type in the following without thinking! Pick a good name for your
    administrator user that isn't easily guessable, and pick a secure password.

To the end of your ``etc/local.ini`` file, after the ``[admins]`` line, add the text
``admin = password``, so it looks like this:

.. code-block:: ini

    [admins]
    admin = password

(Don't worry about the password being in plain text; we'll come back to this.)

Now, restart CouchDB using the method appropriate for your operating system.
You should now be able to access CouchDB using your new administrator account::

    > curl http://admin:password@127.0.0.1:5984/_up
    {"status":"ok","seeds":{}}

Great!

Let's create an admin user through the HTTP API. We'll call her ``anna``, and
her password is ``secret``.  Note the double quotes in the following code; they
are needed to denote a string value for the :ref:`configuration API
<api/config>`::

    > HOST="http://admin:password@127.0.0.1:5984"
    > NODENAME="_local"
    > curl -X PUT $HOST/_node/$NODENAME/_config/admins/anna -d '"secret"'
    ""

As per the :ref:`_config <api/config>` API's behavior, we're getting the previous value
for the config item we just wrote. Since our admin user didn't exist, we get an empty
string.

Please note that ``_local`` serves as an  alias for the local node name, so for all
configuration URLs, ``NODENAME`` may be set to ``_local``, to interact with the local
node’s configuration.

.. seealso::
    :ref:`Node Management <cluster/nodes>`

Hashing Passwords
^^^^^^^^^^^^^^^^^

Seeing the plain-text password is scary, isn't it? No worries, CouchDB doesn't
show the plain-text password anywhere. It gets hashed right away. Go ahead and
look at your ``local.ini`` file now. You'll see that CouchDB has rewritten the
plain text passwords so they are hashed:

.. code-block:: ini

    [admins]
    admin = -pbkdf2-71c01cb429088ac1a1e95f3482202622dc1e53fe,226701bece4ae0fc9a373a5e02bf5d07,10
    anna = -pbkdf2-2d86831c82b440b8887169bd2eebb356821d621b,5e11b9a9228414ab92541beeeacbf125,10

The hash is that big, ugly, long string that starts out with ``-pbkdf2-``.

To compare a plain-text password during authentication with the stored hash,
the hashing algorithm is run and the resulting hash is compared to the stored
hash. The probability of two identical hashes for different passwords is too
insignificant to mention (c.f. `Bruce Schneier`_). Should the stored hash fall
into the hands of an attacker, it is, by current standards, way too inconvenient
(i.e., it'd take a lot of money and time) to find the plain-text password from
the hash.

.. _Bruce Schneier: http://en.wikipedia.org/wiki/Bruce_Schneier

When CouchDB starts up, it reads a set of ``.ini`` files with config settings. It
loads these settings into an internal data store (not a database). The config
API lets you read the current configuration as well as change it and create new
entries. CouchDB writes any changes back to the ``.ini`` files.

The ``.ini`` files can also be edited by hand when CouchDB is not running.
Instead of creating the admin user as we showed previously, you could have
stopped CouchDB, opened your ``local.ini``, added ``anna = secret`` to the
:config:section:`admins`, and restarted CouchDB. Upon reading the new line from
``local.ini``, CouchDB would run the hashing algorithm and write back the hash
to ``local.ini``, replacing the plain-text password — just as it did for our
original ``admin`` user. To make sure CouchDB only hashes plain-text passwords
and not an existing hash a second time, it prefixes the hash with ``-pbkdf2-``,
to distinguish between plain-text passwords and `PBKDF2`_ hashed passwords. This
means your plain-text password can't start with the characters ``-pbkdf2-``,
but that's pretty unlikely to begin with.

.. _PBKDF2: http://en.wikipedia.org/wiki/PBKDF2

.. _intro/security/basicauth:

Basic Authentication
--------------------

CouchDB will not allow us to create new databases unless we give the correct admin user
credentials. Let's verify::

    > HOST="http://127.0.0.1:5984"
    > curl -X PUT $HOST/somedatabase
    {"error":"unauthorized","reason":"You are not a server admin."}

That looks about right. Now we try again with the correct credentials::

    > HOST="http://anna:secret@127.0.0.1:5984"
    > curl -X PUT $HOST/somedatabase
    {"ok":true}

If you have ever accessed a website or FTP server that was password-protected,
the ``username:password@`` URL variant should look familiar.

If you are security conscious, the missing ``s`` in ``http://`` will make you
nervous. We're sending our password to CouchDB in plain text. This is a bad
thing, right? Yes, but consider our scenario: CouchDB listens on ``127.0.0.1``
on a development box that we're the sole user of. Who could possibly sniff our
password?

If you are in a production environment, however, you need to reconsider. Will
your CouchDB instance communicate over a public network? Even a LAN shared
with other collocation customers is public. There are multiple ways to secure
communication between you or your application and CouchDB that exceed the
scope of this documentation. CouchDB as of version :ref:`1.1.0 <release/1.1.0>`
comes with :ref:`SSL built in <config/ssl>`.

.. seealso::
    :ref:`Basic Authentication API Reference <api/auth/basic>`

.. _intro/security/cookie:

Cookie Authentication
---------------------

Basic authentication that uses plain-text passwords is nice and convenient,
but not very secure if no extra measures are taken. It is also a very poor
user experience. If you use basic authentication to identify admins,
your application's users need to deal with an ugly, unstylable browser modal
dialog that says non-professional at work more than anything else.

To remedy some of these concerns, CouchDB supports cookie authentication.
With cookie authentication your application doesn't have to include the ugly
login dialog that the users' browsers come with. You can use a regular HTML
form to submit logins to CouchDB. Upon receipt, CouchDB will generate a
one-time token that the client can use in its next request to CouchDB. When
CouchDB sees the token in a subsequent request, it will authenticate the user
based on the token without the need to see the password again. By default,
a token is valid for 10 minutes.

To obtain the first token and thus authenticate a user for the first time,
the username and password must be sent to the :ref:`_session <api/auth/session>`
API. The API is smart enough to decode HTML form submissions, so you don't have
to resort to any smarts in your application.

If you are not using HTML forms to log in, you need to send an HTTP request
that looks as if an HTML form generated it. Luckily, this is super simple::

    > HOST="http://127.0.0.1:5984"
    > curl -vX POST $HOST/_session \
           -H 'Content-Type:application/x-www-form-urlencoded' \
           -d 'name=anna&password=secret'

CouchDB replies, and we'll give you some more detail::

    < HTTP/1.1 200 OK
    < Set-Cookie: AuthSession=YW5uYTo0QUIzOTdFQjrC4ipN-D-53hw1sJepVzcVxnriEw;
    < Version=1; Path=/; HttpOnly
    > ...
    <
    {"ok":true}

A :statuscode:`200` response code tells us all is well, a :header:`Set-Cookie`
header includes the token we can use for the next request, and the standard JSON
response tells us again that the request was successful.

Now we can use this token to make another request as the same user without
sending the username and password again::

    > curl -vX PUT $HOST/mydatabase \
           --cookie AuthSession=YW5uYTo0QUIzOTdFQjrC4ipN-D-53hw1sJepVzcVxnriEw \
           -H "X-CouchDB-WWW-Authenticate: Cookie" \
           -H "Content-Type:application/x-www-form-urlencoded"
    {"ok":true}

You can keep using this token for 10 minutes by default. After 10 minutes you
need to authenticate your user again. The token lifetime can be configured
with the timeout (in seconds) setting in the :ref:`chttpd_auth
<config/chttpd_auth>` configuration section.

.. seealso::
    :ref:`Cookie Authentication API Reference <api/auth/cookie>`

Authentication Database
=======================

You may already note that CouchDB administrators are defined within the config
file and are wondering if regular users are also stored there. No, they are not.
CouchDB has a special `authentication database`, named ``_users`` by default,
that stores all registered users as JSON documents.

This special database is a `system database`. This means that while it shares
the common :ref:`database API <api/database>`, there are some
special security-related constraints applied. Below is a list of how the
`authentication database` is different from the other databases.

- Only administrators may browse list of all documents
  (:get:`GET /_users/_all_docs </{db}/_all_docs>`)
- Only administrators may listen to :ref:`changes feed
  <changes>` (:get:`GET /_users/_changes </{db}/_changes>`)
- Only administrators may execute design functions like :ref:`views <viewfun>`.
- There is a special design document ``_auth`` that cannot be modified
- Every document except the `design documents` represent registered
  CouchDB users and belong to them
- By default, the ``_security`` settings of the ``_users`` database disallow
  users from accessing or modifying documents

.. note::

    Settings can be changed so that users do have access to the ``_users`` database,
    but even then they may only access (:get:`GET /_users/org.couchdb.user:Jan
    </{db}/{docid}>`) or modify (:put:`PUT /_users/org.couchdb.user:Jan
    </{db}/{docid}>`) documents that they own. This will not be possible in CouchDB 4.0.

These draconian rules are necessary since CouchDB cares about its users'
personal information and will not disclose it to just anyone. Often, user
documents contain system information like `login`, `password hash` and `roles`,
apart from sensitive personal information like real name, email, phone, special
internal identifications and more. This is not information that you
want to share with the World.

Users Documents
---------------

Each CouchDB user is stored in document format. These documents contain
several *mandatory* fields, that CouchDB needs for authentication:

- **_id** (*string*): Document ID. Contains user's login with special prefix
  :ref:`org.couchdb.user`
- **derived_key** (*string*): `PBKDF2`_ key derived from salt/iterations.
- **name** (*string*): User's name aka login. **Immutable** e.g. you cannot
  rename an existing user - you have to create new one
- **roles** (*array* of *string*): List of user roles. CouchDB doesn't provide
  any built-in roles, so you're free to define your own depending on your needs.
  However, you cannot set system roles like ``_admin`` there. Also, only
  administrators may assign roles to users - by default all users have no roles
- **password** (*string*): A plaintext password can be provided, but will be replaced
  by hashed fields before the document is actually stored.
- **password_sha** (*string*): Hashed password with salt. Used for ``simple``
  `password_scheme`
- **password_scheme** (*string*): Password hashing scheme. May be ``simple`` or
  ``pbkdf2``
- **salt** (*string*): Hash salt. Used for both ``simple`` and ``pbkdf2``
  ``password_scheme`` options.
- **iterations** (*integer*): Number of iterations to derive key, used for ``pbkdf2``
  ``password_scheme`` See the :ref:`configuration API <config/chttpd_auth>`:: for details.
- **type** (*string*): Document type. Constantly has the value ``user``

Additionally, you may specify any custom fields that relate to the target
user.

.. _org.couchdb.user:

Why the ``org.couchdb.user:`` prefix?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The reason there is a special prefix before a user's login name is to have
namespaces that users belong to. This prefix is designed to prevent
replication conflicts when you try merging two or more `_user` databases.

For current CouchDB releases, all users belong to the same
``org.couchdb.user`` namespace and this cannot be changed. This may be changed
in future releases.

Creating a New User
-------------------

Creating a new user is a very trivial operation. You just need to do a
:method:`PUT` request with the user's data to CouchDB. Let's create a user with
login `jan` and password `apple`::

    curl -X PUT http://localhost:5984/_users/org.couchdb.user:jan \
         -H "Accept: application/json" \
         -H "Content-Type: application/json" \
         -d '{"name": "jan", "password": "apple", "roles": [], "type": "user"}'

This `curl` command will produce the following HTTP request:

.. code-block:: http

    PUT /_users/org.couchdb.user:jan HTTP/1.1
    Accept: application/json
    Content-Length: 62
    Content-Type: application/json
    Host: localhost:5984
    User-Agent: curl/7.31.0

And CouchDB responds with:

.. code-block:: http

    HTTP/1.1 201 Created
    Cache-Control: must-revalidate
    Content-Length: 83
    Content-Type: application/json
    Date: Fri, 27 Sep 2013 07:33:28 GMT
    ETag: "1-e0ebfb84005b920488fc7a8cc5470cc0"
    Location: http://localhost:5984/_users/org.couchdb.user:jan
    Server: CouchDB (Erlang OTP)

    {"ok":true,"id":"org.couchdb.user:jan","rev":"1-e0ebfb84005b920488fc7a8cc5470cc0"}

The document was successfully created! The user `jan` should now exist in our
database. Let's check if this is true::

    curl -X POST http://localhost:5984/_session -d 'name=jan&password=apple'

CouchDB should respond with:

.. code-block:: javascript

    {"ok":true,"name":"jan","roles":[]}

This means that the username was recognized and the password's hash matches
with the stored one. If we specify an incorrect login and/or password, CouchDB
will notify us with the following error message:

.. code-block:: javascript

    {"error":"unauthorized","reason":"Name or password is incorrect."}

Password Changing
-----------------

Let's define what is password changing from the point of view of CouchDB and
the authentication database. Since "users" are "documents", this operation is
just updating the document with a special field ``password`` which contains
the *plain text password*. Scared? No need to be. The authentication database
has a special internal hook on document update which looks for this field and
replaces it with the *secured hash* depending on the chosen ``password_scheme``.

Summarizing the above process - we need to get the document's content, add
the ``password`` field with the new password in plain text and then store the
JSON result to the authentication database. ::

    curl -X GET http://localhost:5984/_users/org.couchdb.user:jan

.. code-block:: javascript

    {
        "_id": "org.couchdb.user:jan",
        "_rev": "1-e0ebfb84005b920488fc7a8cc5470cc0",
        "derived_key": "e579375db0e0c6a6fc79cd9e36a36859f71575c3",
        "iterations": 10,
        "name": "jan",
        "password_scheme": "pbkdf2",
        "roles": [],
        "salt": "1112283cf988a34f124200a050d308a1",
        "type": "user"
    }

Here is our user's document. We may strip hashes from the stored document to
reduce the amount of posted data::

    curl -X PUT http://localhost:5984/_users/org.couchdb.user:jan \
         -H "Accept: application/json" \
         -H "Content-Type: application/json" \
         -H "If-Match: 1-e0ebfb84005b920488fc7a8cc5470cc0" \
         -d '{"name":"jan", "roles":[], "type":"user", "password":"orange"}'

.. code-block:: javascript

    {"ok":true,"id":"org.couchdb.user:jan","rev":"2-ed293d3a0ae09f0c624f10538ef33c6f"}

Updated! Now let's check that the password was really changed::

    curl -X POST http://localhost:5984/_session -d 'name=jan&password=apple'

CouchDB should respond with:

.. code-block:: javascript

    {"error":"unauthorized","reason":"Name or password is incorrect."}

Looks like the password ``apple`` is wrong, what about ``orange``? ::

    curl -X POST http://localhost:5984/_session -d 'name=jan&password=orange'

CouchDB should respond with:

.. code-block:: javascript

    {"ok":true,"name":"jan","roles":[]}

Hooray! You may wonder why this was so complex - we need to retrieve user's
document, add a special field to it, and post it back.

.. note::
    There is no password confirmation for API request: you should implement it
    in your application layer.

Authorization
=============

Now that you have a few users who can log in, you probably want to set up some
restrictions on what actions they can perform based on their identity and their
roles.  Each database on a CouchDB server can contain its own set of
authorization rules that specify which users are allowed to read and write
documents, create design documents, and change certain database configuration
parameters.  The authorization rules are set up by a server admin and can be
modified at any time.

Database authorization rules assign a user into one of two classes:

- `members`, who are allowed to read all documents and create and modify any
  document except for design documents.
- `admins`, who can read and write all types of documents, modify which users
  are members or admins, and set certain per-database configuration options.

Note that a database admin is not the same as a server admin -- the actions
of a database admin are restricted to a specific database.

When a database is first created, there are no members or admins.  HTTP
requests that have no authentication credentials or have credentials for a
normal user are treated as members, and those with server admin credentials
are treated as database admins.  To change the default permissions, you must
create a :ref:`_security <api/db/security>` document in the database::

    > curl -X PUT http://localhost:5984/mydatabase/_security \
         -u anna:secret \
         -H "Content-Type: application/json" \
         -d '{"admins": { "names": [], "roles": [] }, "members": { "names": ["jan"], "roles": [] } }'

The HTTP request to create the `_security` document must contain the
credentials of a server admin.  CouchDB will respond with:

.. code-block:: javascript

    {"ok":true}

The database is now secured against anonymous reads and writes::

    > curl http://localhost:5984/mydatabase/

.. code-block:: javascript

    {"error":"unauthorized","reason":"You are not authorized to access this db."}

You declared user "jan" as a member in this database, so he is able to read and
write normal documents::

    > curl -u jan:apple http://localhost:5984/mydatabase/

.. code-block:: javascript

    {"db_name":"mydatabase","doc_count":1,"doc_del_count":0,"update_seq":3,"purge_seq":0,
    "compact_running":false,"sizes":{"active":272,"disk":12376,"external":350},
    "instance_start_time":"0","disk_format_version":6,"committed_update_seq":3}

If Jan attempted to create a design doc, however, CouchDB would return a
401 Unauthorized error because the username "jan" is not in the list of
admin names and the `/_users/org.couchdb.user:jan` document doesn't contain
a role that matches any of the declared admin roles.  If you want to promote
Jan to an admin, you can update the security document to add `"jan"` to
the `names` array under `admin`.  Keeping track of individual database
admin usernames is tedious, though, so you would likely prefer to create a
database admin role and assign that role to the `org.couchdb.user:jan` user
document::

    > curl -X PUT http://localhost:5984/mydatabase/_security \
         -u anna:secret \
         -H "Content-Type: application/json" \
         -d '{"admins": { "names": [], "roles": ["mydatabase_admin"] }, "members": { "names": [], "roles": [] } }'

See the :ref:`_security document reference page <api/db/security>` for
additional details about specifying database members and admins.
