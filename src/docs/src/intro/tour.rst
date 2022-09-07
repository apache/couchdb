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

.. _intro/tour:

===============
Getting Started
===============

In this document, we'll take a quick tour of CouchDB's features.
We'll create our first document and experiment with CouchDB views.

All Systems Are Go!
===================

We'll have a very quick look at CouchDB's bare-bones Application Programming
Interface (API) by using the command-line utility curl. Please note that this
is not the only way of talking to CouchDB. We will show you plenty more
throughout the rest of the documents. What's interesting about curl is that it
gives you control over raw HTTP requests, and you can see exactly what is
going on "underneath the hood" of your database.

Make sure CouchDB is still running, and then do::

    curl http://127.0.0.1:5984/

This issues a GET request to your newly installed CouchDB instance.

The reply should look something like:

.. code-block:: javascript

    {
      "couchdb": "Welcome",
      "version": "3.0.0",
      "git_sha": "83bdcf693",
      "uuid": "56f16e7c93ff4a2dc20eb6acc7000b71",
      "features": [
        "access-ready",
        "partitioned",
        "pluggable-storage-engines",
        "reshard",
        "scheduler"
      ],
      "vendor": {
        "name": "The Apache Software Foundation"
      }
    }

Not all that spectacular. CouchDB is saying "hello" with the running version
number.

Next, we can get a list of databases::

    curl -X GET http://admin:password@127.0.0.1:5984/_all_dbs

All we added to the previous request is the _all_dbs string, and our admin user
name and password (set when installing CouchDB).

The response should look like::

    ["_replicator","_users"]

.. note::
    In case this returns an empty Array for you, it means you haven't finished
    installation correctly. Please refer to :ref:`setup` for further
    information on this.

    For the purposes of this example, we'll not be showing the system databases
    past this point. In *your* installation, any time you ``GET /_all_dbs``,
    you should see the system databases in the list, too.

Oh, that's right, we didn't create any user databases yet!

.. note::
    The curl command issues GET requests by default. You can issue POST requests
    using ``curl -X POST``. To make it easy to work with our terminal history,
    we usually use the ``-X`` option even when issuing GET requests.
    If we want to send a POST next time, all we have to change is the method.

    HTTP does a bit more under the hood than you can see in the examples here.
    If you're interested in every last detail that goes over the wire,
    pass in the ``-v`` option (e.g., ``curl -vX GET``), which will show you
    the server curl tries to connect to, the request headers it sends,
    and response headers it receives back. Great for debugging!

Let's create a database::

    curl -X PUT http://admin:password@127.0.0.1:5984/baseball

CouchDB will reply with::

    {"ok":true}

Retrieving the list of databases again shows some useful results this time::

    curl -X GET http://admin:password@127.0.0.1:5984/_all_dbs

::

    ["baseball"]

.. note::
    We should mention JavaScript Object Notation (JSON) here, the data format
    CouchDB speaks. JSON is a lightweight data interchange format based on
    JavaScript syntax. Because JSON is natively compatible with JavaScript, your
    web browser is an ideal client for CouchDB.

    Brackets (``[]``) represent ordered lists, and curly braces (``{}``)
    represent key/value dictionaries. Keys must be strings, delimited by quotes
    (``"``), and values can be strings, numbers, booleans, lists, or key/value
    dictionaries. For a more detailed description of JSON, see Appendix E, JSON
    Primer.

Let's create another database::

    curl -X PUT http://admin:password@127.0.0.1:5984/baseball

CouchDB will reply with::

    {"error":"file_exists","reason":"The database could not be created,
    the file already exists."}

We already have a database with that name, so CouchDB will respond with an
error. Let's try again with a different database name::

    curl -X PUT http://admin:password@127.0.0.1:5984/plankton

CouchDB will reply with::

    {"ok":true}

Retrieving the list of databases yet again shows some useful results::

    curl -X GET http://admin:password@127.0.0.1:5984/_all_dbs

CouchDB will respond with::

    ["baseball", "plankton"]

To round things off, let's delete the second database::

    curl -X DELETE http://admin:password@127.0.0.1:5984/plankton

CouchDB will reply with::

    {"ok":true}

The list of databases is now the same as it was before::

    curl -X GET http://admin:password@127.0.0.1:5984/_all_dbs

CouchDB will respond with::

    ["baseball"]

For brevity, we'll skip working with documents, as the next section covers a
different and potentially easier way of working with CouchDB that should
provide experience with this. As we work through the example,
keep in mind that "under the hood" everything is being done by the
application exactly as you have been doing here manually.
Everything is done using GET, PUT, POST, and DELETE with a URI.

.. _intro/tour/fauxton:

Welcome to Fauxton
==================

After having seen CouchDB's raw API, let's get our feet wet by playing with
Fauxton, the built-in administration interface. Fauxton provides full access
to all of CouchDB's features and makes it easy to work with some of the more
complex ideas involved. With Fauxton we can create and destroy databases; view
and edit documents; compose and run MapReduce views; and trigger replication
between databases.

To load Fauxton in your browser, visit::

    http://127.0.0.1:5984/_utils/

and log in when prompted with your admin password.

In later documents, we'll focus on using CouchDB from server-side languages
such as Ruby and Python. As such, this document is a great opportunity to
showcase an example of natively serving up a dynamic web application using
nothing more than CouchDB's integrated web server, something you may wish to do
with your own applications.

The first thing we should do with a fresh installation of CouchDB is run the
test suite to verify that everything is working properly. This assures us
that any problems we may run into aren't due to bothersome issues with our
setup. By the same token, failures in the Fauxton test suite are a red flag,
telling us to double-check our installation before attempting to use a
potentially broken database server, saving us the confusion when nothing
seems to be working quite like we expect!

To validate your installation, click on the `Verify` link on the left-hand
side, then press the green `Verify Installation` button. All tests should
pass with a check mark. If any fail, re-check your installation steps.

Your First Database and Document
================================

Creating a database in Fauxton is simple. From the overview page,
click "Create Database." When asked for a name, enter ``hello-world`` and click
the Create button.

After your database has been created, Fauxton will display a list of all its
documents. This list will start out empty, so let's
create our first document. Click the plus sign next to "All Documents" and
select the "New Doc" link. CouchDB will generate a UUID for you.

For demoing purposes, having CouchDB assign a UUID is fine. When you write
your first programs, we recommend assigning your own UUIDs. If you rely on
the server to generate the UUID and you end up making two POST requests
because the first POST request bombed out, you might generate two docs and
never find out about the first one because only the second one will be
reported back. Generating your own UUIDs makes sure that you'll never end up
with duplicate documents.

Fauxton will display the newly created document, with its _id field. To create
a new field, simply use the editor to write valid JSON. Add a new field by
appending a comma to the ``_id`` value, then adding the text::

    "hello": "my new value"

Click the green Create Document button to finalize creating the
document.

You can experiment with other JSON values; e.g., ``[1, 2, "c"]`` or
``{"foo": "bar"}``.

You'll notice that the document's _rev has been added. We'll go into more detail
about this in later documents, but for now, the important thing to note is
that _rev acts like a safety feature when saving a document. As long as you
and CouchDB agree on the most recent _rev of a document, you can successfully
save your changes.

For clarity, you may want to display the contents of the document in the all
document view. To enable this, from the upper-right corner of the window,
select Options, then check the Include Docs option. Finally, press the Run
Query button. The full document should be displayed along with the ``_id``
and ``_rev`` values.

Running a Mango Query
=====================

Now that we have stored documents successfully, we want to be able to query
them. The easiest way to do this in CouchDB is running a Mango Query. There are
always two parts to a Mango Query: the index and the selector.

The index specifies which fields we want to be able to query on, and the
selector includes the actual query parameters that define what we are looking
for exactly.

Indexes are stored as rows that are kept sorted by the fields you specify. This
makes retrieving data from a range of keys efficient even when there are
thousands or millions of rows.

Before we can run an example query, we'll need some data to run it on. We'll
create documents with information about movies. Let's create documents for
three movies. (Allow CouchDB to generate the ``_id`` and ``_rev`` fields.) Use Fauxton
to create documents that have a final JSON structure that look like this:

.. code-block:: javascript

    {
        "_id": "00a271787f89c0ef2e10e88a0c0001f4",
        "type": "movie",
        "title": "My Neighbour Totoro",
        "year": 1988,
        "director": "miyazaki",
        "rating": 8.2
    }

.. code-block:: javascript

    {
        "_id": "00a271787f89c0ef2e10e88a0c0003f0",
        "type": "movie",
        "title": "Kikis Delivery Service",
        "year": 1989,
        "director": "miyazaki",
        "rating": 7.8
    }

.. code-block:: javascript

    {
        "_id": "00a271787f89c0ef2e10e88a0c00048b",
        "type": "movie",
        "title": "Princess Mononoke",
        "year": 1997,
        "director": "miyazaki",
        "rating": 8.4
    }

Now we want to be able to find a movie by its release year, we need to create a
Mango Index. To do this, go to “Run A Query with Mango” in the Database
overview. Then click on “manage indexes”, and change the index field on the
left to look like this:

.. code-block:: javascript

    {
       "index": {
          "fields": [
             "year"
          ]
       },
       "name": "year-json-index",
       "type": "json"
    }

This defines an index on the field ``year`` and allows us to send queries for
documents from a specific year.

Next, click on “edit query” and change the Mango Query to look like this:

.. code-block:: javascript

  {
     "selector": {
        "year": {
           "$eq": 1988
        }
     }
  }

Then click on ”Run Query”.

The result should be a single result, the movie “My Neighbour Totoro” which
has the year value of 1988. ``$eq`` here stands for “equal”.

.. note::
    Note that if you skip adding the index, the query will still return the
    correct results, although you will see a warning about not using a
    pre-existing index. Not using an index will work fine on small databases
    and is acceptable for testing out queries in development or training, but
    we very strongly discourage doing this in any other case, since an index is
    absolutely vital to good query performance.

You can also query for all movies during the 1980s, with this selector:

.. code-block:: javascript

  {
     "selector": {
        "year": {
           "$lt": 1990,
           "$gte": 1980
        }
     }
  }

The result are the two movies from 1988 and 1989. ``$lt`` here means “lower
than”, and ``$gte`` means “greater than or equal to”. The latter currently
doesn’t have any effect, given that all of our movies are more recent than
1980, but this makes the query future-proof and allows us to add older
movies later.

Triggering Replication
======================

Fauxton can trigger replication between two local databases,
between a local and remote database, or even between two remote databases.
We'll show you how to replicate data from one local database to another,
which is a simple way of making backups of your databases as we're working
through the examples.

First we'll need to create an empty database to be the target of replication.
Return to the Databases overview and create a database called
``hello-replication``. Now click "Replication" in the sidebar and choose
``hello-world`` as the source and ``hello-replication`` as the target. Click
"Replicate" to replicate your database.

To view the result of your replication, click on the Databases tab again.
You should see the ``hello-replication`` database has the same number of documents
as the ``hello-world`` database, and it should take up roughly the same size as
well.

.. note::
    For larger databases, replication can take much longer. It is important to
    leave the browser window open while replication is taking place.
    As an alternative, you can trigger replication via curl or some other HTTP
    client that can handle long-running connections. If your client closes the
    connection before replication finishes, you'll have to retrigger it.
    Luckily, CouchDB's replication can take over from where it left off
    instead of starting from scratch.

Wrapping Up
===========

Now that you've seen most of Fauxton's features, you'll be prepared to dive in
and inspect your data as we build our example application in the next few
documents. Fauxton's pure JavaScript approach to managing CouchDB shows how
it's possible to build a fully featured web application using only CouchDB's
HTTP API and integrated web server.

But before we get there, we'll have another look at CouchDB's HTTP API -- now
with a magnifying glass. Let's curl up on the couch and relax.
