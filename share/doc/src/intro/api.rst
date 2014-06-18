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


.. _intro/api:

============
The Core API
============

This document explores the CouchDB in minute detail. It shows all the
nitty-gritty and clever bits. We show you best practices and guide you around
common pitfalls.

We start out by revisiting the basic operations we ran in the previous document
:ref:`intro/tour`, looking behind the scenes. We also show what Futon needs to 
do behind its user interface to give us the nice features we saw earlier.

This document is both an introduction to the core CouchDB API as well as a
reference. If you can't remember how to run a particular request or why some
parameters are needed, you can always come back here and look things up (we
are probably the heaviest users of this document).

While explaining the API bits and pieces, we sometimes need to take a larger
detour to explain the reasoning for a particular request. This is a good
opportunity for us to tell you why CouchDB works the way it does.

The API can be subdivided into the following sections. We'll explore them
individually:

.. contents::
   :depth: 1
   :local:


Server
======

This one is basic and simple. It can serve as a sanity check to see if
CouchDB is running at all. It can also act as a safety guard for libraries
that require a certain version of CouchDB. We're using the `curl`_ utility
again::

  curl http://127.0.0.1:5984/

CouchDB replies, all excited to get going:

.. code-block:: javascript

  {
    "couchdb": "Welcome",
    "uuid": "85fb71bf700c17267fef77535820e371",
    "vendor": {
        "name": "The Apache Software Foundation",
        "version": "1.5.0"
    },
    "version": "1.5.0"
  }

You get back a JSON string, that, if parsed into a native object or data
structure of your programming language, gives you access to the welcome
string and version information.

This is not terribly useful, but it illustrates nicely the way CouchDB
behaves. You send an HTTP request and you receive a JSON string in the HTTP
response as a result.

.. _curl: http://curl.haxx.se/


Databases
=========

Now let's do something a little more useful: *create databases*.
For the strict, CouchDB is a *database management system* (DMS). That means it
can hold multiple databases. A database is a bucket that holds "related data".
We'll explore later what that means exactly. In practice, the terminology is
overlapping -- often people refer to a DMS as "a database" and also a database
within the DMS as "a database." We might follow that slight oddity, so don't
get confused by it. In general, it should be clear from the context if we are
talking about the whole of CouchDB or a single database within CouchDB.

Now let's make one! We want to store our favorite music albums,
and we creatively give our database the name albums. Note that we're now
using the ``-X`` option again to tell curl to send a :method:`PUT` request
instead of the default :method:`GET` request::

  curl -X PUT http://127.0.0.1:5984/albums

CouchDB replies:

.. code-block:: javascript

  {"ok":true}

That's it. You created a database and CouchDB told you that all went well.
What happens if you try to create a database that already exists? Let's try
to create that database again::

  curl -X PUT http://127.0.0.1:5984/albums

CouchDB replies:

.. code-block:: javascript

  {"error":"file_exists","reason":"The database could not be created, the file already exists."}

We get back an error. This is pretty convenient. We also learn a little bit
about how CouchDB works. CouchDB stores each database in a single file.
Very simple.

Let's create another database, this time with curl's ``-v`` (for "verbose")
option. The verbose option tells curl to show us not only the essentials -- 
the HTTP response body -- but all the underlying request and response details::

  curl -vX PUT http://127.0.0.1:5984/albums-backup

curl elaborates::

  * About to connect() to 127.0.0.1 port 5984 (#0)
  *   Trying 127.0.0.1... connected
  * Connected to 127.0.0.1 (127.0.0.1) port 5984 (#0)
  > PUT /albums-backup HTTP/1.1
  > User-Agent: curl/7.16.3 (powerpc-apple-darwin9.0) libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3
  > Host: 127.0.0.1:5984
  > Accept: */*
  >
  < HTTP/1.1 201 Created
  < Server: CouchDB (Erlang/OTP)
  < Date: Sun, 05 Jul 2009 22:48:28 GMT
  < Content-Type: text/plain;charset=utf-8
  < Content-Length: 12
  < Cache-Control: must-revalidate
  <
  {"ok":true}
  * Connection #0 to host 127.0.0.1 left intact
  * Closing connection #0

What a mouthful. Let's step through this line by line to understand what's
going on and find out what's important. Once you've seen this output a few
times, you'll be able to spot the important bits more easily.

::

  * About to connect() to 127.0.0.1 port 5984 (#0)

This is curl telling us that it is going to establish a TCP connection to the
CouchDB server we specified in our request URI. Not at all important,
except when debugging networking issues.

::

  *   Trying 127.0.0.1... connected
  * Connected to 127.0.0.1 (127.0.0.1) port 5984 (#0)

curl tells us it successfully connected to CouchDB. Again,
not important if you aren't trying to find problems with your network.

The following lines are prefixed with ``>`` and ``<`` characters.
The ``>`` means the line was sent to CouchDB verbatim (without the actual
``>``). The ``<`` means the line was sent back to curl by CouchDB.

::

  > PUT /albums-backup HTTP/1.1

This initiates an HTTP request. Its *method* is :method:`PUT`, the *URI* is
``/albums-backup``, and the HTTP version is ``HTTP/1.1``. There is also 
``HTTP/1.0``, which is simpler in some cases, but for all practical reasons 
you should be using ``HTTP/1.1``.

Next, we see a number of *request headers*. These are used to provide
additional details about the request to CouchDB.

::

  > User-Agent: curl/7.16.3 (powerpc-apple-darwin9.0) libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3

The User-Agent header tells CouchDB which piece of client software is doing
the HTTP request. We don't learn anything new: it's curl. This header is
often useful in web development when there are known errors in client
implementations that a server might want to prepare the response for.
It also helps to determine which platform a user is on. This information 
can be used for technical and statistical reasons. For CouchDB, the 
:header:`User-Agent` header is irrelevant.

::

  > Host: 127.0.0.1:5984

The :header:`Host` header is required by ``HTTP 1.1``. It tells the server
the hostname that came with the request.

::

  > Accept: */*

The :header:`Accept` header tells CouchDB that curl accepts any media type.
We'll look into why this is useful a little later.

::

  >

An empty line denotes that the request headers are now finished and the rest
of the request contains data we're sending to the server. In this case,
we're not sending any data, so the rest of the curl output is dedicated to
the HTTP response.

::

  < HTTP/1.1 201 Created

The first line of CouchDB's HTTP response includes the HTTP version
information (again, to acknowledge that the requested version could be
processed), an HTTP *status code*, and a *status code message*.
Different requests trigger different response codes. There's a whole range of
them telling the client (curl in our case) what effect the request had on the
server. Or, if an error occurred, what kind of error. :rfc:`2616` (the HTTP 1.1
specification) defines clear behavior for response codes. CouchDB fully
follows the RFC.

The :statuscode:`201` status code tells the client that the resource 
the request was made against was successfully created. No surprise here,
but if you remember that we got an error message when we tried to create this
database twice, you now know that this response could include a different
response code. Acting upon responses based on response codes is a common
practice. For example, all response codes of :statuscode:`400` or larger 
tell you that some error occurred. If you want to shortcut your logic and 
immediately deal with the error, you could just check a >= ``400`` response 
code.

::

  < Server: CouchDB (Erlang/OTP)

The :header:`Server` header is good for diagnostics. It tells us which 
CouchDB version and which underlying Erlang version we are talking to. 
In general, you can ignore this header, but it is good to know it's there if 
you need it.

::

  < Date: Sun, 05 Jul 2009 22:48:28 GMT

The :header:`Date` header tells you the time of the server. Since client 
and server time are not necessarily synchronized, this header is purely 
informational. You shouldn't build any critical application logic on top 
of this!

::

  < Content-Type: text/plain;charset=utf-8

The :header:`Content-Type` header tells you which MIME type 
the HTTP response body is and its encoding. We already know CouchDB returns 
JSON strings. The appropriate :header:`Content-Type` header is 
:mimetype:`application/json`. Why do we see :mimetype:`text/plain`? 
This is where pragmatism wins over purity. Sending an 
:mimetype:`application/json` :header:`Content-Type` header will make 
a browser offer you the returned JSON for download instead of 
just displaying it. Since it is extremely useful to be able to test CouchDB 
from a browser, CouchDB sends a :mimetype:`text/plain` content type, so all 
browsers will display the JSON as text.

.. note::

  There are some extensions that make your browser JSON-aware,
  but they are not installed by default. For more information, look at
  the popular `JSONView`_ extension, available for both Firefox and Chrome.

  .. _JSONView: http://jsonview.com/

Do you remember the :header:`Accept` request header and how it is set to 
``*/*`` to express interest in any MIME type? If you send ``Accept:
application/json`` in your request, CouchDB knows that you can deal with a pure 
JSON response with the proper :header:`Content-Type` header and will 
use it instead of :mimetype:`text/plain`.

::

  < Content-Length: 12

The :header:`Content-Length` header simply tells us how many bytes 
the response body has.

::

  < Cache-Control: must-revalidate

This :header:`Cache-Control` header tells you, or any proxy server between 
CouchDB and you, not to cache this response.

::

  <

This empty line tells us we're done with the response headers and what
follows now is the response body.

.. code-block:: javascript

  {"ok":true}

We've seen this before.

::

  * Connection #0 to host 127.0.0.1 left intact
  * Closing connection #0

The last two lines are curl telling us that it kept the TCP connection it
opened in the beginning open for a moment, but then closed it after it
received the entire response.

Throughout the documents, we'll show more requests with the ``-v`` option,
but we'll omit some of the headers we've seen here and include only those
that are important for the particular request.

Creating databases is all fine, but how do we get rid of one? Easy -- just
change the HTTP method::

  > curl -vX DELETE http://127.0.0.1:5984/albums-backup

This deletes a CouchDB database. The request will remove the file that the
database contents are stored in. There is no *"Are you sure?"* safety net or
any *"Empty the trash"* magic you've got to do to delete a database. Use this
command with care. Your data will be deleted without a chance to bring it
back easily if you don't have a backup copy.

This section went knee-deep into HTTP and set the stage for discussing the
rest of the core CouchDB API. Next stop: documents.


Documents
=========

.. _GUID: http://en.wikipedia.org/wiki/Globally_unique_identifier
.. _UUID: http://en.wikipedia.org/wiki/Universally_unique_identifier

Documents are CouchDB's central data structure. The idea behind a document
is, unsurprisingly, that of a real-world document -- a sheet of paper such as
an invoice, a recipe, or a business card. We already learned that CouchDB uses
the JSON format to store documents. Let's see how this storing works at the
lowest level.

Each document in CouchDB has an *ID*. This ID is unique per database. You are
free to choose any string to be the ID, but for best results we recommend a
`UUID`_ (or `GUID`_), i.e., a Universally (or Globally) Unique IDentifier.
UUIDs are random numbers that have such a low collision probability that
everybody can make thousands of UUIDs a minute for millions of years without
ever creating a duplicate. This is a great way to ensure two independent people
cannot create two different documents with the same ID. Why should you care
what somebody else is doing? For one, that somebody else could be you at a
later time or on a different computer; secondly, CouchDB replication lets you
share documents with others and using UUIDs ensures that it all works.
But more on that later; let's make some documents::

  curl -X PUT http://127.0.0.1:5984/albums/6e1295ed6c29495e54cc05947f18c8af -d '{"title":"There is Nothing Left to Lose","artist":"Foo Fighters"}'

CouchDB replies:

.. code-block:: javascript

  {"ok":true,"id":"6e1295ed6c29495e54cc05947f18c8af","rev":"1-2902191555"}
  
The curl command appears complex, but let's break it down. 
First, ``-X PUT`` tells curl to make a :method:`PUT` request. 
It is followed by the URL that specifies your CouchDB IP address and port. 
The resource part of the URL ``/albums/6e1295ed6c29495e54cc05947f18c8af``
specifies the location of a document inside our albums database. 
The wild collection of numbers and characters is a UUID. This UUID is your 
document's ID. Finally, the ``-d`` flag tells curl to use the following 
string as the body for the :method:`PUT` request. The string is a simple JSON
structure including ``title`` and ``artist`` attributes with their respective
values.

.. note::

  If you don't have a UUID handy, you can ask CouchDB to give you one (in fact,
  that is what we did just now without showing you). Simply send a
  :get:`/_uuids` request::

    curl -X GET http://127.0.0.1:5984/_uuids

  CouchDB replies:

  .. code-block:: javascript

    {"uuids":["6e1295ed6c29495e54cc05947f18c8af"]}
  
  Voilà, a UUID. If you need more than one, you can pass in the ``?count=10`` HTTP
  parameter to request 10 UUIDs, or really, any number you need.

To double-check that CouchDB isn't lying about having saved your document (it
usually doesn't), try to retrieve it by sending a GET request::

  curl -X GET http://127.0.0.1:5984/albums/6e1295ed6c29495e54cc05947f18c8af
 
We hope you see a pattern here. Everything in CouchDB has an address, a URI,
and you use the different HTTP methods to operate on these URIs.

CouchDB replies:

.. code-block:: javascript

  {"_id":"6e1295ed6c29495e54cc05947f18c8af","_rev":"1-2902191555","title":"There is Nothing Left to Lose","artist":"Foo Fighters"}

This looks a lot like the document you asked CouchDB to save, which is good.
But you should notice that CouchDB added two fields to your JSON structure.
The first is ``_id``, which holds the UUID we asked CouchDB to save our document
under. We always know the ID of a document if it is included, which is very
convenient.

The second field is ``_rev``. It stands for *revision*.

Revisions
---------

If you want to change a document in CouchDB, you don't tell it to go and find
a field in a specific document and insert a new value. Instead, you load 
the full document out of CouchDB, make your changes in the JSON structure 
(or object, when you are doing actual programming), and save the entire new 
revision (or version) of that document back into CouchDB. Each revision is 
identified by a new ``_rev`` value.

If you want to update or delete a document, CouchDB expects you to include
the ``_rev`` field of the revision you wish to change. When CouchDB accepts
the change, it will generate a new revision number. This mechanism ensures that,
in case somebody else made a change without you knowing before you got to
request the document update, CouchDB will not accept your update because you
are likely to overwrite data you didn't know existed. Or simplified: whoever
saves a change to a document first, wins. Let's see what happens if we don't
provide a ``_rev`` field (which is equivalent to providing a outdated value)::

  curl -X PUT http://127.0.0.1:5984/albums/6e1295ed6c29495e54cc05947f18c8af \
       -d '{"title":"There is Nothing Left to Lose","artist":"Foo Fighters","year":"1997"}'

CouchDB replies:

.. code-block:: javascript

  {"error":"conflict","reason":"Document update conflict."}
  
If you see this, add the latest revision number of your document to the JSON
structure::

  curl -X PUT http://127.0.0.1:5984/albums/6e1295ed6c29495e54cc05947f18c8af \
       -d '{"_rev":"1-2902191555","title":"There is Nothing Left to Lose","artist":"Foo Fighters","year":"1997"}'

Now you see why it was handy that CouchDB returned that ``_rev`` when we made 
the initial request. CouchDB replies:

.. code-block:: javascript

  {"ok":true,"id":"6e1295ed6c29495e54cc05947f18c8af","rev":"2-8aff9ee9d06671fa89c99d20a4b3ae"}
  
CouchDB accepted your write and also generated a new revision number. 
The revision number is the *MD5 hash* of the transport representation of a
document with an ``N-`` prefix denoting the number of times a document got 
updated. This is useful for replication. See :ref:`replication/conflicts` for
more information.

There are multiple reasons why CouchDB uses this revision system,
which is also called Multi-Version Concurrency Control (`MVCC`_). They all work
hand-in-hand, and this is a good opportunity to explain some of them.

.. _MVCC: http://en.wikipedia.org/wiki/Multiversion_concurrency_control

One of the aspects of the HTTP protocol that CouchDB uses is that it is
stateless. What does that mean? When talking to CouchDB you need to make
requests. Making a request includes opening a network connection to CouchDB,
exchanging bytes, and closing the connection. This is done every time you
make a request. Other protocols allow you to open a connection, exchange bytes,
keep the connection open, exchange more bytes later -- maybe depending on the
bytes you exchanged at the beginning -- and eventually close the connection.
Holding a connection open for later use requires the server to do extra work.
One common pattern is that for the lifetime of a connection, the client has
a consistent and static view of the data on the server. Managing huge amounts
of parallel connections is a significant amount of work. HTTP connections are
usually short-lived, and making the same guarantees is a lot easier.
As a result, CouchDB can handle many more concurrent connections.

Another reason CouchDB uses MVCC is that this model is simpler conceptually
and, as a consequence, easier to program. CouchDB uses less code to make this
work, and less code is always good because the ratio of defects per lines of
code is static.

The revision system also has positive effects on replication and storage
mechanisms, but we'll explore these later in the documents.

.. warning::

  The terms *version* and *revision* might sound familiar (if you are
  programming without version control, stop reading this guide right now and start
  learning one of the popular systems). Using new versions for document changes
  works a lot like version control, but there's an important difference:
  **CouchDB does not guarantee that older versions are kept around**.


Documents in Detail
-------------------

Now let's have a closer look at our document creation requests with the curl
``-v`` flag that was helpful when we explored the database API earlier.
This is also a good opportunity to create more documents that we can use in
later examples.

We'll add some more of our favorite music albums. Get a fresh UUID from the
``/_uuids`` resource. If you don't remember how that works, you can look it up
a few pages back.

::

  curl -vX PUT http://127.0.0.1:5984/albums/70b50bfa0a4b3aed1f8aff9e92dc16a0 \
       -d '{"title":"Blackened Sky","artist":"Biffy Clyro","year":2002}'

.. note::

  By the way, if you happen to know more information about your favorite
  albums, don't hesitate to add more properties. And don't worry about not
  knowing all the information for all the albums. CouchDB's schema-less
  documents can contain whatever you know. After all, you should relax and not
  worry about data.

Now with the ``-v`` option, CouchDB's reply (with only the important bits shown)
looks like this::

  > PUT /albums/70b50bfa0a4b3aed1f8aff9e92dc16a0 HTTP/1.1
  >
  < HTTP/1.1 201 Created
  < Location: http://127.0.0.1:5984/albums/70b50bfa0a4b3aed1f8aff9e92dc16a0
  < ETag: "1-e89c99d29d06671fa0a4b3ae8aff9e"
  <
  {"ok":true,"id":"70b50bfa0a4b3aed1f8aff9e92dc16a0","rev":"1-e89c99d29d06671fa0a4b3ae8aff9e"}

We're getting back the :statuscode:`201` HTTP status code in the response
headers, as we saw earlier when we created a database. The :header:`Location`
header gives us a full URL to our newly created document. And there's a new
header. An :header:`ETag` in HTTP-speak identifies a specific version of a
resource. In this case, it identifies a specific version (the first one) of our
new document. Sound familiar? Yes, conceptually, an :header:`ETag` is the same
as a CouchDB document revision number, and it shouldn't come as a surprise that
CouchDB uses revision numbers for ETags. ETags are useful for caching
infrastructures.


Attachments
-----------

CouchDB documents can have attachments just like an email message can have
attachments. An attachment is identified by a name and includes its MIME type
(or :header:`Content-Type`) and the number of bytes the attachment
contains. Attachments can be any data. It is easiest to think about attachments
as files attached to a document. These files can be text, images, Word
documents, music, or movie files. Let's make one.

Attachments get their own URL where you can upload data. Say we want to add
the album artwork to the ``6e1295ed6c29495e54cc05947f18c8af`` document
(*"There is Nothing Left to Lose"*), and let's also say the artwork is in a file
`artwork.jpg` in the current directory::

  curl -vX PUT http://127.0.0.1:5984/albums/6e1295ed6c29495e54cc05947f18c8af/artwork.jpg?rev=2-2739352689 \
       --data-binary @artwork.jpg -H "Content-Type:image/jpg"

.. note::

  The ``--data-binary`` ``@`` option tells curl to read a file's contents into 
  the HTTP request body. We're using the ``-H`` option to tell CouchDB that 
  we're uploading a JPEG file. CouchDB will keep this information around and 
  will send the appropriate header when requesting this attachment; in case of 
  an image like this, a browser will render the image instead of offering you 
  the data for download. This will come in handy later. Note that you need 
  to provide the current revision number of the document you're attaching 
  the artwork to, just as if you would update the document. Because, after all,
  attaching some data is changing the document.

You should now see your artwork image if you point your browser to 
http://127.0.0.1:5984/albums/6e1295ed6c29495e54cc05947f18c8af/artwork.jpg

If you request the document again, you'll see a new member::

  curl http://127.0.0.1:5984/albums/6e1295ed6c29495e54cc05947f18c8af

CouchDB replies:

.. code-block:: javascript

  {
    "_id": "6e1295ed6c29495e54cc05947f18c8af",
    "_rev": "3-131533518",
    "title": "There is Nothing Left to Lose",
    "artist": "Foo Fighters",
    "year": "1997",
    "_attachments": {
        "artwork.jpg": {
            "stub": true,
            "content_type": "image/jpg",
            "length": 52450
        }
    }
  }

``_attachments`` is a list of keys and values where the values are JSON objects
containing the attachment metadata. ``stub=true`` tells us that this entry is
just the metadata. If we use the ``?attachments=true`` HTTP option when 
requesting this document, we'd get a `Base64`_ encoded string containing the
attachment data.

.. _Base64: http://en.wikipedia.org/wiki/Base64

We'll have a look at more document request options later as we explore more
features of CouchDB, such as replication, which is the next topic.


Replication
===========

CouchDB replication is a mechanism to synchronize databases. Much like `rsync`_
synchronizes two directories locally or over a network, replication synchronizes 
two databases locally or remotely.

.. _rsync: http://en.wikipedia.org/wiki/Rsync

In a simple :method:`POST` request, you tell CouchDB the *source* and the
*target* of a replication and CouchDB will figure out which documents and new
document revisions are on *source* that are not yet on *target*, and will
proceed  to move the missing documents and revisions over.

We'll take an in-depth look at replication in the document :ref:`replication/intro`;
in this document, we'll just show you how to use it.

First, we'll create a target database. Note that CouchDB won't automatically
create a target database for you, and will return a replication failure if
the target doesn't exist (likewise for the source, but that mistake isn't as
easy to make)::

  curl -X PUT http://127.0.0.1:5984/albums-replica

Now we can use the database `albums-replica` as a replication target::

  curl -vX POST http://127.0.0.1:5984/_replicate \
       -d '{"source":"albums","target":"albums-replica"}' \
       -H "Content-Type: application/json"

.. note::

  CouchDB supports the option ``"create_target":true`` placed in the JSON POSTed
  to the :ref:`_replicate <api/server/replicate>` URL. It implicitly creates
  the target database if it doesn't exist.

CouchDB replies (this time we formatted the output so you can read it more
easily):

.. code-block:: javascript

  {
    "history": [
      {
        "start_last_seq": 0,
        "missing_found": 2,
        "docs_read": 2,
        "end_last_seq": 5,
        "missing_checked": 2,
        "docs_written": 2,
        "doc_write_failures": 0,
        "end_time": "Sat, 11 Jul 2009 17:36:21 GMT",
        "start_time": "Sat, 11 Jul 2009 17:36:20 GMT"
      }
    ],
    "source_last_seq": 5,
    "session_id": "924e75e914392343de89c99d29d06671",
    "ok": true
  }
  
CouchDB maintains a *session history* of replications. The response for a
replication request contains the history entry for this *replication session*.
It is also worth noting that the request for replication will stay open until
replication closes. If you have a lot of documents, it'll take a while until
they are all replicated and you won't get back the replication response
until all documents are replicated. It is important to note that
replication replicates the database only as it was at the point in time
when replication was started. So, any additions, modifications,
or deletions subsequent to the start of replication will not be replicated.

We'll punt on the details again -- the ``"ok": true`` at the end tells us all 
went well. If you now have a look at the albums-replica database,
you should see all the documents that you created in the albums database.
Neat, eh?

What you just did is called local replication in CouchDB terms. You created a
local copy of a database. This is useful for backups or to keep snapshots of
a specific state of your data around for later. You might want to do this
if you are developing your applications but want to be able to roll back to
a stable version of your code and data.

There are more types of replication useful in other situations. The source
and target members of our replication request are actually links (like in
HTML) and so far we've seen links relative to the server we're working on
(hence local). You can also specify a remote database as the target::

  curl -vX POST http://127.0.0.1:5984/_replicate \
       -d '{"source":"albums","target":"http://example.org:5984/albums-replica"}' \
       -H "Content-Type:application/json"

Using a *local source* and a *remote target* database is called *push
replication*. We're pushing changes to a remote server.

.. note::

  Since we don't have a second CouchDB server around just yet, we'll just use
  the absolute address of our single server, but you should be able to infer
  from this that you can put any remote server in there.

This is great for sharing local changes with remote servers or buddies next
door.

You can also use a *remote source* and a *local target* to do a *pull
replication*. This is great for getting the latest changes from a server that
is used by others::

  curl -vX POST http://127.0.0.1:5984/_replicate \
       -d '{"source":"http://example.org:5984/albums-replica","target":"albums"}' \
       -H "Content-Type:application/json"

Finally, you can run remote replication, which is mostly useful for management 
operations::

  curl -vX POST http://127.0.0.1:5984/_replicate \
       -d '{"source":"http://example.org:5984/albums","target":"http://example.org:5984/albums-replica"}' \
       -H"Content-Type: application/json"

.. note::

  **CouchDB and REST**

  CouchDB prides itself on having a `RESTful`_ API, but these replication
  requests don't look very RESTy to the trained eye. What's up with that?
  While CouchDB's core database, document, and attachment API are RESTful,
  not all of CouchDB's API is. The replication API is one example. There are
  more, as we'll see later in the documents.

  Why are there RESTful and non-RESTful APIs mixed up here? Have the developers
  been too lazy to go REST all the way? Remember, REST is an architectural
  style that lends itself to certain architectures (such as the CouchDB
  document API). But it is not a one-size-fits-all. Triggering an event like
  replication does not make a whole lot of sense in the REST world. It is more
  like a traditional remote procedure call. And there is nothing wrong with
  this.

  We very much believe in the "use the right tool for the job" philosophy,
  and REST does not fit every job. For support, we refer to Leonard Richardson
  and Sam Ruby who wrote `RESTful Web Services`_ (O'Reilly), as they share our
  view.

  .. _RESTful: http://en.wikipedia.org/wiki/Representational_state_transfer
  .. _RESTful Web Services: http://oreilly.com/catalog/9780596529260


Wrapping Up
===========

This is still not the full CouchDB API, but we discussed the essentials in
great detail. We're going to fill in the blanks as we go. For now, we believe 
you're ready to start building CouchDB applications.

.. seealso::

  :ref:`Complete HTTP API Reference <api>`:

  - :ref:`Server API Reference <api/server>`
  - :ref:`Database API Reference <api/database>`
  - :ref:`Document API Reference <api/document>`
  - :ref:`Replication API <api/server/replicate>`
