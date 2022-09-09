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

.. _api/basics:

==========
API Basics
==========

The CouchDB API is the primary method of interfacing to a CouchDB instance.
Requests are made using HTTP and requests are used to request information from
the database, store new data, and perform views and formatting of the
information stored within the documents.

Requests to the API can be categorised by the different areas of the CouchDB
system that you are accessing, and the HTTP method used to send the request.
Different methods imply different operations, for example retrieval of
information from the database is typically handled by the ``GET`` operation,
while updates are handled by either a ``POST`` or ``PUT`` request. There are
some differences between the information that must be supplied for the
different methods. For a guide to the basic HTTP methods and request structure,
see :ref:`api/format`.

For nearly all operations, the submitted data, and the returned data structure,
is defined within a JavaScript Object Notation (JSON) object. Basic information
on the content and data types for JSON are provided in :ref:`json`.

Errors when accessing the CouchDB API are reported using standard HTTP Status
Codes. A guide to the generic codes returned by CouchDB are provided in
:ref:`errors`.

When accessing specific areas of the CouchDB API, specific information and
examples on the HTTP methods and request, JSON structures, and error codes are
provided.

.. _api/format:

Request Format and Responses
============================

CouchDB supports the following HTTP request methods:

- ``GET``

  Request the specified item. As with normal HTTP requests, the format of the
  URL defines what is returned. With CouchDB this can include static items,
  database documents, and configuration and statistical information. In most
  cases the information is returned in the form of a JSON document.

- ``HEAD``

  The ``HEAD`` method is used to get the HTTP header of a ``GET`` request
  without the body of the response.

- ``POST``

  Upload data. Within CouchDB ``POST`` is used to set values, including
  uploading documents, setting document values, and starting certain
  administration commands.

- ``PUT``

  Used to put a specified resource. In CouchDB ``PUT`` is used to create new
  objects, including databases, documents, views and design documents.

- ``DELETE``

  Deletes the specified resource, including documents, views, and design
  documents.

- ``COPY``

  A special method that can be used to copy documents and objects.

If you use an unsupported HTTP request type with an URL that does not support
the specified type then a ``405 - Method Not Allowed`` will be returned,
listing the supported HTTP methods. For example:

.. code-block:: javascript

    {
        "error":"method_not_allowed",
        "reason":"Only GET,HEAD allowed"
    }

HTTP Headers
============

Because CouchDB uses HTTP for all communication, you need to ensure that the
correct HTTP headers are supplied (and processed on retrieval) so that you get
the right format and encoding. Different environments and clients will be more
or less strict on the effect of these HTTP headers (especially when not
present). Where possible you should be as specific as possible.

Request Headers
---------------

- ``Accept``

  Specifies the list of accepted data types to be returned by the server (i.e.
  that are accepted/understandable by the client). The format should be a list
  of one or more MIME types, separated by colons.

  For the majority of requests the definition should be for JSON data
  (``application/json``). For attachments you can either specify the MIME type
  explicitly, or use ``*/*`` to specify that all file types are supported. If
  the ``Accept`` header is not supplied, then the ``*/*`` MIME type is assumed
  (i.e. client accepts all formats).

  The use of ``Accept`` in queries for CouchDB is not required, but is highly
  recommended as it helps to ensure that the data returned can be processed by
  the client.

  If you specify a data type using the ``Accept`` header, CouchDB will honor
  the specified type in the ``Content-type`` header field returned. For
  example, if you explicitly request ``application/json`` in the ``Accept`` of
  a request, the returned HTTP headers will use the value in the returned
  ``Content-type`` field.

  For example, when sending a request without an explicit ``Accept`` header, or
  when specifying ``*/*``:

  .. code-block:: http

      GET /recipes HTTP/1.1
      Host: couchdb:5984
      Accept: */*

  The returned headers are:

  .. code-block:: http

      HTTP/1.1 200 OK
      Server: CouchDB (Erlang/OTP)
      Date: Thu, 13 Jan 2011 13:39:34 GMT
      Content-Type: text/plain;charset=utf-8
      Content-Length: 227
      Cache-Control: must-revalidate

  .. Note::
      The returned content type is ``text/plain`` even though the information
      returned by the request is in JSON format.

  Explicitly specifying the ``Accept`` header:

  .. code-block:: http

      GET /recipes HTTP/1.1
      Host: couchdb:5984
      Accept: application/json

  The headers returned include the ``application/json`` content type:

  .. code-block:: http

      HTTP/1.1 200 OK
      Server: CouchDB (Erlang/OTP)
      Date: Thu, 13 Jan 2013 13:40:11 GMT
      Content-Type: application/json
      Content-Length: 227
      Cache-Control: must-revalidate

- ``Content-type``

  Specifies the content type of the information being supplied within the
  request. The specification uses MIME type specifications. For the majority of
  requests this will be JSON (``application/json``). For some settings the MIME
  type will be plain text. When uploading attachments it should be the
  corresponding MIME type for the attachment or binary
  (``application/octet-stream``).

  The use of the ``Content-type`` on a request is highly recommended.

Response Headers
----------------

Response headers are returned by the server when sending back content and
include a number of different header fields, many of which are standard HTTP
response header and have no significance to CouchDB operation. The list of
response headers important to CouchDB are listed below.

- ``Cache-control``

  The cache control HTTP response header provides a suggestion for client
  caching mechanisms on how to treat the returned information. CouchDB
  typically returns the ``must-revalidate``, which indicates that the
  information should be revalidated if possible. This is used to ensure that
  the dynamic nature of the content is correctly updated.

- ``Content-length``

  The length (in bytes) of the returned content.

- ``Content-type``

  Specifies the MIME type of the returned data. For most request, the returned
  MIME type is ``text/plain``. All text is encoded in Unicode (UTF-8), and this
  is explicitly stated in the returned ``Content-type``, as
  ``text/plain;charset=utf-8``.

- ``Etag``

  The ``Etag`` HTTP header field is used to show the revision for a document,
  or a view.

  ETags have been assigned to a map/reduce group (the collection of views in a
  single design document). Any change to any of the indexes for those views
  would generate a new ETag for all view URLs in a single design doc, even if
  that specific view's results had not changed.

  Each ``_view`` URL has its own ETag which only gets updated when changes are
  made to the database that effect that index. If the index for that specific
  view does not change, that view keeps the original ETag head (therefore
  sending back ``304 - Not Modified`` more often).

- ``Transfer-Encoding``

  If the response uses an encoding, then it is specified in this header field.

  ``Transfer-Encoding: chunked`` means that the response is sent in parts, a
  method known as `chunked transfer encoding`_. This is used when CouchDB does
  not know beforehand the size of the data it will send (for example,
  the :ref:`changes feed <changes>`).

- ``X-CouchDB-Body-Time``

  Time spent receiving the request body in milliseconds.

  Available when body content is included in the request.

- ``X-Couch-Request-ID``

  Unique identifier for the request.

.. _chunked transfer encoding:
    https://en.wikipedia.org/wiki/Chunked_transfer_encoding

.. _json:

JSON Basics
===========

The majority of requests and responses to CouchDB use the JavaScript Object
Notation (JSON) for formatting the content and structure of the data and
responses.

JSON is used because it is the simplest and easiest solution for working with
data within a web browser, as JSON structures can be evaluated and used as
JavaScript objects within the web browser environment. JSON also integrates
with the server-side JavaScript used within CouchDB.

JSON supports the same basic types as supported by JavaScript, these are:

- Array - a list of values enclosed in square brackets. For example:

  .. code-block:: javascript

      ["one", "two", "three"]

- Boolean - a ``true`` or ``false`` value. You can use these strings directly.
  For example:

  .. code-block:: javascript

      { "value": true}

- Number - an integer or floating-point number.

- Object - a set of key/value pairs (i.e. an associative array, or hash). The
  key must be a string, but the value can be any of the supported JSON values.
  For example:

  .. code-block:: javascript

      {
          "servings" : 4,
          "subtitle" : "Easy to make in advance, and then cook when ready",
          "cooktime" : 60,
          "title" : "Chicken Coriander"
      }

  In CouchDB, the JSON object is used to represent a variety of structures,
  including the main CouchDB document.

- String - this should be enclosed by double-quotes and supports Unicode
  characters and backslash escaping. For example:

  .. code-block:: javascript

      "A String"

Parsing JSON into a JavaScript object is supported through the ``JSON.parse()``
function in JavaScript, or through various libraries that will perform the
parsing of the content into a JavaScript object for you. Libraries for parsing
and generating JSON are available in many languages, including Perl, Python,
Ruby, Erlang and others.

.. warning::
    Care should be taken to ensure that your JSON structures are valid,
    invalid structures will cause CouchDB to return an HTTP status code of 500
    (server error).

.. _json/numbers:

Number Handling
---------------

Developers and users new to computer handling of numbers often encounter
surprises when expecting that a number stored in JSON format does not
necessarily return as the same number as compared character by character.

Any numbers defined in JSON that contain a decimal point or exponent will be
passed through the Erlang VM's idea of the "double" data type. Any numbers that
are used in views will pass through the view server's idea of a number (the
common JavaScript case means even integers pass through a double due to
JavaScript's definition of a number).

Consider this document that we write to CouchDB:

.. code-block:: javascript

    {
        "_id":"30b3b38cdbd9e3a587de9b8122000cff",
        "number": 1.1
    }

Now let’s read that document back from CouchDB:

.. code-block:: javascript

    {
        "_id":"30b3b38cdbd9e3a587de9b8122000cff",
        "_rev":"1-f065cee7c3fd93aa50f6c97acde93030",
        "number":1.1000000000000000888
    }

What happens is CouchDB is changing the textual representation of the
result of decoding what it was given into some numerical format. In most
cases this is an `IEEE 754`_ double precision floating point number which
is exactly what almost all other languages use as well.

.. _IEEE 754: https://en.wikipedia.org/wiki/IEEE_754-2008

What Erlang does a bit differently than other languages is that it does not
attempt to pretty print the resulting output to use the shortest number of
characters. For instance, this is why we have this relationship:

.. code-block:: erlang

    ejson:encode(ejson:decode(<<"1.1">>)).
    <<"1.1000000000000000888">>

What can be confusing here is that internally those two formats decode into the
same IEEE-754 representation. And more importantly, it will decode into a
fairly close representation when passed through all major parsers that we know
about.

While we've only been discussing cases where the textual representation
changes, another important case is when an input value contains more precision
than can actually represented in a double. (You could argue that this case is
actually "losing" data if you don't accept that numbers are stored in doubles).

Here's a log for a couple of the more common JSON libraries that happen to be
on the author's machine:

Ejson (CouchDB's current parser) at CouchDB sha 168a663b::

    $ ./utils/run -i
    Erlang R14B04 (erts-5.8.5) [source] [64-bit] [smp:2:2] [rq:2]
    [async-threads:4] [hipe] [kernel-poll:true]

    Eshell V5.8.5  (abort with ^G)
    1> ejson:encode(ejson:decode(<<"1.01234567890123456789012345678901234567890">>)).
    <<"1.0123456789012346135">>
    2> F = ejson:encode(ejson:decode(<<"1.01234567890123456789012345678901234567890">>)).
    <<"1.0123456789012346135">>
    3> ejson:encode(ejson:decode(F)).
    <<"1.0123456789012346135">>

Node::

    $ node -v
    v0.6.15
    $ node
    JSON.stringify(JSON.parse("1.01234567890123456789012345678901234567890"))
    '1.0123456789012346'
    var f = JSON.stringify(JSON.parse("1.01234567890123456789012345678901234567890"))
    undefined
    JSON.stringify(JSON.parse(f))
    '1.0123456789012346'

Python::

    $ python
    Python 2.7.2 (default, Jun 20 2012, 16:23:33)
    [GCC 4.2.1 Compatible Apple Clang 4.0 (tags/Apple/clang-418.0.60)] on darwin
    Type "help", "copyright", "credits" or "license" for more information.
    import json
    json.dumps(json.loads("1.01234567890123456789012345678901234567890"))
    '1.0123456789012346'
    f = json.dumps(json.loads("1.01234567890123456789012345678901234567890"))
    json.dumps(json.loads(f))
    '1.0123456789012346'

Ruby::

    $ irb --version
    irb 0.9.5(05/04/13)
    require 'JSON'
    => true
    JSON.dump(JSON.load("[1.01234567890123456789012345678901234567890]"))
    => "[1.01234567890123]"
    f = JSON.dump(JSON.load("[1.01234567890123456789012345678901234567890]"))
    => "[1.01234567890123]"
    JSON.dump(JSON.load(f))
    => "[1.01234567890123]"

.. note::
    A small aside on Ruby, it requires a top level object or array, so I just
    wrapped the value. Should be obvious it doesn't affect the result of
    parsing the number though.

Spidermonkey::

    $ js -h 2>&1 | head -n 1
    JavaScript-C 1.8.5 2011-03-31
    $ js
    js> JSON.stringify(JSON.parse("1.01234567890123456789012345678901234567890"))
    "1.0123456789012346"
    js> var f = JSON.stringify(JSON.parse("1.01234567890123456789012345678901234567890"))
    js> JSON.stringify(JSON.parse(f))
    "1.0123456789012346"

As you can see they all pretty much behave the same except for Ruby actually
does appear to be losing some precision over the other libraries.

The astute observer will notice that ejson (the CouchDB JSON library) reported
an extra three digits. While its tempting to think that this is due to some
internal difference, its just a more specific case of the 1.1 input as
described above.

The important point to realize here is that a double can only hold a finite
number of values. What we're doing here is generating a string that when passed
through the "standard" floating point parsing algorithms (ie, ``strtod``) will
result in the same bit pattern in memory as we started with. Or, slightly
different, the bytes in a JSON serialized number are chosen such that they
refer to a single specific value that a double can represent.

The important point to understand is that we're mapping from one infinite set
onto a finite set. An easy way to see this is by reflecting on this::

    1.0 == 1.00 == 1.000 = 1.(infinite zeros)

Obviously a computer can't hold infinite bytes so we have to decimate our
infinitely sized set to a finite set that can be represented concisely.

The game that other JSON libraries are playing is merely:

"How few characters do I have to use to select this specific value for a
double"

And that game has lots and lots of subtle details that are difficult to
duplicate in C without a significant amount of effort (it took Python over a
year to get it sorted with their fancy build systems that automatically run on
a number of different architectures).

Hopefully we've shown that CouchDB is not doing anything "funky" by changing
input. Its behaving the same as any other common JSON library does, its just
not pretty printing its output.

On the other hand, if you actually are in a position where an IEEE-754 double
is not a satisfactory data type for your numbers, then the answer as has been
stated is to not pass your numbers through this representation. In JSON this is
accomplished by encoding them as a string or by using integer types (although
integer types can still bite you if you use a platform that has a different
integer representation than normal, ie, JavaScript).

Further information can be found easily, including the
`Floating Point Guide`_, and  `David Goldberg's Reference`_.

.. _Floating Point Guide: http://floating-point-gui.de/
.. _David Goldberg's Reference: http://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html

Also, if anyone is really interested in changing this behavior, we're all ears
for contributions to `jiffy`_ (which is theoretically going to replace ejson
when we get around to updating the build system). The places we've looked for
inspiration are TCL and Python. If you know a decent implementation of this
float printing algorithm give us a holler.

.. _jiffy: https://github.com/davisp/jiffy

.. _errors:

HTTP Status Codes
=================

With the interface to CouchDB working through HTTP, error codes and statuses
are reported using a combination of the HTTP status code number, and
corresponding data in the body of the response data.

A list of the error codes returned by CouchDB, and generic descriptions of the
related errors are provided below. The meaning of different status codes for
specific request types are provided in the corresponding API call reference.

- ``200 - OK``

  Request completed successfully.

- ``201 - Created``

  Document created successfully.

- ``202 - Accepted``

  Request has been accepted, but the corresponding operation may not have
  completed. This is used for background operations, such as database
  compaction.

- ``304 - Not Modified``

  The additional content requested has not been modified. This is used with the
  ETag system to identify the version of information returned.

- ``400 - Bad Request``

  Bad request structure. The error can indicate an error with the request URL,
  path or headers. Differences in the supplied MD5 hash and content also
  trigger this error, as this may indicate message corruption.

- ``401 - Unauthorized``

  The item requested was not available using the supplied authorization, or
  authorization was not supplied.

- ``403 - Forbidden``

  The requested item or operation is forbidden.

- ``404 - Not Found``

  The requested content could not be found. The content will include further
  information, as a JSON object, if available. The structure will contain two
  keys, ``error`` and ``reason``. For example:

  .. code-block:: javascript

      {"error":"not_found","reason":"no_db_file"}

- ``405 - Method Not Allowed``

  A request was made using an invalid HTTP request type for the URL requested.
  For example, you have requested a ``PUT`` when a ``POST`` is required. Errors
  of this type can also triggered by invalid URL strings.

- ``406 - Not Acceptable``

  The requested content type is not supported by the server.

- ``409 - Conflict``

  Request resulted in an update conflict.

- ``412 - Precondition Failed``

  The request headers from the client and the capabilities of the server do not
  match.

- ``413 - Request Entity Too Large``

  A document exceeds the configured :config:option:`couchdb/max_document_size`
  value or the entire request exceeds the
  :config:option:`chttpd/max_http_request_size` value.

- ``415 - Unsupported Media Type``

  The content types supported, and the content type of the information being
  requested or submitted indicate that the content type is not supported.

- ``416 - Requested Range Not Satisfiable``

  The range specified in the request header cannot be satisfied by the server.

- ``417 - Expectation Failed``

  When sending documents in bulk, the bulk load operation failed.

- ``500 - Internal Server Error``

  The request was invalid, either because the supplied JSON was invalid, or
  invalid information was supplied as part of the request.
