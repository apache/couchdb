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

.. _api-basics:

===========
CouchDB API
===========

The CouchDB API is the primary method of interfacing to a CouchDB
instance. Requests are made using HTTP and requests are used to request
information from the database, store new data, and perform views and
formatting of the information stored within the documents.

Requests to the API can be categorised by the different areas of the
CouchDB system that you are accessing, and the HTTP method used to send
the request. Different methods imply different operations, for example
retrieval of information from the database is typically handled by the
``GET`` operation, while updates are handled by either a ``POST`` or
``PUT`` request. There are some differences between the information that
must be supplied for the different methods. For a guide to the basic
HTTP methods and request structure, see :ref:`api-format`.

For nearly all operations, the submitted data, and the returned data
structure, is defined within a JavaScript Object Notation (JSON) object.
Basic information on the content and data types for JSON are provided in
:ref:`json`.

Errors when accessing the CouchDB API are reported using standard HTTP
Status Codes. A guide to the generic codes returned by CouchDB are
provided in :ref:`errors`.

When accessing specific areas of the CouchDB API, specific information
and examples on the HTTP methods and request, JSON structures, and error
codes are provided. For a guide to the different areas of the API, see
:ref:`api-overview`.

.. _api-format:

Request Format and Responses
============================

CouchDB supports the following HTTP request methods:

-  ``GET``

   Request the specified item. As with normal HTTP requests, the format
   of the URL defines what is returned. With CouchDB this can include
   static items, database documents, and configuration and statistical
   information. In most cases the information is returned in the form of
   a JSON document.

-  ``HEAD``

   The ``HEAD`` method is used to get the HTTP header of a ``GET``
   request without the body of the response.

-  ``POST``

   Upload data. Within CouchDB ``POST`` is used to set values, including
   uploading documents, setting document values, and starting certain
   administration commands.

-  ``PUT``

   Used to put a specified resource. In CouchDB ``PUT`` is used to
   create new objects, including databases, documents, views and design
   documents.

-  ``DELETE``

   Deletes the specified resource, including documents, views, and
   design documents.

-  ``COPY``

   A special method that can be used to copy documents and objects.

If you use the an unsupported HTTP request type with a URL that does not
support the specified type, a 405 error will be returned, listing the
supported HTTP methods. For example:

.. code-block:: javascript

    {
        "error":"method_not_allowed",
        "reason":"Only GET,HEAD allowed"
    }
          

The CouchDB design document API and the functions when returning HTML
(for example as part of a show or list) enables you to include custom
HTTP headers through the ``headers`` block of the return object.

HTTP Headers
============

Because CouchDB uses HTTP for all communication, you need to ensure that
the correct HTTP headers are supplied (and processed on retrieval) so
that you get the right format and encoding. Different environments and
clients will be more or less strict on the effect of these HTTP headers
(especially when not present). Where possible you should be as specific
as possible.

Request Headers
---------------

-  ``Content-type``

   Specifies the content type of the information being supplied within
   the request. The specification uses MIME type specifications. For the
   majority of requests this will be JSON (``application/json``). For
   some settings the MIME type will be plain text. When uploading
   attachments it should be the corresponding MIME type for the
   attachment or binary (``application/octet-stream``).

   The use of the ``Content-type`` on a request is highly recommended.

-  ``Accept``

   Specifies the list of accepted data types to be returned by the
   server (i.e. that are accepted/understandable by the client). The
   format should be a list of one or more MIME types, separated by
   colons.

   For the majority of requests the definition should be for JSON data
   (``application/json``). For attachments you can either specify the
   MIME type explicitly, or use ``*/*`` to specify that all file types
   are supported. If the ``Accept`` header is not supplied, then the
   ``*/*`` MIME type is assumed (i.e. client accepts all formats).

   The use of ``Accept`` in queries for CouchDB is not required, but is
   highly recommended as it helps to ensure that the data returned can
   be processed by the client.

   If you specify a data type using the ``Accept`` header, CouchDB will
   honor the specified type in the ``Content-type`` header field
   returned. For example, if you explicitly request ``application/json``
   in the ``Accept`` of a request, the returned HTTP headers will use
   the value in the returned ``Content-type`` field.

   For example, when sending a request without an explicit ``Accept``
   header, or when specifying ``*/*``:

   .. code-block:: http

       GET /recipes HTTP/1.1
       Host: couchdb:5984
       Accept: */*

   The returned headers are:

   .. code-block:: http

       Server: CouchDB/1.0.1 (Erlang OTP/R13B)
       Date: Thu, 13 Jan 2011 13:39:34 GMT
       Content-Type: text/plain;charset=utf-8
       Content-Length: 227
       Cache-Control: must-revalidate

   Note that the returned content type is ``text/plain`` even though the
   information returned by the request is in JSON format.

   Explicitly specifying the ``Accept`` header:

   .. code-block:: http

       GET /recipes HTTP/1.1
       Host: couchdb:5984
       Accept: application/json

   The headers returned include the ``application/json`` content type:

   .. code-block:: http

       Server: CouchDB/|version| (Erlang OTP/R13B)
       Date: Thu, 13 Jan 2011 13:40:11 GMT
       Content-Type: application/json
       Content-Length: 227
       Cache-Control: must-revalidate

Response Headers
----------------

Response headers are returned by the server when sending back content
and include a number of different header fields, many of which are
standard HTTP response header and have no significance to CouchDB
operation. The list of response headers important to CouchDB are listed
below.

-  ``Content-type``

   Specifies the MIME type of the returned data. For most request, the
   returned MIME type is ``text/plain``. All text is encoded in Unicode
   (UTF-8), and this is explicitly stated in the returned
   ``Content-type``, as ``text/plain;charset=utf-8``.

-  ``Cache-control``

   The cache control HTTP response header provides a suggestion for
   client caching mechanisms on how to treat the returned information.
   CouchDB typically returns the ``must-revalidate``, which indicates
   that the information should be revalidated if possible. This is used
   to ensure that the dynamic nature of the content is correctly
   updated.

-  ``Content-length``

   The length (in bytes) of the returned content.

-  ``Etag``

   The ``Etag`` HTTP header field is used to show the revision for a
   document, or a view.

   ETags have been assigned to a map/reduce group (the collection of
   views in a single design document). Any change to any of the indexes
   for those views would generate a new ETag for all view URL's in a
   single design doc, even if that specific view's results had not
   changed.

   Each ``_view`` URL has its own ETag which only gets updated when
   changes are made to the database that effect that index. If the
   index for that specific view does not change, that view keeps the
   original ETag head (therefore sending back 304 Not Modified more
   often).

.. _json:

JSON Basics
===========

The majority of requests and responses to CouchDB use the JavaScript
Object Notation (JSON) for formatting the content and structure of the
data and responses.

JSON is used because it is the simplest and easiest to use solution for
working with data within a web browser, as JSON structures can be
evaluated and used as JavaScript objects within the web browser
environment. JSON also integrates with the server-side JavaScript used
within CouchDB.

JSON supports the same basic types as supported by JavaScript, these
are:

-  Number (either integer or floating-point).

-  String; this should be enclosed by double-quotes and supports Unicode
   characters and backslash escaping. For example:

   .. code-block:: javascript

       "A String"

-  Boolean - a ``true`` or ``false`` value. You can use these strings
   directly. For example:

   .. code-block:: javascript

       { "value": true}

-  Array - a list of values enclosed in square brackets. For example:

   .. code-block:: javascript

       ["one", "two", "three"]

-  Object - a set of key/value pairs (i.e. an associative array, or
   hash). The key must be a string, but the value can be any of the
   supported JSON values. For example:

   .. code-block:: javascript

       {
          "servings" : 4,
          "subtitle" : "Easy to make in advance, and then cook when ready",
          "cooktime" : 60,
          "title" : "Chicken Coriander"
       }
           

   In CouchDB, the JSON object is used to represent a variety of
   structures, including the main CouchDB document.

Parsing JSON into a JavaScript object is supported through the
``JSON.parse()`` function in JavaScript, or through various libraries that
will perform the parsing of the content into a JavaScript object for
you. Libraries for parsing and generating JSON are available in many
languages, including Perl, Python, Ruby, Erlang and others.

.. warning::
   Care should be taken to ensure that your JSON structures are
   valid, invalid structures will cause CouchDB to return an HTTP status code
   of 500 (server error).

.. _errors:

HTTP Status Codes
=================

With the interface to CouchDB working through HTTP, error codes and
statuses are reported using a combination of the HTTP status code
number, and corresponding data in the body of the response data.

A list of the error codes returned by CouchDB, and generic descriptions
of the related errors are provided below. The meaning of different
status codes for specific request types are provided in the
corresponding API call reference.

-  ``200 - OK``

   Request completed successfully.

-  ``201 - Created``

   Document created successfully.

-  ``202 - Accepted``

   Request has been accepted, but the corresponding operation may not
   have completed. This is used for background operations, such as
   database compaction.

-  ``304 - Not Modified``

   The additional content requested has not been modified. This is used
   with the ETag system to identify the version of information returned.

-  ``400 - Bad Request``

   Bad request structure. The error can indicate an error with the
   request URL, path or headers. Differences in the supplied MD5 hash
   and content also trigger this error, as this may indicate message
   corruption.

-  ``401 - Unauthorized``

   The item requested was not available using the supplied
   authorization, or authorization was not supplied.

-  ``403 - Forbidden``

   The requested item or operation is forbidden.

-  ``404 - Not Found``

   The requested content could not be found. The content will include
   further information, as a JSON object, if available. The structure
   will contain two keys, ``error`` and ``reason``. For example:

   .. code-block:: javascript

       {"error":"not_found","reason":"no_db_file"}

-  ``405 - Resource Not Allowed``

   A request was made using an invalid HTTP request type for the URL
   requested. For example, you have requested a ``PUT`` when a ``POST``
   is required. Errors of this type can also triggered by invalid URL
   strings.

-  ``406 - Not Acceptable``

   The requested content type is not supported by the server.

-  ``409 - Conflict``

   Request resulted in an update conflict.

-  ``412 - Precondition Failed``

   The request headers from the client and the capabilities of the
   server do not match.

-  ``415 - Bad Content Type``

   The content types supported, and the content type of the information
   being requested or submitted indicate that the content type is not
   supported.

-  ``416 - Requested Range Not Satisfiable``

   The range specified in the request header cannot be satisfied by the
   server.

-  ``417 - Expectation Failed``

   When sending documents in bulk, the bulk load operation failed.

-  ``500 - Internal Server Error``

   The request was invalid, either because the supplied JSON was
   invalid, or invalid information was supplied as part of the request.

HTTP Range Requests
===================

HTTP allows you to specify byte ranges for requests. This allows the
implementation of resumable downloads and skippable audio and video
streams alike. This is available for all attachments inside CouchDB.

This is just a real quick run through how this looks under the hood.
Usually, you will have larger binary files to serve from CouchDB, like
MP3s and videos, but to make things a little more obvious, I use a text
file here (Note that I use the ``application/octet-stream`` Content-Type
instead of ``text/plain``).

.. code-block:: bash

    shell> cat file.txt
    My hovercraft is full of eels!

Now let's store this text file as an attachment in CouchDB. First, we
create a database:

.. code-block:: bash

    shell> curl -X PUT http://127.0.0.1:5984/test
    {"ok":true}

Then we create a new document and the file attachment in one go:

.. code-block:: bash

    shell> curl -X PUT http://127.0.0.1:5984/test/doc/file.txt \
                -H "Content-Type: application/octet-stream" -d@file.txt
    {"ok":true,"id":"doc","rev":"1-287a28fa680ae0c7fb4729bf0c6e0cf2"}

Now we can request the whole file easily:

.. code-block:: bash

    shell> curl -X GET http://127.0.0.1:5984/test/doc/file.txt
    My hovercraft is full of eels!

But say we only want the first 13 bytes:

.. code-block:: bash

    shell> curl -X GET http://127.0.0.1:5984/test/doc/file.txt \
                -H "Range: bytes=0-12"
    My hovercraft

HTTP supports many ways to specify single and even multiple byte
ranges. Read all about it in `RFC 2616`_.

.. note::
   Databases that have been created with CouchDB 1.0.2 or earlier will
   support range requests in |version|, but they are using a less-optimal
   algorithm. If you plan to make heavy use of this feature, make sure
   to compact your database with CouchDB |version| to take advantage of a
   better algorithm to find byte ranges.

.. _RFC 2616: http://tools.ietf.org/html/rfc2616#section-14.27
