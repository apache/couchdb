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
