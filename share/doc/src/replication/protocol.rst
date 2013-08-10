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

.. _replication/protocol:

============================
CouchDB Replication Protocol
============================

The **CouchDB Replication protocol** is a protocol for synchronizing
documents between 2 peers over HTTP/1.1.

Language
--------

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT",
"SHOULD", "SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL" in this
document are to be interpreted as described in :rfc:`2119`.


Goals
-----

The CouchDB Replication protocol is a synchronization protocol for
synchronizing documents between 2 peers over HTTP/1.1.

In theory the CouchDB protocol can be used between products that
implement it. However the reference implementation, written in Erlang_, is
provided by the couch_replicator_ module available in Apache CouchDB.


The CouchDB_ replication protocol is using the `CouchDB REST API
<http://wiki.apache.org/couchdb/Reference>`_ and so is based on HTTP and
the Apache CouchDB MVC Data model. The primary goal of this
specification is to describe the CouchDB replication algorithm.


Definitions
-----------

ID:
    An identifier (could be an UUID) as described in :rfc:`4122`

Sequence:
    An ID provided by the changes feed. It can be numeric but not
    necessarily.

Revision:
    (to define)

Document
    A document is JSON entity with a unique ID and revision.

Database
    A collection of documents with a unique URI

URI
    An uri is defined by the :rfc:`2396` . It can be an URL as defined
    in :rfc:`1738`.

Source
    Database from where the Documents are replicated

Target
    Database where the Document are replicated

Checkpoint
    Last source sequence ID


Algorithm
---------

1. Get unique identifiers for the Source and Target based on their URI if
   replication task ID is not available.

2. Save this identifier in a special Document named `_local/<uniqueid>`
   on the Target database. This document isn't replicated. It will
   collect the last Source sequence ID, the Checkpoint, from the
   previous replication process.

3. Get the Source changes feed by passing it the Checkpoint using the
   `since` parameter by calling the `/<source>/_changes` URL. The
   changes feed only return a list of current revisions.


.. note::

    This step can be done continuously using the `feed=longpoll` or
    `feed=continuous` parameters. Then the feed will continuously get
    the changes.


4. Collect a group of Document/Revisions ID pairs from the **changes
   feed** and send them to the target databases on the
   `/<target>/_revs_diffs` URL. The result will contain the list of
   revisions **NOT** in the Target.

5. GET each revisions from the source Database by calling the URL
   `/<source>/<docid>?revs=true&open_revs`=<revision>` . This
   will get the document with teh parent revisions. Also don't forget to
   get attachments that aren't already stored at the target. As an
   optimisation you can use the HTTP multipart api to get all.

6. Collect a group of revisions fetched at previous step and store them
   on the target database using the `Bulk Docs
   <http://wiki.apache.org/couchdb/HTTP_Document_API#Bulk_Docs>`_ API
   with the `new_edit: false` JSON property to preserve their revisions
   ID.

7. After the group of revision is stored on the Target, save
   the new Checkpoint on the Source.


.. note::

    - Even if some revisions have been ignored the sequence should be
      take in consideration for the Checkpoint.

    - To compare non numeric sequence ordering, you will have to keep an
      ordered list of the sequences IDS as they appear in the _changes
      feed and compare their indices.

Filter replication
------------------

The replication can be filtered by passing the `filter` parameter to the
changes feeds with a function name. This will call a function on each
changes. If this function return True, the document will be added to the
feed.


Optimisations
-------------

- The system should run each steps in parallel to reduce the latency.

- The number of revisions passed to the step 3 and 6 should be large
  enough to reduce the bandwidth and make sure to reduce the latency.


API Reference
-------------

- :ref:`api/db.head` -- Check Database existence
- :ref:`api/db/ensure_full_commit` -- Ensure that all changes are stored on disk
- :http:get:`/{db}/_local/{id}` -- Read the last Checkpoint
- :http:put:`/{db}/_local/{id}` -- Save a new Checkpoint

Push Only
~~~~~~~~~

- :ref:`api/db.put` -- Create Target if it not exists and option was provided
- :ref:`api/db/revs_diff.post` -- Locate Revisions that are not known to the
  Target
- :ref:`api/db/bulk_docs.post` -- Upload Revisions to the Target
- :ref:`api/doc.put`?new_edits=false -- Upload a single Document with
  attachments to the Target

Pull Only
~~~~~~~~~

- :ref:`api/db/changes.get` -- Locate changes since on Source the last pull.
  The request uses next query parameters:

  - ``style=all_docs``
  - ``feed=feed`` , where feed is :ref:`normal <changes/normal>` or
    :ref:`longpoll <changes/longpoll>`
  - ``limit=limit``
  - ``heartbeat=heartbeat``

- :ref:`api/doc.get` -- Retrieve a single Document from Source with attachments.
  The request uses next query parameters:

  - ``open_revs=revid`` - where ``revid`` is the actual Document Revision at the
    moment of the pull request
  - ``revs=true``
  - ``atts_since=lastrev``

Reference
---------

* `TouchDB Ios wiki <https://github.com/couchbaselabs/TouchDB-iOS/wiki/Replication-Algorithm>`_
* `CouchDB documentation
  <http://wiki.apache.org/couchdb/Replication>`_
* CouchDB `change notifications`_

.. _CouchDB: http://couchdb.apache.org
.. _Erlang: http://erlang.org
.. _couch_replicator: https://github.com/apache/couchdb/tree/master/src/couch_replicator
.. _change notifications: http://guide.couchdb.org/draft/notifications.html

