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


.. _intro/overview:

==================
Technical Overview
==================

Document Storage
================

A CouchDB server hosts named databases, which store **documents**.
Each document is uniquely named in the database, and CouchDB provides
a `RESTful`_ :ref:`HTTP API <api/basics>` for reading and updating (add, edit,
delete)  database documents.

Documents are the primary unit of data in CouchDB and consist of any number
of fields and attachments. Documents also include metadata that’s maintained
by the database system. Document fields are uniquely named and contain values
of :ref:`varying types <json>` (text, number, boolean, lists, etc),
and there is no set limit to text size or element count.

The CouchDB document update model is lockless and optimistic.
Document edits are made by client applications loading documents,
applying changes, and saving them back to the database. If another client
editing the same document saves their changes first, the client gets an edit
conflict error on save. To resolve the update conflict, the latest document
version can be opened, the edits reapplied and the update tried again.

Document updates (add, edit, delete) are all or nothing, either succeeding
entirely or failing completely. The database never contains partially saved
or edited documents.

.. _RESTful: http://en.wikipedia.org/wiki/REST


ACID Properties
===============

The CouchDB file layout and commitment system features all `Atomic Consistent
Isolated Durable` (`ACID`_) properties. On-disk, CouchDB never overwrites
committed data or associated structures, ensuring the database file is always
in a consistent state. This is a "crash-only" design where the CouchDB
server does not go through a shut down process, it's simply terminated.

Document updates (add, edit, delete) are serialized, except for binary blobs
which are written concurrently. Database readers are never locked out and
never have to wait on writers or other readers. Any number of clients can be
reading documents without being locked out or interrupted by concurrent
updates, even on the same document. CouchDB read operations use a
`Multi-Version Concurrency Control` (`MVCC`_) model where each client sees a
consistent snapshot of the database from the beginning to the end of the read
operation.

Documents are indexed in `B-trees`_ by their name (DocID) and a Sequence ID.
Each update to a database instance generates a new sequential number.
Sequence IDs are used later for incrementally finding changes in a database.
TheseBb-tree indexes are updated simultaneously when documents are saved or
deleted. The index updates always occur at the end of the file (append-only
updates).

Documents have the advantage of data being already conveniently packaged for
storage rather than split out across numerous tables and rows in most
databases systems. When documents are committed to disk, the document fields
and metadata are packed into buffers, sequentially one document after another
(helpful later for efficient building of views).

When CouchDB documents are updated, all data and associated indexes are
flushed to disk and the transactional commit always leaves the database
in a completely consistent state. Commits occur in two steps:

#. All document data and associated index updates are synchronously flushed
   to disk.

#. The updated database header is written in two consecutive, identical chunks
   to make up the first 4k of the file, and then synchronously flushed to disk.

In the event of an OS crash or power failure during step 1,
the partially flushed updates are simply forgotten on restart. If such a
crash happens during step 2 (committing the header), a surviving copy of the
previous identical headers will remain, ensuring coherency of all previously
committed data. Excepting the header area, consistency checks or fix-ups
after a crash or a power failure are never necessary.

.. _ACID: http://en.wikipedia.org/wiki/ACID
.. _MVCC: http://en.wikipedia.org/wiki/Multiversion_concurrency_control
.. _B-trees: http://en.wikipedia.org/wiki/B-tree


Compaction
==========

Wasted space is recovered by occasional compaction. On schedule, or when the
database file exceeds a certain amount of wasted space, the compaction process
clones all the active data to a new file and then discards the old file.
The database remains completely online the entire time and all updates and
reads are allowed to complete successfully. The old file is deleted only when
all the data has been copied and all users transitioned to the new file.


Views
=====

ACID properties only deal with storage and updates, we also need the ability
to show our data in interesting and useful ways. Unlike SQL databases where
data must be carefully decomposed into tables, data in CouchDB is stored in
semi-structured documents. CouchDB documents are flexible and each has its
own implicit structure, which alleviates the most difficult problems and
pitfalls of bi-directionally replicating table schemas and their contained data.

But beyond acting as a fancy file server, a simple document model for data
storage and sharing is too simple to build real applications on -- it simply
doesn't do enough of the things we want and expect. We want to slice and dice
and see our data in many different ways. What is needed is a way to filter,
organize and report on data that hasn't been decomposed into tables.

.. seealso::

   :ref:`views`


View Model
----------

To address this problem of adding structure back to unstructured and
semi-structured data, CouchDB integrates a view model. Views are the method
of aggregating and reporting on the documents in a database, and are built
on-demand to aggregate, join and report on database documents. Views are built
dynamically and don’t affect the underlying document, you can have as many
different view representations of the same data as you like.

View definitions are strictly virtual and only display the documents from the
current database instance, making them separate from the data they display
and compatible with replication. CouchDB views are defined inside special
**design documents** and can replicate across database instances like
regular documents, so that not only data replicates in CouchDB,
but entire application designs replicate too.


Javascript View Functions
-------------------------

Views are defined using Javascript functions acting as the map part in a
`map-reduce system`_. A :ref:`view function <viewfun>` takes a CouchDB document
as an argument and then does whatever computation it needs to do to determine
the data that is to be made available through the view, if any.
It can add multiple rows to the view based on a single document,
or it can add no rows at all.

.. _map-reduce system: http://en.wikipedia.org/wiki/MapReduce

.. seealso::

  :ref:`viewfun`


View Indexes
------------

Views are a dynamic representation of the actual document contents of a
database, and CouchDB makes it easy to create useful views of data.
But generating a view of a database with hundreds of thousands or millions of
documents is time and resource consuming, it's not something the system
should do from scratch each time.

To keep view querying fast, the view engine maintains indexes of its views,
and incrementally updates them to reflect changes in the database.
CouchDB’s core design is largely optimized around the need for efficient,
incremental creation of views and their indexes.

Views and their functions are defined inside special "design" documents,
and a design document may contain any number of uniquely named view functions.
When a user opens a view and its index is automatically updated, all the views
in the same design document are indexed as a single group.

The view builder uses the database sequence ID to determine if the view group
is fully up-to-date with the database. If not, the view engine examines the
all database documents (in packed sequential order) changed since the last
refresh. Documents are read in the order they occur in the disk file,
reducing the frequency and cost of disk head seeks.

The views can be read and queried simultaneously while also being refreshed.
If a client is slowly streaming out the contents of a large view,
the same view can be concurrently opened and refreshed for another client
without blocking the first client. This is true for any number of
simultaneous client readers, who can read and query the view while the index
is concurrently being refreshed for other clients without causing problems
for the readers.

As documents are examined, their previous row values are removed from the
view indexes, if they exist. If the document is selected by a view function,
the function results are inserted into the view as a new row.

When view index changes are written to disk, the updates are always appended
at the end of the file, serving to both reduce disk head seek times during
disk commits and to ensure crashes and power failures can not cause
corruption of indexes. If a crash occurs while updating a view index,
the incomplete index updates are simply lost and rebuilt incrementally from
its previously committed state.


Security and Validation
=======================

To protect who can read and update documents, CouchDB has a simple reader
access and update validation model that can be extended to implement custom
security models.

.. seealso::

   :ref:`api/db/security`


Administrator Access
--------------------

CouchDB database instances have administrator accounts. Administrator
accounts can create other administrator accounts and update design documents.
Design documents are special documents containing view definitions and other
special formulas, as well as regular fields and blobs.


Update Validation
-----------------

As documents written to disk, they can be validated dynamically by javascript
functions for both security and data validation. When the document passes
all the formula validation criteria, the update is allowed to continue.
If the validation fails, the update is aborted and the user client gets an
error response.

Both the user's credentials and the updated document are given as inputs to
the validation formula, and can be used to implement custom security models
by validating a user's permissions to update a document.

A basic "author only" update document model is trivial to implement,
where document updates are validated to check if the user is listed in an
"author" field in the existing document. More dynamic models are also possible,
like checking a separate user account profile for permission settings.

The update validations are enforced for both live usage and replicated
updates, ensuring security and data validation in a shared, distributed system.

.. seealso::

   :ref:`vdufun`


Distributed Updates and Replication
===================================

CouchDB is a peer-based distributed database system, it allows for users and
servers to access and update the same shared data while disconnected and then
bi-directionally replicate those changes later.

The CouchDB document storage, view and security models are designed to work
together to make true bi-directional replication efficient and reliable.
Both documents and designs can replicate, allowing full database applications
(including application design, logic and data) to be replicated to laptops
for offline use, or replicated to servers in remote offices where slow or
unreliable connections make sharing data difficult.

The replication process is incremental. At the database level,
replication only examines documents updated since the last replication.
Then for each updated document, only fields and blobs that have changed are
replicated across the network. If replication fails at any step, due to network
problems or crash for example, the next replication restarts at the same
document where it left off.

Partial replicas can be created and maintained. Replication can be filtered
by a javascript function, so that only particular documents or those meeting
specific criteria are replicated. This can allow users to take subsets of a
large shared database application offline for their own use, while maintaining
normal interaction with the application and that subset of data.


Conflicts
---------

Conflict detection and management are key issues for any distributed edit
system. The CouchDB storage system treats edit conflicts as a common state,
not an exceptional one. The conflict handling model is simple and
"non-destructive" while preserving single document semantics and allowing for
decentralized conflict resolution.

CouchDB allows for any number of conflicting documents to exist
simultaneously in the database, with each database instance deterministically
deciding which document is the "winner" and which are conflicts. Only the
winning document can appear in views, while "losing" conflicts are still
accessible and remain in the database until deleted or purged during
database compaction. Because conflict documents are still regular documents,
they replicate just like regular documents and are subject to the same
security and validation rules.

When distributed edit conflicts occur, every database replica sees the same
winning revision and each has the opportunity to resolve the conflict.
Resolving conflicts can be done manually or, depending on the nature of the
data and the conflict, by automated agents. The system makes decentralized
conflict resolution possible while maintaining single document database
semantics.

Conflict management continues to work even if multiple disconnected users or
agents attempt to resolve the same conflicts. If resolved conflicts result in
more conflicts, the system accommodates them in the same manner, determining
the same winner on each machine and maintaining single document semantics.

.. seealso::

   :ref:`replication/conflicts`


Applications
------------

Using just the basic replication model, many traditionally single server
database applications can be made distributed with almost no extra work.
CouchDB replication is designed to be immediately useful for basic database
applications, while also being extendable for more elaborate and full-featured
uses.

With very little database work, it is possible to build a distributed
document management application with granular security and full revision
histories. Updates to documents can be implemented to exploit incremental
field and blob replication, where replicated updates are nearly as efficient
and incremental as the actual edit differences ("diffs").

The CouchDB replication model can be modified for other distributed update
models. If the storage engine is enhanced to allow multi-document update
transactions, it is possible to perform Subversion-like "all or nothing"
atomic commits when replicating with an upstream server, such that any single
document conflict or validation failure will cause the entire update to fail.
Like Subversion, conflicts would be resolved by doing a "pull" replication to
force the conflicts locally, then merging and  re-replicating to the upstream
server.


Implementation
==============

CouchDB is built on the `Erlang OTP platform`_, a functional,
concurrent programming language and development platform. Erlang was
developed for real-time telecom applications with an extreme emphasis on
reliability and availability.

Both in syntax and semantics, Erlang is very different from conventional
programming languages like C or Java. Erlang uses lightweight "processes" and
message passing for concurrency, it has no shared state threading and all
data is immutable. The robust, concurrent nature of Erlang is ideal for a
database server.

CouchDB is designed for lock-free concurrency, in the conceptual model and
the actual Erlang implementation. Reducing bottlenecks and avoiding locks
keeps the entire system working predictably under heavy loads. CouchDB can
accommodate many clients replicating changes, opening and updating documents,
and querying views whose indexes are simultaneously being refreshed for
other clients, without needing locks.

For higher availability and more concurrent users, CouchDB is designed for
"shared nothing" clustering. In a "shared nothing" cluster, each machine
is independent and replicates data with its cluster mates, allowing individual
server failures with zero downtime. And because consistency scans
and fix-ups aren’t needed on restart,
if the entire cluster fails -- due to a power outage in a datacenter,
for example -- the entire CouchDB distributed system becomes immediately
available after a restart.

CouchDB is built from the start with a consistent vision of a distributed
document database system. Unlike cumbersome attempts to bolt distributed
features on top of the same legacy models and databases,
it is the result of careful ground-up design, engineering and integration.
The document, view, security and replication models, the special purpose query
language, the efficient and robust disk layout and the concurrent and reliable
nature of the Erlang platform are all carefully integrated for a reliable
and efficient system.

.. _Erlang OTP platform: http://www.erlang.org/
