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

.. _best-practices/documents:

==============================
Document Design Considerations
==============================

When designing your database, and your document structure, there are a number of
best practices to take into consideration. Especially for people accustomed to
relational databases, some of these techniques may be non-obvious.

Don't rely on CouchDB's auto-UUID generation
--------------------------------------------

While CouchDB will generate a unique identifier for the ``_id`` field of any doc
that you create, in most cases you are better off generating them yourself for
a few reasons:

- If for any reason you miss the ``200 OK`` reply from CouchDB, and storing the
  document is attempted again, you would end up with the same document content
  stored under multiple ``_id``\ s. This could easily happen with intermediary
  proxies and cache systems that may not inform developers that the failed
  transaction is being retried.
- ``_id``\ s are the only unique enforced value within CouchDB so you might
  as well make use of this. CouchDB stores its documents in a B+ tree. Each
  additional or updated document is stored as a leaf node, and may require
  re-writing intermediary and parent nodes. You may be able to take advantage of
  sequencing your own ids more effectively than the automatically generated ids
  if you can arrange them to be sequential yourself.

Alternatives to auto-incrementing sequences
-------------------------------------------

Because of replication, as well as the distributed nature of CouchDB, it is not
practical to use auto-incrementing sequences with CouchDB. These are often used
to ensure unique identifiers for each row in a database table. CouchDB generates
unique ids on its own and you can specify your own as well, so you don't really
need a sequence here. If you use a sequence for something else, you will be
better off finding another way to express it in CouchDB in another way.

Pre-aggregating your data
-------------------------

If your intent for CouchDB is as a collect-and-report model, not a real-time view,
you may not need to store a single document for every event you're recording.
In this case, pre-aggregating your data may be a good idea. You probably don't
need 1000 documents per second if all you are trying to do is to track
summary statistics about those documents. This reduces the computational pressure
on CouchDB's MapReduce engine(s), as well as reduces its storage requirements.

In this case, using an in-memory store to summarize your statistical information,
then writing out to CouchDB every 10 seconds / 1 minute / whatever level of
granularity you need would greatly reduce the number of documents you'll put in
your database.

Later, you can then further `decimate
<https://en.wikipedia.org/wiki/Downsampling_(signal_processing)>`_ your data by
walking the entire database and generating documents to be stored in a new
database with a lower level of granularity (say, 1 document a day). You can then
delete the older, more fine-grained database when you're done with it.

Designing an application to work with replication
-------------------------------------------------

Whilst CouchDB includes replication and a conflict-flagging mechanism, this is
not the whole story for building an application which replicates in a way which
users expect.

Here we consider a simple example of a bookmarks application. The idea is that
a user can replicate their own bookmarks, work with them on another machine,
and then synchronise their changes later.

Let's start with a very simple definition of bookmarks: an ordered, nestable
mapping of name to URL. Internally the application might represent it like
this:

.. code-block:: javascript

    [
      {"name":"Weather", "url":"http://www.bbc.co.uk/weather"},
      {"name":"News", "url":"http://news.bbc.co.uk/"},
      {"name":"Tech", "bookmarks": [
        {"name":"Register", "url":"http://www.theregister.co.uk/"},
        {"name":"CouchDB", "url":"http://couchdb.apache.org/"}
      ]}
    ]

It can then present the bookmarks menu and sub-menus by traversing this structure.

Now consider this scenario: the user has a set of bookmarks on her PC, and then
replicates it to her laptop. On the laptop, she changes the News link to point
to CNN, renames "Register" to "The Register", and adds a new link to slashdot
just after it. On the desktop, her husband deletes the Weather link, and adds a
new link to CNET in the Tech folder.

So after these changes, the laptop has:

.. code-block:: javascript

    [
      {"name":"Weather", "url":"http://www.bbc.co.uk/weather"},
      {"name":"News", "url":"http://www.cnn.com/"},
      {"name":"Tech", "bookmarks": [
        {"name":"The Register", "url":"http://www.theregister.co.uk/"},
        {"name":"Slashdot", "url":"http://www.slashdot.new/"},
        {"name":"CouchDB", "url":"http://couchdb.apache.org/"}
      ]}
    ]

and the PC has:

.. code-block:: javascript

    [
      {"name":"News", "url":"http://www.cnn.com/"},
      {"name":"Tech", "bookmarks": [
        {"name":"Register", "url":"http://www.theregister.co.uk/"},
        {"name":"CouchDB", "url":"http://couchdb.apache.org/"},
        {"name":"CNET", "url":"http://news.cnet.com/"}
      ]}
    ]

Upon the next synchronisation, we want the expected merge to take place. That
is: links which were changed, added or deleted on one side are also changed,
added or deleted on the other side - with no human intervention required unless
absolutely necessary.

We will also assume that both sides are doing a CouchDB "compact" operation
periodically, and are disconnected for more than this time before they
resynchronise.

All of the approaches below which allow automated merging of changes rely on
having some sort of history, back to the point where the replicas diverged.

CouchDB does not provide a mechanism for this itself. It stores arbitrary
numbers of old _ids for one document (trunk now has a mechanism for pruning the
_id history), for the purposes of replication. However it will not keep the
documents themselves through a compaction cycle, except where there are
conflicting versions of a document.

*Do not rely on the CouchDB revision history mechanism to help you build an
application-level version history.* Its sole purpose is to ensure eventually
consistent replication between databases. It is up to you to maintain history
explicitly in whatever form makes sense for your application, and to prune it
to avoid excessive storage utilisation, whilst not pruning past the point where
live replicas last diverged.

Approach 1: Single JSON doc
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The above structure is already valid JSON, and so could be represented in
CouchDB just by wrapping it in an object and storing as a single document:

.. code-block:: javascript

    {
      "bookmarks":
      // ... same as above
    }

This makes life very easy for the application, as the ordering and nesting is
all taken care of. The trouble here is that on replication, only two sets of
bookmarks will be visible: example B and example C. One will be chosen as the
main revision, and the other will be stored as a conflicting revision.

At this point, the semantics are very unsatisfactory from the user's point of
view. The best that can be offered is a choice saying "Which of these two sets
of bookmarks do you wish to keep: B or C?" However neither represents the
desired outcome. There is also insufficient data to be able to correctly merge
them, since the base revision A is lost.

This is going to be highly unsatisfactory for the user, who will have to apply
one set of changes again manually.

Approach 2: Separate document per bookmark
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

An alternative solution is to make each field (bookmark) a separate document in
its own right. Adding or deleting a bookmark is then just a case of adding or
deleting a document, which will never conflict (although if the same bookmark
is added on both sides, then you will end up with two copies of it). Changing a
bookmark will only conflict if both sides made changes to the same one, and
then it is reasonable to ask the user to choose between them.

Since there will now be lots of small documents, you may either wish to keep a
completely separate database for bookmarks, or else add an attribute to
distinguish bookmarks from other kinds of document in the database. In the
latter case, a view can be made to return only bookmark documents.

Whilst replication is now fixed, care is needed with the "ordered" and
"nestable" properties of bookmarks.

For ordering, one suggestion is to give each item a floating-point index, and
then when inserting an object between A and B, give it an index which is the
average of A and B's indices. Unfortunately, this will fail after a while when
you run out of precision, and the user will be bemused to find that their most
recent bookmarks no longer remember the exact position they were put in.

A better way is to keep a string representation of index, which can grow as the
tree is subdivided. This will not suffer the above problem, but it may result
in this string becoming arbitrarily long after time. They could be renumbered,
but the renumbering operation could introduce a lot of conflicts, especially if
attempted by both sides independently.

For "nestable", you can have a separate doc which represents a list of
bookmarks, and each bookmark can have a "belongs to" field which identifies the
list. It may be useful anyway to be able to have multiple top-level bookmark
sets (Bob's bookmarks, Jill's bookmarks etc). Some care is needed when deleting
a list or sub-list, to ensure that all associated bookmarks are also deleted,
otherwise they will become orphaned.

Building the entire bookmark set can be performed through the use of emitting
a compound key that describes the path to the document, then using group levels
to retrieve the position of the tree in the document. The following code
excerpt describes a tree of files, where the path to the file is stored in
the document under the ``"path"`` key:

.. code-block:: javascript

    // map function
    function(doc) {
      if (doc.type === "file") {
        if (doc.path.substr(-1) === "/") {
          var raw_path = doc.path.slice(0, -1);
        } else {
          var raw_path = doc.path;
        }
        emit (raw_path.split('/'), 1);
      }
    }

    // reduce
    _sum

This will emit rows into the view of the form ``["opt", "couchdb", "etc",
"local.ini"]`` for a ``doc.path`` of ``/opt/couchdb/etc/local.ini``. You can
then query a list of files in the ``/opt/couchdb/etc`` directory by specifying
a ``startkey`` of ``["opt", "couchdb", "etc"]`` and an ``endkey`` of ``["opt",
"couchdb", "etc", {}]``.

Approach 3: Immutable history / event sourcing
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Another approach to consider is `Event Sourcing
<https://martinfowler.com/eaaDev/EventSourcing.html>`_ or Command Logging, as
implemented in many NoSQL databases and as used in many `operational
transformation <https://en.wikipedia.org/wiki/Operational_transformation>`_
systems.

In this model, instead of storing individual bookmarks, you store records of
changes made - "Bookmark added", "Bookmark changed", "Bookmark moved",
"Bookmark deleted". These are stored in an append-only fashion. Since records
are never modified or deleted, only added to, there are never any replication
conflicts.

These records can also be stored as an array in a single CouchDB document.
Replication can cause a conflict, but in this case it is easy to resolve by
simply combining elements from the two arrays.

In order to see the full set of bookmarks, you need to start with a baseline
set (initially empty) and run all the change records since the baseline was
created; and/or you need to maintain a most-recent version and update it with
changes not yet seen.

Care is needed after replication when merging together history from multiple
sources. You may get different results depending on how you order them -
consider taking all A's changes before B's, taking all B's before A's, or
interleaving them (e.g. if each change has a timestamp).

Also, over time the amount of storage used can grow arbitrarily large, even if
the set of bookmarks itself is small. This can be controlled by moving the
baseline version forwards and then keeping only the changes after that point.
However, care is needed not to move the baseline version forward so far that
there are active replicas out there which last synchronised before that time,
as this may result in conflicts which cannot be resolved automatically.

If there is any uncertainty, it is best to present the user with a prompt to
assist with merging the content in the application itself.

Approach 4: Keep historic versions explicitly
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you are going to keep a command log history, then it may be simpler just to
keep old revisions of the bookmarks list itself around. The intention is to
subvert CouchDB's automatic behaviour of purging old revisions, by keeping
these revisions as separate documents.

You can keep a pointer to the 'most current' revision, and each revision can
point to its predecessor. On replication, merging can take place by diffing
each of the previous versions (in effect synthesising the command logs) back to
a common ancestor.

This is the sort of behaviour which revision control systems such as `Git
<http://git-scm.org/>`_ implement as a matter of routine, although generally
comparing text files line-by-line rather than comparing JSON objects
field-by-field.

Systems like Git will accumulate arbitrarily large amounts of history (although
they will attempt to compress it by packing multiple revisions so that only
their diffs are stored). With Git you can use "history rewriting" to remove old
history, but this may prohibit merging if history doesn't go back far enough in
time.

Adding client-side security with a translucent database
-------------------------------------------------------

Many applications do not require a thick layer of security at the server. It is
possible to use a modest amount of encryption and one-way functions to obscure
the sensitive columns or key-value pairs, a technique often called a
translucent database. (See `a description <http://www.wayner.org/node/52>`_.)

The simplest solutions use a one-way function like SHA-256 at the client to
scramble the name and password before storing the information.  This solution
gives the client control of the data in the database without requiring a thick
layer on the database to test each transaction. Some advantages are:

* Only the client or someone with the knowledge of the name and password can compute
  the value of SHA256 and recover the data.
* Some columns are still left in the clear, an advantage for computing aggregated
  statistics.
* Computation of SHA256 is left to the client side computer which usually has cycles
  to spare.
* The system prevents server-side snooping by insiders and any attacker who might
  penetrate the OS or any of the tools running upon it.

There are limitations:

* There is no root password. If the person forgets their name and password, their
  access is gone forever. This limits its use to databases that can continue by
  issuing a new user name and password.

There are many variations on this theme detailed in the book `Translucent Databases
<http://www.wayner.org/node/46>`_, including:

* Adding a backdoor with public-key cryptography.
* Adding a second layer with steganography.
* Dealing with typographical errors.
* Mixing encryption with one-way functions.
