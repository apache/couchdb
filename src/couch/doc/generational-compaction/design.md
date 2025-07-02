# Generational compaction

This describes where the implementation of this feature is -- what design we
have ended up with and why. I'm writing this up to gather feedback from the
CouchDB devs on any issues this might cause, things we might have overlooked,
and to make decisions on unresolved matters.


## Compaction in CouchDB 3.x

We'll briefly describe relevant details of CouchDB's storage model and
compaction process and how they work today, before describing how we've changed
them.

- A database shard is stored in a single file, with a name like `dbname.t.couch`
  where `t` is a timestamp like `1743602741`. This contains both _data_, that is
  document bodies and attachments the user has stored, as well as _structure_,
  i.e. the internal data structures used by the CouchDB storage engine. This
  includes the by-id and by-seq B+trees, the revtrees of all the documents,
  purge history information, the database header, and any other bookkeeping
  information needed by the engine.

- Document bodies are written to disk as a single blob, and a pointer to its
  location is stored as a single integer in the `#leaf.ptr` field.

- Attachments may be stored in multiple chunks, and their pointers in the
  attachment `Sp` field look like `[{Start, Length}, ...]` i.e. a list of one or
  more pairs giving the start offset and length of each chunk.

- Attachments are considered part of the document data; their metadata and
  pointers are stored in the document body, and editing attachments creates a
  new document revision. This means if you issue a `PUT /db/doc` to create a
  document, followed by `PUT /db/doc/att` to add an attachment, then a fresh
  copy of the document body will be created with the attachment metadata in it.
  There will be two copies of the document body on disk; one for revpos 1
  without the attachment, and one at revpos 2 with the attachment.

- Compaction works by creating a new empty database file named
  `db.t.couch.compact.data`, which we'll call the _compaction target_. For each
  document in `db.t.couch`, the doc body and attachments for the leaf revisions
  are copied to the compaction target, along with a new revtree whose leaves
  point at the new copies. The body/attachments for non-leaf revisions are not
  copied over. When every document has been visited, the compaction target is
  renamed to `db.t.couch` and becomes the new active shard file.

- Compaction discards two types of garbage that will become more distinct under
  a generational model. First, it discards garbage _data_: document bodies and
  attachments which are not referenced by leaf revisions. Second, it discards
  garbage _structure_: it builds new by-id and by-seq B+trees (among other
  things) in the compaction target and thereby discards old B+tree nodes which
  are no longer referenced from the current tree roots.

- An extra file named `db.t.couch.compact.meta` is created when compaction
  starts, to indicate an ongoing compaction of that shard and to store any
  process state needed to resume it if it crashes. This file can serve as a lock
  to prevent multiple compactions of the same shard happening concurrently.


## The generational model

The main motivation for the generational model is that if a document is rarely
changed, then the same revision of it will be repeatedly copied on each
compaction. For large documents/attachments this creates an especially large
amount of overhead. Compaction still removes garbage _structure_ but also spends
most of its time copying an almost unchanged corpus of _data_ on each run. The
generational model aims to separate data from structure so that in many cases
compaction will run much faster, by avoiding copying the same set of data.

At the same time, there are compatibility concerns. If we change how data is
stored, it becomes impossible to roll back to a previous release in the event of
production problems. We would therefore like to introduce as few changes as
possible and only change how data is stored if the user has explicitly opted in
to turn this feature on. Ideally we can do this without needed a lot of
conditional code paths that would increase complexity.

### Design and implementation changes

With these aims in mind we have ended up with the following design, which we
have a (mostly) working implementation of:

- A shard continues to be stored in a file named `db.t.couch`, which we will now
  refer to as "generation zero" or _gen-0_. All new data (including that from
  replication) is written to this file, and the pointer format for bodies and
  attachments remain unchanged for data stored in this file. This means we
  continue to write data that can be read by 3.x in the gen-0 file.

- A shard may also have a number of _generation_ files named `db.t.n.couch`
  where `n` > 0. We'll use the shorthand _gen-n_ to refer to the file
  `db.t.n.couch`. These files only store _data_, not _structure_, i.e. document
  bodies and attachments can be written here, not B+tree nodes. When a datum is
  stored in one of these files, its pointer is represented as `{Gen, Ptr}` where
  `Gen` is the generation number (the `n` in the filename) and `Ptr` is the
  regular pointer value (an int for bodies, and `[{Start, Length}, ...]` for
  attachments.

- The number of generation files is capped by a field called `max_generations`
  stored in the database header. This field is not set by default, meaning the
  database will not create generation files. The user would need to explicitly
  set this to a value greater than 0 to turn the feature on.

- Compaction is now parameterised by a generation `G`. It works as it does in
  3.x, with the alteration that leaf bodies or attachments currently stored in
  generation `G` are moved to `G+1`, unless `G` is the maximum allowed
  generation. That is, if a datum is currently stored in gen-0, it is appended
  to the existing gen-1 file, rather than being stored in the compaction target.
  If it currently in gen-1, it is copied to gen-2, and so on up to the max
  generation.

- The compactor still creates the compaction target `db.t.couch.compact.data`
  and builds new database structures in that file, and renames it to
  `db.t.couch` on completion.

- Any leaf data that is not in the compaction generation `G` is left where it
  is. If it is in gen-0, then it must be copied to the compaction target to
  avoid being lost when compaction completes. This means that compactions of `G`
  above 0 also implicitly compact the gen-0 file as it would be in 3.x. It also
  means that if generational compaction is not enabled, compaction continues to
  work as it does in 3.x.

- Put another way, data is only copied to somewhere else if it is in gen-0 (when
  it is copied to the compaction target) or if it is in gen `G` (when it is
  copied to `G+1`). For all other data, its existing pointer is retained in the
  new leaves built by the compactor.

- Because attachment pointers are part of the document data, moving an
  attachment requires creating a new copy of the document even if the document
  itself is not being moved. For example, if a document is stored in gen-1 but
  has an attachment stored in gen-2, and we are compacting with `G=2`, then the
  attachment will be moved to gen-3. Since this changes its pointer, we need to
  write a fresh copy of the document data in gen-1. When gen-1 is compacted
  we'll end up with only one copy of the document in gen-2.

- We still have the constraint that only one compaction of a shard can run at
  once, for _any generation_. If a compaction with `G=0` is running, you cannot
  start one at `G=1`.

- Since the compactor is the only thing that ever writes data to gen-1 or above,
  and only one compactor per shard runs at once, the generation's file can be
  deleted on completion. For example, if compaction is running for `G=1`, it
  will copy any leaf data in gen-1 into gen-2, and nothing new will have been
  written to gen-1 while it was running. This means all data is gen-1 is now
  unreferenced and can be trivially dropped by deleting/truncating the file.

### Observations

Taken together, these changes mean that if the feature is not explicitly turned
on by the user, compaction continues to work as it does today, and data
continues to be written in a format that can be read by 3.x, making rollback
possible.

If the feature is turned on by setting `max_generations`, data written to gen-0
remains 3.x compatible but the header will no longer be intelligible to 3.x as
it contains an extra field and may have its version incremented, so this might
be a change you cannot roll back from. It should be an error to reduce the
number of generations as this would lose data.

The implementation also works without needing feature checks everywhere. It
works consistently all the time subject to the limit imposed by
`max_generations` which defaults to zero (i.e. feature is turned off).

Writing pointers to gen-0 data using a 3.x-style pointer, rather than writing
`{Gen, Ptr}` with `Gen=0`, provides a free compatibility check. Since the code
needs to continue to understand old-style "bare" pointers, we know it can handle
files from 3.x installations without needing to explicitly test using files
retained from an actual 3.x system.
