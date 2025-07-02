# Crash safety

In 3.4 the process for ending compaction in `finish_compaction` goes like this:

    0. state during compaction:

        db.couch
        db.couch.compact.data
        db.couch.compact.meta

    1. rename the data file:

        db.couch
        db.couch.compact
        db.couch.compact.meta

    2. delete the original DB file:

        db.couch.compact
        db.couch.compact.meta

    3. rename the .compact file:

        db.couch
        db.couch.compact.meta

    4. delete the meta file:

        db.couch

Crash safety: if a crash occurs after step 2, then on restart the
`db.couch.compact` file is taken to be the canonical file, renamed to
`db.couch`, and we continue as normal.

Question: why can't we get the same behaviour by directly renaming
`db.couch.compact.data` to `db.couch`, producing the same state as after step 3?
Rename in the same directory is atomic and we should have stopped writing to the
original `db.couch` file in order to perform this switch-over.

Answer: this might not be true on Windows, you cannot remove a file that is in
use, we have to close `db.couch`, remove it, then rename the compacted file into
its place. The rename of `.compact.data` to `.compact` signals a state where
compaction completed and we were in the middle of swapping the files over, so
nothing new has been written to `db.couch` and we can resume by using
`db.couch.compact`. This is not necessarily the case if `db.couch.compact.data`
still exists.

After `open_db_file` returns, the code in `init` that reads/creates the DB
header also deletes all the compaction files (`.compact`, `.compact.data`,
`.compact.meta`) if the header was newly created, rather than read from existing
file data.

For generational compaction, there are several cases to consider.


## A. Compacting gen 0

In this case we're appending to `db.1.couch`; references to new data written
there ends up in `db.couch.compact.data`, along with references to _existing_
data that was already in `db.{1,2,...}.couch`. The following files exist during
compaction:

    db.couch
    db.1.couch
    db.couch.compact.data
    db.couch.compact.meta

On completion, none of the generational files will be removed. Therefore all
pointers in `db.couch` and `db.couch.compact.data` remain valid and we are free
to use either file as our canonical DB file. The original cleanup procedure can
be used without modification.


## B. Compacting gen 1, 2, ...

One generation up from 0, we have data being moved from gen 1 to 2, or in
general from G to G+1. Files existing during compaction are:

    db.couch
    db.1.couch
    db.2.couch
    db.couch.compact.data
    db.couch.compact.meta

When compacting gen 1, if `db.couch` points to gen 1 then
`db.couch.compact.data` will refer to gen 2. Also, data from `db.couch` has to
be moved to `db.couch.compact.data` in order to avoid being lost on completion.
All pointers to gen 2 and above remain unmodified.

On completion, `db.1.couch` will be removed on the grounds that all its data has
been moved to `db.2.couch` and nothing new has been written to `db.1.couch`.
This is because the compactor is the only thing that writes to `db.G.couch` and
only one compaction per shard runs at a time.

However, it is only safe to remove `db.1.couch` when nothing is referring to it
any more, i.e. after `db.couch.compact.data` is renamed to `db.couch.compact`.
Once this has happened, any future DB access will either use `db.couch.compact`,
or the contents of it after moving to `db.couch`, not the original `db.couch`,
and so the old pointers into `db.1.couch` have expired.

At any point before `db.couch.compact.data` is renamed, the data in `db.1.couch`
is still being referenced, and so it cannot be removed.

Therefore we can extend the cleanup process to:

1. Rename `db.couch.compact.data` to `db.couch.compact`
2. Delete `db.1.couch`
3. Delete `db.couch`
4. Rename `db.couch.compact` to `db.couch`
5. Delete `db.couch.compact.meta`

The reason for putting the deletion of `db.1.couch` as early as possible is to
reduce the set of crash scenarios where this file remains in place. If
`db.1.couch` remains after a crash, this is not _unsafe_ (i.e. it does not cause
a consistency problem or data loss) but it does leave a pile of unreferenced
data that needs to be cleaned up. The simplest way to achieve this would be to
re-run compaction of gen 1, which could be triggered by noticing the file has an
active size of 0. Possibly Smoosh could prioritise the generation with the
largest proportion of garbage when deciding what to compact next.


## C. Compacting the last generation

Say the DB has a maximum generation of 2. This means that normally, the existing
files are:

    db.couch
    db.1.couch
    db.2.couch

During compaction of gen 2, a temporary additional generation is created along
with the usual compaction files:

    db.couch
    db.1.couch
    db.2.couch
    db.2.couch.compact.maxgen
    db.couch.compact.data
    db.couch.compact.meta

Live data is copied from `db.2.couch` to `db.2.couch.compact.maxgen`, but the
pointers stored in `db.couch.compact.data` refer to gen 2, with the intention
that `db.2.couch.compact.maxgen` will eventually be renamed to `db.2.couch`
rather than letting the generation number grow indefinitely.

This requires a further change the cleanup process:

1. Rename `db.couch.compact.data` to `db.couch.compact`
2. Delete `db.2.couch`
3. Rename `db.2.couch.compact.maxgen` to `db.2.couch`
4. Delete `db.couch`
5. Rename `db.couch.compact` to `db.couch`
6. Delete `db.couch.compact.meta`

A crash after steps 1, 2, or 3 produces one of these states:

    Step 1                      Step 2                      Step 3
    -------------------------   -------------------------   -------------------------
    db.couch                    db.couch                    db.couch
    db.2.couch (old)                                        db.2.couch (new)
    db.2.couch.compact.maxgen   db.2.couch.compact.maxgen
    db.couch.compact            db.couch.compact            db.couch.compact

These states have an ambiguity to them; all of them will cause `db.couch` to be
used when the DB is re-opened, but it's not clear whether the data in
`db.2.couch` is valid for that file or not. It creates a state where you need to
determine which data to preserved based on the presence of all the other files,
which is complicated. In state 1 you can continue using the old `db.2.couch`,
but in the other states you need to decide to open `db.couch.compact` instead
and possibly clean up the `db.2.couch.compact.maxgen` file. This suggests that
removing `db.couch` earlier in the process might be good.

1. Rename `db.couch.compact.data` to `db.couch.compact`
2. Delete `db.couch`
3. Delete `db.2.couch`
4. Rename `db.2.couch.compact.maxgen` to `db.2.couch`
5. Rename `db.couch.compact` to `db.couch`
6. Delete `db.couch.compact.meta`

Having done that, we need to extend the recovery path where we fail to find
`db.couch` and check for `db.couch.compact`. The possible crash states of the
above process are:

    Step 1                      Step 2                      Step 3                      Step 4
    -------------------------   -------------------------   -------------------------   -------------------------
    db.couch
    db.2.couch (old)            db.2.couch (old)                                        db.2.couch (new)
    db.2.couch.compact.maxgen   db.2.couch.compact.maxgen   db.2.couch.compact.maxgen
    db.couch.compact            db.couch.compact            db.couch.compact            db.couch.compact

In the first state, we would open `db.couch` on restart, and this refers to data
in the _old_ `db.2.couch`. We can continue to use this and either leave
`db.2.couch.compact.maxgen` in place for some future compaction of gen 2, or we
can delete it as we're not using any data in it.

In all the other states we will use `db.couch.compact` and therefore need to
complete the process of moving `db.2.couch.compact.maxgen` to `db.2.couch`
before using it. If `db.2.couch` exists, we remove it, and then we rename
`db.2.couch.compact.maxgen` to `db.2.couch`. This process is safe if we crash
after removing the old `db.2.couch`.

We could also try to resolve these two questions independently:

1. What to do if both `db.couch` and `db.couch.compact` exist
2. What to do if `db.G.couch` where `G > max_generation` exists

But as we've seen, the cleanup operations for both these questions create states
where the mutual ordering of their operations is important and it would be wise
to minimise the set of possible such states. Therefore we suggest the following
recovery routine:

1. Attempt to open `db.couch`. If this succeeds, remove any generation files
   above `max_generation`. Otherwise...

2. If `db.M+1.couch` where `M = max_generation` exists, then remove `db.M.couch`
   then rename `db.M+1.couch` to `db.M.couch`

3. Rename `db.couch.compact` to `db.couch`

4. Open `db.couch`

This process works correctly if the generation files are cleaned up _before_ the
rename of `db.couch.compact` to `db.couch` in `finish_compaction`. Otherwise the
`.compact` file that indicates the partial completion of this process may not
exist following a crash and this makes it harder to tell which generation files
are valid on recovery.

This also highlights that letting users modify `max_generation` for a DB is not
safe while compaction is happening, because it may lead to confusion during
recovery that could cause data loss (i.e. mistaken deletion of
`db.2.couch.compact.maxgen`).

If we allow post-creation changes to `max_generation`, then:

- Increasing it is "free"; all existing data remains valid but it simply becomes
  _possible_ for future compactions to create higher generations.

- Decreasing it requires a "reversed" compaction to move data from higher
  generations to lower ones followed by deleting the emptied generation(s). And
  so this change should properly be thought of as requesting a compaction with
  special properties.
