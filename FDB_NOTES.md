Things of Note
===


1. If a replication sends us two revisions A and B where one is an
   ancestor of the other, we likely have divergent behavior. However,
   this should never happen In Theory.

2. Multiple updates to the same document in a _bulk_docs (or if they
   just happen to be in the same update batch in non-fdb CouchDB)
   we likely have subtly different behavior.

3. I'm relying on repeated reads in an fdb transaction to be "cheap"
   in that the reads would be cached in the fdb_transaction object.
   This needs to be checked for certainty but that appeared to
   be how things behaved in testing.

4. When attempting to create a doc from scratch in an interacitve_edit
   update, with revisions specified *and* attachment stubs, the reported
   error is now a conflict. Previously the missing_stubs error was
   raised earlier.

5. There may be a difference in behavior if a) there are no VDU functions
   set on a db and no design documents in a batch. This is because in
   this situation we don't run the prep_and_validate code on pre-fdb
   CouchDB. The new code always checks stubs before merging revision trees.
   I'm sure the old way would fail somehow, but it would fail further on
   which means we may have failed with a different reason (conflict, etc)
   before we got to the next place we check for missing stubs.

6. For multi-doc updates we'll need to investigate user versions on
   versionstamps within a transaction. Also this likely prevents the
   ability to have multiple updates to the same doc in a single
   _bulk_docs transaction

7. Document body storage needs to be implemented beyond the single
   key/value approach.

8. We'll want to look at how we currently apply open options to individual
    elements of an open_revs call. Might turn out that we have to grab a
    full FDI even if we could look up a rev directly. (i.e., revs_info
    would require us having the entire FDI, however it'd be wasteful to return
    all of that in an open_revs call, but bug compatibility ftw!)

9. Is it possible that a server_admin can delete a db without being able
    to open it? If so that's probably changed behavior.

10. All docs on large active databases might be a thing getting the doc
    count. If we allow range requests up to 5s, and we continue to return
    the doc count total we may have to play games with snapshot reads on
    the doc count key or else it'll whack any _all_docs range requests

11. Revision infos need to track their size f we want to maintain a database
    size counter we'll want to store the size of a given doc body for each
    revision so that we don't have to read the old body when updating the tree.

12. Update sequences do not yet include an incarnation value.