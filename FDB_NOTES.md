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

6. I still need add paging for the DirectoryLayer results so that we
   can use that for the _all_dbs end point.

7. For multi-doc updates we'll need to investigate user versions on
   versionstamps within a transaction. Also this likely prevents the
   ability to have multiple updates to the same doc in a single
   _bulk_docs transaction

8. I'm not currently decreasing size after updating an existing document.
   It looks like we're gonna have to do some sort of cleverness with the
   existing FDI sizes field maybe?

9. I'm cheating really bad with term_to_binary and ignoring serialization
   but given that's all going to change I'm not too concerned about it
   at this point.

10. We'll need to think harder on open_revs, especially when opening all
    revisions. I could see us easily getting into an absurd case where the
    amount of revisions for a given document exceed the fdb transaction
    limits.

11. We'll want to look at how we currently apply open options to individual
    elements of an open_revs call. Might turn out that we have to grab a
    full FDI even if we could look up a rev directly. (i.e., revs_info
    would require us having the entire FDI, however it'd be wasteful to return
    all of that in an open_revs call, but bug compatibility ftw!)

12.