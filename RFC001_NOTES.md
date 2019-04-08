Notes
===

Mark winning revs explicitly
---

Currently we rely on winners sorting last and denote that by the fact of
them having a different value structure. I think it'd be good to assert
that the winner is a winner or not in the key structure and then have
a consistent value structure (although Sequence and BranchCount can be
both null in non-winners).

This allows us to assert that our tree stored in fdb is not corrupt in a
number of places rather than possibly allowing for confusion if we
have a bug when updating the revision tree.


Revision infos need to track their size
---

If we want to maintain a database size counter we'll want to store the
size of a given doc body for each revision so that we don't have to
read the old body when updating the tree.

Need to add Incarnation
---

Currently ignoring this in favor of the rest of the RFC


Need to add Batch Id
---

Currently ignoring as well.


Incarnation
---

Defined as a single byte which seems a bit limiting. The tuple layer
has variable length integers which seem like a better solution.

