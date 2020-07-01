A B+Tree (all values stored in leaves) with configurable order, where
all data is stored in FoundationDB.

The tree is balanced at all times. A bidirectional linked list is
maintained between leaf nodes for efficient range queries in either
direction. You can pass in an fdb Db or open Tx, the latter is vastly
more efficient for multiple inserts, so batch if you can.

A reduction function can be specified, the B+Tree calculates and stores
intermediate reduction values on the inner nodes for performance.

The FoundationDB keys are currently random UUID's.

TODO

1. Rewrite inner node ids (non-root, non-leaf) so we can safely cache
    them outside of a transaction.
2. Chunkify large values over multiple rows?
3. Sequential node ids?

