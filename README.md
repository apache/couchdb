A B+Tree (all values stored in leaves) with configurable order, where
all data is stored in FoundationDB.

The tree is balanced at all times. A bidirectional linked list is
maintained between leaf nodes for efficient range queries in either
direction. You can pass in an fdb Db or open Tx, the latter is vastly
efficient for multiple inserts, so batch if you can.

The FoundationDB keys are currently random UUID's.

TODO

1. Rewrite inner node ids (non-root, non-leaf) so we can safely cache
    them outside of a transaction.
2. Chunkify large values over multiple rows?
3. Sequential node ids?
4. encode values in a non-erlang way?
