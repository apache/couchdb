### global\_changes

This app supplies the functionality for the `/_db_updates` endpoint.

When a database is created, deleted, or updated, a corresponding event will be persisted to disk (Note: This was designed without the guarantee that a DB event will be persisted or ever occur in the `_db_updates` feed. It probably will, but it isn't guaranteed). Users can subscribe to a `_changes`-like feed of these database events by querying the `_db_updates` endpoint.

When an admin user queries the `/_db_updates` endpoint, they will see the account name associated with the DB update as well as update

### Captured Metrics

1: `global_changes`, `db_writes`: The number of doc updates caused by global\_changes.

2: `global_changes`, `server_pending_updates`: The number of documents aggregated into the pending write batch.

3: `global_changes`, `listener_pending_updates`: The number of documents aggregated into the pending event batch.

4: `global_changes`, `event_doc_conflict`: The number of rev tree branches in event docs encountered by global\_changes. Should never happen.

5: `global_changes`, `rpcs`: The number of non-fabric RPCs caused by global\_changes.

### Important Configs

1: `global_changes`, `max_event_delay`: (integer, milliseconds) The total timed added before an event is forwarded to the writer.

2: `global_changes`, `max_write_delay`: (integer, milliseconds) The time added before an event is sent to disk.

3: `global_changes`, `update_db`: (true/false) A flag setting whether to update the global\_changes database. If false, changes will be lost and there will be no performance impact of global\_changes on the cluster.
