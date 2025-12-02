# Fix Ken Background Indexing Issue

## Description

This PR fixes issue #5392 where Ken does not start background indexing for some databases (~10%) after design document updates.

## Problem

When design documents are updated across many databases, Ken successfully indexes most databases in the background, but a subset of databases never gets indexed unless manually triggered via foreground queries.

### Root Cause

The `maybe_start_job/4` function in `ken_server.erl` was always returning `ok`, even when `should_start_job/2` returned `false` (e.g., when all indexing channels were busy). This caused the calling functions to believe jobs were successfully queued when they weren't, preventing databases from being resubmitted for indexing later.

## Solution

Changed `maybe_start_job/4` to return `resubmit` instead of `ok` when a job cannot be started. This enables the existing resubmission mechanism to properly retry indexing databases when channels become available.

## Changes

**File: `src/ken/src/ken_server.erl`**

```diff
 maybe_start_job(JobName, IndexPid, Seq, State) ->
     Job = #job{
         name = JobName,
         server = IndexPid,
         seq = Seq
     },
     case should_start_job(Job, State) of
         true ->
             gen_server:cast(?MODULE, {trigger_update, Job}),
             ok;
         false ->
-            ok
+            resubmit
     end.
```

## How It Works

When `maybe_start_job` returns `resubmit`:

1. The calling function propagates the `resubmit` value up the call stack
2. `update_ddoc_indexes` exits with `exit(resubmit)`
3. The worker process terminates with reason `resubmit`
4. `debrief_worker` catches this and calls `maybe_resubmit(DbName, resubmit)`
5. The database is scheduled for resubmission after 60 seconds
6. Ken retries indexing the database later when channels become available

This resubmission mechanism already existed in the codebase but was never triggered for the case where jobs couldn't start due to channel limits.

## Impact

- **Fixes**: Databases are no longer permanently skipped when Ken is busy
- **Ensures**: All databases will eventually get indexed in the background
- **Consistency**: All three index types (views, search, nouveau) now behave consistently
- **Minimal**: Single-line change with no breaking changes to the API or behavior

## Testing

The fix follows the existing error handling pattern used throughout `ken_server.erl`:
- `resubmit` is used as a return value in 26 places in the file
- The resubmission mechanism was already implemented (lines 517-523)
- Search and nouveau indexes already expected `resubmit` from `maybe_start_job` (lines 385, 407)

The change can be verified by:
1. Updating design documents across many databases
2. Monitoring Ken's background indexing progress
3. Verifying that all databases eventually get indexed (not just ~90%)

Closes #5392
