# Summary of Changes for CouchDB Issue #5392

## File Modified

**`src/ken/src/ken_server.erl`** - Line 488

## Change Made

Changed the return value from `ok` to `resubmit` in the `maybe_start_job/4` function when a job cannot be started.

### Before:
```erlang
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
            ok  % BUG: Always returned ok
    end.
```

### After:
```erlang
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
            resubmit  % FIXED: Now returns resubmit
    end.
```

## What This Fixes

- **Problem**: Ken was permanently skipping ~10% of databases when background indexing channels were busy
- **Root Cause**: `maybe_start_job` always returned `ok` even when jobs couldn't start
- **Solution**: Return `resubmit` to trigger the existing retry mechanism
- **Impact**: Databases are now properly resubmitted and eventually indexed

## Files for Reference

- **PR Description**: `/Users/harshit/.gemini/antigravity/scratch/couchdb/PR_DESCRIPTION.md`
- **Modified Code**: `/Users/harshit/.gemini/antigravity/scratch/couchdb/src/ken/src/ken_server.erl`
- **Walkthrough**: See artifact for detailed explanation

## Next Steps

1. Review the change in `ken_server.erl`
2. Use the PR description in `PR_DESCRIPTION.md` when creating your pull request
3. The PR description includes all necessary context and closes issue #5392
