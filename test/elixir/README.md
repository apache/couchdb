# Elixir Test Suite

Proof of concept porting the JS test suite to Elixir.

Currently the basics.js suite has been partially ported over.

To run the suite:

```
mix deps.get
mix test --trace
```

## Set CouchDB credentials

By default the Elixir tests require CouchDB running at http://127.0.0.1:15984 with credentials `adm:pass`.
You can override those using the following:

```
$ EX_USERNAME=myusername EX_PASSWORD=password EX_COUCH_URL=http://my-couchdb.com mix test
```

## Tests to port

X means done, - means partially

  - [X] Port all_docs.js
  - [X] Port attachment_names.js
  - [X] Port attachment_paths.js
  - [X] Port attachment_ranges.js
  - [X] Port attachments.js
  - [X] Port attachments_multipart.js
  - [X] Port attachment_views.js
  - [X] Port auth_cache.js
  - [X] Port basics.js
  - [X] Port batch_save.js
  - [X] Port bulk_docs.js
  - [X] Port changes.js
  - [X] Port coffee.js
  - [X] Port compact.js
  - [X] Port config.js
  - [X] Port conflicts.js
  - [X] Port cookie_auth.js
  - [X] Port copy_doc.js
  - [X] Port design_docs.js
  - [X] Port design_docs_query.js
  - [X] Port design_options.js
  - [X] Port design_paths.js
  - [X] Port erlang_views.js
  - [X] Port etags_head.js
  - [ ] ~~Port etags_views.js~~ (skipped in js test suite)
  - [X] Port form_submit.js
  - [X] Port http.js
  - [X] Port invalid_docids.js
  - [X] Port jsonp.js
  - [X] Port large_docs.js
  - [ ] Port list_views.js
  - [X] Port lorem_b64.txt
  - [X] Port lorem.txt
  - [X] Port lots_of_docs.js
  - [X] Port method_override.js
  - [X] Port multiple_rows.js
  - [X] Port proxyauth.js
  - [X] Port purge.js
  - [X] Port reader_acl.js
  - [X] Port recreate_doc.js
  - [X] Port reduce_builtin.js
  - [X] Port reduce_false.js
  - [ ] ~~Port reduce_false_temp.js~~
  - [X] Port reduce.js
  - [X] Port replication.js
  - [X] Port replicator_db_bad_rep_id.js
  - [X] Port replicator_db_by_doc_id.js
  - [ ] Port replicator_db_compact_rep_db.js
  - [ ] Port replicator_db_continuous.js
  - [ ] Port replicator_db_credential_delegation.js
  - [ ] Port replicator_db_field_validation.js
  - [ ] Port replicator_db_filtered.js
  - [ ] Port replicator_db_identical_continuous.js
  - [ ] Port replicator_db_identical.js
  - [ ] Port replicator_db_invalid_filter.js
  - [ ] Port replicator_db_security.js
  - [ ] Port replicator_db_simple.js
  - [ ] Port replicator_db_successive.js
  - [ ] Port replicator_db_survives.js
  - [ ] Port replicator_db_swap_rep_db.js
  - [ ] Port replicator_db_update_security.js
  - [ ] Port replicator_db_user_ctx.js
  - [ ] Port replicator_db_write_auth.js
  - [X] Port rev_stemming.js
  - [X] Port rewrite.js
  - [ ] Port rewrite_js.js
  - [X] Port security_validation.js
  - [ ] Port show_documents.js
  - [ ] Port stats.js
  - [X] Port update_documents.js
  - [X] Port users_db.js
  - [ ] Port users_db_security.js
  - [X] Port utf8.js
  - [X] Port uuids.js
  - [X] Port view_collation.js
  - [X] Port view_collation_raw.js
  - [X] Port view_compaction.js
  - [ ] Port view_conflicts.js
  - [ ] Port view_errors.js
  - [ ] Port view_include_docs.js
  - [X] Port view_multi_key_all_docs.js
  - [X] Port view_multi_key_design.js
  - [ ] ~~Port view_multi_key_temp.js~~
  - [X] Port view_offsets.js
  - [X] Port view_pagination.js
  - [X] Port view_sandboxing.js
  - [X] Port view_update_seq.js

# Using ExUnit to write unit tests

Elixir has a number of benefits which makes writing unit tests easier.
For example it is trivial to do codegeneration of tests.
Bellow we present a few use cases where code-generation is really helpful.

## How to write ExUnit tests

1. Create new file in test/exunit/ directory (the file name should match `*_test.exs`)
2. In case it is a first file in the directory create `test_helper.exs` (look at `src/couch/test/exunit/test_helper.exs` to get an idea)
3. define test module which does `use Couch.Test.ExUnit.Case`
4. Define test cases in the module

You can run tests either:
- using make: `make exunit`
- using mix: BUILDDIR=`pwd` ERL_LIBS=`pwd`/src MIX_ENV=test mix test --trace

## Generating tests from spec

Sometimes we have some data in structured format and want
to generate test cases using that data. This is easy in Elixir.
For example suppose we have following spec:
```
{
	"{db_name}/_view_cleanup": {
		"roles": ["_admin"]
    }
}
```
We can use this spec to generate test cases
```
defmodule GenerateTestsFromSpec do
  use ExUnit.Case
  require Record
  Record.defrecordp :user_ctx, Record.extract(:user_ctx, from_lib: "couch/include/couch_db.hrl")
  Record.defrecordp :httpd, Record.extract(:httpd, from_lib: "couch/include/couch_db.hrl")

  {:ok, spec_bin} = File.read("roles.json")
  spec = :jiffy.decode(spec_bin, [:return_maps])
  Enum.each spec, fn {path, path_spec} ->
    roles = path_spec["roles"]
    @roles roles
    @path_parts String.split(path, "/")
    test "Access with `#{inspect(roles)}` roles" do
      req = httpd(path_parts: @path_parts, user_ctx: user_ctx(roles: @roles))
      :chttpd_auth_request.authorize_request(req)
    end
  end
end
```
As a result we would get
```
GenerateTestsFromSpec
  * test Access with `["_admin"]` roles (0.00ms)
```

## Test all possible combinations

Sometimes we want to test all possible permutations for parameters.
This can be accomplished using something like the following:

```
defmodule Permutations do
  use ExUnit.Case
  pairs = :couch_tests_combinatorics.product([
    [:remote, :local], [:remote, :local]
  ])
  for [source, dest] <- pairs do
    @source source
    @dest dest
    test "Replication #{source} -> #{dest}" do
     assert :ok == :ok
    end
  end
end
```

This would produce following tests
```
Permutations
  * test Replication remote -> remote (0.00ms)
  * test Replication local -> remote (0.00ms)
  * test Replication remote -> local (0.00ms)
  * test Replication local -> local (0.00ms)
```

## Reuseing of common setups

The setup functions are quite similar in lots of tests therefore it makes
sense to reuse them. The idea is to add shared setup functions into either
- test/elixir/lib/setup/common.ex
- test/elixir/lib/setup/<something>.ex

The setup functions looks like the following:
```
defmodule Foo do
  alias Couch.Test.Setup.Step

  def httpd_with_admin(setup) do
    setup
      |> Step.Start.new(:start, extra_apps: [:chttpd])
      |> Step.User.new(:admin, roles: [:server_admin])
  end
end
```

These parts of a setup chain can be invoked as follows:
```
defmodule Couch.Test.CRUD do
  use Couch.Test.ExUnit.Case
  alias Couch.Test.Utils

  alias Couch.Test.Setup

  alias Couch.Test.Setup.Step

  def with_db(context, setup) do
    setup =
      setup
      |> Setup.Common.httpd_with_db()
      |> Setup.run()

    context =
      Map.merge(context, %{
        db_name: setup |> Setup.get(:db) |> Step.Create.DB.name(),
        base_url: setup |> Setup.get(:start) |> Step.Start.clustered_url(),
        user: setup |> Setup.get(:admin) |> Step.User.name()
      })

    {context, setup}
  end

  describe "Database CRUD using Fabric API" do
    @describetag setup: &__MODULE__.with_db/2
    test "Create DB", ctx do
      IO.puts("base_url: #{ctx.base_url}")
      IO.puts("admin: #{ctx.user}")
      IO.puts("db_name: #{ctx.db_name}")
    end
  end
end
```
