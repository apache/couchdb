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
  - [ ] Port auth_cache.js
  - [X] Port basics.js
  - [X] Port batch_save.js
  - [X] Port bulk_docs.js
  - [X] Port changes.js
  - [X] Port coffee.js
  - [X] Port compact.js
  - [X] Port config.js
  - [X] Port conflicts.js
  - [ ] Port cookie_auth.js
  - [X] Port copy_doc.js
  - [X] Port delayed_commits.js
  - [ ] Port design_docs.js
  - [ ] Port design_options.js
  - [ ] Port design_paths.js
  - [ ] Port erlang_views.js
  - [ ] Port etags_head.js
  - [ ] Port etags_views.js
  - [ ] Port form_submit.js
  - [ ] Port http.js
  - [X] Port invalid_docids.js
  - [ ] Port jsonp.js
  - [X] Port large_docs.js
  - [ ] Port list_views.js
  - [ ] Port lorem_b64.txt
  - [ ] Port lorem.txt
  - [X] Port lots_of_docs.js
  - [ ] Port method_override.js
  - [ ] Port multiple_rows.js
  - [ ] Port proxyauth.js
  - [ ] Port purge.js
  - [ ] Port reader_acl.js
  - [ ] Port recreate_doc.js
  - [ ] Port reduce_builtin.js
  - [ ] Port reduce_false.js
  - [ ] Port reduce_false_temp.js
  - [X] Port reduce.js
  - [X] Port replication.js
  - [ ] Port replicator_db_bad_rep_id.js
  - [ ] Port replicator_db_by_doc_id.js
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
  - [ ] Port rev_stemming.js
  - [X] Port rewrite.js
  - [ ] Port rewrite_js.js
  - [X] Port security_validation.js
  - [ ] Port show_documents.js
  - [ ] Port stats.js
  - [ ] Port update_documents.js
  - [ ] Port users_db.js
  - [ ] Port users_db_security.js
  - [ ] Port utf8.js
  - [X] Port uuids.js
  - [X] Port view_collation.js
  - [ ] Port view_collation_raw.js
  - [ ] Port view_compaction.js
  - [ ] Port view_conflicts.js
  - [ ] Port view_errors.js
  - [ ] Port view_include_docs.js
  - [ ] Port view_multi_key_all_docs.js
  - [ ] Port view_multi_key_design.js
  - [ ] Port view_multi_key_temp.js
  - [ ] Port view_offsets.js
  - [ ] Port view_pagination.js
  - [ ] Port view_sandboxing.js
  - [ ] Port view_update_seq.js

# Using ExUnit to write unit tests

Elixir has a number of benefits which makes writing unit tests easier.
For example it is trivial to do codegeneration of tests.
Bellow we present a few use cases where code-generation is really helpful.

## How to write ExUnit tests

1. Create new file in test/exunit/ directory (the file name should match *_test.exs)
2. In case it is a first file in the directory create test_helper.exs (look at src/couch/test/exunit/test_helper.exs to get an idea)
3. define test module which does `use ExUnit.Case`
4. Define test cases in the module

You can run tests either:
- using make: `make exunit`
- using mix: BUILDDIR=`pwd` ERL_LIBS=`pwd`/src MIX_ENV=test mix test --trace

## Test another implementation

Imagine that we are doing a major rewrite of a module which would implement the same interface.
How do we compare both implementations return the same results for the same input?
It is easy in Elixir, here is a sketch:
```
defmodule Couch.Test.Fabric.Rewrite do
  use ExUnit.Case
  alias Couch.Test.Utils, as: Utils

  # we cannot use defrecord here because we need to construct
  # record at compile time
  admin_ctx = {:user_ctx, Utils.erlang_record(
    :user_ctx, "couch/include/couch_db.hrl", roles: ["_admin"])}

  test_cases = [
    {"create database": {create_db, [:db_name, []]}},
    {"create database as admin": {create_db, [:db_name, [admin_ctx]]}}
  ]
  module_a = :fabric
  module_b = :fabric3

  describe "Test compatibility of '#{module_a}' with '#{module_b}: '" do
    for {description, {function, args}} <- test_cases do
      test "#{description}" do
        result_a = unquote(module_a).unquote(function)(unquote_splicing(args))
        result_b = unquote(module_b).unquote(function)(unquote_splicing(args))
        assert result_a == result_b
      end
    end
  end

end
```
As a result we would get following tests
```
Couch.Test.Fabric.Rewrite
  * test Test compatibility of 'fabric' with 'fabric3': create database (0.01ms)
  * test Test compatibility of 'fabric' with 'fabric3': create database as admin (0.01ms)
```

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

## Generating tests for multiple APIs using test adapter

CouchDB has four different interfaces which we need to test. These are:
- chttpd
- couch_httpd
- fabric
- couch_db

There is a bunch of operations which are very similar. The only differences between them are:
- setup/teardown needs a different set of applications
- we need to use different modules to test the operations

This problem is solved by using testing adapter. We would define a common protocol, which we would use for testing.
Then we implement this protocol for every interface we want to use.

Here is the example of how it might look like:
```
defmodule Couch.Test.CRUD do
  use ExUnit.Case
  alias Couch.Test.Adapter
  alias Couch.Test.Utils, as: Utils

  alias Couch.Test.Setup

  require Record

  test_groups = [
    "using Clustered API": Adapter.Clustered,
    "using Backdoor API": Adapter.Backdoor,
    "using Fabric API": Adapter.Fabric,
  ]

  for {describe, adapter} <- test_groups do
    describe "Database CRUD #{describe}" do
      @describetag setup: %Setup{}
        |> Setup.Start.new([:chttpd])
        |> Setup.Adapter.new(adapter)
        |> Setup.Admin.new(user: "adm", password: "pass")
        |> Setup.Login.new(user: "adm", password: "pass")
      test "Create", %{setup: setup} do
        db_name = Utils.random_name("db")
        setup_ctx = setup |> Setup.run()
        assert {:ok, resp} = Adapter.create_db(Setup.get(setup_ctx, :adapter), db_name)
        assert resp.body["ok"]
      end
    end
  end
end
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