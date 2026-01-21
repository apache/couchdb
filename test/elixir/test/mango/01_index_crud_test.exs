# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

defmodule IndexCrudTests do
  use CouchTestCase

  @users_db "_users"
  @db_name "index-crud"

  @moduletag config: [
    {
      "chttpd_auth",
      "authentication_db",
      @users_db
    },
    {
      "couch_httpd_auth",
      "authentication_db",
      @users_db
    },
    {
      "chttpd_auth",
      "iterations",
      "1"
    },
    {
      "admins",
      "jan",
      "apple"
    }
  ]


  setup do
    MangoDatabase.recreate(@db_name)
    MangoDatabase.recreate(@users_db)
    :ok
  end

  test "bad fields" do
    bad_fields = [
      nil,
      true,
      false,
      "bing",
      2.0,
      %{"foo" => "bar"},
      [%{"foo" => 2}],
      [%{"foo" => "asc", "bar" => "desc"}],
      [%{"foo" => "asc"}, %{"bar" => "desc"}],
      [""],
    ]

    Enum.each(bad_fields, fn bad_field ->
      {:error, resp} = MangoDatabase.create_index(@db_name, bad_field)
      assert resp.status_code == 400
    end)
  end

  test "bad types" do
    bad_types = [
      nil,
      true,
      false,
      1.5,
      "foo",  # Future support
      "geo",  # Future support
      %{"foo" => "bar"},
      ["baz", 3.0]
    ]

    Enum.each(bad_types, fn bad_type ->
      {:error, resp} = MangoDatabase.create_index(
        @db_name,
        ["foo"],
        name: "bad field",
        idx_type: bad_type
      )
      assert resp.status_code == 400
    end)
  end

  test "bad names" do
    bad_names = ["", true, false, 1.5, %{"foo" => "bar"}, [nil, false]]

    Enum.each(bad_names, fn bad_name ->
      {:error, resp} = MangoDatabase.create_index(
        @db_name,
        ["foo"],
        name: bad_name
      )
      assert resp.status_code == 400
    end)
  end

  test "bad ddocs" do
    bad_ddocs = [
      "",
      "_design/",
      true,
      false,
      1.5,
      %{"foo" => "bar"},
      [nil, false]
    ]

    Enum.each(bad_ddocs, fn bad_ddoc ->
      {:error, resp} = MangoDatabase.create_index(
        @db_name,
        ["foo"],
        ddoc: bad_ddoc
      )
      assert resp.status_code == 400
    end)
  end

  defp all_but(method, methods) do
    Enum.reject(methods, fn {m, _} -> m == method end)
  end

  test "bad urls" do
    # These are only the negative test cases because ideally the
    # positive ones are implicitly tested by other ones.
    sess = Couch.login("jan", "apple")

    # The connect method got lost in the python to elixir conversion
    # and needs further research
    all_methods = [
      {"PUT",     fn path -> Couch.Session.put(sess, path) end},
      {"GET",     fn path -> Couch.Session.get(sess, path) end},
      {"POST",    fn path -> Couch.Session.post(sess, path) end},
      {"PATCH",   fn path -> Couch.Session.get(sess, path) end},
      {"DELETE",  fn path -> Couch.Session.delete(sess, path) end},
      {"HEAD",    fn path -> Couch.Session.go(sess, :head, path, []) end},
      {"COPY",    fn path -> Couch.Session.go(sess, :copy, path, []) end},
      {"OPTIONS", fn path -> Couch.Session.go(sess, :options, path, []) end},
      {"TRACE",   fn path -> Couch.Session.go(sess, :trace, path, []) end},
      # {"CONNECT", fn path -> Couch.Session.go(sess, :connect, path, parse_response: true) end}
    ]

    # These are only the negative test cases.
    subpaths = ["a", "a/b", "a/b/c/d", "a/b/c/d/e", "a/b/c/d/e/f"]
    Enum.each(subpaths, fn subpath ->
      path = MangoDatabase.path(@db_name, "_index/#{subpath}")
      Enum.each(all_methods, fn {_, method} ->
        response = method.(path)
        assert response.status_code == 404
      end)
    end)

    path = MangoDatabase.path(@db_name, "_index/_bulk_delete")
    Enum.each(all_but("POST", all_methods), fn {_, method} ->
      response = method.(path)
      assert response.status_code == 405
    end)

    fields = ["foo", "bar"]
    ddoc = "dd"
    idx = "idx_01"
    {:ok, ret} = MangoDatabase.create_index(@db_name, fields, name: idx, ddoc: ddoc)
    assert ret == true

    subpaths = ["#{ddoc}/json/#{idx}", "_design/#{ddoc}/json/#{idx}"]
    Enum.each(subpaths, fn subpath ->
      path = MangoDatabase.path(@db_name, "_index/#{subpath}")

      Enum.each(all_but("DELETE", all_methods), fn {method_name, method} ->
        response = method.(path)
        assert response.status_code == 405,
              "Expected 405 for #{method_name} on path #{path}"
      end)
    end)
  end

  test "create idx 01" do
    fields = ["foo", "bar"]
    ret = MangoDatabase.create_index(@db_name, fields, name: "idx_1")
    assert ret == {:ok, true}

    {:ok, indexes} = MangoDatabase.list_indexes(@db_name)

    Enum.each(indexes, fn idx ->
      if idx["name"] == "idx_01" do
        assert idx["def"]["fields"] == [%{"foo" => "asc"}, %{"bar" => "asc"}]
      end
    end)
  end

  test "create idx 01 exists" do
    fields = ["foo", "bar"]
    ret = MangoDatabase.create_index(@db_name, fields, name: "idx_1")
    assert ret == {:ok, true}

    {:ok, resp} = MangoDatabase.create_index(@db_name, fields, name: "idx_1")
    assert resp == false
  end

  test "create idx 02" do
    fields = ["baz", "foo"]
    ret = MangoDatabase.create_index(@db_name, fields, name: "idx_2")
    assert ret == {:ok, true}

    {:ok, indexes} = MangoDatabase.list_indexes(@db_name)

    Enum.each(indexes, fn idx ->
      if idx["name"] == "idx_02" do
        assert idx["def"]["fields"] == [%{"baz" => "asc"}, %{"foo" => "asc"}]
      end
    end)
  end

  test "read idx doc" do
    MangoDatabase.create_index(@db_name, ["foo", "bar"], name: "idx_1")
    MangoDatabase.create_index(@db_name, ["hello", "bar"])

    {:ok, indexes} = MangoDatabase.list_indexes(@db_name)
    Enum.each(indexes, fn idx ->
      if idx["type"] != "special" do
        ddocid = idx["ddoc"]
        doc = MangoDatabase.open_doc(@db_name, ddocid)
        info = MangoDatabase.ddoc_info(@db_name, ddocid)

        assert doc["_id"] == ddocid
        [_prefix, expected_name] = String.split(ddocid, "_design/")
        assert info["name"] == expected_name
      end
    end)
  end

  test "delete idx escaped" do
    MangoDatabase.create_index(@db_name, ["foo", "bar"], name: "idx_1")
    {:ok, pre_indexes} = MangoDatabase.list_indexes(@db_name)
    ret = MangoDatabase.create_index(@db_name, ["bing"], name: "idx_del_1")
    assert ret == {:ok, true}

    {:ok, indexes} = MangoDatabase.list_indexes(@db_name)
    Enum.each(indexes, fn idx ->
      if idx["name"] == "idx_del_1" do
        assert idx["def"]["fields"] == [%{"bing" => "asc"}]

        ddoc_escaped = String.replace(idx["ddoc"], "/", "%2F")
        MangoDatabase.delete_index(@db_name, ddoc_escaped, idx["name"])
      end
    end)
    {:ok, post_indexes} = MangoDatabase.list_indexes(@db_name)
    assert pre_indexes == post_indexes
  end

  test "delete idx unescaped" do
    {:ok, pre_indexes} = MangoDatabase.list_indexes(@db_name)
    ret = MangoDatabase.create_index(@db_name, ["bing"], name: "idx_del_2")
    assert ret == {:ok, true}

    {:ok, indexes} = MangoDatabase.list_indexes(@db_name)
    Enum.each(indexes, fn idx ->
      if idx["name"] == "idx_del_2" do
        assert idx["def"]["fields"] == [%{"bing" => "asc"}]

        MangoDatabase.delete_index(@db_name, idx["ddoc"], idx["name"])
      end
    end)
    {:ok, post_indexes} = MangoDatabase.list_indexes(@db_name)
    assert pre_indexes == post_indexes
  end

  test "delete idx no design" do
    {:ok, pre_indexes} = MangoDatabase.list_indexes(@db_name)
    ret = MangoDatabase.create_index(@db_name, ["bing"], name: "idx_del_3")
    assert ret == {:ok, true}

    {:ok, indexes} = MangoDatabase.list_indexes(@db_name)
    Enum.each(indexes, fn idx ->
      if idx["name"] == "idx_del_3" do
        assert idx["def"]["fields"] == [%{"bing" => "asc"}]

        ddocid = List.last(String.split(idx["ddoc"], "/"))
        MangoDatabase.delete_index(@db_name, ddocid, idx["name"])
      end
    end)
    {:ok, post_indexes} = MangoDatabase.list_indexes(@db_name)
    assert pre_indexes == post_indexes
  end

  test "bulk delete" do
    ret = MangoDatabase.create_index(@db_name, ["field1"], name: "idx_01")
    assert ret == {:ok, true}

    ret = MangoDatabase.create_index(@db_name, ["field2"], name: "idx_02")
    assert ret == {:ok, true}

    ret = MangoDatabase.create_index(@db_name, ["field3"], name: "idx_03")
    assert ret == {:ok, true}

    {:ok, indexes} = MangoDatabase.list_indexes(@db_name)
    docids = []
    docids = Enum.reduce(indexes, docids, fn idx, acc ->
      case idx["ddoc"] do
        nil -> acc
        ddoc -> acc ++ [ddoc]
      end
    end)
    docids = docids ++ ["_design/this_is_not_an_index_name"]

    ret = MangoDatabase.bulk_delete(@db_name, docids)

    assert Enum.at(ret["fail"], 0)["id"] == "_design/this_is_not_an_index_name"
    assert length(ret["success"]) == 3
  end

  test "recreate index" do
    {:ok, pre_indexes} = MangoDatabase.list_indexes(@db_name)

    Enum.each(0..4, fn _i ->
      ret = MangoDatabase.create_index(@db_name, ["bing"], name: "idx_recreate")
      assert ret == {:ok, true}

      {:ok, indexes} = MangoDatabase.list_indexes(@db_name)

      Enum.each(indexes, fn idx ->
        if idx["name"] == "idx_recreate" do
          assert idx["def"]["fields"] == [%{"bing" => "asc"}]
          MangoDatabase.delete_index(@db_name, idx["ddoc"], idx["name"])
        end
      end)

      {:ok, post_indexes} = MangoDatabase.list_indexes(@db_name)
      assert pre_indexes == post_indexes
    end)
  end

  test "delete missing" do
    # Missing design doc
    ret = MangoDatabase.delete_index(@db_name, "this_is_not_a_design_doc_id", "foo")
    assert ret.status_code == 404

    MangoDatabase.create_index(@db_name, ["fields"], name: "idx_01")
    {:ok, indexes} = MangoDatabase.list_indexes(@db_name)

    # Missing view name
    not_special = Enum.filter(indexes, &(&1["type"] != "special"))
    idx = Enum.random(not_special)
    ddocid = List.last(String.split(idx["ddoc"], "/"))
    ret = MangoDatabase.delete_index(@db_name, ddocid, "this_is_not_an_index_name")
    assert ret.status_code == 404

    # Bad view type
    ret = MangoDatabase.delete_index(@db_name, ddocid, idx["name"], "not_a_real_type")
    assert ret.status_code == 404
  end

  test "limit skip index" do
    ret = MangoDatabase.create_index(@db_name, ["field1"], name: "idx_01")
    assert ret == {:ok, true}

    ret = MangoDatabase.create_index(@db_name, ["field2"], name: "idx_02")
    assert ret == {:ok, true}

    ret = MangoDatabase.create_index(@db_name, ["field3"], name: "idx_03")
    assert ret == {:ok, true}

    ret = MangoDatabase.create_index(@db_name, ["field4"], name: "idx_04")
    assert ret == {:ok, true}

    ret = MangoDatabase.create_index(@db_name, ["field5"], name: "idx_05")
    assert ret == {:ok, true}

    {:ok, limit2} = MangoDatabase.list_indexes(@db_name, limit: 2)
    {:ok, limit5_skip4} = MangoDatabase.list_indexes(@db_name, limit: 5, skip: 4)
    {:ok, skip5} = MangoDatabase.list_indexes(@db_name, skip: 5)
    {:ok, skip6} = MangoDatabase.list_indexes(@db_name, skip: 6)
    {:ok, skip100} = MangoDatabase.list_indexes(@db_name, skip: 100)
    {:ok, limit1} = MangoDatabase.list_indexes(@db_name, limit: 10_000_000)

    assert length(limit2) == 2
    assert length(limit5_skip4) == 2
    assert length(skip5) == 1
    assert Enum.empty?(skip6)
    assert Enum.empty?(skip100)
    assert length(limit1) == 6

    {:error, bad_skip} = MangoDatabase.list_indexes(@db_name, skip: -1)
    assert bad_skip.status_code == 500

    {:error, limit0} = MangoDatabase.list_indexes(@db_name, limit: 0)
    assert limit0.status_code == 500
  end

  test "out of sync" do
    docs = [
      %{"_id" => "1", "name" => "Jimi", "age" => 10, "cars" => 1},
      %{"_id" => "2", "name" => "kate", "age" => 8, "cars" => 0}
    ]
    MangoDatabase.save_docs(@db_name, docs)
    MangoDatabase.create_index(@db_name, ["age"], name: "age")

    selector = %{"age" => %{"$gt" => 0}}
    {:ok, docs} = MangoDatabase.find(
      @db_name, selector, use_index: "_design/a017b603a47036005de93034ff689bbbb6a873c4"
    )
    assert length(docs) == 2

    MangoDatabase.delete_doc(@db_name, "1")

    {:ok, docs1} = MangoDatabase.find(
      @db_name,
      selector,
      update: false,
      use_index: "_design/a017b603a47036005de93034ff689bbbb6a873c4"
    )
    assert length(docs1) == 1
  end
end

defmodule IndexCrudTextTests do
  use CouchTestCase

  @db_name "index-crud-text"

  setup do
    MangoDatabase.recreate(@db_name)
    :ok
  end

  test "create text idx" do
    fields = [
      %{"name" => "stringidx", "type" => "string"},
      %{"name" => "booleanidx", "type" => "boolean"},
    ]

    ret = MangoDatabase.create_text_index(@db_name, fields: fields, name: "text_idx_01")
    assert ret == {:ok, true}

    {:ok, indexes} = MangoDatabase.list_indexes(@db_name)
    Enum.each(indexes, fn idx ->
      if idx["name"] == "text_idx_01" do
        assert idx["def"]["fields"] == [%{"stringidx" => "string"}, %{"booleanidx" => "boolean"}]
      end
    end)
  end

  test "create bad text idx" do
    bad_fields = [
      true,
      false,
      "bing",
      2.0,
      %{"foo" => "bar"},
      [%{"foo" => 2}],
      [%{"name" => "foo2"}],
      [%{"name" => "foo3", "type" => "garbage"}],
      [%{"type" => "number"}],
      [%{"name" => "age", "type" => "number"}, %{"name" => "bad"}],
      [%{"name" => "age", "type" => "number"}, "bla"],
      [%{"name" => "", "type" => "number"}, "bla"],
    ]

    Enum.each(bad_fields, fn bad_field ->
      {:error, resp} = MangoDatabase.create_text_index(@db_name, fields: bad_field)
      assert resp.status_code == 400
    end)
  end

  test "limit skip index" do
    ret = MangoDatabase.create_index(@db_name, ["field1"], name: "idx_01")
    assert ret == {:ok, true}

    ret = MangoDatabase.create_index(@db_name, ["field2"], name: "idx_02")
    assert ret == {:ok, true}

    ret = MangoDatabase.create_index(@db_name, ["field3"], name: "idx_03")
    assert ret == {:ok, true}

    ret = MangoDatabase.create_index(@db_name, ["field4"], name: "idx_04")
    assert ret == {:ok, true}

    fields = [
      %{"name" => "stringidx", "type" => "string"},
      %{"name" => "booleanidx", "type" => "boolean"},
    ]
    ret = MangoDatabase.create_text_index(fields: fields, name: "idx_05")
    assert ret == {:ok, true}

    {:ok, limit2} = MangoDatabase.list_indexes(@db_name, limit: 2)
    {:ok, limit5_skip4} = MangoDatabase.list_indexes(@db_name, limit: 5, skip: 4)
    {:ok, skip5} = MangoDatabase.list_indexes(@db_name, skip: 5)
    {:ok, skip6} = MangoDatabase.list_indexes(@db_name, skip: 6)
    {:ok, skip100} = MangoDatabase.list_indexes(@db_name, skip: 100)
    {:ok, limit1} = MangoDatabase.list_indexes(@db_name, limit: 10_000_000)

    assert length(limit2) == 2
    assert length(limit5_skip4) == 2
    assert length(skip5) == 1
    assert Enum.empty?(skip6)
    assert Enum.empty?(skip100)
    assert length(limit1) == 6

    {:error, bad_skip} = MangoDatabase.list_indexes(@db_name, skip: -1)
    assert bad_skip.status_code == 500

    {:error, limit0} = MangoDatabase.list_indexes(@db_name, limit: 0)
    assert limit0.status_code == 500
  end
end
