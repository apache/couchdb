defmodule RevStemmingTest do
  use CouchTestCase

  @moduletag :revs
  @moduletag kind: :single_node

  @moduledoc """
  This is a port of the rev_stemming.js suite
  """

  @new_limit 5

  @tag :with_db
  test "revs limit update", context do
    db_name = context[:db_name]

    resp = Couch.get("/#{db_name}/_revs_limit")
    assert resp.body == 1000

    create_rev_doc(db_name, "foo", @new_limit + 1)
    resp = Couch.get("/#{db_name}/foo?revs=true")
    assert length(resp.body["_revisions"]["ids"]) == @new_limit + 1

    resp =
      Couch.put("/#{db_name}/_revs_limit",
        body: "#{@new_limit}",
        headers: ["Content-type": "application/json"]
      )

    assert resp.status_code == 200

    create_rev_doc(db_name, "foo", @new_limit + 1)
    resp = Couch.get("/#{db_name}/foo?revs=true")
    assert length(resp.body["_revisions"]["ids"]) == @new_limit
  end

  @tag :with_db
  test "revs limit produces replication conflict ", context do
    db_name = context[:db_name]

    db_name_b = "#{db_name}_b"
    create_db(db_name_b)
    delete_db_on_exit([db_name_b])

    resp =
      Couch.put("/#{db_name}/_revs_limit",
        body: "#{@new_limit}",
        headers: ["Content-type": "application/json"]
      )

    assert resp.status_code == 200

    create_rev_doc(db_name, "foo", @new_limit + 1)
    resp = Couch.get("/#{db_name}/foo?revs=true")
    assert length(resp.body["_revisions"]["ids"]) == @new_limit

    # If you replicate after you make more edits than the limit, you'll
    # cause a spurious edit conflict.
    replicate(db_name, db_name_b)
    resp = Couch.get("/#{db_name_b}/foo?conflicts=true")
    assert not Map.has_key?(resp.body, "_conflicts")

    create_rev_doc(db_name, "foo", @new_limit - 1)

    # one less edit than limit, no conflict
    replicate(db_name, db_name_b)
    resp = Couch.get("/#{db_name_b}/foo?conflicts=true")
    assert not Map.has_key?(resp.body, "_conflicts")
    prev_conflicted_rev = resp.body["_rev"]

    # now we hit the limit
    create_rev_doc(db_name, "foo", @new_limit + 1)

    replicate(db_name, db_name_b)
    resp = Couch.get("/#{db_name_b}/foo?conflicts=true")
    assert Map.has_key?(resp.body, "_conflicts")

    conflicted_rev =
      resp.body["_conflicts"]
      |> Enum.at(0)

    # we have a conflict, but the previous replicated rev is always the losing
    # conflict
    assert conflicted_rev == prev_conflicted_rev
  end

  @tag :with_db
  test "revs limit is kept after compaction", context do
    db_name = context[:db_name]

    create_rev_doc(db_name, "bar", @new_limit + 1)
    resp = Couch.get("/#{db_name}/bar?revs=true")
    assert length(resp.body["_revisions"]["ids"]) == @new_limit + 1

    resp =
      Couch.put("/#{db_name}/_revs_limit",
        body: "#{@new_limit}",
        headers: ["Content-type": "application/json"]
      )

    assert resp.status_code == 200

    # We having already updated bar before setting the limit, so it's still got
    # a long rev history. compact to stem the revs.
    resp = Couch.get("/#{db_name}/bar?revs=true")
    assert length(resp.body["_revisions"]["ids"]) == @new_limit

    compact(db_name)

    # force reload because ETags don't honour compaction
    resp =
      Couch.get("/#{db_name}/bar?revs=true",
        headers: ["if-none-match": "pommes"]
      )

    assert length(resp.body["_revisions"]["ids"]) == @new_limit
  end

  # function to create a doc with multiple revisions
  defp create_rev_doc(db_name, id, num_revs) do
    resp = Couch.get("/#{db_name}/#{id}")

    doc =
      if resp.status_code == 200 do
        resp.body
      else
        %{_id: id, count: 0}
      end

    {:ok, resp} = create_doc(db_name, doc)
    create_rev_doc(db_name, id, num_revs, [Map.put(doc, :_rev, resp.body["rev"])])
  end

  defp create_rev_doc(db_name, id, num_revs, revs) do
    if length(revs) < num_revs do
      doc = %{_id: id, _rev: Enum.at(revs, -1)[:_rev], count: length(revs)}
      {:ok, resp} = create_doc(db_name, doc)

      create_rev_doc(
        db_name,
        id,
        num_revs,
        revs ++ [Map.put(doc, :_rev, resp.body["rev"])]
      )
    else
      revs
    end
  end

  def delete_db_on_exit(db_names) when is_list(db_names) do
    on_exit(fn ->
      Enum.each(db_names, fn name ->
        delete_db(name)
      end)
    end)
  end

end
