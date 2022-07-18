defmodule Couch.Test.Fabric do
  use Couch.Test.ExUnit.Case
  alias Couch.Test.Utils

  alias Couch.Test.Setup

  alias Couch.Test.Setup.Step

  import Couch.DBTest

  import Utils

  @admin {:user_ctx, user_ctx(roles: ["_admin"])}

  def with_db(context, setup) do
    setup =
      setup
      |> Setup.Common.with_db()
      |> Setup.run()

    context =
      Map.merge(context, %{
        db_name: setup |> Setup.get(:db) |> Step.Create.DB.name()
      })

    {context, setup}
  end

  describe "Fabric miscellaneous API" do
    @describetag setup: &__MODULE__.with_db/2
    test "Get inactive_index_files", ctx do
      {:ok, _rev} = update_doc(ctx.db_name, %{"_id" => "doc1"})

      design_doc = %{
        "_id" => "_design/test",
        "language" => "javascript",
        "views" => %{
          "view" => %{
            "map" => "function(doc){emit(doc._id, doc._rev)}"
          }
        }
      }

      {:ok, rev1} = update_doc(ctx.db_name, design_doc)
      wait_sig_update(ctx.db_name, "test", "")
      prev_active = get_active_sig(ctx.db_name, "test")

      updated_design_doc =
        put_in(design_doc, ["views", "view", "map"], "function(doc){emit(doc._id, null)}")

      {:ok, rev2} =
        update_doc(
          ctx.db_name,
          Map.put(updated_design_doc, "_rev", rev1)
        )

      assert rev1 != rev2
      wait_sig_update(ctx.db_name, "test", prev_active)

      {:ok, info} = :fabric.get_view_group_info(ctx.db_name, "_design/test")
      active = info[:signature]

      files = Enum.map(:fabric.inactive_index_files(ctx.db_name), &List.to_string/1)

      assert [] != files, "We should have some inactive"

      assert not Enum.any?(files, fn
               file_path -> String.contains?(file_path, active)
             end),
             "We are not suppose to return active views"

      assert Enum.all?(files, fn
               file_path -> String.contains?(file_path, prev_active)
             end),
             "We expect all files to contain previous active signature"
    end
  end

  defp update_doc(db_name, body) do
    json_body = :jiffy.decode(:jiffy.encode(body))

    case :fabric.update_doc(db_name, json_body, [@admin]) do
      {:ok, rev} ->
        {:ok, :couch_doc.rev_to_str(rev)}

      error ->
        error
    end
  end

  defp get_active_sig(db_name, ddoc_id) do
    {:ok, info} = :fabric.get_view_group_info(db_name, "_design/#{ddoc_id}")
    info[:signature]
  end

  defp wait_sig_update(db_name, ddoc_id, prev_active) do
    retry_until(fn ->
      get_active_sig(db_name, ddoc_id) != prev_active
    end)
  end
end
