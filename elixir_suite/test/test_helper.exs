ExUnit.start()

# TODO
#def random_db_name do
#  "asdf"
#end

defmodule CouchTestCase do
  use ExUnit.Case

  defmacro __using__(_opts) do
    quote do
      require Logger
      use ExUnit.Case

      setup context do
        db_name = if context[:with_db] != nil or context[:with_db_name] != nil do
          if context[:with_db] != nil and context[:with_db] != true do
            context[:with_db]
          else
            case context[:with_db_name] do
              nil -> random_db_name()
              true -> random_db_name()
              name -> name
            end
          end
        end

        if context[:with_db] != nil do
          {:ok, _} = create_db(db_name)

          on_exit(fn -> delete_db(db_name) end)
        end

        {:ok, db_name: db_name}
      end

      def random_db_name do
        time = :erlang.monotonic_time()
        umi = :erlang.unique_integer([:monotonic])
        "random-test-db-#{time}-#{umi}"
      end

      def create_db(db_name) do
        resp = Couch.put("/#{db_name}")
        assert resp.status_code == 201
        assert resp.body == %{"ok" => true}
        {:ok, resp}
      end

      def delete_db(db_name) do
        resp = Couch.delete("/#{db_name}")
        assert resp.status_code == 200
        assert resp.body == %{"ok" => true}
        {:ok, resp}
      end

      def create_doc(db_name, body) do
        resp = Couch.post("/#{db_name}", [body: body])
        assert resp.status_code == 201
        assert resp.body["ok"]
        {:ok, resp}
      end

      def sample_doc_foo do
        %{
          "_id": "foo",
          "bar": "baz"
        }
      end
    end
  end
end
