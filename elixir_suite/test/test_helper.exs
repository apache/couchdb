ExUnit.configure(exclude: [pending: true])
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
        {:ok, db_context} = set_db_context(context)
        {:ok, cfg_context} = set_config_context(context)
        {:ok, db_context ++ cfg_context}
      end

      def set_db_context(context) do
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

      def set_config_context(context) do
        if is_list(context[:config]) do
          Enum.each(context[:config], fn cfg ->
            set_config(cfg)
          end)
        end
        {:ok, []}
      end

      def random_db_name do
        time = :erlang.monotonic_time()
        umi = :erlang.unique_integer([:monotonic])
        "random-test-db-#{time}-#{umi}"
      end

      def set_config({section, key, value}) do
        resp = Couch.get("/_membership")
        existing = Enum.map(resp.body["all_nodes"], fn node ->
          url = "/_node/#{node}/_config/#{section}/#{key}"
          headers = ["X-Couch-Persist": "false"]
          body = :jiffy.encode(value)
          resp = Couch.put(url, headers: headers, body: body)
          assert resp.status_code == 200
          {node, resp.body}
        end)
        on_exit(fn ->
          Enum.each(existing, fn {node, prev_value} ->
            if prev_value != "" do
              url = "/_node/#{node}/_config/#{section}/#{key}"
              headers = ["X-Couch-Persist": "false"]
              body = :jiffy.encode(prev_value)
              resp = Couch.put(url, headers: headers, body: body)
              assert resp.status_code == 200
            else
              url = "/_node/#{node}/_config/#{section}/#{key}"
              headers = ["X-Couch-Persist": "false"]
              resp = Couch.delete(url, headers: headers)
              assert resp.status_code == 200
            end
          end)
        end)
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
