ExUnit.configure(exclude: [pending: true])
ExUnit.start()

defmodule CouchTestCase do
  use ExUnit.Case

  defmacro __using__(_opts) do
    quote do
      require Logger
      use ExUnit.Case

      setup context do
        setup_funs = [
          &set_db_context/1,
          &set_config_context/1
        ]
        context = Enum.reduce(setup_funs, context, fn setup_fun, acc ->
          setup_fun.(acc)
        end)
        {:ok, context}
      end

      def set_db_context(context) do
        context = case context do
          %{:with_db_name => true} ->
            Map.put(context, :db_name, random_db_name())
          %{:with_db_name => db_name} when is_binary(db_name) ->
            Map.put(context, :db_name, db_name)
          %{:with_random_db => db_name} when is_binary(db_name) ->
            context
            |> Map.put(:db_name, random_db_name(db_name))
            |> Map.put(:with_db, true)
          %{:with_db => true} ->
            Map.put(context, :db_name, random_db_name())
          %{:with_db => db_name} when is_binary(db_name) ->
            Map.put(context, :db_name, db_name)
          _ ->
            context
        end

        if Map.has_key? context, :with_db do
          {:ok, _} = create_db(context[:db_name])
          on_exit(fn -> delete_db(context[:db_name]) end)
        end

        context
      end

      def set_config_context(context) do
        if is_list(context[:config]) do
          Enum.each(context[:config], fn cfg ->
            set_config(cfg)
          end)
        end
        context
      end

      def random_db_name do
        random_db_name("random-test-db")
      end

      def random_db_name(prefix) do
        time = :erlang.monotonic_time()
        umi = :erlang.unique_integer([:monotonic])
        "#{prefix}-#{time}-#{umi}"
      end

      def set_config({section, key, value}) do
        existing = set_config_raw(section, key, value)
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

      def set_config_raw(section, key, value) do
        resp = Couch.get("/_membership")
        Enum.map(resp.body["all_nodes"], fn node ->
          url = "/_node/#{node}/_config/#{section}/#{key}"
          headers = ["X-Couch-Persist": "false"]
          body = :jiffy.encode(value)
          resp = Couch.put(url, headers: headers, body: body)
          assert resp.status_code == 200
          {node, resp.body}
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

      def make_docs(id, count) do
        for i <- id..count do
          %{
            :_id => Integer.to_string(i),
            :integer => i,
            :string => Integer.to_string(i)
          }
        end
      end
    end
  end
end
