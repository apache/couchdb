defmodule CouchTestCase do
  use ExUnit.CaseTemplate

  using do
    quote do
      require Logger
      use ExUnit.Case

      import Couch.DBTest
    end
  end

  setup context do
    setup_funs = [
      &Couch.DBTest.set_db_context/1,
      &Couch.DBTest.set_config_context/1,
      &Couch.DBTest.set_user_context/1
    ]

    context =
      Enum.reduce(setup_funs, context, fn setup_fun, acc ->
        setup_fun.(acc)
      end)

    {:ok, context}
  end
end
