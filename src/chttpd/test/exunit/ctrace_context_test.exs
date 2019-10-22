defmodule Couch.HTTP.CTrace.Context.Test do
  require Record

  use Couch.Test.ExUnit.Case
  alias Couch.Test.Utils

  alias Couch.Test.Setup

  alias Couch.Test.Setup.Step

  require Logger

  @moduletag capture_log: true

  def with_db_name(context, setup) do
    Application.put_env(:fabric, :eunit_run, true)
    setup =
      setup
        |> Step.Start.new(:start, extra_apps: [:chttpd, :couch_log])
        |> Step.User.new(:admin, roles: [:server_admin])
        |> Setup.run()

    base_url = setup |> Setup.get(:start) |> Step.Start.clustered_url()
    {user, pass} = setup
      |> Setup.get(:admin)
      |> Step.User.credentials()

    auth = {:basic_auth, {String.to_charlist(user), String.to_charlist(pass)}}
    db_name = Utils.random_name("db")
    db_url = String.to_charlist("#{base_url}/#{db_name}")
    context =
      Map.merge(context, %{
        db_name: db_name,
        db_url: db_url,
        base_url: base_url,
        user: user,
        auth: auth
      })

    assert match?(
      {:ok, 201, _, _},
      :test_request.put(db_url, [auth], ''))

    :ok = :meck.new([:fabric2_fdb], [:passthrough])
    :ok = :meck.new([:couch_httpd], [:passthrough])
    :ok = :meck.new([:chttpd], [:passthrough])
    :ok = :meck.new([:chttpd_plugin], [:passthrough])
    :ok = :meck.new([:ctrace], [:passthrough])

    on_exit(fn ->
      :meck.unload()
    end)

    {context, setup}
  end

  describe "Tracing context (tracing enabled) :" do
    @tag setup: &__MODULE__.with_db_name/2
    test "context is available in db structure", ctx do
      :config.set_boolean('tracing', 'enabled', true, false)
      %{db_url: url, auth: auth} = ctx
      :meck.reset(:fabric2_fdb)
      assert match?(
        {:ok, 200, _, _},
        :test_request.get(url, [auth]))

      events = capture(:fabric2_fdb, :transactional, 2, 1)
      for db <- events do
        assert has_context?(db), "expecting request_ctx"
        assert context(db).tracing, "expecting tracing to be enabled"
        assert has_span?(db), "expecting tracing_span"
        assert is_tuple(span(db))
      end
    end

    @tag setup: &__MODULE__.with_db_name/2
    test "context is available in after_request hook", ctx do
      :config.set_boolean('tracing', 'enabled', true, false)
      %{db_url: url, auth: auth} = ctx
      :meck.reset(:chttpd_plugin)
      assert match?(
        {:ok, 200, _, _},
        :test_request.get(url, [auth]))

      events = capture(:chttpd_plugin, :after_request, 2, 1)
      for http_req <- events do
        assert Record.is_record(http_req, :httpd), ~S"expecting #httpd{} record"
        assert has_context?(http_req), "expecting request_ctx"
        assert context(http_req).tracing, "expecting tracing to be enabled"
        assert has_span?(http_req), "expecting tracing_span"
        assert is_tuple(span(http_req))
      end
    end

    @tag setup: &__MODULE__.with_db_name/2
    test "context is available in before_request hook", ctx do
      :config.set_boolean('tracing', 'enabled', true, false)
      %{db_url: url, auth: auth} = ctx
      :meck.reset(:chttpd_plugin)
      assert match?(
        {:ok, 200, _, _},
        :test_request.get(url, [auth]))

      events = capture(:chttpd_plugin, :before_request, 1, 1)
      for http_req <- events do
        assert Record.is_record(http_req, :httpd), ~S"expecting #httpd{} record"
        assert has_context?(http_req), "expecting request_ctx"
        assert context(http_req).tracing, "expecting tracing to be enabled"
        assert has_span?(http_req), "expecting tracing_span"
        assert is_tuple(span(http_req))
      end
    end

    @tag setup: &__MODULE__.with_db_name/2
    test "context is available in before_response hook", ctx do
      :config.set_boolean('tracing', 'enabled', true, false)
      %{db_url: url, auth: auth} = ctx
      :meck.reset(:chttpd_plugin)
      assert match?(
        {:ok, 200, _, _},
        :test_request.get(url, [auth]))

      events = capture(:chttpd_plugin, :before_response, 4, 1)
      for http_req <- events do
        assert Record.is_record(http_req, :httpd), ~S"expecting #httpd{} record"
        assert has_context?(http_req), "expecting request_ctx"
        assert context(http_req).tracing, "expecting tracing to be enabled"
        assert has_span?(http_req), "expecting tracing_span"
        assert is_tuple(span(http_req))
      end
    end
  end

  describe "Tracing context (tracing disabled) :" do
    @tag setup: &__MODULE__.with_db_name/2
    test "context is available in db structure", ctx do
      :config.set_boolean('tracing', 'enabled', false, false)
      %{db_url: url, auth: auth} = ctx
      :meck.reset(:fabric2_fdb)
      assert match?(
        {:ok, 200, _, _},
        :test_request.get(url, [auth]))

      events = capture(:fabric2_fdb, :transactional, 2, 1)
      for db <- events do
        assert has_context?(db), "expecting request_ctx"
        assert not context(db).tracing, "expecting tracing to be disabled"
        assert has_span?(db), "expecting tracing_span"
        assert is_tuple(span(db))
      end
    end

    @tag setup: &__MODULE__.with_db_name/2
    test "context is available in after_request hook", ctx do
      :config.set_boolean('tracing', 'enabled', false, false)
      %{db_url: url, auth: auth} = ctx
      :meck.reset(:chttpd_plugin)
      assert match?(
        {:ok, 200, _, _},
        :test_request.get(url, [auth]))

      events = capture(:chttpd_plugin, :after_request, 2, 1)
      for http_req <- events do
        assert Record.is_record(http_req, :httpd), ~S"expecting #httpd{} record"
        assert has_context?(http_req), "expecting request_ctx"
        assert not context(http_req).tracing, "expecting tracing to be disabled"
        assert has_span?(http_req), "expecting tracing_span"
        assert is_tuple(span(http_req))
      end
    end

    @tag setup: &__MODULE__.with_db_name/2
    test "context is available in before_request hook", ctx do
      :config.set_boolean('tracing', 'enabled', false, false)
      %{db_url: url, auth: auth} = ctx
      :meck.reset(:chttpd_plugin)
      assert match?(
        {:ok, 200, _, _},
        :test_request.get(url, [auth]))

      events = capture(:chttpd_plugin, :before_request, 1, 1)
      for http_req <- events do
        assert Record.is_record(http_req, :httpd), ~S"expecting #httpd{} record"
        assert has_context?(http_req), "expecting request_ctx"
        assert not context(http_req).tracing, "expecting tracing to be disabled"
        assert has_span?(http_req), "expecting tracing_span"
        assert is_tuple(span(http_req))
      end
    end

    @tag setup: &__MODULE__.with_db_name/2
    test "context is available in before_response hook", ctx do
      :config.set_boolean('tracing', 'enabled', false, false)
      %{db_url: url, auth: auth} = ctx
      :meck.reset(:chttpd_plugin)
      assert match?(
        {:ok, 200, _, _},
        :test_request.get(url, [auth]))

      events = capture(:chttpd_plugin, :before_response, 4, 1)
      for http_req <- events do
        assert Record.is_record(http_req, :httpd), ~S"expecting #httpd{} record"
        assert has_context?(http_req), "expecting request_ctx"
        assert not context(http_req).tracing, "expecting tracing to be disabled"
        assert has_span?(http_req), "expecting tracing_span"
        assert is_tuple(span(http_req))
      end
    end

  end

  defp history(module, function, arity) do
    :meck.history(module)
      |> Enum.filter(fn event ->
          {_, fun, args} = elem(event, 1)
          function == fun and arity == length(args)
        end)
  end

  defp capture(module, function, arity, arg_idx) do
    history(module, function, arity)
      |> Enum.map(fn event ->
          {_, _, args} = elem(event, 1)
          Enum.at(args, arg_idx - 1) # zero based indexing
        end)
  end

  defp has_context?(http_req) when Record.is_record(http_req, :httpd) do
    contexts = :chttpd.contexts_to_list(http_req)
    Keyword.has_key?(contexts, :request_ctx)
  end
  defp has_context?(%{request_ctx: %{trace_span: _}}) do
    true
  end
  defp has_context?(_) do
    false
  end

  defp context(http_req) when Record.is_record(http_req, :httpd) do
    contexts = :chttpd.contexts_to_list(http_req)
    Keyword.get(contexts, :request_ctx)
  end
  defp context(%{request_ctx: %{trace_span: _} = request_ctx}) do
    request_ctx
  end

  defp has_span?(http_req) when Record.is_record(http_req, :httpd) do
    case context(http_req) do
      %{trace_span: _} -> true
      _ -> false
    end
  end
  defp has_span?(%{request_ctx: %{trace_span: _}}) do
    true
  end
  defp has_span?(_) do
    false
  end

  defp span(http_req) when Record.is_record(http_req, :httpd) do
    %{trace_span: span} = context(http_req)
    span
  end
  defp span(%{request_ctx: %{trace_span: span}}) do
    span
  end

end