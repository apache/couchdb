defmodule Couch.HTTP.CTrace.Sans.Test do
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

    :config.set_boolean('tracing', 'enabled', true, false)
    :config.set('tracing.samplers', 'database-info.read', 'all', false)
    :config.set('tracing.database-info.read', 'all', ~C"(#{method := M}) when M == 'GET' -> []", false)

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
    :ok = :meck.new([:chttpd_plugin], [:passthrough])
    :ok = :meck.new([:ctrace], [:passthrough])

    on_exit(fn ->
      :meck.unload()
    end)

    {context, setup}
  end

  describe "Tracing root spans :" do
    @tag setup: &__MODULE__.with_db_name/2
    test "span started", ctx do
      %{db_url: url, auth: auth, db_name: db_name} = ctx
      :meck.reset(:chttpd_plugin)
      assert match?(
        {:ok, 200, _, _},
        :test_request.get(url, [auth]))

      events = capture(:chttpd_plugin, :before_response, 4, 1)
      for http_req <- events do
        assert Record.is_record(http_req, :httpd), ~S"expecting #httpd{} record"
        assert has_context?(http_req), "expecting request_ctx"
        assert context(http_req).tracing, "expecting tracing to be disabled"
        assert has_span?(http_req), "expecting tracing_span"
        span = span(http_req)
        assert is_tuple(span), ~S"expecting #span{} record"
        assert valid_id?(:ctrace.trace_id(span)), "expecting valid trace_id"
        assert valid_id?(:ctrace.span_id(span)), "expecting valid span_id"
        assert :"database-info.read" == :ctrace.operation_name(span), "expecting correct operation name"
        tags = :ctrace.tags(span)
        assert Map.has_key?(tags, :"http.method"), "expecting to have 'http.method' tag"
        assert :GET == Map.get(tags, :"http.method")
        assert Map.has_key?(tags, :"http.url"), "expecting to have 'http.component' tag"
        assert '/' ++ String.to_charlist(db_name) == Map.get(tags, :"http.url")
      end
    end
  end

  describe "Tracing database spans :" do
    @tag setup: &__MODULE__.with_db_name/2
    test "span has reference to parrent", ctx do
      %{db_url: url, db_name: db_name, auth: auth} = ctx
      :meck.reset(:fabric2_fdb)
      :meck.reset(:ctrace)
      assert match?(
        {:ok, 200, _, _},
        :test_request.get(url, [auth]))

      events = capture(:fabric2_fdb, :transactional, 2, 1)
      for db <- events do
        assert has_context?(db), "expecting request_ctx"
        assert context(db).tracing, "expecting tracing to be disabled"
        assert has_span?(db), "expecting tracing_span"
        span = span(db)
        assert is_tuple(span), ~S"expecting #span{} record"
        assert valid_id?(:ctrace.trace_id(span)), "expecting valid trace_id"
        assert valid_id?(:ctrace.span_id(span)), "expecting valid span_id"
        tags = :ctrace.tags(span)
        assert Map.has_key?(tags, :"http.method"), "expecting to have 'http.method' tag"
        assert :GET == Map.get(tags, :"http.method")
        assert Map.has_key?(tags, :"http.url"), "expecting to have 'http.component' tag"
        assert '/' ++ String.to_charlist(db_name) == Map.get(tags, :"http.url")
      end
      events = capture(:ctrace, :finish_span, 1, 1)
      assert Enum.any?(events, fn event ->
        [] != :ctrace.refs(span(event))
      end), "at least one event should have reference to parrent"
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
  defp has_span?(%{__struct__: :request_ctx, trace_span: _}) do
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
  defp span(%{__struct__: :request_ctx, trace_span: span}) do
    span
  end

  defp valid_id?(subject) do
    is_integer(subject) and subject > 0
  end

end