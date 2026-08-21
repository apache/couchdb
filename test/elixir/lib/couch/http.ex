defmodule Couch.Response do
  @moduledoc """
  Response to request
  """
  defstruct status_code: nil, headers: %{}, body: ""
  def success?(%__MODULE__{status_code: code}), do: code in 200..299
  def success?(_), do: false
end

defmodule Couch.ErrorResponse do
  @moduledoc """
  Error response
  """
  defstruct message: ""
end

defmodule Couch.AsyncResponse do
  @moduledoc """
  Response to streaming request iniated by the stream_to: pid option
  """
  defstruct [:id]
end

defmodule Couch.AsyncHeaders do
  @moduledoc """
  Streaming header response
  """
  defstruct [:id, :status_code, :headers]
end

defmodule Couch.AsyncChunk do
  @moduledoc """
  Streaming body chunk
  """
  defstruct [:id, :chunk]
end

defmodule Couch.AsyncEnd do
  @moduledoc """
  Stream end
  """
  defstruct [:id]
end

defmodule Couch.Http do
  @moduledoc """

  Small http client built on gun. It looks a bit odd because it's trying to
  mimick the now removed httpotion client shape a bit. Some of the patterns
  here are also copied from couch_gun.erl. The one differense if we keep a
  connected process cached in the process dict to speed up test runs here.

  Normal requests return Couch.Response | Couch.ErrorResponse results.

  Response headers are a plain map with gun's lowercase binary header names
  as keys; a repeated header collects its values into a list, in arrival
  order.

  Streaming requests should pass `stream_to: pid` as the option. Their response
  will be Couch.AsyncResponse then followed by Couch.AsyncChunk messages and
  finally Couch.AsyncEnd.

  Some requests options are:
   :body
   :headers
   :query
   :timeout
   :cookie
   :no_auth
  """

  @request_timeout 60_000
  @inactivity_timeout 55_000
  @attempts 3

  def base_url do
    System.get_env("EX_COUCH_URL") || "http://127.0.0.1:15984"
  end

  def process_url("http://" <> _ = url), do: url
  def process_url("https://" <> _ = url), do: url
  def process_url(url), do: base_url() <> url

  def request(method, url, options, body_mode) when body_mode in [:json, :raw] do
    ensure_gun_started()
    url = url |> to_string() |> process_url()
    url = append_query(url, Keyword.get(options, :query))
    method = method |> to_string() |> String.upcase()
    headers = build_headers(options)
    body = encode_body(Keyword.get(options, :body, ""))

    case Keyword.get(options, :stream_to) do
      nil ->
        sync_request(method, url, headers, body, options, body_mode)

      target when is_pid(target) ->
        async_request(method, url, headers, body, target)
    end
  end

  defp sync_request(method, url, headers, body, options, body_mode) do
    with {:ok, origin, path} <- parse_url(url),
         {:ok, status, resp_headers, resp_body} <-
           do_sync_request(origin, method, path, headers, body, options, 1) do
      resp_headers = headers_map(resp_headers)

      %Couch.Response{
        status_code: status,
        headers: resp_headers,
        body: process_body(body_mode, resp_headers, resp_body)
      }
    else
      {:error, reason} -> %Couch.ErrorResponse{message: error_message(reason)}
    end
  end

  defp do_sync_request(origin, method, path, headers, body, options, attempt) do
    timeout = Keyword.get(options, :timeout, @request_timeout)
    deadline = now_ms() + timeout
    conn = cached_conn(origin)
    stream = :gun.request(conn, method, path, headers, body, %{})
    mref = Process.monitor(conn)
    result = await_response(conn, stream, mref, deadline)
    Process.demonitor(mref, [:flush])

    case result do
      {:ok, _status, resp_headers, _body} = ok ->
        # If server closed the connection we drop it as well
        if close_after?(resp_headers), do: invalidate(origin, conn)
        ok

      {:error, reason} ->
        reason = norm_error(reason)
        # Teardown the cached connection on error and start fresh
        invalidate(origin, conn)

        if conn_lost?(reason) and attempt < @attempts do
          # Retry a few times on times or server start/stop race
          do_sync_request(origin, method, path, headers, body, options, attempt + 1)
        else
          {:error, reason}
        end
    end
  end

  defp await_response(conn, stream, mref, deadline) do
    case gun_await(conn, stream, mref, deadline) do
      {:inform, _status, _headers} ->
        # These are 1xx bits and such and we don't care about them
        await_response(conn, stream, mref, deadline)

      {:response, :fin, status, headers} ->
        {:ok, status, headers, ""}

      {:response, :nofin, status, headers} ->
        collect_body(conn, stream, mref, deadline, status, headers, [])

      {:error, _} = error ->
        error
    end
  end

  defp collect_body(conn, stream, mref, deadline, status, headers, acc) do
    case gun_await(conn, stream, mref, deadline) do
      {:data, :nofin, data} ->
        collect_body(conn, stream, mref, deadline, status, headers, [acc | data])

      {:data, :fin, data} ->
        {:ok, status, headers, IO.iodata_to_binary([acc | data])}

      {:trailers, _} ->
        # We don't care about trailers
        {:ok, status, headers, IO.iodata_to_binary(acc)}

      {:error, _} = error ->
        error
    end
  end

  defp gun_await(conn, stream, mref, deadline) do
    remaining = deadline - now_ms()

    if remaining <= 0 do
      {:error, :req_timedout}
    else
      :gun.await(conn, stream, min(remaining, @inactivity_timeout), mref)
    end
  end

  # Streaming stuff
  #
  # A helper relay process opens the connection, makes the request forward
  # response to the target as Couch.Async* messages.

  defp async_request(method, url, headers, body, target) do
    case parse_url(url) do
      {:ok, origin, path} ->
        relay = spawn(fn -> relay_init(target, origin, method, path, headers, body) end)
        %Couch.AsyncResponse{id: relay}

      {:error, reason} ->
        %Couch.ErrorResponse{message: error_message(reason)}
    end
  end

  defp relay_init(target, origin, method, path, headers, body) do
    tref = Process.monitor(target)
    conn = open_conn(origin)
    cref = Process.monitor(conn)
    stream = :gun.request(conn, method, path, headers, body, %{})
    relay_loop(%{target: target, conn: conn, stream: stream, tref: tref, cref: cref})
  end

  defp relay_loop(state) do
    %{target: target, conn: conn, stream: stream} = state

    receive do
      {:gun_inform, ^conn, ^stream, _status, _headers} ->
        # skip 1xx stuff
        relay_loop(state)

      {:gun_response, ^conn, ^stream, fin, status, headers} ->
        async_headers = %Couch.AsyncHeaders{
          id: self(),
          status_code: status,
          headers: headers_map(headers)
        }

        send(target, async_headers)
        if fin == :fin, do: relay_done(state), else: relay_loop(state)

      {:gun_data, ^conn, ^stream, fin, data} ->
        send(target, %Couch.AsyncChunk{id: self(), chunk: data})
        if fin == :fin, do: relay_done(state), else: relay_loop(state)

      {:gun_trailers, ^conn, ^stream, _trailers} ->
        # don't care about trailers
        relay_done(state)

      {:gun_error, ^conn, ^stream, _reason} ->
        relay_done(state)

      {:gun_error, ^conn, _reason} ->
        relay_done(state)

      {:DOWN, mref, :process, _pid, _reason} ->
        cond do
          mref == state.cref ->
            # connection died
            send(target, %Couch.AsyncEnd{id: self()})
            :ok

          mref == state.tref ->
            # target (test) process died, clean up
            close_conn(conn)
            :ok

          true ->
            relay_loop(state)
        end
    end
  end

  defp relay_done(state) do
    send(state.target, %Couch.AsyncEnd{id: self()})
    close_conn(state.conn)
    :ok
  end

  # Connection handling. This works for the test with one
  # test client and one server decently enough.
  defp cached_conn(origin) do
    key = {:couch_http_conn, origin}

    case Process.get(key) do
      pid when is_pid(pid) ->
        if Process.alive?(pid) do
          pid
        else
          Process.delete(key)
          cached_conn(origin)
        end

      nil ->
        conn = open_conn(origin)
        Process.put(key, conn)
        conn
    end
  end

  defp invalidate(origin, conn) do
    key = {:couch_http_conn, origin}
    if Process.get(key) == conn, do: Process.delete(key)
    close_conn(conn)
  end

  defp open_conn({transport, host, port}) do
    host_chars = String.to_charlist(host)
    # gun expects IP address tuples
    host_addr =
      case :inet.parse_strict_address(host_chars) do
        {:ok, addr} -> addr
        {:error, _} -> host_chars
      end

    opts = %{transport: transport, protocols: [:http], retry: 0}

    opts =
      case transport do
        :tls -> Map.put(opts, :tls_opts, [{:verify, :verify_none}])
        :tcp -> opts
      end

    {:ok, conn} = :gun.open(host_addr, port, opts)
    conn
  end

  defp close_conn(conn) do
    try do
      :gun.close(conn)
    catch
      _, _ -> :ok
    end
  end

  defp ensure_gun_started() do
    case Process.get(:couch_http_gun_started) do
      true ->
        :ok

      _ ->
        {:ok, _} = Application.ensure_all_started(:gun)
        Process.put(:couch_http_gun_started, true)
        :ok
    end
  end

  defp parse_url(url) do
    case URI.parse(url) do
      %URI{scheme: scheme, host: host} = uri
      when scheme in ["http", "https"] and is_binary(host) and host != "" ->
        transport = if scheme == "https", do: :tls, else: :tcp
        path = uri.path || "/"
        path = if uri.query, do: path <> "?" <> uri.query, else: path
        {:ok, {transport, host, uri.port}, path}

      _ ->
        {:error, :invalid_uri}
    end
  end

  defp append_query(url, query) when query == nil or query == [] or query == %{} do
    url
  end

  defp append_query(url, query) do
    sep = if String.contains?(url, "?"), do: "&", else: "?"
    url <> sep <> URI.encode_query(query)
  end

  defp encode_body(nil), do: ""
  defp encode_body(body) when is_map(body), do: :jiffy.encode(body, [:use_nil])
  defp encode_body(body), do: body

  defp build_headers(options) do
    headers =
      for {k, v} <- Keyword.get(options, :headers, []) do
        {k |> to_string() |> String.downcase(), to_string(v)}
      end

    headers =
      headers
      |> put_new_header("user-agent", "couch-potion")
      |> put_new_header("content-type", "application/json")

    case Keyword.get(options, :cookie) do
      nil -> set_auth(headers, options)
      cookie -> put_new_header(headers, "cookie", cookie)
    end
  end

  # Auth may come from the environtment test setup
  defp set_auth(headers, options) do
    conf_auth? =
      List.keymember?(headers, "authorization", 0) or
        List.keymember?(headers, "x-auth-couchdb-username", 0)

    if Keyword.get(options, :no_auth, false) or conf_auth? do
      headers
    else
      username = System.get_env("EX_USERNAME") || "adm"
      password = System.get_env("EX_PASSWORD") || "pass"
      credentials = Base.encode64("#{username}:#{password}")
      [{"authorization", "Basic #{credentials}"} | headers]
    end
  end

  defp put_new_header(headers, key, value) do
    if List.keymember?(headers, key, 0) do
      headers
    else
      [{key, value} | headers]
    end
  end

  # Response stuff

  defp process_body(:raw, _headers, body), do: body

  defp process_body(:json, headers, body) do
    content_type = headers["content-type"]

    json? =
      is_binary(content_type) and
        String.match?(content_type, ~r/application\/json/)

    if json? and body != "" do
      :jiffy.decode(body, [:return_maps, :use_nil])
    else
      body
    end
  end

  # Errors

  defp headers_map(headers) when is_list(headers) do
    Enum.reduce(headers, %{}, fn {name, value}, acc ->
      Map.update(acc, name, value, fn
        values when is_list(values) -> values ++ [value]
        value0 -> [value0, value]
      end)
    end)
  end

  defp norm_error({:stream_error, reason}), do: norm_error(reason)
  defp norm_error({:connection_error, reason}), do: norm_error(reason)
  defp norm_error({:down, {:shutdown, reason}}), do: norm_error(reason)
  defp norm_error({:down, reason}), do: norm_error(reason)
  defp norm_error({:shutdown, reason}), do: norm_error(reason)
  defp norm_error(reason), do: reason

  defp conn_lost?(:closed), do: true
  defp conn_lost?({:closed, _}), do: true
  defp conn_lost?(:normal), do: true
  defp conn_lost?(:shutdown), do: true
  defp conn_lost?(:noproc), do: true
  defp conn_lost?(:einval), do: true
  defp conn_lost?(:socket_closed_remotely), do: true
  defp conn_lost?(_), do: false

  defp close_after?(headers) do
    case List.keyfind(headers, "connection", 0) do
      {_, value} -> String.downcase(value) == "close"
      nil -> false
    end
  end

  defp error_message(:timeout), do: "req_timedout"
  defp error_message(:req_timedout), do: "req_timedout"
  defp error_message(reason) when is_atom(reason), do: Atom.to_string(reason)
  defp error_message(reason), do: inspect(reason)

  # Helpers
  defp now_ms(), do: System.monotonic_time(:millisecond)
end
