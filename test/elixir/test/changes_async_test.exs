defmodule ChangesAsyncTest do
  use CouchTestCase

  @moduletag :changes
  @moduletag kind: :single_node

  @moduledoc """
  Test CouchDB /{db}/_changes
  """

  @tag :with_db
  test "live changes", context do
    db_name = context[:db_name]
    test_changes(db_name, "live")
  end

  @tag :with_db
  test "continuous changes", context do
    db_name = context[:db_name]
    test_changes(db_name, "continuous")
  end

  @tag :with_db
  test "longpoll changes", context do
    db_name = context[:db_name]

    check_empty_db(db_name)

    create_doc(db_name, sample_doc_foo())

    req_id =
      Couch.get("/#{db_name}/_changes?feed=longpoll",
        stream_to: self()
      )

    changes = process_response(req_id.id, &parse_chunk/1)
    {changes_length, last_seq_prefix} = parse_changes_response(changes)
    assert changes_length == 1, "db should not be empty"
    assert last_seq_prefix == "1-", "seq must start with 1-"

    last_seq = changes["last_seq"]
    {:ok, worker_pid} = HTTPotion.spawn_link_worker_process(Couch.process_url(""))

    req_id =
      Couch.get("/#{db_name}/_changes?feed=longpoll&since=#{last_seq}",
        stream_to: self(),
        direct: worker_pid
      )

    :ok = wait_for_headers(req_id.id, 200)

    create_doc_bar(db_name, "bar")

    {changes_length, last_seq_prefix} =
      req_id.id
      |> process_response(&parse_chunk/1)
      |> parse_changes_response()

    assert changes_length == 1, "should return one change"
    assert last_seq_prefix == "2-", "seq must start with 2-"

    req_id =
      Couch.get("/#{db_name}/_changes?feed=longpoll&since=now",
        stream_to: self(),
        direct: worker_pid
      )

    :ok = wait_for_headers(req_id.id, 200)

    create_doc_bar(db_name, "barzzzz")

    changes = process_response(req_id.id, &parse_chunk/1)
    {changes_length, last_seq_prefix} = parse_changes_response(changes)
    assert changes_length == 1, "should return one change"
    assert Enum.at(changes["results"], 0)["id"] == "barzzzz"
    assert last_seq_prefix == "3-", "seq must start with 3-"
  end

  @tag :with_db
  test "eventsource changes", context do
    db_name = context[:db_name]

    check_empty_db(db_name)

    create_doc(db_name, sample_doc_foo())
    {:ok, worker_pid} = HTTPotion.spawn_link_worker_process(Couch.process_url(""))

    req_id =
      Rawresp.get("/#{db_name}/_changes?feed=eventsource&timeout=500",
        stream_to: self(),
        direct: worker_pid
      )

    :ok = wait_for_headers(req_id.id, 200)

    create_doc_bar(db_name, "bar")

    changes = process_response(req_id.id, &parse_event/1)

    assert length(changes) == 2
    assert Enum.at(changes, 0)["id"] == "foo"
    assert Enum.at(changes, 1)["id"] == "bar"

    HTTPotion.stop_worker_process(worker_pid)
  end

  @tag :with_db
  test "eventsource heartbeat", context do
    db_name = context[:db_name]

    {:ok, worker_pid} = HTTPotion.spawn_link_worker_process(Couch.process_url(""))

    req_id =
      Rawresp.get("/#{db_name}/_changes?feed=eventsource&heartbeat=10",
        stream_to: {self(), :once},
        direct: worker_pid
      )

    :ok = wait_for_headers(req_id.id, 200)
    beats = wait_for_heartbeats(req_id.id, 0, 3)
    assert beats == 3
    HTTPotion.stop_worker_process(worker_pid)
  end

  @tag :with_db
  test "longpoll filtered changes", context do
    db_name = context[:db_name]
    create_filters_view(db_name)

    create_doc(db_name, %{bop: "foom"})
    create_doc(db_name, %{bop: false})

    req_id =
      Couch.get("/#{db_name}/_changes?feed=longpoll&filter=changes_filter/bop",
        stream_to: self()
      )

    changes = process_response(req_id.id, &parse_chunk/1)
    {changes_length, last_seq_prefix} = parse_changes_response(changes)
    assert changes_length == 1, "db should not be empty"
    assert last_seq_prefix == "3-", "seq must start with 3-"

    last_seq = changes["last_seq"]
    # longpoll waits until a matching change before returning
    {:ok, worker_pid} = HTTPotion.spawn_link_worker_process(Couch.process_url(""))

    req_id =
      Couch.get(
        "/#{db_name}/_changes?feed=longpoll&filter=changes_filter/bop&since=#{last_seq}",
        stream_to: self(),
        direct: worker_pid
      )

    :ok = wait_for_headers(req_id.id, 200)
    create_doc(db_name, %{_id: "falsy", bop: ""})
    # Doc doesn't match the filter
    changes = process_response(req_id.id, &parse_chunk/1)
    assert changes == :timeout

    # Doc matches the filter
    create_doc(db_name, %{_id: "bingo", bop: "bingo"})
    changes = process_response(req_id.id, &parse_chunk/1)
    {changes_length, last_seq_prefix} = parse_changes_response(changes)
    assert changes_length == 1, "db should not be empty"
    assert last_seq_prefix == "5-", "seq must start with 5-"
    assert Enum.at(changes["results"], 0)["id"] == "bingo"
  end

  @tag :with_db
  test "continuous filtered changes", context do
    db_name = context[:db_name]
    create_filters_view(db_name)

    create_doc(db_name, %{bop: false})
    create_doc(db_name, %{_id: "bingo", bop: "bingo"})

    {:ok, worker_pid} = HTTPotion.spawn_link_worker_process(Couch.process_url(""))

    req_id =
      Rawresp.get(
        "/#{db_name}/_changes?feed=continuous&filter=changes_filter/bop&timeout=500",
        stream_to: self(),
        direct: worker_pid
      )

    :ok = wait_for_headers(req_id.id, 200)
    create_doc(db_name, %{_id: "rusty", bop: "plankton"})

    changes = process_response(req_id.id, &parse_changes_line_chunk/1)

    changes_ids =
      changes
      |> Enum.filter(fn p -> Map.has_key?(p, "id") end)
      |> Enum.map(fn p -> p["id"] end)

    assert Enum.member?(changes_ids, "bingo")
    assert Enum.member?(changes_ids, "rusty")
    assert length(changes_ids) == 2
  end

  @tag :with_db
  test "continuous filtered changes with doc ids", context do
    db_name = context[:db_name]
    doc_ids = %{doc_ids: ["doc1", "doc3", "doc4"]}

    create_doc(db_name, %{_id: "doc1", value: 1})
    create_doc(db_name, %{_id: "doc2", value: 2})

    {:ok, worker_pid} = HTTPotion.spawn_link_worker_process(Couch.process_url(""))

    req_id =
      Rawresp.post(
        "/#{db_name}/_changes?feed=continuous&timeout=500&filter=_doc_ids",
        body: doc_ids,
        headers: ["Content-Type": "application/json"],
        stream_to: self(),
        direct: worker_pid
      )

    :ok = wait_for_headers(req_id.id, 200)
    create_doc(db_name, %{_id: "doc3", value: 3})

    changes = process_response(req_id.id, &parse_changes_line_chunk/1)

    changes_ids =
      changes
      |> Enum.filter(fn p -> Map.has_key?(p, "id") end)
      |> Enum.map(fn p -> p["id"] end)

    assert Enum.member?(changes_ids, "doc1")
    assert Enum.member?(changes_ids, "doc3")
    assert length(changes_ids) == 2
  end

  @tag :with_db
  test "COUCHDB-1852", context do
    db_name = context[:db_name]

    create_doc(db_name, %{bop: "foom"})
    create_doc(db_name, %{bop: "foom"})
    create_doc(db_name, %{bop: "foom"})
    create_doc(db_name, %{bop: "foom"})

    resp = Couch.get("/#{db_name}/_changes")
    assert length(resp.body["results"]) == 4
    seq = Enum.at(resp.body["results"], 1)["seq"]

    {:ok, worker_pid} = HTTPotion.spawn_link_worker_process(Couch.process_url(""))

    # simulate an EventSource request with a Last-Event-ID header
    req_id =
      Rawresp.get(
        "/#{db_name}/_changes?feed=eventsource&timeout=100&since=0",
        headers: [Accept: "text/event-stream", "Last-Event-ID": seq],
        stream_to: self(),
        direct: worker_pid
      )

    changes = process_response(req_id.id, &parse_event/1)
    assert length(changes) == 2
  end

  defp wait_for_heartbeats(id, beats, expexted_beats) do
    if beats < expexted_beats do
      :ibrowse.stream_next(id)
      is_heartbeat = process_response(id, &parse_heartbeat/1)

      case is_heartbeat do
        :heartbeat -> wait_for_heartbeats(id, beats + 1, expexted_beats)
        :timeout -> beats
        _ -> wait_for_heartbeats(id, beats, expexted_beats)
      end
    else
      beats
    end
  end

  defp wait_for_headers(id, status, timeout \\ 1000) do
    receive do
      %HTTPotion.AsyncHeaders{id: ^id, status_code: ^status} ->
        :ok

      _ ->
        wait_for_headers(id, status, timeout)
    after
      timeout -> :timeout
    end
  end

  defp process_response(id, chunk_parser, timeout \\ 1000) do
    receive do
      %HTTPotion.AsyncChunk{id: ^id} = msg ->
        chunk_parser.(msg)

      _ ->
        process_response(id, chunk_parser, timeout)
    after
      timeout -> :timeout
    end
  end

  defp parse_chunk(msg) do
    msg.chunk |> IO.iodata_to_binary() |> :jiffy.decode([:return_maps])
  end

  defp parse_event(msg) do
    captures = Regex.scan(~r/data: (.*)/, msg.chunk)

    captures
    |> Enum.map(fn p -> Enum.at(p, 1) end)
    |> Enum.filter(fn p -> String.trim(p) != "" end)
    |> Enum.map(fn p ->
      p
      |> IO.iodata_to_binary()
      |> :jiffy.decode([:return_maps])
    end)
  end

  defp parse_heartbeat(msg) do
    is_heartbeat = Regex.match?(~r/event: heartbeat/, msg.chunk)

    if is_heartbeat do
      :heartbeat
    else
      :other
    end
  end

  defp parse_changes_response(changes) do
    {length(changes["results"]), String.slice(changes["last_seq"], 0..1)}
  end

  defp check_empty_db(db_name) do
    resp = Couch.get("/#{db_name}/_changes")
    assert resp.body["results"] == [], "db must be empty"
    assert String.at(resp.body["last_seq"], 0) == "0", "seq must start with 0"
  end

  defp test_changes(db_name, feed) do
    check_empty_db(db_name)
    {_, resp} = create_doc(db_name, sample_doc_foo())
    rev = resp.body["rev"]

    # TODO: retry_part
    resp = Couch.get("/#{db_name}/_changes")
    assert length(resp.body["results"]) == 1, "db must not be empty"
    assert String.at(resp.body["last_seq"], 0) == "1", "seq must start with 1"

    # increase timeout to 100 to have enough time 2 assemble
    # (seems like too little timeouts kill
    resp = Rawresp.get("/#{db_name}/_changes?feed=#{feed}&timeout=100")
    changes = parse_changes_line(resp.body)

    change = Enum.at(changes, 0)
    assert Enum.at(change["changes"], 0)["rev"] == rev

    # the sequence is not fully ordered and a complex structure now
    change = Enum.at(changes, 1)
    assert String.at(change["last_seq"], 0) == "1"

    # create_doc_bar(db_name,"bar")
    {:ok, worker_pid} = HTTPotion.spawn_worker_process(Couch.process_url(""))

    %HTTPotion.AsyncResponse{id: req_id} =
      Rawresp.get("/#{db_name}/_changes?feed=#{feed}&timeout=500",
        stream_to: self(),
        direct: worker_pid
      )

    :ok = wait_for_headers(req_id, 200)
    create_doc_bar(db_name, "bar")

    changes = process_response(req_id, &parse_changes_line_chunk/1)
    assert length(changes) == 3

    HTTPotion.stop_worker_process(worker_pid)
  end

  def create_doc_bar(db_name, id) do
    create_doc(db_name, %{:_id => id, :bar => 1})
  end

  defp parse_changes_line_chunk(msg) do
    parse_changes_line(msg.chunk)
  end

  defp parse_changes_line(body) do
    body_lines = String.split(body, "\n")

    body_lines
    |> Enum.filter(fn line -> line != "" end)
    |> Enum.map(fn line ->
      line |> IO.iodata_to_binary() |> :jiffy.decode([:return_maps])
    end)
  end

  defp create_filters_view(db_name) do
    dynamic_fun = """
    function(doc, req) {
      var field = req.query.field;
      return doc[field];
    }
    """

    userctx_fun = """
    function(doc, req) {
      var field = req.query.field;
      return doc[field];
    }
    """

    blah_fun = """
    function(doc) {
              if (doc._id == "blah") {
              emit(null, null);
            }
          }
    """

    ddoc = %{
      _id: "_design/changes_filter",
      filters: %{
        bop: "function(doc, req) { return (doc.bop);}",
        dynamic: dynamic_fun,
        userCtx: userctx_fun,
        conflicted: "function(doc, req) { return (doc._conflicts);}"
      },
      options: %{
        local_seq: true
      },
      views: %{
        local_seq: %{
          map: "function(doc) {emit(doc._local_seq, null)}"
        },
        blah: %{
          map: blah_fun
        }
      }
    }

    create_doc(db_name, ddoc)
  end
end

defmodule Rawresp do
  use HTTPotion.Base

  @request_timeout 60_000
  @inactivity_timeout 55_000

  def process_url("http://" <> _ = url) do
    url
  end

  def process_url(url) do
    base_url = System.get_env("EX_COUCH_URL") || "http://127.0.0.1:15984"
    base_url <> url
  end

  def process_request_headers(headers, _body, options) do
    headers =
      headers
      |> Keyword.put(:"User-Agent", "couch-potion")

    headers =
      if headers[:"Content-Type"] do
        headers
      else
        Keyword.put(headers, :"Content-Type", "application/json")
      end

    case Keyword.get(options, :cookie) do
      nil ->
        headers

      cookie ->
        Keyword.put(headers, :Cookie, cookie)
    end
  end

  def process_options(options) do
    options
    |> set_auth_options()
    |> set_inactivity_timeout()
    |> set_request_timeout()
  end

  def process_request_body(body) do
    if is_map(body) do
      :jiffy.encode(body)
    else
      body
    end
  end

  def set_auth_options(options) do
    if Keyword.get(options, :cookie) == nil do
      headers = Keyword.get(options, :headers, [])

      if headers[:basic_auth] != nil or headers[:authorization] != nil do
        options
      else
        username = System.get_env("EX_USERNAME") || "adm"
        password = System.get_env("EX_PASSWORD") || "pass"
        Keyword.put(options, :basic_auth, {username, password})
      end
    else
      options
    end
  end

  def set_inactivity_timeout(options) do
    Keyword.update(
      options,
      :ibrowse,
      [{:inactivity_timeout, @inactivity_timeout}],
      fn ibrowse ->
        Keyword.put_new(ibrowse, :inactivity_timeout, @inactivity_timeout)
      end
    )
  end

  def set_request_timeout(options) do
    timeout = Application.get_env(:httpotion, :default_timeout, @request_timeout)
    Keyword.put_new(options, :timeout, timeout)
  end

  def login(userinfo) do
    [user, pass] = String.split(userinfo, ":", parts: 2)
    login(user, pass)
  end

  def login(user, pass, expect \\ :success) do
    resp = Couch.post("/_session", body: %{:username => user, :password => pass})

    if expect == :success do
      true = resp.body["ok"]
      cookie = resp.headers[:"set-cookie"]
      [token | _] = String.split(cookie, ";")
      %Couch.Session{cookie: token}
    else
      true = Map.has_key?(resp.body, "error")
      %Couch.Session{error: resp.body["error"]}
    end
  end
end
