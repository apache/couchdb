defmodule Couch.Session do
  @moduledoc """
  CouchDB session helpers.
  """

  defstruct [:cookie, :error]

  def new(cookie, error \\ "") do
    %Couch.Session{cookie: cookie, error: error}
  end

  def logout(sess) do
    headers = [
      "Content-Type": "application/x-www-form-urlencoded",
      "X-CouchDB-WWW-Authenticate": "Cookie",
      Cookie: sess.cookie
    ]

    Couch.delete!("/_session", headers: headers)
  end

  def info(sess) do
    headers = [
      "Content-Type": "application/x-www-form-urlencoded",
      "X-CouchDB-WWW-Authenticate": "Cookie",
      Cookie: sess.cookie
    ]

    Couch.get("/_session", headers: headers).body
  end

  def get(sess, url, opts \\ []), do: go(sess, :get, url, opts)
  def get!(sess, url, opts \\ []), do: go!(sess, :get, url, opts)
  def put(sess, url, opts \\ []), do: go(sess, :put, url, opts)
  def put!(sess, url, opts \\ []), do: go!(sess, :put, url, opts)
  def post(sess, url, opts \\ []), do: go(sess, :post, url, opts)
  def post!(sess, url, opts \\ []), do: go!(sess, :post, url, opts)
  def delete(sess, url, opts \\ []), do: go(sess, :delete, url, opts)
  def delete!(sess, url, opts \\ []), do: go!(sess, :delete, url, opts)

  # Skipping head/patch/options for YAGNI. Feel free to add
  # if the need arises.
  def go(%Couch.Session{} = sess, method, url, opts) do
    parse_response = Keyword.get(opts, :parse_response, true)

    opts =
      opts
      |> Keyword.merge(cookie: sess.cookie)
      |> Keyword.delete(:parse_response)

    if parse_response do
      Couch.request(method, url, opts)
    else
      Rawresp.request(method, url, opts)
    end
  end

  def go!(%Couch.Session{} = sess, method, url, opts) do
    parse_response = Keyword.get(opts, :parse_response, true)

    opts =
      opts
      |> Keyword.merge(cookie: sess.cookie)
      |> Keyword.delete(:parse_response)

    if parse_response do
      Couch.request!(method, url, opts)
    else
      Rawresp.request!(method, url, opts)
    end
  end
end

defmodule Couch do
  @moduledoc """
  CouchDB library to power test suite.
  """

  defdelegate process_url(url), to: Couch.Http

  def get(url, opts \\ []), do: request(:get, url, opts)
  def get!(url, opts \\ []), do: request!(:get, url, opts)
  def put(url, opts \\ []), do: request(:put, url, opts)
  def put!(url, opts \\ []), do: request!(:put, url, opts)
  def post(url, opts \\ []), do: request(:post, url, opts)
  def post!(url, opts \\ []), do: request!(:post, url, opts)
  def delete(url, opts \\ []), do: request(:delete, url, opts)
  def delete!(url, opts \\ []), do: request!(:delete, url, opts)
  def head(url, opts \\ []), do: request(:head, url, opts)
  def head!(url, opts \\ []), do: request!(:head, url, opts)

  def request(method, url, opts \\ []) do
    Couch.Http.request(method, url, opts, :json)
  end

  def request!(method, url, opts \\ []) do
    case request(method, url, opts) do
      %Couch.ErrorResponse{message: message} ->
        raise "HTTP request failed: #{method} #{url}: #{message}"

      resp ->
        resp
    end
  end

  def login(userinfo) do
    [user, pass] = String.split(userinfo, ":", parts: 2)
    login(user, pass)
  end

  def login(user, pass, expect \\ :success) do
    resp = Couch.post("/_session", body: %{:username => user, :password => pass})

    if expect == :success do
      true = resp.body["ok"]
      cookie = resp.headers["set-cookie"]
      [token | _] = String.split(cookie, ";")
      %Couch.Session{cookie: token}
    else
      true = Map.has_key?(resp.body, "error")
      %Couch.Session{error: resp.body["error"]}
    end
  end
end
