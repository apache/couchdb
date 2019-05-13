defmodule Couch.Session do
  @moduledoc """
  CouchDB session helpers.
  """

  @enforce_keys [:client, :cookie]
  defstruct [:client, :cookie, :base_url]

  def new(client, cookie) do
    %Couch.Session{client: client, cookie: cookie}
  end

  def new(client, cookie) do
    %Couch.Session{client: client, cookie: cookie}
  end

  def logout(sess) do
    headers = [
      "Content-Type": "application/x-www-form-urlencoded",
      "X-CouchDB-WWW-Authenticate": "Cookie",
      Cookie: sess.cookie
    ]

    sess.client.delete!("/_session", headers: headers)
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
    opts = Keyword.merge(opts, cookie: sess.cookie)
    opts = Keyword.merge(opts, base_url: sess.base_url)
    Couch.request(method, url, opts)
  end

  def go!(%Couch.Session{} = sess, method, url, opts) do
    opts = Keyword.merge(opts, cookie: sess.cookie)
    opts = Keyword.merge(opts, base_url: sess.base_url)
    sess.client.request!(method, url, opts)
  end
end

defmodule Couch do
  use HTTPotion.Base

  @moduledoc """
  CouchDB library to power test suite.
  """

  def process_url("http://" <> _ = url) do
    url
  end

  def process_url(url, options) do
    base_url = case Keyword.get(options, :base_url) do
      nil ->
        System.get_env("EX_COUCH_URL") || "http://127.0.0.1:15984"
      base_url ->
        base_url
    end
    base_url <> url
  end

  def process_request_headers(headers, options) do
    headers = Keyword.put(headers, :"User-Agent", "couch-potion")

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

  def process_request_body(body) do
    if is_map(body) do
      :jiffy.encode(body)
    else
      body
    end
  end

  def process_response_body(headers, body) do
    content_type = headers[:"Content-Type"]

    if !!content_type and String.match?(content_type, ~r/application\/json/) do
      body |> IO.iodata_to_binary() |> :jiffy.decode([:return_maps])
    else
      process_response_body(body)
    end
  end

  def login(userinfo) do
    [user, pass] = String.split(userinfo, ":", parts: 2)
    login(user, pass)
  end

  def login(user, pass) do
    login(nil, user, pass)
  end
  
  def login(base_url, user, pass) do
    resp = Couch.post("/_session", body: %{:username => user, :password => pass}, base_url: base_url)
    true = resp.body["ok"]
    cookie = resp.headers[:"set-cookie"]
    [token | _] = String.split(cookie, ";")
    %Couch.Session{
      client: __MODULE__,
      cookie: token,
      base_url: base_url
    }
  end

  # Anther HACK: Until we can get process_request_headers/2 merged
  # upstream.
  @spec process_arguments(atom, String.t(), [{atom(), any()}]) :: %{}
  def process_arguments(method, url, options) do
    options = process_options(options)

    body = Keyword.get(options, :body, "")

    headers =
      Keyword.merge(
        Application.get_env(:httpotion, :default_headers, []),
        Keyword.get(options, :headers, [])
      )

    timeout =
      Keyword.get(
        options,
        :timeout,
        Application.get_env(:httpotion, :default_timeout, 5000)
      )

    ib_options =
      Keyword.merge(
        Application.get_env(:httpotion, :default_ibrowse, []),
        Keyword.get(options, :ibrowse, [])
      )

    follow_redirects =
      Keyword.get(
        options,
        :follow_redirects,
        Application.get_env(:httpotion, :default_follow_redirects, false)
      )

    ib_options =
      if stream_to = Keyword.get(options, :stream_to),
        do:
          Keyword.put(
            ib_options,
            :stream_to,
            spawn(__MODULE__, :transformer, [stream_to, method, url, options])
          ),
        else: ib_options

    ib_options =
      if user_password = Keyword.get(options, :basic_auth) do
        {user, password} = user_password
        Keyword.put(ib_options, :basic_auth, {to_charlist(user), to_charlist(password)})
      else
        ib_options
      end

    %{
      method: method,
      url: url |> to_string |> process_url(options) |> to_charlist,
      body: body |> process_request_body,
      headers:
        headers
        |> process_request_headers(options)
        |> Enum.map(fn {k, v} -> {to_charlist(k), to_charlist(v)} end),
      timeout: timeout,
      ib_options: ib_options,
      follow_redirects: follow_redirects
    }
  end
end
