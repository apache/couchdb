defmodule Couch.Session do
  @moduledoc """
  CouchDB session helpers.
  """

  @enforce_keys [:cookie]
  defstruct [:cookie]

  def new(cookie) do
    %Couch.Session{cookie: cookie}
  end

  def logout(sess) do
    headers = [
      "Content-Type": "application/x-www-form-urlencoded",
      "X-CouchDB-WWW-Authenticate": "Cookie",
      Cookie: sess.cookie
    ]

    Couch.delete!("/_session", headers: headers)
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
    Couch.request(method, url, opts)
  end

  def go!(%Couch.Session{} = sess, method, url, opts) do
    opts = Keyword.merge(opts, cookie: sess.cookie)
    Couch.request!(method, url, opts)
  end
end

defmodule Couch do
  use HTTPotion.Base

  @moduledoc """
  CouchDB library to power test suite.
  """

  # These constants are supplied to the underlying HTTP client and control
  # how long we will wait before timing out a test. The inactivity timeout
  # specifically fires during an active HTTP response and defaults to 10_000
  # if not specified. We're defining it to a different value than the
  # request_timeout largely just so we know which timeout fired.
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
    resp = Couch.post("/_session", body: %{:username => user, :password => pass})
    true = resp.body["ok"]
    cookie = resp.headers[:"set-cookie"]
    [token | _] = String.split(cookie, ";")
    %Couch.Session{cookie: token}
  end

end
