defmodule Rawresp do
  use HTTPotion.Base

  @moduledoc """
  HTTP client that provides raw response as result
  """
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
