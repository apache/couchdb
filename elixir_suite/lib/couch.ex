defmodule Couch do
  use HTTPotion.Base

  @moduledoc """
  CouchDB library to power test suite.
  """

  def process_url(url) do
    "http://localhost:15984" <> url
  end

  def process_request_headers(headers) do
    headers
    |> Dict.put(:"User-Agent", "couch-potion")
    |> Dict.put(:"Content-Type", "application/json")
  end

  def process_options(options) do
    Dict.put options, :basic_auth, {"adm", "pass"}
  end

  def process_request_body(body) do
    if is_map(body) do
      :jiffy.encode(body)
    else
      body
    end
  end

  def process_response_body(body) do
    body |> IO.iodata_to_binary |> :jiffy.decode([:return_maps])
  end

  def login(user, pass) do
    resp = Couch.post("/_session", body: %{:username => user, :password => pass})
    true = resp.body["ok"]
    resp.body
  end
end
