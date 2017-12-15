defmodule Couch do
  use HTTPotion.Base

  @moduledoc """
  CouchDB library to power test suite.
  """

  def process_url(url) do
    "http://localhost:15984" <> url
  end

  def process_request_headers(headers) do
    headers = Keyword.put(headers, :"User-Agent", "couch-potion")
    if headers[:"Content-Type"] do
      headers
    else
      Keyword.put(headers, :"Content-Type", "application/json")
    end
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

  def process_response_body(headers, body) do
    if String.match?(headers[:"Content-Type"], ~r/application\/json/) do
      body |> IO.iodata_to_binary |> :jiffy.decode([:return_maps])
    else
      process_response_body(body)
    end
  end

  def login(user, pass) do
    resp = Couch.post("/_session", body: %{:username => user, :password => pass})
    true = resp.body["ok"]
    resp.body
  end

  # HACK: this is here until this commit lands in a release
  # https://github.com/myfreeweb/httpotion/commit/f3fa2f0bc3b9b400573942b3ba4628b48bc3c614
  def handle_response(response) do
    case response do
      { :ok, status_code, headers, body, _ } ->
        processed_headers = process_response_headers(headers)
        %HTTPotion.Response{
          status_code: process_status_code(status_code),
          headers: processed_headers,
          body: process_response_body(processed_headers, body)
        }
      { :ok, status_code, headers, body } ->
        processed_headers = process_response_headers(headers)
        %HTTPotion.Response{
          status_code: process_status_code(status_code),
          headers: processed_headers,
          body: process_response_body(processed_headers, body)
        }
      { :ibrowse_req_id, id } ->
        %HTTPotion.AsyncResponse{ id: id }
      { :error, { :conn_failed, { :error, reason }}} ->
        %HTTPotion.ErrorResponse{ message: error_to_string(reason)}
      { :error, :conn_failed } ->
        %HTTPotion.ErrorResponse{ message: "conn_failed"}
      { :error, reason } ->
        %HTTPotion.ErrorResponse{ message: error_to_string(reason)}
    end
  end
end
