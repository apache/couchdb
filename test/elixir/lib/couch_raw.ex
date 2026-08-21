defmodule Rawresp do
  @moduledoc """
  HTTP client that provides raw response as result. Same as `Couch` but
  response bodies are returned as-is, without JSON decoding.
  """

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
  def options(url, opts \\ []), do: request(:options, url, opts)

  def request(method, url, opts \\ []) do
    Couch.Http.request(method, url, opts, :raw)
  end

  def request!(method, url, opts \\ []) do
    case request(method, url, opts) do
      %Couch.ErrorResponse{message: message} ->
        raise "HTTP request failed: #{method} #{url}: #{message}"

      resp ->
        resp
    end
  end
end
