defmodule FormSubmitTest do
  use CouchTestCase

  @moduletag :form_submit
  @moduletag kind: :single_node

  @moduledoc """
  Test that form submission is invalid
  This is a port of form_submit.js
  """

  @tag :with_db
  test "form submission gives back invalid content-type", context do
    headers = [
      Referer: "http://127.0.0.1:15984",
      "Content-Type": "application/x-www-form-urlencoded"
    ]

    body = %{}

    %{:body => response_body, :status_code => status_code} =
      Couch.post("/#{context[:db_name]}/baz", headers: headers, body: body)

    %{"error" => error, "reason" => reason} = response_body

    assert status_code == 415
    assert error == "bad_content_type"
    assert reason == "Content-Type must be multipart/form-data"
  end
end
