defmodule ConfigTest do
  use CouchTestCase

  @moduletag :config
  @moduletag kind: :single_node

  @moduledoc """
  Test CouchDB config API
  This is a port of the config.js suite
  """

  setup do
    # TODO: switch this to _local when that's landed
    config_url = "/_node/node1@127.0.0.1/_config"
    resp = Couch.get(config_url)
    assert resp.status_code == 200
    {:ok, config: resp.body, config_url: config_url}
  end

  def set_config(context, section, key, val) do
    set_config(context, section, key, val, 200)
  end

  def set_config(context, section, key, val, status_assert) do
    url = "#{context[:config_url]}/#{section}/#{key}"
    headers = ["X-Couch-Persist": "false"]
    resp = Couch.put(url, headers: headers, body: :jiffy.encode(val))

    if status_assert do
      assert resp.status_code == status_assert
    end

    resp.body
  end

  def get_config(context, section) do
    get_config(context, section, nil, 200)
  end

  def get_config(context, section, key) do
    get_config(context, section, key, 200)
  end

  def get_config(context, section, key, status_assert) do
    url =
      if key do
        "#{context[:config_url]}/#{section}/#{key}"
      else
        "#{context[:config_url]}/#{section}"
      end

    resp = Couch.get(url)

    if status_assert do
      assert resp.status_code == status_assert
    end

    resp.body
  end

  def delete_config(context, section, key) do
    delete_config(context, section, key, 200)
  end

  def delete_config(context, section, key, status_assert) do
    url = "#{context[:config_url]}/#{section}/#{key}"
    resp = Couch.delete(url)

    if status_assert do
      assert resp.status_code == status_assert
    end
  end

  # TODO: port sever_port tests from config.js
  @tag :pending
  test "CouchDB respects configured protocols"

  test "Standard config options are present", context do
    assert context[:config]["couchdb"]["database_dir"]
    assert context[:config]["log"]["level"]
  end

  test "Settings can be altered with undefined whitelist allowing any change", context do
    refute context["config"]["httpd"]["config_whitelist"], "Default whitelist is empty"
    set_config(context, "test", "foo", "bar")
    assert get_config(context, "test")["foo"] == "bar"
    assert get_config(context, "test", "foo") == "bar"
  end

  test "Server-side password hashing, and raw updates disabling that", context do
    plain_pass = "s3cret"
    set_config(context, "admins", "administrator", plain_pass)
    assert Couch.login("administrator", plain_pass)
    hash_pass = get_config(context, "admins", "administrator")

    assert Regex.match?(~r/^-pbkdf2-/, hash_pass) or
             Regex.match?(~r/^-hashed-/, hash_pass)

    delete_config(context, "admins", "administrator")
    assert Couch.delete("/_session").body["ok"]
  end

  @tag :pending
  test "PORT `BUGGED` ?raw tests from config.js"

  test "Non-term whitelist values allow further modification of the whitelist", context do
    val = "!This is an invalid Erlang term!"
    set_config(context, "httpd", "config_whitelist", val)
    assert val == get_config(context, "httpd", "config_whitelist")
    delete_config(context, "httpd", "config_whitelist")
  end

  test "Non-list whitelist values allow further modification of the whitelist", context do
    val = "{[yes, a_valid_erlang_term, but_unfortunately, not_a_list]}"
    set_config(context, "httpd", "config_whitelist", val)
    assert val == get_config(context, "httpd", "config_whitelist")
    delete_config(context, "httpd", "config_whitelist")
  end

  test "Keys not in the whitelist may not be modified", context do
    val = "[{httpd,config_whitelist}, {test,foo}]"
    set_config(context, "httpd", "config_whitelist", val)
    assert val == get_config(context, "httpd", "config_whitelist")
    set_config(context, "test", "foo", "PUT to whitelisted config variable")
    delete_config(context, "test", "foo")
  end

  test "Non-2-tuples in the whitelist are ignored", context do
    val =
      "[{httpd,config_whitelist}, these, {are}, {nOt, 2, tuples}, [so], [they, will], [all, become, noops], {test,foo}]"

    set_config(context, "httpd", "config_whitelist", val)
    assert val == get_config(context, "httpd", "config_whitelist")
    set_config(context, "test", "foo", "PUT to whitelisted config variable")
    delete_config(context, "test", "foo")
  end

  test "Atoms, binaries, and strings suffice as whitelist sections and keys.", context do
    vals = ["{test,foo}", "{\"test\",\"foo\"}", "{<<\"test\">>,<<\"foo\">>}"]

    Enum.each(vals, fn pair ->
      set_config(
        context,
        "httpd",
        "config_whitelist",
        "[{httpd,config_whitelist}, #{pair}"
      )

      pair_format =
        case String.at(pair, 1) do
          "t" -> "tuple"
          "\"" -> "string"
          "<" -> "binary"
        end

      set_config(context, "test", "foo", "PUT with #{pair_format}")
      delete_config(context, "test", "foo")
    end)

    delete_config(context, "httpd", "config_whitelist")
  end

  test "Blacklist is functional", context do
    sections = [
      "daemons",
      "external",
      "httpd_design_handlers",
      "httpd_db_handlers",
      "native_query_servers",
      "os_daemons",
      "query_servers"
    ]

    Enum.each(sections, fn section ->
      set_config(context, section, "wohali", "rules", 403)
    end)
  end

  test "Reload config", context do
    url = "#{context[:config_url]}/_reload"
    resp = Couch.post(url)

    assert resp.status_code == 200
  end
end
