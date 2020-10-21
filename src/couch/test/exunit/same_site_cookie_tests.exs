defmodule SameSiteCookieTests do
  use CouchTestCase

  @moduletag :authentication

  def get_cookie(user, pass) do
    resp = Couch.post("/_session", body: %{:username => user, :password => pass})

    true = resp.body["ok"]
    resp.headers[:"set-cookie"]
  end

  @tag config: [{"admins", "jan", "apple"}, {"couch_httpd_auth", "same_site", "None"}]
  test "Set same_site None" do
    cookie = get_cookie("jan", "apple")
    assert cookie =~ "; SameSite=None"
  end

  @tag config: [{"admins", "jan", "apple"}, {"couch_httpd_auth", "same_site", ""}]
  test "same_site not set" do
    cookie = get_cookie("jan", "apple")
    assert cookie
    refute cookie =~ "; SameSite="
  end

  @tag config: [{"admins", "jan", "apple"}, {"couch_httpd_auth", "same_site", "Strict"}]
  test "Set same_site Strict" do
    cookie = get_cookie("jan", "apple")
    assert cookie =~ "; SameSite=Strict"
  end

  @tag config: [{"admins", "jan", "apple"}, {"couch_httpd_auth", "same_site", "Lax"}]
  test "Set same_site Lax" do
    cookie = get_cookie("jan", "apple")
    assert cookie =~ "; SameSite=Lax"
  end

  @tag config: [{"admins", "jan", "apple"}, {"couch_httpd_auth", "same_site", "Invalid"}]
  test "Set same_site invalid" do
    cookie = get_cookie("jan", "apple")
    assert cookie
    refute cookie =~ "; SameSite="
  end
end
