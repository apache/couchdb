defmodule Couch.Test.Setup.Common do
  @moduledoc """
  A set of common setup pipelines for reuse

  - httpd_with_admin - chttpd is started and new admin is created
  - httpd_with_db - httpd_with_admin and new database is created
  """
  alias Couch.Test.Setup.Step

  def httpd_with_admin(setup) do
    setup
      |> Step.Start.new(:start, extra_apps: [:chttpd])
      |> Step.User.new(:admin, roles: [:server_admin])
  end

  def httpd_with_db(setup) do
    setup
      |> httpd_with_admin()
      |> Step.Create.DB.new(:db)
  end

  def with_db(setup) do
    setup
      |> Step.Start.new(:start, extra_apps: [:fabric])
      |> Step.Create.DB.new(:db)
  end
end