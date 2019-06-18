# If build number detected assume we running on Jenkins
# and skip certain tests that fail on jenkins.
exclude =
  case System.get_env("BUILD_NUMBER") !== nil do
    true -> [pending: true, skip_on_jenkins: true]
    false -> [pending: true]
  end

ExUnit.configure(
  exclude: exclude,
  formatters: [JUnitFormatter, ExUnit.CLIFormatter]
)

ExUnit.start()
