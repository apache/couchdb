# If build number detected assume we running on Jenkins
# and skip certain tests that fail on jenkins.
exclude =
  case System.get_env("BUILD_NUMBER") !== nil do
    true -> [:pending, :skip_on_jenkins]
    false -> [:pending]
  end

current_exclude = Keyword.get(ExUnit.configuration(), :exclude, [])

ExUnit.configure(
  exclude: Enum.uniq(exclude ++ current_exclude),
  formatters: [JUnitFormatter, ExUnit.CLIFormatter]
)

ExUnit.start()
