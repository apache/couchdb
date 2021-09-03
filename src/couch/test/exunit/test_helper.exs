ExUnit.configure(formatters: [JUnitFormatter, ExUnit.CLIFormatter])

Kernel.function_exported?(:logger, :set_primary_config, 2) and
  :logger.set_primary_config(:level, :info)

ExUnit.start()
