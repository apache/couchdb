ExUnit.configure(exclude: [pending: true], formatters: [JUnitFormatter, ExUnit.CLIFormatter])
ExUnit.start()
Code.require_file("partition_helpers.exs", __DIR__)
