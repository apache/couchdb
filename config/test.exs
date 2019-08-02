use Mix.Config

config :logger,
  backends: [:console],
  compile_time_purge_level: :debug,
  level: :debug

config :kernel,
  error_logger: false

config :sasl,
  sasl_error_logger: false
