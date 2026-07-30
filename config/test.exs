import Config

config :logger,
  compile_time_purge_matching: [
    [level_lower_than: :debug]
  ],
  level: :debug

config :kernel,
  error_logger: false

config :sasl,
  sasl_error_logger: false
