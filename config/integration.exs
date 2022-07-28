import Config

config :logger,
  backends: [:console],
  compile_time_purge_matching: [
    [level_lower_than: :debug]
  ],
  level: :debug

config :sasl,
  sasl_error_logger: false
