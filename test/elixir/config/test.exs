config :logger,
  compile_time_purge_matching: [
    [level_lower_than: :debug]
  ]
