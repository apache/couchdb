@ECHO OFF

cd %~dp0
call mix local.hex --force
call mix local.rebar --force
call mix deps.get
call mix test --trace --exclude without_quorum_test --exclude with_quorum_test
