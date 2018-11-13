@ECHO OFF

cd %~dp0
call mix deps.get
call mix test --trace
