@echo off

IF NOT EXIST "%CD%\.venv\Scripts\activate" (
  DEL /S "%CD%\.venv"
  python "-m" "venv" ".venv"
  "%CD%\.venv\Scripts\activate.bat"
  pip "install" "--upgrade" "pip"
  pip "install" "-r" "requirements.txt"
) ELSE (
  "%CD%\.venv\Scripts\activate.bat"
)
