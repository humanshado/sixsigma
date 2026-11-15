@echo off
rem Double-click to start Six Sigma Studio on Windows.
cd /d "%~dp0"
set "RSCRIPT="
for /f "delims=" %%i in ('where Rscript 2^>nul') do if not defined RSCRIPT set "RSCRIPT=%%i"
if not defined RSCRIPT (
  for /d %%d in ("%ProgramFiles%\R\R-*") do if exist "%%d\bin\Rscript.exe" set "RSCRIPT=%%d\bin\Rscript.exe"
)
if not defined RSCRIPT (
  echo R was not found. Install R 4.1 or later from https://cran.r-project.org/bin/windows/base/ and try again.
  pause
  exit /b 1
)
echo Using %RSCRIPT%
"%RSCRIPT%" launch.R
if errorlevel 1 pause
