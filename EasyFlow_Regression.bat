@echo off
setlocal EnableExtensions EnableDelayedExpansion

cd /d "%~dp0"

echo Starting EasyFlow Regression...
echo.

echo Closing existing EasyFlow Regression process, if any...
powershell -NoProfile -Command "Get-CimInstance Win32_Process | Where-Object { $_.Name -eq 'Rscript.exe' -and $_.CommandLine -like '*run_app.R*' } | ForEach-Object { Stop-Process -Id $_.ProcessId -Force -ErrorAction SilentlyContinue }"
timeout /t 2 /nobreak >nul

set "RSCRIPT="

for /d %%D in ("C:\Program Files\R\R-*") do (
    if exist "%%D\bin\x64\Rscript.exe" set "RSCRIPT=%%D\bin\x64\Rscript.exe"
)

if not defined RSCRIPT (
    for /d %%D in ("%LocalAppData%\Programs\R\R-*") do (
        if exist "%%D\bin\x64\Rscript.exe" set "RSCRIPT=%%D\bin\x64\Rscript.exe"
    )
)

if not defined RSCRIPT (
    for /f "delims=" %%R in ('where Rscript 2^>nul') do (
        if not defined RSCRIPT set "RSCRIPT=%%R"
    )
)

if not defined RSCRIPT (
    echo Rscript was not found.
    echo Please install R from https://cran.r-project.org/bin/windows/base/
    echo.
    pause
    exit /b 1
)

echo Using Rscript: "%RSCRIPT%"
echo Keep this window open while using the app.
echo.

"%RSCRIPT%" run_app.R

echo.
echo EasyFlow Regression stopped.
pause
