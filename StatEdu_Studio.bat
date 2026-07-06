@echo off
setlocal EnableExtensions EnableDelayedExpansion

cd /d "%~dp0"

echo Starting StatEdu Studio...
echo.

echo Closing existing StatEdu Studio process on port 7894, if any...
for /f "tokens=5" %%P in ('netstat -ano ^| findstr /C:":7894" ^| findstr /C:"LISTENING"') do (
    if not "%%P"=="0" taskkill /F /PID %%P >nul 2>nul
)
timeout /t 1 /nobreak >nul

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
echo StatEdu Studio stopped.
pause
