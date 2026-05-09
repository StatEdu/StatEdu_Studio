@echo off
setlocal EnableExtensions EnableDelayedExpansion

if /i not "%EASYFLOW_HIDDEN%"=="1" (
    set "EASYFLOW_LAUNCHER=%TEMP%\EasyFlow_Regression_Launcher_%RANDOM%.vbs"
    > "!EASYFLOW_LAUNCHER!" echo Set shell = CreateObject("WScript.Shell"^)
    >> "!EASYFLOW_LAUNCHER!" echo shell.Environment("PROCESS"^)("EASYFLOW_HIDDEN"^) = "1"
    >> "!EASYFLOW_LAUNCHER!" echo shell.Environment("PROCESS"^)("EASYFLOW_SILENT"^) = "1"
    >> "!EASYFLOW_LAUNCHER!" echo shell.Run """%~f0""", 0, False
    wscript //nologo "!EASYFLOW_LAUNCHER!"
    del "!EASYFLOW_LAUNCHER!" >nul 2>nul
    exit /b
)

cd /d "%~dp0"

set "RSCRIPT="

for /f "delims=" %%R in ('where Rscript 2^>nul') do (
    if not defined RSCRIPT set "RSCRIPT=%%R"
)

if not defined RSCRIPT (
    for /d %%D in ("C:\Program Files\R\R-*") do (
        if exist "%%D\bin\Rscript.exe" set "RSCRIPT=%%D\bin\Rscript.exe"
    )
)

if not defined RSCRIPT (
    for /d %%D in ("%LocalAppData%\Programs\R\R-*") do (
        if exist "%%D\bin\Rscript.exe" set "RSCRIPT=%%D\bin\Rscript.exe"
    )
)

if not defined RSCRIPT (
    powershell -NoProfile -WindowStyle Hidden -Command "Add-Type -AssemblyName PresentationFramework; [System.Windows.MessageBox]::Show('Rscript was not found. Please install R from https://cran.r-project.org/bin/windows/base/ and run EasyFlow Regression again.', 'EasyFlow Regression')"
    exit /b 1
)

"%RSCRIPT%" run_app.R

