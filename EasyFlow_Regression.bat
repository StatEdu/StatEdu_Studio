@echo off
setlocal

cd /d "%~dp0"

where Rscript >nul 2>nul
if %errorlevel% neq 0 (
    echo Rscript was not found.
    echo Please install R and make sure Rscript is available in PATH.
    echo.
    pause
    exit /b 1
)

Rscript run_app.R

pause

