@echo off
setlocal EnableExtensions

cd /d "%~dp0"

powershell.exe -NoLogo -NoProfile -ExecutionPolicy Bypass -File "%~dp0scripts\launch_statedu.ps1"
set "STATEDU_EXIT_CODE=%ERRORLEVEL%"

if not "%STATEDU_EXIT_CODE%"=="0" (
    echo.
    echo StatEdu Studio could not be started.
    pause
)

exit /b %STATEDU_EXIT_CODE%
