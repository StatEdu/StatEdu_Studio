@echo off
setlocal
cd /d "%~dp0"
set R_EXE=
if exist "D:\Program\R\R-4.5.3\bin\Rscript.exe" set R_EXE=D:\Program\R\R-4.5.3\bin\Rscript.exe
if "%R_EXE%"=="" if exist "C:\Program Files\R\R-4.5.3\bin\Rscript.exe" set R_EXE=C:\Program Files\R\R-4.5.3\bin\Rscript.exe
if "%R_EXE%"=="" set R_EXE=Rscript
powershell -NoProfile -ExecutionPolicy Bypass -WindowStyle Hidden -Command "Start-Process -FilePath $env:R_EXE -ArgumentList 'run_latent_mplus.R' -WorkingDirectory (Get-Location).Path -WindowStyle Hidden"
endlocal
