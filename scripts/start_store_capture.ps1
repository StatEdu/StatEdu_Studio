param([int]$Port = 18794)
$ErrorActionPreference = 'Stop'
$repo = Split-Path $PSScriptRoot -Parent
$bundle = Join-Path $repo 'dist/electron/win-unpacked/resources/app.asar.unpacked'
$appDir = Join-Path $bundle 'app'
$runtime = Join-Path $bundle 'runtime/R-4.5.3'
$capture = Join-Path $repo 'output/microsoft-store/capture'
[IO.Directory]::CreateDirectory($capture) | Out-Null
if (Get-NetTCPConnection -LocalPort $Port -State Listen -ErrorAction SilentlyContinue) { throw "Port $Port is already in use" }
$env:STATEDU_PORT = "$Port"
$env:STATEDU_APP_DIR = $appDir
$env:STATEDU_LAUNCH_BROWSER = 'false'
$env:STATEDU_NO_PACKAGE_INSTALL = 'true'
$env:STATEDU_PUBLIC_RELEASE = '1'
$env:STATEDU_APP_LANGUAGE = 'ko'
$env:STATEDU_USER_DATA_DIR = Join-Path $capture 'profile'
$env:STATEDU_RESULT_STORE = Join-Path $capture 'results.json'
$env:STATEDU_APP_PREFERENCES_FILE = Join-Path $capture 'preferences.json'
$env:STATEDU_APP_LANGUAGE_FILE = Join-Path $capture 'language.txt'
$env:STATEDU_RESULT_ZOOM_FILE = Join-Path $capture 'zoom.txt'
$env:STATEDU_MODULE_CACHE_DIR = Join-Path $capture 'module-cache'
$env:STATEDU_CAPTURE_DATA_FILE = Join-Path $appDir 'sample/HolzingerSwineford1939.csv'
$env:STATEDU_STARTUP_LOG = Join-Path $capture 'startup.log'
$env:R_HOME = $runtime
$env:R_LIBS_USER = Join-Path $runtime 'library'
$env:PATH = (Join-Path $runtime 'bin/x64') + ';' + $env:PATH
$env:LC_ALL = 'English_United States.utf8'
$env:LANG = 'English_United States.utf8'
$process = Start-Process -FilePath (Join-Path $runtime 'bin/x64/Rscript.exe') -ArgumentList 'run_app.R' -WorkingDirectory $appDir -WindowStyle Hidden -PassThru -RedirectStandardOutput (Join-Path $capture 'stdout.log') -RedirectStandardError (Join-Path $capture 'stderr.log')
@{ pid=$process.Id; port=$Port; appDir=$appDir; version=(Get-Content (Join-Path $appDir 'VERSION') -Raw).Trim(); purpose='Store screenshots of packaged Shiny UI with bundled sample data, isolated profile; not installed APPX certification' } | ConvertTo-Json | Set-Content (Join-Path $capture 'session.json') -Encoding utf8
Write-Output "Capture server PID $($process.Id), http://127.0.0.1:$Port"
