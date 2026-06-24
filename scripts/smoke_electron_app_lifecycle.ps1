param(
  [string]$RepoRoot = "",
  [string]$ElectronExe = "",
  [int]$StartupTimeoutSeconds = 90,
  [int]$ShutdownTimeoutSeconds = 20
)

$ErrorActionPreference = "Stop"

if (-not $RepoRoot) {
  $RepoRoot = Resolve-Path (Join-Path $PSScriptRoot "..")
} else {
  $RepoRoot = Resolve-Path $RepoRoot
}

if (-not $ElectronExe) {
  $ElectronExe = Join-Path $RepoRoot "dist\electron\win-unpacked\StatEdu Studio Beta.exe"
}

function Assert-Path {
  param(
    [string]$Path,
    [string]$Label
  )

  if (-not (Test-Path -LiteralPath $Path)) {
    throw "$Label was not found: $Path"
  }
}

function Get-BundledRProcesses {
  $runtimeNeedle = [regex]::Escape((Join-Path $RepoRoot "dist\electron\win-unpacked\resources\app\runtime\R-4.5.2"))
  @(Get-CimInstance Win32_Process | Where-Object {
    $_.Name -eq "Rscript.exe" -and
    $_.CommandLine -match $runtimeNeedle -and
    $_.CommandLine -match "run_app\.R"
  })
}

function Get-PackagedAppProcesses {
  $exeNeedle = [regex]::Escape((Resolve-Path $ElectronExe))
  @(Get-CimInstance Win32_Process | Where-Object {
    $_.ExecutablePath -eq $ElectronExe -or
    ($_.CommandLine -and $_.CommandLine -match $exeNeedle)
  })
}

function Get-PackagedMainProcesses {
  @(Get-PackagedAppProcesses | Where-Object {
    -not ($_.CommandLine -and $_.CommandLine -match "\s--type=")
  })
}

function Stop-BundledRProcesses {
  foreach ($process in Get-BundledRProcesses) {
    Stop-Process -Id $process.ProcessId -Force -ErrorAction SilentlyContinue
  }
}

function Stop-PackagedAppProcesses {
  foreach ($process in Get-PackagedAppProcesses) {
    Stop-Process -Id $process.ProcessId -Force -ErrorAction SilentlyContinue
  }
}

function Read-NewLogText {
  param(
    [string]$Path,
    [long]$InitialLength
  )

  if (-not (Test-Path -LiteralPath $Path)) {
    return ""
  }
  $bytes = [System.IO.File]::ReadAllBytes($Path)
  if ($bytes.Length -le $InitialLength) {
    return ""
  }
  [System.Text.Encoding]::UTF8.GetString($bytes, $InitialLength, $bytes.Length - $InitialLength)
}

Assert-Path $ElectronExe "packaged Electron executable"
Stop-BundledRProcesses
Stop-PackagedAppProcesses

$startupLog = Join-Path $env:APPDATA "statedu-studio-beta\logs\startup.log"
$initialLogLength = 0
if (Test-Path -LiteralPath $startupLog) {
  $initialLogLength = (Get-Item -LiteralPath $startupLog).Length
}

$appProcess = Start-Process `
  -FilePath $ElectronExe `
  -WorkingDirectory (Split-Path $ElectronExe -Parent) `
  -PassThru

try {
  $appReady = $false
  for ($i = 0; $i -lt $StartupTimeoutSeconds; $i++) {
    Start-Sleep -Seconds 1
    if ($appProcess.HasExited) {
      throw "Packaged Electron app exited before startup completed."
    }
    $newLogText = Read-NewLogText -Path $startupLog -InitialLength $initialLogLength
    if ($newLogText -match "Shiny ready" -and $newLogText -match "BrowserWindow loaded") {
      $appReady = $true
      break
    }
  }

  if (-not $appReady) {
    $tail = ""
    if (Test-Path -LiteralPath $startupLog) {
      $tail = (Get-Content -LiteralPath $startupLog -Tail 30) -join [Environment]::NewLine
    }
    throw "Packaged Electron app did not reach Shiny ready state within $StartupTimeoutSeconds seconds.`n$tail"
  }
  Write-Host "[ok] packaged Electron app loaded bundled Shiny URL"

  $closed = $false
  try {
    $closed = $appProcess.CloseMainWindow()
  } catch {
    $closed = $false
  }
  if (-not $closed) {
    & taskkill.exe /pid $appProcess.Id /t /f | Out-Null
  }

  for ($i = 0; $i -lt $ShutdownTimeoutSeconds; $i++) {
    Start-Sleep -Seconds 1
    $newLogText = Read-NewLogText -Path $startupLog -InitialLength $initialLogLength
    if ($newLogText -match "R process exited" -and (Get-PackagedMainProcesses).Count -eq 0) {
      Write-Host "[ok] closing packaged Electron app stopped bundled Shiny process"
      Write-Host "Packaged Electron lifecycle smoke passed."
      exit 0
    }
  }

  throw "Packaged Electron app or bundled Shiny process was still running after close."
} finally {
  if ($appProcess -and -not $appProcess.HasExited) {
    & taskkill.exe /pid $appProcess.Id /t /f | Out-Null
  }
  Stop-PackagedAppProcesses
  Stop-BundledRProcesses
}
