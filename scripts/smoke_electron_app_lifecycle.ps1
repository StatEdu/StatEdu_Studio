param(
  [string]$RepoRoot = "",
  [string]$ElectronExe = "",
  [int]$StartupTimeoutSeconds = 180,
  [int]$ShutdownTimeoutSeconds = 20
)

$ErrorActionPreference = "Stop"

if (-not $RepoRoot) {
  $RepoRoot = Resolve-Path (Join-Path $PSScriptRoot "..")
} else {
  $RepoRoot = Resolve-Path $RepoRoot
}

function Get-ProjectVersion {
  (Get-Content -LiteralPath (Join-Path $RepoRoot "VERSION") -Raw).Trim()
}

function Get-ElectronReleaseProfile {
  param([string]$Version)

  if ($Version -match "^\d+\.\d+\.\d+-dev$") {
    return [pscustomobject]@{
      ProductName = "StatEdu Studio Dev"
      ExeName = "StatEdu Studio Dev.exe"
      AppDataDirName = "statedu-studio-dev"
    }
  }

  if ($Version -match "^\d+\.\d+\.\d+$") {
    return [pscustomobject]@{
      ProductName = "StatEdu Studio"
      ExeName = "StatEdu Studio.exe"
      AppDataDirName = "statedu-studio"
    }
  }

  [pscustomobject]@{
    ProductName = "StatEdu Studio Beta"
    ExeName = "StatEdu Studio Beta.exe"
    AppDataDirName = "statedu-studio-beta"
  }
}

$projectVersion = Get-ProjectVersion
$releaseProfile = Get-ElectronReleaseProfile -Version $projectVersion

if (-not $ElectronExe) {
  $ElectronExe = Join-Path $RepoRoot "dist\electron\win-unpacked\$($releaseProfile.ExeName)"
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
  $resourcesDir = Join-Path $RepoRoot "dist\electron\win-unpacked\resources"
  $unpackedDir = Join-Path $resourcesDir "app.asar.unpacked"
  $appDir = if (Test-Path -LiteralPath $unpackedDir) { $unpackedDir } else { Join-Path $resourcesDir "app" }
  $runtimeNeedle = [regex]::Escape((Join-Path $appDir "runtime\R-4.5.3"))
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

function Get-SameProductAppProcesses {
  $productExeName = [regex]::Escape($releaseProfile.ExeName)
  @(Get-CimInstance Win32_Process | Where-Object {
    $_.Name -eq $releaseProfile.ExeName -or
    ($_.ExecutablePath -and $_.ExecutablePath -match "\\$productExeName$") -or
    ($_.CommandLine -and $_.CommandLine -match "\\$productExeName(`"|\\s|$)")
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
  foreach ($process in @(Get-PackagedAppProcesses) + @(Get-SameProductAppProcesses)) {
    Stop-Process -Id $process.ProcessId -Force -ErrorAction SilentlyContinue
  }
}

function Close-PackagedMainWindows {
  $closedAny = $false
  foreach ($processInfo in Get-PackagedMainProcesses) {
    try {
      $process = Get-Process -Id $processInfo.ProcessId -ErrorAction Stop
      if ($process.CloseMainWindow()) {
        $closedAny = $true
      }
    } catch {
    }
  }
  if (-not $closedAny -and $appProcess -and -not $appProcess.HasExited) {
    try {
      $closedAny = $appProcess.CloseMainWindow()
    } catch {
      $closedAny = $false
    }
  }
  return $closedAny
}

function Read-NewLogText {
  param(
    [string]$Path,
    [long]$InitialLength
  )

  if (-not (Test-Path -LiteralPath $Path)) {
    return ""
  }
  $stream = [System.IO.File]::Open($Path, [System.IO.FileMode]::Open, [System.IO.FileAccess]::Read, [System.IO.FileShare]::ReadWrite)
  try {
    if ($stream.Length -le $InitialLength) {
      return ""
    }
    $stream.Seek($InitialLength, [System.IO.SeekOrigin]::Begin) | Out-Null
    $reader = [System.IO.StreamReader]::new($stream, [System.Text.Encoding]::UTF8, $true, 4096, $true)
    try {
      return $reader.ReadToEnd()
    } finally {
      $reader.Dispose()
    }
  } finally {
    $stream.Dispose()
  }
}

Assert-Path $ElectronExe "packaged Electron executable"
Stop-BundledRProcesses
Stop-PackagedAppProcesses

$startupLog = Join-Path $env:APPDATA "$($releaseProfile.AppDataDirName)\logs\startup.log"
$initialLogLength = 0
if (Test-Path -LiteralPath $startupLog) {
  $initialLogLength = (Get-Item -LiteralPath $startupLog).Length
}

$appProcess = Start-Process `
  -FilePath $ElectronExe `
  -WorkingDirectory (Split-Path $ElectronExe -Parent) `
  -WindowStyle Hidden `
  -PassThru

try {
  $appReady = $false
  for ($i = 0; $i -lt $StartupTimeoutSeconds; $i++) {
    Start-Sleep -Seconds 1
    if ($appProcess.HasExited) {
      throw "Packaged Electron app exited before startup completed."
    }
    $newLogText = Read-NewLogText -Path $startupLog -InitialLength $initialLogLength
    $shinyListening = $newLogText -match "Shiny ready" -or $newLogText -match "Listening on http://127\.0\.0\.1:"
    if ($shinyListening -and $newLogText -match "BrowserWindow loaded") {
      $appReady = $true
      break
    }
  }

  if (-not $appReady) {
    $tail = ""
    if (Test-Path -LiteralPath $startupLog) {
      $tail = (Get-Content -LiteralPath $startupLog -Tail 30) -join [Environment]::NewLine
    }
    throw "Packaged Electron app did not reach the bundled Shiny URL within $StartupTimeoutSeconds seconds.`n$tail"
  }
  Write-Host "[ok] packaged Electron app loaded bundled Shiny URL"

  $closed = Close-PackagedMainWindows
  if (-not $closed) {
    & taskkill.exe /pid $appProcess.Id /t /f | Out-Null
  }

  for ($i = 0; $i -lt $ShutdownTimeoutSeconds; $i++) {
    Start-Sleep -Seconds 1
    $newLogText = Read-NewLogText -Path $startupLog -InitialLength $initialLogLength
    $appProcessCount = (Get-PackagedMainProcesses).Count
    $bundledRProcessCount = (Get-BundledRProcesses).Count
    if ($appProcessCount -eq 0 -and ($bundledRProcessCount -eq 0 -or $newLogText -match "R process exited")) {
      Write-Host "[ok] closing packaged Electron app stopped bundled Shiny process"
      Write-Host "Packaged Electron lifecycle smoke passed."
      return
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
