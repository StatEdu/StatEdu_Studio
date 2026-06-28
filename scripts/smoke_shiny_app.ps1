param(
  [string]$RepoRoot = "",
  [string]$RscriptPath = "",
  [int]$Port = 7896,
  [int]$TimeoutSeconds = 90
)

$ErrorActionPreference = "Stop"

if (-not $RepoRoot) {
  $RepoRoot = Resolve-Path (Join-Path $PSScriptRoot "..")
} else {
  $RepoRoot = Resolve-Path $RepoRoot
}

function Find-Rscript {
  $command = Get-Command "Rscript.exe" -ErrorAction SilentlyContinue
  if (-not $command) {
    $command = Get-Command "Rscript" -ErrorAction SilentlyContinue
  }
  if ($command) {
    return $command.Source
  }

  $candidates = @(
    "D:\Program\R\R-4.5.3\bin\x64\Rscript.exe",
    "D:\Program\R\R-4.5.3\bin\Rscript.exe",
    "C:\Program Files\R\R-4.5.3\bin\x64\Rscript.exe",
    "C:\Program Files\R\R-4.5.3\bin\Rscript.exe"
  )
  foreach ($candidate in $candidates) {
    if (Test-Path -LiteralPath $candidate) {
      return $candidate
    }
  }

  throw "Rscript was not found. Install R or pass -RscriptPath."
}

if (-not $RscriptPath) {
  $RscriptPath = Find-Rscript
}

$stdoutLog = Join-Path $env:TEMP "statedu_shiny_smoke.out.log"
$stderrLog = Join-Path $env:TEMP "statedu_shiny_smoke.err.log"
$runScript = Join-Path $env:TEMP ("statedu_shiny_smoke_" + [guid]::NewGuid().ToString() + ".R")
foreach ($path in @($stdoutLog, $stderrLog)) {
  if (Test-Path -LiteralPath $path) {
    Remove-Item -LiteralPath $path -Force
  }
}

$runScriptText = "shiny::runApp('.', host='127.0.0.1', port=$Port, launch.browser=FALSE)`n"
[System.IO.File]::WriteAllText($runScript, $runScriptText, [System.Text.UTF8Encoding]::new($false))
$arguments = @($runScript)

$process = Start-Process `
  -FilePath $RscriptPath `
  -ArgumentList $arguments `
  -WorkingDirectory $RepoRoot `
  -RedirectStandardOutput $stdoutLog `
  -RedirectStandardError $stderrLog `
  -WindowStyle Hidden `
  -PassThru

try {
  $ok = $false
  for ($i = 0; $i -lt $TimeoutSeconds; $i++) {
    Start-Sleep -Seconds 1
    try {
      $response = Invoke-WebRequest -Uri "http://127.0.0.1:$Port/" -UseBasicParsing -TimeoutSec 3
      if ($response.StatusCode -eq 200 -and $response.Content -match "StatEdu Studio") {
        $ok = $true
        break
      }
    } catch {
      if ($process.HasExited) {
        break
      }
    }
  }

  if (-not $ok) {
    if (Test-Path -LiteralPath $stdoutLog) {
      Write-Host "--- stdout ---"
      Get-Content -LiteralPath $stdoutLog -Tail 120
    }
    if (Test-Path -LiteralPath $stderrLog) {
      Write-Host "--- stderr ---"
      Get-Content -LiteralPath $stderrLog -Tail 120
    }
    throw "Shiny app smoke check failed."
  }

  Write-Host "Shiny app smoke check passed."
} finally {
  if ($process -and -not $process.HasExited) {
    Stop-Process -Id $process.Id -Force
    Wait-Process -Id $process.Id -Timeout 10 -ErrorAction SilentlyContinue
  }
  if (Test-Path -LiteralPath $runScript) {
    $removedRunScript = $false
    for ($i = 0; $i -lt 20; $i++) {
      try {
        Remove-Item -LiteralPath $runScript -Force
        $removedRunScript = $true
        break
      } catch {
        Start-Sleep -Milliseconds 500
      }
    }
    if (-not $removedRunScript) {
      Write-Warning "Could not remove temporary smoke script: $runScript"
    }
  }
}
