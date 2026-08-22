param(
  [string]$RepoRoot = "",
  [string]$RscriptPath = ""
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

function Invoke-RegressionStep {
  param(
    [string]$Label,
    [scriptblock]$Command
  )

  Write-Host "==> $Label"
  $stopwatch = [System.Diagnostics.Stopwatch]::StartNew()
  & $Command
  $exitCode = $LASTEXITCODE
  $stopwatch.Stop()
  if ($exitCode -ne 0) {
    throw "$Label failed with exit code $exitCode"
  }
  Write-Host ("    passed in {0:N2}s" -f $stopwatch.Elapsed.TotalSeconds)
}

if (-not $RscriptPath) {
  $RscriptPath = Find-Rscript
}
if (-not (Test-Path -LiteralPath $RscriptPath -PathType Leaf)) {
  throw "Rscript was not found: $RscriptPath"
}

$env:LC_ALL = "English_United States.utf8"
$env:LANG = "English_United States.utf8"

$regressions = @(
  [pscustomobject]@{
    Label = "Installer regression gate contract"
    Path = "scripts\validate_installer_regression_gate.R"
  },
  [pscustomobject]@{
    Label = "Data import contract"
    Path = "scripts\validate_data_io.R"
  },
  [pscustomobject]@{
    Label = "Small-file upload performance"
    Path = "scripts\validate_data_upload_performance.R"
  },
  [pscustomobject]@{
    Label = "Startup and stale-backend contract"
    Path = "scripts\validate_startup_performance_contract.R"
  },
  [pscustomobject]@{
    Label = "Canvas, progress, result, and Delta R-squared regressions"
    Path = "scripts\validate_custom_model_canvas.R"
  },
  [pscustomobject]@{
    Label = "Mediation and moderation calculations"
    Path = "scripts\validate_mediation_moderation.R"
  },
  [pscustomobject]@{
    Label = "Exact 10,000-sample custom bootstrap runtime"
    Path = "scripts\validate_mediation_moderation_runtime.R"
  },
  [pscustomobject]@{
    Label = "Shared analysis UI layout contract"
    Path = "scripts\validate_ui_layout_contract.R"
  },
  [pscustomobject]@{
    Label = "Release hygiene"
    Path = "scripts\validate_release_hygiene.R"
  }
)

Push-Location $RepoRoot
try {
  Invoke-RegressionStep "git diff --check" { git diff --check }

  foreach ($regression in $regressions) {
    if (-not (Test-Path -LiteralPath $regression.Path -PathType Leaf)) {
      throw "Regression validation was not found: $($regression.Path)"
    }
    Invoke-RegressionStep $regression.Label { & $RscriptPath $regression.Path }
  }

  Write-Host "Installer regression gate passed."
  Write-Host "Complete the packaged-app timing and visual checks in docs/INSTALLER_REGRESSION_CHECKLIST_2026-08-22_KO.md before publishing."
} finally {
  Pop-Location
}
