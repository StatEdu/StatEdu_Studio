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
$previousStructuralBootstrapMode = $env:STATEDU_STRUCTURAL_BOOTSTRAP_MODE
$previousStructuralBootstrapReport = $env:STATEDU_STRUCTURAL_BOOTSTRAP_REPORT
$previousStructuralBootstrapRunId = $env:STATEDU_STRUCTURAL_BOOTSTRAP_RUN_ID
$structuralBootstrapRunId = [Guid]::NewGuid().ToString("N")
$structuralGateStartedAtUtc = [DateTime]::UtcNow
$structuralBootstrapReport = $env:STATEDU_STRUCTURAL_BOOTSTRAP_REPORT
if ([string]::IsNullOrWhiteSpace($structuralBootstrapReport)) {
  $structuralEvidenceDirectory = Join-Path ([System.IO.Path]::GetTempPath()) "StatEdu\release-evidence"
  New-Item -ItemType Directory -Path $structuralEvidenceDirectory -Force | Out-Null
  $structuralEvidenceName = "structural-bootstrap-{0}-{1}.json" -f `
    (Get-Date -Format "yyyyMMdd-HHmmss"), ([Guid]::NewGuid().ToString("N"))
  $structuralBootstrapReport = Join-Path $structuralEvidenceDirectory $structuralEvidenceName
} elseif (-not [System.IO.Path]::IsPathRooted($structuralBootstrapReport)) {
  $structuralBootstrapReport = Join-Path $RepoRoot $structuralBootstrapReport
}
if (Test-Path -LiteralPath $structuralBootstrapReport) {
  Remove-Item -LiteralPath $structuralBootstrapReport -Force
}
if (Test-Path -LiteralPath $structuralBootstrapReport) {
  throw "Previous structural bootstrap evidence could not be invalidated: $structuralBootstrapReport"
}
$env:STATEDU_STRUCTURAL_BOOTSTRAP_MODE = "installer"
$env:STATEDU_STRUCTURAL_BOOTSTRAP_REPORT = $structuralBootstrapReport
$env:STATEDU_STRUCTURAL_BOOTSTRAP_RUN_ID = $structuralBootstrapRunId

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
  # Keep the interactive latency sentinel ahead of the intentionally long
  # SEM/CFA stress profiles so thermal throttling cannot masquerade as a
  # custom-bootstrap regression. The 20-second budget remains unchanged.
  [pscustomobject]@{
    Label = "Exact 10,000-sample custom bootstrap runtime"
    Path = "scripts\validate_mediation_moderation_runtime.R"
  },
  [pscustomobject]@{
    Label = "CFA defaults, labels, and shared structural UI"
    Path = "scripts\validate_cfa_ui.R"
  },
  [pscustomobject]@{
    Label = "SEM and PLS-SEM defaults, labels, bootstrap, and exports"
    Path = "scripts\validate_sem_canvas.R"
  },
  [pscustomobject]@{
    Label = "PLS-SEM and PLSc numerical regression"
    Path = "scripts\validate_pls_fit_csem.R"
  },
  [pscustomobject]@{
    Label = "Measured CFA, SEM, and PLS-SEM bootstrap performance"
    Path = "scripts\validate_structural_bootstrap_performance.R"
  },
  [pscustomobject]@{
    Label = "Mediation and moderation calculations"
    Path = "scripts\validate_mediation_moderation.R"
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

  if (-not (Test-Path -LiteralPath $structuralBootstrapReport -PathType Leaf)) {
    throw "Structural bootstrap validation did not write its required timing record: $structuralBootstrapReport"
  }
  $structuralEvidence = Get-Content -LiteralPath $structuralBootstrapReport -Raw -Encoding UTF8 |
    ConvertFrom-Json
  if (-not $structuralEvidence.passed -or $structuralEvidence.mode -ne "installer" -or
      $structuralEvidence.schema_version -ne 2 -or
      $structuralEvidence.run_id -ne $structuralBootstrapRunId) {
    throw "Structural bootstrap timing record is not a verified installer-mode pass: $structuralBootstrapReport"
  }
  $structuralEvidenceWriteTimeUtc = (Get-Item -LiteralPath $structuralBootstrapReport).LastWriteTimeUtc
  if ($structuralEvidenceWriteTimeUtc -lt $structuralGateStartedAtUtc.AddSeconds(-2)) {
    throw "Structural bootstrap timing record predates this installer gate run: $structuralBootstrapReport"
  }
  $structuralExactness = $structuralEvidence.metrics.exactness
  if (-not $structuralExactness.passed -or
      -not $structuralExactness.sem_two_stage_vs_full_se -or
      -not $structuralExactness.sem_seed_reproducible -or
      -not $structuralExactness.sem_product_index_vs_legacy -or
      -not $structuralExactness.sem_product_index_fail_open -or
      -not $structuralExactness.sem_product_index_missing_guard -or
      -not $structuralExactness.sem_product_index_single_position -or
      -not $structuralExactness.sem_fixed_index_vs_legacy -or
      -not $structuralExactness.sem_fixed_index_fail_open -or
      -not $structuralExactness.sem_fixed_index_normal_default -or
      -not $structuralExactness.sem_fixed_index_worker_payload_small -or
      -not $structuralExactness.cfa_legacy_vs_fast_serial -or
      -not $structuralExactness.cfa_fast_serial_vs_psock -or
      -not $structuralExactness.metadata_restore) {
    throw "Structural bootstrap timing record is missing required SEM/CFA exactness evidence."
  }
  if ($structuralEvidence.metrics.installer.cfa.repetitions -ne 1000 -or
      $structuralEvidence.metrics.installer.sem.repetitions -ne 5000 -or
      $structuralEvidence.metrics.installer.pls_sem.repetitions -ne 1000) {
    throw "Structural bootstrap timing record does not contain CFA 1,000 / SEM 5,000 / PLS-SEM 1,000 actual repetitions."
  }
  Write-Host "Verified structural bootstrap timing record: $structuralBootstrapReport"
  Write-Host ("    CFA 1,000: {0:N2}s; SEM 5,000: {1:N2}s; PLS-SEM 1,000: {2:N2}s" -f `
    $structuralEvidence.metrics.installer.cfa.total_seconds,
    $structuralEvidence.metrics.installer.sem.total_seconds,
    $structuralEvidence.metrics.installer.pls_sem.total_seconds)

  Write-Host "Installer regression gate passed."
  Write-Host "Complete the packaged-app timing and visual checks in docs/INSTALLER_REGRESSION_CHECKLIST_2026-08-22_KO.md before publishing."
} finally {
  if ($null -eq $previousStructuralBootstrapMode) {
    Remove-Item Env:\STATEDU_STRUCTURAL_BOOTSTRAP_MODE -ErrorAction SilentlyContinue
  } else {
    $env:STATEDU_STRUCTURAL_BOOTSTRAP_MODE = $previousStructuralBootstrapMode
  }
  if ($null -eq $previousStructuralBootstrapReport) {
    Remove-Item Env:\STATEDU_STRUCTURAL_BOOTSTRAP_REPORT -ErrorAction SilentlyContinue
  } else {
    $env:STATEDU_STRUCTURAL_BOOTSTRAP_REPORT = $previousStructuralBootstrapReport
  }
  if ($null -eq $previousStructuralBootstrapRunId) {
    Remove-Item Env:\STATEDU_STRUCTURAL_BOOTSTRAP_RUN_ID -ErrorAction SilentlyContinue
  } else {
    $env:STATEDU_STRUCTURAL_BOOTSTRAP_RUN_ID = $previousStructuralBootstrapRunId
  }
  Pop-Location
}
