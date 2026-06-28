param(
  [string]$RepoRoot = "",
  [string]$RscriptPath = "",
  [switch]$Full
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

function Invoke-Step {
  param(
    [string]$Label,
    [scriptblock]$Command
  )

  Write-Host "==> $Label"
  & $Command
  if ($LASTEXITCODE -ne 0) {
    throw "$Label failed with exit code $LASTEXITCODE"
  }
}

if (-not $RscriptPath) {
  $RscriptPath = Find-Rscript
}

$env:LC_ALL = "English_United States.utf8"
$env:LANG = "English_United States.utf8"

$coreValidations = @(
  "scripts\validate_version_metadata.R",
  "scripts\validate_document_encoding.R",
  "scripts\validate_brand_metadata.R",
  "scripts\validate_settings_dialogs.R",
  "scripts\validate_ui_layout_contract.R",
  "scripts\validate_data_editor_wide_long.R",
  "scripts\validate_data_editor_recode.R",
  "scripts\validate_ttest_anova.R",
  "scripts\validate_regression_coefficients.R",
  "scripts\validate_logistic_analysis.R",
  "scripts\validate_longitudinal.R",
  "scripts\validate_release_hygiene.R",
  "scripts\validate_data_io.R"
)

$fullOnlyValidations = @(
  "scripts\validate_ancova.R",
  "scripts\validate_calculators.R",
  "scripts\validate_correlation_auto.R",
  "scripts\validate_crosstabs.R",
  "scripts\validate_factor_pca.R",
  "scripts\validate_generalized.R",
  "scripts\validate_logistic_ui.R",
  "scripts\validate_paired_guards.R",
  "scripts\validate_p_formatting.R",
  "scripts\validate_result_history.R",
  "scripts\validate_sample_size.R",
  "scripts\validate_analysis_reference_comparison.R"
)

$validations = $coreValidations
if ($Full) {
  $validations += $fullOnlyValidations
}

Push-Location $RepoRoot
try {
  $knownValidationScripts = @($coreValidations + $fullOnlyValidations) | Sort-Object -Unique
  $allValidationScripts = Get-ChildItem -LiteralPath (Join-Path $RepoRoot "scripts") -Filter "validate_*.R" |
    ForEach-Object { "scripts\$($_.Name)" } |
    Sort-Object -Unique
  $missingFromRunner = @($allValidationScripts | Where-Object { $knownValidationScripts -notcontains $_ })
  $missingFiles = @($knownValidationScripts | Where-Object { $allValidationScripts -notcontains $_ })
  if ($missingFromRunner.Count -gt 0) {
    throw "Validation script(s) not listed in validate_stabilization.ps1: $($missingFromRunner -join ', ')"
  }
  if ($missingFiles.Count -gt 0) {
    throw "Validation script(s) listed but not found: $($missingFiles -join ', ')"
  }

  Invoke-Step "git diff --check" { git diff --check }

  foreach ($script in $validations) {
    if (-not (Test-Path -LiteralPath $script)) {
      throw "Validation script was not found: $script"
    }
    Invoke-Step $script { & $RscriptPath $script }
  }

  Write-Host "Stabilization validations passed."
} finally {
  Pop-Location
}
