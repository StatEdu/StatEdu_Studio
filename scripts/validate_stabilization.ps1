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
  "scripts\validate_sem_release_promotion.R",
  "scripts\validate_sem_public_claims.R",
  "scripts\validate_document_encoding.R",
  "scripts\validate_repository_fixtures.R",
  "scripts\validate_brand_metadata.R",
  "scripts\validate_i18n_contract.R",
  "scripts\validate_settings_dialogs.R",
  "scripts\validate_ui_layout_contract.R",
  "scripts\validate_data_editor_wide_long.R",
  "scripts\validate_data_editor_recode.R",
  "scripts\validate_ttest_anova.R",
  "scripts\validate_regression_coefficients.R",
  "scripts\validate_linear_regression_sci.R",
  "scripts\validate_logistic_analysis.R",
  "scripts\validate_logistic_regression_sci.R",
  "scripts\validate_complex_sample_analysis.R",
  "scripts\validate_complex_sample_custom_model.R",
  "scripts\validate_custom_model_canvas.R",
  "scripts\validate_mediation_moderation.R",
  "scripts\validate_longitudinal.R",
  "scripts\validate_mixed_rm_anova.R",
  "scripts\validate_release_hygiene.R",
  "scripts\validate_data_io.R",
  "scripts\validate_data_upload_performance.R",
  "scripts\validate_startup_performance_contract.R"
)

$fullOnlyValidations = @(
  "scripts\validate_ancova.R",
  "scripts\validate_amos_external_comparator.R",
  "scripts\validate_amos23_cfa_results.R",
  "scripts\validate_calculators.R",
  "scripts\validate_cfa_all.R",
  "scripts\validate_correlation_auto.R",
  "scripts\validate_crosstabs.R",
  "scripts\validate_factor_pca.R",
  "scripts\validate_generalized.R",
  "scripts\validate_interrater.R",
  "scripts\validate_logistic_ui.R",
  "scripts\validate_ordinal_category_order.R",
  "scripts\validate_paired_guards.R",
  "scripts\validate_pls_external_comparator.R",
  "scripts\validate_pls_fit_csem.R",
  "scripts\validate_pls_smartpls_tam.R",
  "scripts\validate_smartpls_cbsem_tam.R",
  "scripts\validate_penalized.R",
  "scripts\validate_p_formatting.R",
  "scripts\validate_reliability.R",
  "scripts\validate_result_history.R",
  "scripts\validate_sample_size.R",
  "scripts\validate_sem_audit_trail.R",
  "scripts\validate_sem_canvas.R",
  "scripts\validate_sem_construct_resolution.R",
  "scripts\validate_sem_construct_specification.R",
  "scripts\validate_sem_estimator_recommendation.R",
  "scripts\validate_sem_group_gate.R",
  "scripts\validate_sem_identification_power.R",
  "scripts\validate_sem_measurement_assessment.R",
  "scripts\validate_sem_micom.R",
  "scripts\validate_sem_missing_sensitivity.R",
  "scripts\validate_sem_parcel_safety.R",
  "scripts\validate_sem_plsc_scope.R",
  "scripts\validate_sem_sci_gap_audit.R",
  "scripts\validate_sem_structure_effects.R",
  "scripts\validate_sem_unspecified_block.R",
  "scripts\validate_survival_preflight.R",
  "scripts\validate_survival.R",
  "scripts\validate_spss31_survival_results.R",
  "scripts\validate_spss31_classical_results.R",
  "scripts\validate_spss31_analysis_results.R",
  "scripts\validate_survival_ui_smoke.R",
  "scripts\validate_analysis_reference_comparison.R"
)

$aggregateCoveredValidations = @(
  "scripts\validate_cfa_bootstrap.R",
  "scripts\validate_cfa_canvas.R",
  "scripts\validate_cfa_common.R",
  "scripts\validate_cfa_external_references.R",
  "scripts\validate_cfa_identification.R",
  "scripts\validate_cfa_invariance.R",
  "scripts\validate_cfa_mi_holdout.R",
  "scripts\validate_cfa_ordinal.R",
  "scripts\validate_cfa_reporting_exports.R",
  "scripts\validate_cfa_ui.R"
)

$validations = $coreValidations
if ($Full) {
  $validations += $fullOnlyValidations
}

Push-Location $RepoRoot
try {
  $knownValidationScripts = @($coreValidations + $fullOnlyValidations + $aggregateCoveredValidations) | Sort-Object -Unique
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
