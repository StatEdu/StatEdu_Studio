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
  param([string]$RepositoryRoot)

  if ($RepositoryRoot) {
    $bundledRuntimeRoot = Join-Path $RepositoryRoot "packaging\electron\runtime\R-4.5.3"
    foreach ($relativePath in @("bin\x64\Rscript.exe", "bin\Rscript.exe")) {
      $bundledCandidate = Join-Path $bundledRuntimeRoot $relativePath
      if (Test-Path -LiteralPath $bundledCandidate -PathType Leaf) {
        return $bundledCandidate
      }
    }
  }

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

function Find-BundledRscript {
  param([string]$RuntimeRoot)

  foreach ($relativePath in @("bin\x64\Rscript.exe", "bin\Rscript.exe")) {
    $candidate = Join-Path $RuntimeRoot $relativePath
    if (Test-Path -LiteralPath $candidate -PathType Leaf) {
      return $candidate
    }
  }

  throw "Bundled Rscript was not found under the release runtime: $RuntimeRoot"
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
  $RscriptPath = Find-Rscript -RepositoryRoot $RepoRoot
}

$defaultBundledRuntimeRoot = Join-Path $RepoRoot "packaging\electron\runtime\R-4.5.3"
$defaultBundledRuntimeLibrary = Join-Path $defaultBundledRuntimeRoot "library"
$resolvedRscriptPath = [System.IO.Path]::GetFullPath($RscriptPath)
$resolvedBundledRuntimeRoot = [System.IO.Path]::GetFullPath($defaultBundledRuntimeRoot).TrimEnd('\') + '\'
$usingBundledRuntime = $resolvedRscriptPath.StartsWith(
  $resolvedBundledRuntimeRoot,
  [System.StringComparison]::OrdinalIgnoreCase
)
$runnerPreviousRLibs = $env:R_LIBS
$runnerPreviousRLibsUser = $env:R_LIBS_USER
$runnerPreviousRLibsSite = $env:R_LIBS_SITE
if ($usingBundledRuntime) {
  $env:R_LIBS = $defaultBundledRuntimeLibrary
  $env:R_LIBS_USER = $defaultBundledRuntimeLibrary
  $env:R_LIBS_SITE = $defaultBundledRuntimeLibrary
}

$env:LC_ALL = "English_United States.utf8"
$env:LANG = "English_United States.utf8"

$coreValidations = @(
  "scripts\validate_installer_regression_gate.R",
  "scripts\validate_version_metadata.R",
  "scripts\validate_sem_release_promotion.R",
  "scripts\validate_sem_public_claims.R",
  "scripts\validate_sem_policy_metadata.R",
  "scripts\validate_document_encoding.R",
  "scripts\validate_localized_about_docs.R",
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
  "scripts\validate_mediation_moderation_runtime.R",
  "scripts\validate_longitudinal.R",
  "scripts\validate_longitudinal_predictor_retention.R",
  "scripts\validate_one_group_rm_anova.R",
  "scripts\validate_pls_bootstrap_parallel_reproducibility.R",
  "scripts\validate_mixed_rm_anova.R",
  "scripts\validate_release_hygiene.R",
  "scripts\validate_data_io.R",
  "scripts\validate_canvas_analysis_results.R",
  "scripts\validate_data_upload_performance.R",
  "scripts\validate_startup_performance_contract.R",
  "scripts\validate_analysis_ui_text_cache.R",
  "scripts\validate_meta_analysis_input.R",
  "scripts\validate_codebook_import.R",
  "scripts\validate_meta_analysis_model.R",
  "scripts\validate_meta_analysis_ui.R",
  "scripts\validate_frequencies_screen_table_contract.R",
  "scripts\validate_general_result_table_contract.R",
  "scripts\validate_longitudinal_result_table_contract.R",
  "scripts\validate_penalized_parallel_bootstrap.R",
  "scripts\validate_regression_screen_table_contract.R",
  "scripts\validate_remaining_result_table_contract.R",
  "scripts\validate_result_table_contract.R",
  "scripts\validate_saved_result_screen_contract.R",
  "scripts\validate_structural_screen_table_contract.R",
  "scripts\validate_survival_complex_screen_table_contract.R",
  "scripts\validate_survival_dynamic_locale.R",
  "scripts\validate_pls_model_contract.R",
  "scripts\validate_pls_failclosed_core.R",
  "scripts\validate_pls_effect_tables.R",
  "scripts\validate_plsc_f2_consistency.R",
  "scripts\validate_pls_missing_policy.R",
  "scripts\validate_plsc_predict_consistency.R",
  "scripts\validate_pls_bootstrap_contract.R",
  "scripts\validate_pls_specific_indirect_engine.R",
  "scripts\validate_pls_mga_effect_engine.R",
  "scripts\validate_pls_latent_moderation_core.R",
  "scripts\validate_pls_modmed_engine.R",
  "scripts\validate_pls_modmed_audit_export.R",
  "scripts\validate_pls_workbook_export.R",
  "scripts\validate_pls_analysis_result_roundtrip.R",
  "scripts\validate_sem_bootstrap_diagram_consistency.R",
  "scripts\validate_structural_bootstrap_performance.R"
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
  "scripts\validate_latent_bch_reporting.R",
  "scripts\validate_latent_mixture_selection_integration.R",
  "scripts\validate_latent_mixture_selection.R",
  "scripts\validate_latent_profile_reporting.R",
  "scripts\validate_latent_r3step_native.R",
  "scripts\validate_latent_table_screen_contract.R",
  "scripts\validate_ordinal_category_order.R",
  "scripts\validate_paired_guards.R",
  "scripts\validate_bundled_runtime_package_versions.R",
  "scripts\validate_bundled_validation_packages.R",
  "scripts\validate_bundled_validation_lock_contract.R",
  "scripts\validate_pls_external_comparator.R",
  "scripts\validate_pls_smartpls_private_evidence.R",
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
  "scripts\validate_sem_micom_stage3_multigroup.R",
  "scripts\validate_sem_missing_sensitivity.R",
  "scripts\validate_sem_missing_fiml_fixed_index.R",
  "scripts\validate_sem_mlr_fixed_index.R",
  "scripts\validate_sem_metadata_cache.R",
  "scripts\validate_sem_multigroup_inference.R",
  "scripts\validate_sem_multigroup_b5_layout.R",
  "scripts\validate_sem_multigroup_moderation.R",
  "scripts\validate_sem_parcel_safety.R",
  "scripts\validate_sem_plsc_scope.R",
  "scripts\validate_sem_sci_gap_audit.R",
  "scripts\validate_sem_result_ui.R",
  "scripts\validate_sem_structural_reporting_tables.R",
  "scripts\validate_sem_structure_effects.R",
  "scripts\validate_sem_product_index_small.R",
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
    if ($script -in @(
      "scripts\validate_bundled_runtime_package_versions.R",
      "scripts\validate_bundled_validation_packages.R",
      "scripts\validate_bundled_validation_lock_contract.R",
      "scripts\validate_structural_bootstrap_performance.R"
    )) {
      $bundledRuntimeRoot = Join-Path $RepoRoot "packaging\electron\runtime\R-4.5.3"
      $bundledRuntimeLibrary = Join-Path $bundledRuntimeRoot "library"
      $bundledRscriptPath = Find-BundledRscript -RuntimeRoot $bundledRuntimeRoot
      $previousRLibs = $env:R_LIBS
      $previousRLibsUser = $env:R_LIBS_USER
      $previousRLibsSite = $env:R_LIBS_SITE
      try {
        $env:R_LIBS = $bundledRuntimeLibrary
        $env:R_LIBS_USER = $bundledRuntimeLibrary
        $env:R_LIBS_SITE = $bundledRuntimeLibrary
        Invoke-Step $script {
          & $bundledRscriptPath --vanilla $script `
            "--repo-root=$RepoRoot" `
            "--runtime-root=$bundledRuntimeRoot"
        }
      } finally {
        if ($null -eq $previousRLibs) {
          Remove-Item Env:\R_LIBS -ErrorAction SilentlyContinue
        } else {
          $env:R_LIBS = $previousRLibs
        }
        if ($null -eq $previousRLibsUser) {
          Remove-Item Env:\R_LIBS_USER -ErrorAction SilentlyContinue
        } else {
          $env:R_LIBS_USER = $previousRLibsUser
        }
        if ($null -eq $previousRLibsSite) {
          Remove-Item Env:\R_LIBS_SITE -ErrorAction SilentlyContinue
        } else {
          $env:R_LIBS_SITE = $previousRLibsSite
        }
      }
    } elseif ($script -eq "scripts\validate_pls_fit_csem.R") {
      $previousCsemValidationMode = $env:STATEDU_CSEM_VALIDATION_MODE
      try {
        $env:STATEDU_CSEM_VALIDATION_MODE = "optional"
        Invoke-Step $script { & $RscriptPath $script }
      } finally {
        if ($null -eq $previousCsemValidationMode) {
          Remove-Item Env:\STATEDU_CSEM_VALIDATION_MODE -ErrorAction SilentlyContinue
        } else {
          $env:STATEDU_CSEM_VALIDATION_MODE = $previousCsemValidationMode
        }
      }
    } elseif ($script -eq "scripts\validate_pls_smartpls_private_evidence.R") {
      $previousSmartplsEvidenceMode = $env:STATEDU_SMARTPLS_EVIDENCE_MODE
      try {
        $env:STATEDU_SMARTPLS_EVIDENCE_MODE = "optional"
        Invoke-Step $script { & $RscriptPath $script }
      } finally {
        if ($null -eq $previousSmartplsEvidenceMode) {
          Remove-Item Env:\STATEDU_SMARTPLS_EVIDENCE_MODE -ErrorAction SilentlyContinue
        } else {
          $env:STATEDU_SMARTPLS_EVIDENCE_MODE = $previousSmartplsEvidenceMode
        }
      }
    } else {
      Invoke-Step $script { & $RscriptPath $script }
    }
  }

  Write-Host "Stabilization validations passed."
} finally {
  Pop-Location
  if ($usingBundledRuntime) {
    if ($null -eq $runnerPreviousRLibs) { Remove-Item Env:\R_LIBS -ErrorAction SilentlyContinue } else { $env:R_LIBS = $runnerPreviousRLibs }
    if ($null -eq $runnerPreviousRLibsUser) { Remove-Item Env:\R_LIBS_USER -ErrorAction SilentlyContinue } else { $env:R_LIBS_USER = $runnerPreviousRLibsUser }
    if ($null -eq $runnerPreviousRLibsSite) { Remove-Item Env:\R_LIBS_SITE -ErrorAction SilentlyContinue } else { $env:R_LIBS_SITE = $runnerPreviousRLibsSite }
  }
}
