$ErrorActionPreference = 'Stop'
Set-Location (Split-Path -Parent $PSScriptRoot)
$env:LC_ALL = 'English_United States.utf8'
$env:LANG = $env:LC_ALL
$env:R_LIBS_USER = (Resolve-Path 'packaging/electron/runtime/R-4.5.3/library').Path
$env:STATEDU_NO_PACKAGE_INSTALL = 'true'
$env:STATEDU_MODULE_CACHE_DIR = 'output/startup-review-20260914/cache-miss-1'
$env:STATEDU_TEST_VISIBILITY = 'selected'
$env:STATEDU_TEST_INIT_JIT = 'on'
$taskOutput = 'output/recent-performance-validation-20260914'
New-Item -ItemType Directory -Force $taskOutput | Out-Null
$taskChecks = @(
  'output/startup-jit-20260914/verify_restore.R',
  'output/km-zero-rows-20260914/verify.R',
  'output/km-zero-rows-20260914/verify_render.R',
  'output/competing-risk-counts-20260914/verify.R',
  'output/competing-risk-counts-20260914/verify_render.R',
  'output/competing-risk-counts-20260914/verify_integrated.R',
  'output/correlation-normality-reuse-20260914/verify.R',
  'output/correlation-normality-reuse-20260914/verify_integration.R'
)
$taskRecords = @()
foreach ($taskCheck in $taskChecks) {
  $taskIndex = $taskRecords.Count + 1
  & './packaging/electron/runtime/R-4.5.3/bin/Rscript.exe' --vanilla $taskCheck 2>&1 |
    Tee-Object -FilePath "$taskOutput/check-$taskIndex.log"
  $taskExit = $LASTEXITCODE
  $taskRecords += [pscustomobject]@{script=$taskCheck;exit_code=$taskExit}
  $taskRecords | Export-Csv "$taskOutput/checks.csv" -NoTypeInformation
  if ($taskExit -ne 0) { throw "Validation failed: $taskCheck" }
}
& './packaging/electron/runtime/R-4.5.3/bin/Rscript.exe' --vanilla `
  'output/startup-jit-20260914/final_measure.R' current recent-combined 2>&1 |
  Tee-Object -FilePath "$taskOutput/server.log"
if ($LASTEXITCODE -ne 0) { throw 'Full server validation failed' }
& './packaging/electron/runtime/R-4.5.3/bin/Rscript.exe' --vanilla `
  'output/recent-performance-validation-20260914/compare_server.R' 2>&1 |
  Tee-Object -FilePath "$taskOutput/server-compare.log"
if ($LASTEXITCODE -ne 0) { throw 'Full server snapshot mismatch' }
