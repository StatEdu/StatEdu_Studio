param(
  [string]$RHome = "",
  [switch]$SkipRuntimeCopy,
  [switch]$SkipNpmInstall
)

$ErrorActionPreference = "Stop"

$buildScript = Join-Path $PSScriptRoot "build_electron_beta.ps1"
& $buildScript `
  -RHome $RHome `
  -SkipRuntimeCopy:$SkipRuntimeCopy.IsPresent `
  -SkipNpmInstall:$SkipNpmInstall.IsPresent
if ($LASTEXITCODE -ne 0) {
  exit $LASTEXITCODE
}
