param(
  [string]$RHome = "",
  [string]$NodePath = "",
  [string]$NpmPath = "",
  [string]$PnpmPath = "",
  [switch]$SkipRuntimeCopy,
  [switch]$SkipNpmInstall
)

$ErrorActionPreference = "Stop"

$buildScript = Join-Path $PSScriptRoot "build_electron_beta.ps1"
& $buildScript `
  -RHome $RHome `
  -NodePath $NodePath `
  -NpmPath $NpmPath `
  -PnpmPath $PnpmPath `
  -SkipRuntimeCopy:$SkipRuntimeCopy.IsPresent `
  -SkipNpmInstall:$SkipNpmInstall.IsPresent
if ($LASTEXITCODE -ne 0) {
  exit $LASTEXITCODE
}
