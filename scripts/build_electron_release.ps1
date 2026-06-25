param(
  [string]$RHome = "",
  [switch]$SkipRuntimeCopy,
  [switch]$SkipNpmInstall
)

$ErrorActionPreference = "Stop"

$arguments = @()
if ($RHome) {
  $arguments += @("-RHome", $RHome)
}
if ($SkipRuntimeCopy) {
  $arguments += "-SkipRuntimeCopy"
}
if ($SkipNpmInstall) {
  $arguments += "-SkipNpmInstall"
}

& (Join-Path $PSScriptRoot "build_electron_beta.ps1") @arguments
if ($LASTEXITCODE -ne 0) {
  exit $LASTEXITCODE
}
