param(
  [string]$RepoRoot = "",
  [string]$RscriptPath = "",
  [int]$Port = 7896,
  [int]$TimeoutSeconds = 90,
  [switch]$FullElectronSmoke
)

$ErrorActionPreference = "Stop"

if (-not $RepoRoot) {
  $RepoRoot = Resolve-Path (Join-Path $PSScriptRoot "..")
} else {
  $RepoRoot = Resolve-Path $RepoRoot
}

function Invoke-PreflightStep {
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

function Invoke-Script {
  param(
    [string]$Label,
    [string]$Path,
    [string[]]$Arguments = @()
  )

  Invoke-PreflightStep $Label {
    & powershell -ExecutionPolicy Bypass -File $Path @Arguments
  }
}

$validationArgs = @("-RepoRoot", $RepoRoot, "-Full")
if ($RscriptPath) {
  $validationArgs += @("-RscriptPath", $RscriptPath)
}

$shinyArgs = @("-RepoRoot", $RepoRoot, "-Port", "$Port", "-TimeoutSeconds", "$TimeoutSeconds")
if ($RscriptPath) {
  $shinyArgs += @("-RscriptPath", $RscriptPath)
}

$electronArgs = @("-RepoRoot", $RepoRoot)
if (-not $FullElectronSmoke) {
  $electronArgs += "-SkipUnpackedChecks"
}

Invoke-Script `
  -Label "Full stabilization validation" `
  -Path (Join-Path $RepoRoot "scripts\validate_stabilization.ps1") `
  -Arguments $validationArgs

Invoke-Script `
  -Label "Shiny startup smoke" `
  -Path (Join-Path $RepoRoot "scripts\smoke_shiny_app.ps1") `
  -Arguments $shinyArgs

Invoke-Script `
  -Label "Electron release smoke" `
  -Path (Join-Path $RepoRoot "scripts\smoke_electron_release.ps1") `
  -Arguments $electronArgs

Write-Host "Release preflight passed."
if (-not $FullElectronSmoke) {
  Write-Host "Note: Electron unpacked-output checks were skipped. Run with -FullElectronSmoke after packaging."
}
