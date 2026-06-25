param(
  [string]$RepoRoot = "",
  [string]$InstallerPath = "",
  [string]$OutFile = ""
)

$ErrorActionPreference = "Stop"

if (-not $RepoRoot) {
  $RepoRoot = Resolve-Path (Join-Path $PSScriptRoot "..")
} else {
  $RepoRoot = Resolve-Path $RepoRoot
}

$version = (Get-Content -LiteralPath (Join-Path $RepoRoot "VERSION") -Raw).Trim()
$distDir = Join-Path $RepoRoot "dist\electron"

function Resolve-InstallerPath {
  param(
    [string]$Root,
    [string]$Version,
    [string]$ExplicitPath
  )

  if ($ExplicitPath) {
    if (-not (Test-Path -LiteralPath $ExplicitPath)) {
      throw "Installer was not found: $ExplicitPath"
    }
    return (Resolve-Path -LiteralPath $ExplicitPath).Path
  }

  $preferredName = if ($Version -match "^1\.") {
    "StatEdu_Studio_Setup_$Version.exe"
  } else {
    "StatEdu_Studio_Beta_Setup_$Version.exe"
  }
  $fallbackName = if ($Version -match "^1\.") {
    "StatEdu_Studio_Beta_Setup_$Version.exe"
  } else {
    "StatEdu_Studio_Setup_$Version.exe"
  }

  $candidateNames = @($preferredName, $fallbackName)

  $candidates = @()
  foreach ($name in $candidateNames) {
    $candidate = Join-Path $Root $name
    if (Test-Path -LiteralPath $candidate) {
      $candidates += (Get-Item -LiteralPath $candidate)
    }
  }

  if ($candidates.Count -eq 0 -and (Test-Path -LiteralPath $Root)) {
    $candidates = @(
      Get-ChildItem -LiteralPath $Root -File -Filter "*Setup_$Version.exe" |
        Where-Object { $_.Name -like "StatEdu_Studio*Setup_$Version.exe" }
    )
  }

  if ($candidates.Count -eq 0) {
    throw "No StatEdu Studio installer for version $Version was found in $Root"
  }
  if ($candidates.Count -gt 1) {
    $names = ($candidates | ForEach-Object { $_.FullName }) -join ", "
    throw "Multiple installers for version $Version were found: $names"
  }

  return $candidates[0].FullName
}

function New-ChecksumRecord {
  param(
    [string]$Path
  )

  $item = Get-Item -LiteralPath $Path
  $hash = Get-FileHash -LiteralPath $Path -Algorithm SHA256
  [pscustomobject]@{
    File = $item.FullName
    Algorithm = "SHA256"
    Hash = $hash.Hash
    Bytes = $item.Length
  }
}

$resolvedInstaller = Resolve-InstallerPath -Root $distDir -Version $version -ExplicitPath $InstallerPath
$records = @()
$records += New-ChecksumRecord -Path $resolvedInstaller

$blockmap = "$resolvedInstaller.blockmap"
if (Test-Path -LiteralPath $blockmap) {
  $records += New-ChecksumRecord -Path $blockmap
}

if ($OutFile) {
  $parent = Split-Path -Parent $OutFile
  if ($parent -and -not (Test-Path -LiteralPath $parent)) {
    New-Item -ItemType Directory -Path $parent | Out-Null
  }
  $records | ConvertTo-Csv -NoTypeInformation | Set-Content -LiteralPath $OutFile -Encoding UTF8
}

$records | Format-List
