param(
  [string]$RepoRoot = "",
  [string]$ElectronOutDir = "",
  [switch]$SkipUnpackedChecks
)

$ErrorActionPreference = "Stop"

if (-not $RepoRoot) {
  $RepoRoot = Resolve-Path (Join-Path $PSScriptRoot "..")
} else {
  $RepoRoot = Resolve-Path $RepoRoot
}

if (-not $ElectronOutDir) {
  $ElectronOutDir = Join-Path $RepoRoot "dist\electron\win-unpacked"
}

$appResourceDir = Join-Path $ElectronOutDir "resources\app"
$bundledAppDir = Join-Path $appResourceDir "app"
$runtimeDir = Join-Path $appResourceDir "runtime\R-4.5.2"
$rscript = Join-Path $runtimeDir "bin\x64\Rscript.exe"

function Assert-Path {
  param(
    [string]$Path,
    [string]$Label
  )
  if (-not (Test-Path -LiteralPath $Path)) {
    throw "$Label was not found: $Path"
  }
  Write-Host "[ok] $Label"
}

function Assert-JsonVersionPin {
  $node = Get-Command "node.exe" -ErrorAction SilentlyContinue
  if (-not $node) {
    $node = Get-Command "node" -ErrorAction SilentlyContinue
  }
  if (-not $node) {
    throw "Node.js was not found; cannot validate Electron package pins."
  }

  $script = @"
const pkg = require('./packaging/electron/package.json');
const lock = require('./packaging/electron/package-lock.json');
for (const name of ['electron', 'electron-builder']) {
  const declared = pkg.devDependencies[name];
  const locked = lock.packages[''].devDependencies[name];
  if (!/^\d+\.\d+\.\d+$/.test(declared)) {
    throw new Error(name + ' is not pinned to an exact version in package.json: ' + declared);
  }
  if (declared !== locked) {
    throw new Error(name + ' package.json and package-lock.json versions differ: ' + declared + ' vs ' + locked);
  }
  console.log('[ok] ' + name + ' pinned at ' + declared);
}
"@
  Push-Location $RepoRoot
  try {
    & $node.Source -e $script
    if ($LASTEXITCODE -ne 0) {
      throw "Electron package pin validation failed."
    }
  } finally {
    Pop-Location
  }
}

function Assert-FileContains {
  param(
    [string]$Path,
    [string]$Pattern,
    [string]$Label
  )
  $content = Get-Content -LiteralPath $Path -Raw
  if ($content -notmatch $Pattern) {
    throw "$Label was not found in $Path"
  }
  Write-Host "[ok] $Label"
}

function Assert-FileNotContains {
  param(
    [string]$Path,
    [string]$Pattern,
    [string]$Label
  )
  $content = Get-Content -LiteralPath $Path -Raw
  if ($content -match $Pattern) {
    throw "$Label was found in $Path"
  }
  Write-Host "[ok] $Label"
}

function Assert-NoDistArtifacts {
  param(
    [string]$Path
  )
  if (-not (Test-Path -LiteralPath $Path)) {
    return
  }
  $blocked = Get-ChildItem -LiteralPath $Path -Force | Where-Object {
    $_.Name -match "^EasyFlow_Statistics_Beta_" -or
    $_.Name -in @(".Rhistory", "builder-debug.yml")
  }
  if ($blocked) {
    throw "Unexpected distribution artifact(s): $($blocked.Name -join ', ')"
  }
  Write-Host "[ok] distribution output has no legacy product or debug artifacts"
}

function Assert-ExeVersionInfo {
  param(
    [string]$Path,
    [string]$ExpectedProductName,
    [string]$ExpectedCompanyName
  )
  $versionInfo = (Get-Item -LiteralPath $Path).VersionInfo
  if ($versionInfo.ProductName -ne $ExpectedProductName) {
    throw "Unexpected ProductName for $Path`: $($versionInfo.ProductName)"
  }
  if ($versionInfo.CompanyName -ne $ExpectedCompanyName) {
    throw "Unexpected CompanyName for $Path`: $($versionInfo.CompanyName)"
  }
  if ($versionInfo.FileDescription -notmatch [regex]::Escape($ExpectedProductName)) {
    throw "Unexpected FileDescription for $Path`: $($versionInfo.FileDescription)"
  }
  Write-Host "[ok] executable version metadata for $ExpectedProductName"
}

function Assert-AppBootstrapModulesTracked {
  $git = Get-Command "git.exe" -ErrorAction SilentlyContinue
  if (-not $git) {
    $git = Get-Command "git" -ErrorAction SilentlyContinue
  }
  if (-not $git) {
    throw "git was not found; cannot validate tracked app module files."
  }

  $tracked = & $git.Source -C $RepoRoot ls-files
  if ($LASTEXITCODE -ne 0) {
    throw "git ls-files failed while validating app module files."
  }
  $bootstrapText = Get-Content -LiteralPath (Join-Path $RepoRoot "R\app_bootstrap.R") -Raw
  $modules = [regex]::Matches($bootstrapText, '"([^"]+\.R)"') |
    ForEach-Object { "R/" + $_.Groups[1].Value } |
    Sort-Object -Unique
  $missing = @($modules | Where-Object { $_ -notin $tracked })
  if ($missing.Count -gt 0) {
    throw "R module(s) referenced by app_bootstrap.R are not tracked by git and would be omitted from git ls-files based packaging: $($missing -join ', ')"
  }
  Write-Host "[ok] app_bootstrap R modules are tracked"
}

Assert-JsonVersionPin

Assert-Path (Join-Path $RepoRoot "docs\RELEASE_CHECKLIST.md") "release checklist"
Assert-Path (Join-Path $RepoRoot "scripts\generate_oss_notices.R") "OSS notice generator"
Assert-Path (Join-Path $RepoRoot "scripts\prune_r_runtime.R") "R runtime prune script"
Assert-Path (Join-Path $RepoRoot "LICENSE") "application license"
Assert-Path (Join-Path $RepoRoot "SOURCE-OFFER.txt") "source offer"

Assert-FileContains (Join-Path $RepoRoot "packaging\electron\main.js") "EASYFLOW_TOKEN" "Electron token handoff"
Assert-FileNotContains (Join-Path $RepoRoot "R\app_server.R") 'session\$close\(\)' "no Shiny startup session close"
Assert-FileContains (Join-Path $RepoRoot "R\app_misc_ui.R") "Source & License" "Source and License About menu"
Assert-FileContains (Join-Path $RepoRoot "packaging\electron\main.js") "contextIsolation:\s*true" "contextIsolation enabled"
Assert-FileContains (Join-Path $RepoRoot "packaging\electron\main.js") "nodeIntegration:\s*false" "nodeIntegration disabled"
Assert-FileContains (Join-Path $RepoRoot "packaging\electron\main.js") "sandbox:\s*true" "sandbox enabled"
Assert-AppBootstrapModulesTracked
Assert-NoDistArtifacts (Join-Path $RepoRoot "dist\electron")

if (-not $SkipUnpackedChecks) {
  Assert-Path $ElectronOutDir "unpacked Electron output"
  $electronExe = Join-Path $ElectronOutDir "StatEdu Studio Beta.exe"
  Assert-Path $electronExe "Electron executable"
  Assert-ExeVersionInfo $electronExe "StatEdu Studio Beta" "StatEdu"
  Assert-Path (Join-Path $ElectronOutDir "LICENSE.electron.txt") "Electron license"
  Assert-Path (Join-Path $ElectronOutDir "LICENSES.chromium.html") "Chromium licenses"
  Assert-Path $bundledAppDir "bundled StatEdu Studio app"
  Assert-Path $runtimeDir "bundled R runtime"
  Assert-Path $rscript "bundled Rscript"
  Assert-Path (Join-Path $bundledAppDir "THIRD-PARTY-NOTICES.txt") "third-party notices"
  Assert-Path (Join-Path $bundledAppDir "SOURCE-OFFER.txt") "bundled source offer"
  Assert-Path (Join-Path $bundledAppDir "LICENSE") "bundled application license"
  Assert-Path (Join-Path $bundledAppDir "license_report.csv") "license report"
  Assert-Path (Join-Path $bundledAppDir "runtime_prune_report.csv") "runtime prune report"
  Assert-Path (Join-Path $bundledAppDir "LICENSES") "license text folder"

  $licenseCount = (Get-ChildItem -LiteralPath (Join-Path $bundledAppDir "LICENSES") -File | Measure-Object).Count
  if ($licenseCount -lt 1) {
    throw "LICENSES folder is empty."
  }
  Write-Host "[ok] LICENSES contains $licenseCount file(s)"

  $pruneActions = Import-Csv -LiteralPath (Join-Path $bundledAppDir "runtime_prune_report.csv") | Group-Object Action
  $unexpected = $pruneActions | Where-Object { $_.Name -ne "keep" }
  if ($unexpected) {
    throw "Unexpected prune actions found: $($unexpected.Name -join ', ')"
  }
  Write-Host "[ok] runtime prune report contains only keep rows"

  & $rscript -e "source('R/app_bootstrap.R'); load_app_packages(); source_app_modules(); cat('bundled R modules ok\n')" |
    ForEach-Object { Write-Host $_ }
  if ($LASTEXITCODE -ne 0) {
    throw "Bundled R module load check failed."
  }
}

Write-Host "Smoke checks passed."
