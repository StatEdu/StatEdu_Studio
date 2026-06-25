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
$runtimeDir = Join-Path $appResourceDir "runtime\R-4.5.3"
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

function Get-ProjectVersion {
  $versionPath = Join-Path $RepoRoot "VERSION"
  if (-not (Test-Path -LiteralPath $versionPath)) {
    throw "VERSION file was not found: $versionPath"
  }
  $version = (Get-Content -LiteralPath $versionPath -TotalCount 1).Trim()
  if (-not $version) {
    throw "VERSION file is empty."
  }
  return $version
}

function Get-ElectronReleaseProfile {
  param(
    [string]$Version
  )
  if ($Version -match "^1\.") {
    return [pscustomobject]@{
      ProductName = "StatEdu Studio"
      SetupPrefix = "StatEdu_Studio_Setup"
      ExeName = "StatEdu Studio.exe"
    }
  }
  [pscustomobject]@{
    ProductName = "StatEdu Studio Beta"
    SetupPrefix = "StatEdu_Studio_Beta_Setup"
    ExeName = "StatEdu Studio Beta.exe"
  }
}

function Assert-TextFileEquals {
  param(
    [string]$Path,
    [string]$Expected,
    [string]$Label
  )
  Assert-Path $Path $Label
  $actual = (Get-Content -LiteralPath $Path -TotalCount 1).Trim()
  if ($actual -ne $Expected) {
    throw "$Label does not match current VERSION. Expected $Expected, found $actual."
  }
  Write-Host "[ok] $Label matches VERSION $Expected"
}

function Assert-PackagedOutputVersion {
  param(
    [string]$ExpectedVersion
  )
  $distDir = Split-Path -Parent $ElectronOutDir
  $bundledVersionPath = Join-Path $bundledAppDir "VERSION"
  Assert-TextFileEquals $bundledVersionPath $ExpectedVersion "bundled app version"

  $resourcePackagePath = Join-Path $appResourceDir "package.json"
  Assert-Path $resourcePackagePath "packaged Electron resource package metadata"
  $resourcePackage = Get-Content -LiteralPath $resourcePackagePath -Raw | ConvertFrom-Json
  if ($resourcePackage.version -ne $ExpectedVersion) {
    throw "Packaged Electron resource version does not match current VERSION. Expected $ExpectedVersion, found $($resourcePackage.version)."
  }
  Write-Host "[ok] packaged Electron resource version matches VERSION $ExpectedVersion"

  if (Test-Path -LiteralPath $distDir) {
    $profile = Get-ElectronReleaseProfile -Version $ExpectedVersion
    $setupFiles = @(Get-ChildItem -LiteralPath $distDir -File -Filter "$($profile.SetupPrefix)_*.exe")
    $expectedSetupName = "$($profile.SetupPrefix)_$ExpectedVersion.exe"
    $unexpectedSetupFiles = @($setupFiles | Where-Object { $_.Name -ne $expectedSetupName })
    if ($unexpectedSetupFiles.Count -gt 0) {
      throw "Installer artifact version does not match current VERSION. Expected only $expectedSetupName, found: $($unexpectedSetupFiles.Name -join ', ')"
    }
    if ($setupFiles.Count -gt 0) {
      Assert-Path (Join-Path $distDir $expectedSetupName) "current-version installer artifact"
      Assert-Path (Join-Path $distDir "$expectedSetupName.blockmap") "current-version installer blockmap"
    }
  }
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

function Assert-NoTrackedGeneratedArtifacts {
  $git = Get-Command "git.exe" -ErrorAction SilentlyContinue
  if (-not $git) {
    $git = Get-Command "git" -ErrorAction SilentlyContinue
  }
  if (-not $git) {
    throw "git was not found; cannot validate generated artifact tracking."
  }

  $tracked = & $git.Source -C $RepoRoot ls-files
  if ($LASTEXITCODE -ne 0) {
    throw "git ls-files failed while validating generated artifacts."
  }

  $blockedPrefixes = @(
    "dist",
    "packaging/electron/app",
    "packaging/electron/runtime",
    "packaging/electron/node_modules",
    "output",
    "scratch",
    "modules/latent_mplus/app/output",
    "modules/latent_mplus/app/outputs",
    "modules/latent_mplus/app/mplus_tmp",
    "modules/latent_mplus/app/settings"
  )
  $blocked = @($tracked | Where-Object {
    $path = $_
    $blockedByPrefix = $false
    foreach ($prefix in $blockedPrefixes) {
      if ($path -eq $prefix -or $path.StartsWith("$prefix/")) {
        $blockedByPrefix = $true
      }
    }
    $blockedByPrefix -or
      $path -match '(^|/)\.Rhistory$' -or
      $path -match '(^|/)\.RData$' -or
      $path -match '(^|/)\.Ruserdata$' -or
      $path -match '(^|/)[^/]+\.(log|tmp)$' -or
      $path -match '^settings/[^/]+\.local\.json$'
  })
  if ($blocked.Count -gt 0) {
    throw "Generated or local-only artifact path(s) are tracked by git: $($blocked -join ', ')"
  }
  Write-Host "[ok] generated and local-only artifacts are not tracked"
}

Assert-JsonVersionPin

Assert-Path (Join-Path $RepoRoot "docs\RELEASE_CHECKLIST.md") "release checklist"
Assert-Path (Join-Path $RepoRoot "docs\RELEASE_MANUAL_QA.md") "manual QA protocol"
Assert-Path (Join-Path $RepoRoot "scripts\validate_stabilization.ps1") "stabilization validation runner"
Assert-Path (Join-Path $RepoRoot "scripts\smoke_shiny_app.ps1") "Shiny app smoke test"
Assert-Path (Join-Path $RepoRoot "scripts\validate_version_metadata.R") "version metadata validation"
Assert-Path (Join-Path $RepoRoot "scripts\validate_brand_metadata.R") "brand metadata validation"
Assert-Path (Join-Path $RepoRoot "scripts\validate_settings_dialogs.R") "settings dialog validation"
Assert-Path (Join-Path $RepoRoot "scripts\generate_oss_notices.R") "OSS notice generator"
Assert-Path (Join-Path $RepoRoot "scripts\prune_r_runtime.R") "R runtime prune script"
Assert-Path (Join-Path $RepoRoot "LICENSE") "application license"
Assert-Path (Join-Path $RepoRoot "SOURCE-OFFER.txt") "source offer"

Assert-FileContains (Join-Path $RepoRoot "packaging\electron\main.js") "EASYFLOW_TOKEN" "Electron token handoff"
Assert-FileContains (Join-Path $RepoRoot "docs\RELEASE_CHECKLIST.md") "validate_stabilization\.ps1 -Full" "full stabilization validation in release checklist"
Assert-FileContains (Join-Path $RepoRoot "docs\RELEASE_CHECKLIST.md") "smoke_shiny_app\.ps1" "Shiny app smoke test in release checklist"
Assert-FileContains (Join-Path $RepoRoot "docs\RELEASE_CHECKLIST.md") "RELEASE_MANUAL_QA\.md" "manual QA protocol in release checklist"
Assert-FileContains (Join-Path $RepoRoot "docs\RELEASE_MANUAL_QA.md") "Packaged Electron Workflow" "packaged Electron manual QA workflow"
Assert-FileNotContains (Join-Path $RepoRoot "R\app_server.R") 'session\$close\(\)' "no Shiny startup session close"
Assert-FileContains (Join-Path $RepoRoot "R\app_misc_ui.R") "Source & License" "Source and License About menu"
Assert-FileContains (Join-Path $RepoRoot "packaging\electron\main.js") "contextIsolation:\s*true" "contextIsolation enabled"
Assert-FileContains (Join-Path $RepoRoot "packaging\electron\main.js") "nodeIntegration:\s*false" "nodeIntegration disabled"
Assert-FileContains (Join-Path $RepoRoot "packaging\electron\main.js") "sandbox:\s*true" "sandbox enabled"
Assert-AppBootstrapModulesTracked
Assert-NoTrackedGeneratedArtifacts
Assert-NoDistArtifacts (Join-Path $RepoRoot "dist\electron")

if (-not $SkipUnpackedChecks) {
  $projectVersion = Get-ProjectVersion
  $releaseProfile = Get-ElectronReleaseProfile -Version $projectVersion
  Assert-Path $ElectronOutDir "unpacked Electron output"
  $electronExe = Join-Path $ElectronOutDir $releaseProfile.ExeName
  Assert-Path $electronExe "Electron executable"
  Assert-ExeVersionInfo $electronExe $releaseProfile.ProductName "StatEdu"
  Assert-PackagedOutputVersion $projectVersion
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
