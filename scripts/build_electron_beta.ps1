param(
  [string]$RHome = "",
  [string]$NodePath = "",
  [string]$NpmPath = "",
  [string]$PnpmPath = "",
  [string]$BundledValidationLibrary = "",
  [string]$SmartplsEvidenceRoot = "",
  [string]$FineGrayComponentRoot = "",
  [switch]$Developer,
  [switch]$SkipRuntimeCopy,
  [switch]$SkipNpmInstall
)

$ErrorActionPreference = "Stop"

$repoRoot = Resolve-Path (Join-Path $PSScriptRoot "..")
$version = (Get-Content (Join-Path $repoRoot "VERSION")).Trim()
if ($Developer) {
  $developerVersionPath = Join-Path $repoRoot "VERSION_DEV"
  if (Test-Path -LiteralPath $developerVersionPath) {
    $version = (Get-Content -LiteralPath $developerVersionPath -Raw).Trim()
    if ($version -notmatch "^\d+\.\d+\.\d+-dev$") { throw "Invalid developer version: $version" }
  } elseif ($version -match "^\d+\.\d+\.\d+$") {
    $version = "$version-dev"
  }
}
$electronDir = Join-Path $repoRoot "packaging\electron"
$appStage = Join-Path $electronDir "app"
$runtimeStage = Join-Path $electronDir "runtime\R-4.5.3"
$runtimeRoot = Join-Path $electronDir "runtime"
$distDir = Join-Path $repoRoot "dist\electron"

# Reuse the verified local evidence bundle for repeat installer builds.
# An explicit argument or environment override always takes precedence.
if (-not $SmartplsEvidenceRoot) {
  $SmartplsEvidenceRoot = $env:STATEDU_SMARTPLS_EVIDENCE_ROOT
}
if (-not $SmartplsEvidenceRoot) {
  $SmartplsEvidenceRoot = Join-Path $repoRoot "output\private-smartpls130"
}
if (-not (Test-Path -LiteralPath $SmartplsEvidenceRoot -PathType Container)) {
  throw "SmartPLS evidence directory missing: $SmartplsEvidenceRoot. Supply -SmartplsEvidenceRoot or STATEDU_SMARTPLS_EVIDENCE_ROOT."
}
$env:STATEDU_SMARTPLS_EVIDENCE_ROOT = (Resolve-Path -LiteralPath $SmartplsEvidenceRoot).Path

function Get-ElectronReleaseProfile {
  if ($version -match "^\d+\.\d+\.\d+$") {
    return [pscustomobject]@{
      PackageName = "statedu-studio"
      Description = "StatEdu Studio desktop installer"
      AppId = "com.statedu.studio"
      ProductName = "StatEdu Studio"
      ArtifactPrefix = "StatEdu_Studio_Setup"
      ShortcutName = "StatEdu Studio"
    }
  }
  if ($version -match "^\d+\.\d+\.\d+-dev$") {
    return [pscustomobject]@{
      PackageName = "statedu-studio-dev"
      Description = "StatEdu Studio developer desktop installer"
      AppId = "com.statedu.studio.dev"
      ProductName = "StatEdu Studio Dev"
      ArtifactPrefix = "StatEdu_Studio_Dev_Setup"
      ShortcutName = "StatEdu Studio Dev"
    }
  }
  [pscustomobject]@{
    PackageName = "statedu-studio-beta"
    Description = "StatEdu Studio beta desktop installer"
    AppId = "com.statedu.studio.beta"
    ProductName = "StatEdu Studio Beta"
    ArtifactPrefix = "StatEdu_Studio_Beta_Setup"
    ShortcutName = "StatEdu Studio Beta"
  }
}

function Sync-ElectronPackageMetadata {
  $profile = Get-ElectronReleaseProfile
  $packagePath = Join-Path $electronDir "package.json"
  $package = Get-Content -LiteralPath $packagePath -Raw | ConvertFrom-Json
  $artifactName = "$($profile.ArtifactPrefix)_`${version}.`${ext}"
  $metadataChanged =
    $package.name -ne $profile.PackageName -or
    $package.version -ne $version -or
    $package.description -ne $profile.Description -or
    $package.build.appId -ne $profile.AppId -or
    $package.build.productName -ne $profile.ProductName -or
    $package.build.win.artifactName -ne $artifactName -or
    $package.build.nsis.shortcutName -ne $profile.ShortcutName
  if (-not $metadataChanged) {
    Write-Host "Electron package metadata already matches $($profile.ProductName) $version"
    return
  }
  $package.name = $profile.PackageName
  $package.version = $version
  $package.description = $profile.Description
  $package.build.appId = $profile.AppId
  $package.build.productName = $profile.ProductName
  $package.build.win.artifactName = $artifactName
  $package.build.nsis.shortcutName = $profile.ShortcutName
  $json = ($package | ConvertTo-Json -Depth 20) + [Environment]::NewLine
  [System.IO.File]::WriteAllText($packagePath, $json, [System.Text.UTF8Encoding]::new($false))
  Write-Host "Electron package metadata: $($profile.ProductName), $($profile.ArtifactPrefix)_$version.exe"
}

function Find-NpmRunner {
  if ($NpmPath) {
    if (-not (Test-Path -LiteralPath $NpmPath)) {
      throw "The requested npm command was not found: $NpmPath"
    }
    return [pscustomobject]@{ Kind = "npm"; Path = $NpmPath }
  }
  $wingetNpm = Get-ChildItem "$env:LOCALAPPDATA\Microsoft\WinGet\Packages" -Recurse -Filter "npm.cmd" -ErrorAction SilentlyContinue |
    Where-Object { $_.Directory.Name -like "node-v*-win-x64" } |
    Sort-Object FullName |
    Select-Object -First 1
  if ($wingetNpm) {
    return [pscustomobject]@{ Kind = "npm"; Path = $wingetNpm.FullName }
  }
  $command = Get-Command "npm.cmd" -ErrorAction SilentlyContinue
  if ($command) {
    return [pscustomobject]@{ Kind = "npm"; Path = $command.Source }
  }
  $resolvedPnpm = $PnpmPath
  if (-not $resolvedPnpm) {
    $pnpmCommand = Get-Command "pnpm.cmd" -ErrorAction SilentlyContinue
    if ($pnpmCommand) { $resolvedPnpm = $pnpmCommand.Source }
  }
  if ($resolvedPnpm -and (Test-Path -LiteralPath $resolvedPnpm)) {
    return [pscustomobject]@{ Kind = "pnpm"; Path = $resolvedPnpm }
  }
  throw "Neither npm.cmd nor pnpm.cmd was found. Install Node.js LTS with npm, or pass -PnpmPath and -NodePath."
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
  throw "Rscript.exe was not found. Install R or pass -RHome to the build script."
}

function Invoke-Native {
  param(
    [string]$FilePath,
    [string[]]$Arguments
  )
  $prevPref = $ErrorActionPreference
  $ErrorActionPreference = "SilentlyContinue"
  & $FilePath @Arguments
  $nativeExitCode = $LASTEXITCODE
  $ErrorActionPreference = $prevPref
  if ($nativeExitCode -ne 0) {
    throw "$FilePath failed with exit code $nativeExitCode"
  }
}

function Invoke-Npm {
  param(
    [pscustomobject]$Runner,
    [string[]]$Arguments
  )
  if ($Runner.Kind -eq "pnpm") {
    Invoke-Native $Runner.Path (@("dlx", "npm@10.9.2") + $Arguments)
  } else {
    Invoke-Native $Runner.Path $Arguments
  }
}

function Invoke-RScript {
  param(
    [string]$RscriptPath,
    [string[]]$Arguments
  )
  $previousLcAll = $env:LC_ALL
  $previousLang = $env:LANG
  $previousPreference = $ErrorActionPreference
  try {
    $env:LC_ALL = "English_United States.utf8"
    $env:LANG = "English_United States.utf8"
    $ErrorActionPreference = "SilentlyContinue"
    $output = & $RscriptPath @Arguments
    $exitCode = $LASTEXITCODE
    if ($exitCode -ne 0) {
      throw "$RscriptPath failed with exit code $exitCode"
    }
    return $output
  } finally {
    $env:LC_ALL = $previousLcAll
    $env:LANG = $previousLang
    $ErrorActionPreference = $previousPreference
  }
}

function Invoke-RScriptFile {
  param(
    [string]$RscriptPath,
    [string]$ScriptText
  )
  $tempScript = Join-Path $env:TEMP ("easyflow-build-" + [guid]::NewGuid().ToString() + ".R")
  try {
    [System.IO.File]::WriteAllText($tempScript, $ScriptText, [System.Text.UTF8Encoding]::new($false))
    Invoke-RScript $RscriptPath @($tempScript)
  } finally {
    if (Test-Path -LiteralPath $tempScript) {
      Remove-Item -LiteralPath $tempScript -Force
    }
  }
}

function Resolve-BundledValidationLibrary {
  param(
    [string]$RequestedPath,
    [string]$RscriptPath
  )

  $candidates = @()
  if ($RequestedPath) {
    $candidates += [pscustomobject]@{ Source = "-BundledValidationLibrary"; Path = $RequestedPath; Required = $true }
  } elseif ($env:STATEDU_BUNDLED_VALIDATION_LIBRARY) {
    $candidates += [pscustomobject]@{ Source = "STATEDU_BUNDLED_VALIDATION_LIBRARY"; Path = $env:STATEDU_BUNDLED_VALIDATION_LIBRARY; Required = $true }
  } else {
    if ($env:TEMP) {
      $candidates += [pscustomobject]@{ Source = "standard TEMP candidate"; Path = (Join-Path $env:TEMP "statedu-csem-validation-lib"); Required = $false }
    }
    $installedCsem = Invoke-RScript $RscriptPath @(
      "--vanilla",
      "-e",
      "path <- find.package('cSEM', quiet = TRUE); if (nzchar(path)) cat(dirname(normalizePath(path, winslash='/', mustWork=TRUE)))"
    )
    if ($installedCsem) {
      $installedCsemLibrary = [string]($installedCsem | Select-Object -First 1)
      $candidates += [pscustomobject]@{ Source = "selected R library paths"; Path = $installedCsemLibrary; Required = $false }
    }
  }

  $checked = @()
  foreach ($candidate in $candidates) {
    if (-not $candidate.Path -or -not (Test-Path -LiteralPath $candidate.Path -PathType Container)) {
      if ($candidate.Required) {
        throw "Bundled validation library from $($candidate.Source) does not exist: $($candidate.Path)"
      }
      continue
    }
    $resolved = (Resolve-Path -LiteralPath $candidate.Path).Path
    $descriptionPath = Join-Path $resolved "cSEM\DESCRIPTION"
    if (-not (Test-Path -LiteralPath $descriptionPath -PathType Leaf)) {
      if ($candidate.Required) {
        throw "Bundled validation library from $($candidate.Source) does not contain cSEM: $resolved"
      }
      $checked += "$resolved (cSEM missing)"
      continue
    }
    $versionLine = Get-Content -LiteralPath $descriptionPath | Where-Object { $_ -match '^Version:\s*' } | Select-Object -First 1
    $version = if ($versionLine) { ($versionLine -replace '^Version:\s*', '').Trim() } else { "" }
    if ($version -ne "0.6.1") {
      if ($candidate.Required) {
        throw "Bundled validation library from $($candidate.Source) must contain cSEM 0.6.1; found '$version' in $resolved"
      }
      $checked += "$resolved (cSEM $version)"
      continue
    }
    Write-Host "Bundled validation library: $resolved (cSEM 0.6.1)"
    return $resolved
  }

  $checkedText = if ($checked.Count -gt 0) { " Checked: $($checked -join '; ')." } else { "" }
  throw "cSEM 0.6.1 validation library was not found.$checkedText Pass -BundledValidationLibrary, set STATEDU_BUNDLED_VALIDATION_LIBRARY, or prepare the standard TEMP candidate. No package is downloaded by the build."
}

function Copy-Directory($source, $target) {
  if (Test-Path -LiteralPath $target) {
    Remove-Item -LiteralPath $target -Recurse -Force
  }
  New-Item -ItemType Directory -Path (Split-Path $target -Parent) -Force | Out-Null
  Copy-Item -LiteralPath $source -Destination $target -Recurse -Force
}

function Copy-R-Package($packageName, $libraryPaths, $runtimeLibrary) {
  foreach ($library in $libraryPaths) {
    $source = Join-Path $library $packageName
    if (Test-Path -LiteralPath $source) {
      $target = Join-Path $runtimeLibrary $packageName
      if (Test-Path -LiteralPath $target) {
        Remove-Item -LiteralPath $target -Recurse -Force
      }
      Copy-Item -LiteralPath $source -Destination $target -Recurse -Force
      return $true
    }
  }
  Write-Warning "Required R package was not found and was not bundled: $packageName"
  return $false
}

function Test-PathWithin {
  param(
    [string]$Path,
    [string]$Root
  )

  $fullPath = [System.IO.Path]::GetFullPath($Path)
  $fullRoot = [System.IO.Path]::GetFullPath($Root).TrimEnd([System.IO.Path]::DirectorySeparatorChar, [System.IO.Path]::AltDirectorySeparatorChar)
  return $fullPath.StartsWith($fullRoot + [System.IO.Path]::DirectorySeparatorChar, [System.StringComparison]::OrdinalIgnoreCase)
}

function Remove-StaleElectronDistArtifacts {
  if (-not (Test-Path -LiteralPath $distDir)) {
    return
  }

  $safeDistDir = [System.IO.Path]::GetFullPath($distDir)
  $profile = Get-ElectronReleaseProfile
  $currentSetupName = "$($profile.ArtifactPrefix)_$version.exe"
  $currentBlockmapName = "$currentSetupName.blockmap"
  $allowedArtifacts = @($currentSetupName, $currentBlockmapName)
  if ($version -match "^(\d+\.\d+\.\d+)-dev$") {
    $publicSetupName = "StatEdu_Studio_Setup_$($Matches[1]).exe"
    $allowedArtifacts += @($publicSetupName, "$publicSetupName.blockmap")
  } elseif ($version -match "^\d+\.\d+\.\d+$") {
    $devSetupName = "StatEdu_Studio_Dev_Setup_$version-dev.exe"
    $allowedArtifacts += @($devSetupName, "$devSetupName.blockmap")
  }
  $artifacts = @(Get-ChildItem -LiteralPath $distDir -File -Force | Where-Object {
    (
      $_.Name -match "^StatEdu_Studio(_Beta|_Dev)?_Setup_.*\.exe(\.blockmap)?$" -and
      # Keep published public installers when building either edition.
      -not ($_.Name -match "^StatEdu_Studio_Setup_.*\.exe(\.blockmap)?$") -and
      $_.Name -notin $allowedArtifacts
    ) -or
    $_.Name -match "^EasyFlow_Statistics_Beta_.*" -or
    $_.Name -in @("builder-debug.yml", ".Rhistory")
  })

  foreach ($artifact in $artifacts) {
    if (-not (Test-PathWithin $artifact.FullName $safeDistDir)) {
      throw "Refusing to remove artifact outside dist directory: $($artifact.FullName)"
    }
    Remove-Item -LiteralPath $artifact.FullName -Force
  }
}

function Remove-StaleRuntimeArtifacts {
  if (-not (Test-Path -LiteralPath $runtimeRoot)) {
    return
  }

  $safeRuntimeRoot = [System.IO.Path]::GetFullPath($runtimeRoot)
  $currentRuntime = [System.IO.Path]::GetFullPath($runtimeStage)
  $runtimeDirs = @(Get-ChildItem -LiteralPath $runtimeRoot -Directory -Force | Where-Object {
    $_.Name -match "^R-\d+\.\d+\.\d+$" -and
    [System.IO.Path]::GetFullPath($_.FullName) -ne $currentRuntime
  })

  foreach ($runtimeDir in $runtimeDirs) {
    if (-not (Test-PathWithin $runtimeDir.FullName $safeRuntimeRoot)) {
      throw "Refusing to remove runtime outside runtime directory: $($runtimeDir.FullName)"
    }
    Remove-Item -LiteralPath $runtimeDir.FullName -Recurse -Force
  }
}

$gateRscript = ""
if ($RHome) {
  foreach ($relativePath in @("bin\x64\Rscript.exe", "bin\Rscript.exe")) {
    $candidate = Join-Path $RHome $relativePath
    if (Test-Path -LiteralPath $candidate -PathType Leaf) {
      $gateRscript = $candidate
      break
    }
  }
  if (-not $gateRscript) {
    throw "Rscript.exe was not found under RHome: $RHome"
  }
} else {
  $gateRscript = Find-Rscript
}

$resolvedBundledValidationLibrary = Resolve-BundledValidationLibrary `
  -RequestedPath $BundledValidationLibrary `
  -RscriptPath $gateRscript

$installerRegressionScript = Join-Path $repoRoot "scripts\validate_installer_regressions.ps1"
Write-Host "Running installer regression gate before Electron staging..."
$previousRLibsUser = $env:R_LIBS_USER
try {
  $gateLibraryPaths = Invoke-RScript $gateRscript @("--vanilla", "-e", "cat(.libPaths(), sep='\n')")
  $gateLibraryPaths = @($resolvedBundledValidationLibrary) + @($gateLibraryPaths)
  $gateLibraryPaths = @($gateLibraryPaths | Where-Object { $_ -and (Test-Path -LiteralPath $_) } | Select-Object -Unique)
  $env:R_LIBS_USER = $gateLibraryPaths -join [System.IO.Path]::PathSeparator
  Invoke-RScript $gateRscript @(
    "--vanilla",
    (Join-Path $repoRoot "scripts\validate_bundled_runtime_package_versions.R"),
    "--repo-root=$repoRoot"
  )
  & powershell.exe `
    -NoProfile `
    -ExecutionPolicy Bypass `
    -File $installerRegressionScript `
    -RepoRoot $repoRoot `
    -RscriptPath $gateRscript
  $installerRegressionExitCode = $LASTEXITCODE
} finally {
  if ($null -eq $previousRLibsUser) {
    Remove-Item Env:\R_LIBS_USER -ErrorAction SilentlyContinue
  } else {
    $env:R_LIBS_USER = $previousRLibsUser
  }
}
if ($installerRegressionExitCode -ne 0) {
  throw "Installer regression gate failed with exit code $installerRegressionExitCode"
}

$releaseProfile = Get-ElectronReleaseProfile
Write-Host "Preparing $($releaseProfile.ProductName) $version Electron installer..."
if ($FineGrayComponentRoot) {
  & (Join-Path $PSScriptRoot 'stage_fine_gray_native.ps1') -ComponentRoot $FineGrayComponentRoot -ValidateOnly
}

Remove-StaleElectronDistArtifacts
Remove-StaleRuntimeArtifacts

if (Test-Path -LiteralPath $appStage) {
  if (-not (Test-PathWithin ([System.IO.Path]::GetFullPath($appStage)) ([System.IO.Path]::GetFullPath($electronDir)))) {
    throw "Refusing to clear app staging outside the Electron packaging directory: $appStage"
  }
  Remove-Item -LiteralPath $appStage -Recurse -Force
}

Push-Location $repoRoot
try {
  $appFiles = git ls-files |
    Where-Object {
      $_ -notmatch "^(packaging/|dist/)" -and
      $_ -notmatch "^easyflow_statistics_.*\.zip$" -and
      $_ -notmatch "^StatEdu_Studio_.*\.zip$"
    }
  $requiredUntrackedAppFiles = git ls-files --others --exclude-standard |
    Where-Object {
      $_ -match "^(R/|www/|README_KO\.md|CHANGELOG_[A-Z]{2}\.md|docs/(ANALYSIS_METHODS|METHOD_NOTES)_[A-Z]{2}\.md|docs/i18n/|scripts/validate_localized_about_docs\.R|docs/ANALYSIS_REFERENCE_COMPARISON_PUBLIC(_KO)?\.md|docs/assets/user-guide/(en|ko)/)"
    }
  $appFiles = @($appFiles + $requiredUntrackedAppFiles) | Sort-Object -Unique
  foreach ($releaseHelper in @(
    'docs/RELEASE_1_3_1_PREPARATION_KO.md', 'docs/MINIMUM_VERSION_POLICY_KO.md',
    'scripts/validate_minimum_version_policy.R', 'scripts/validate_public_131_scope.R'
  )) {
    if (Test-Path -LiteralPath (Join-Path $repoRoot $releaseHelper)) { $appFiles += $releaseHelper }
  }
  $appFiles = @($appFiles | Sort-Object -Unique)
  $documentationManifest = Get-Content -LiteralPath (Join-Path $repoRoot 'docs/i18n/document_specs.json') -Raw -Encoding UTF8 | ConvertFrom-Json
  $documentationFiles = @('docs/i18n/document_specs.json')
  foreach ($languageSpec in $documentationManifest.PSObject.Properties) {
    foreach ($documentSpec in $languageSpec.Value.PSObject.Properties) {
      $documentationFiles += $documentSpec.Value.path
    }
  }
  foreach ($documentFile in ($documentationFiles | Sort-Object -Unique)) {
    if (-not (Test-Path -LiteralPath (Join-Path $repoRoot $documentFile) -PathType Leaf)) {
      throw "Required localized document missing: $documentFile"
    }
  }
  $appFiles = @($appFiles + $documentationFiles) | Sort-Object -Unique
  # Native HWPX uses packaged XML defaults, not a Word/Hancom converter.
  $requiredRuntimeHelpers = @(
    "www/hwpx-base/header.xml", "www/hwpx-base/section-properties.xml",
    "www/hwpx-base/picture.xml", "www/hwpx-base/version.xml", "www/hwpx-base/manifest.xml"
  )
  foreach ($helper in $requiredRuntimeHelpers) {
    if (-not (Test-Path -LiteralPath (Join-Path $repoRoot $helper) -PathType Leaf)) {
      throw "Required runtime helper is missing: $helper"
    }
  }
  $appFiles = @($appFiles + $requiredRuntimeHelpers) | Sort-Object -Unique
  $bootstrapText = Get-Content -LiteralPath (Join-Path $repoRoot "R\app_bootstrap.R") -Raw
  $bootstrapModules = [regex]::Matches($bootstrapText, '"([^"]+\.R)"') |
    ForEach-Object { "R/" + $_.Groups[1].Value } |
    Sort-Object -Unique
  $missingTrackedModules = @($bootstrapModules | Where-Object { $_ -notin $appFiles })
  if ($missingTrackedModules.Count -gt 0) {
    throw "R module(s) referenced by app_bootstrap.R are not tracked by git and would be omitted from the Electron app stage: $($missingTrackedModules -join ', ')"
  }
  $focusedPlsDriverRelative = "scripts/run_bundled_pls_focused_regressions.R"
  $focusedPlsDriverPath = Join-Path $repoRoot ($focusedPlsDriverRelative -replace "/", "\")
  $focusedPlsDriverText = Get-Content -LiteralPath $focusedPlsDriverPath -Raw
  $focusedPlsValidationFiles = [regex]::Matches(
    $focusedPlsDriverText,
    'script\s*=\s*"(scripts/[^"\r\n]+\.R)"'
  ) | ForEach-Object { $_.Groups[1].Value } | Sort-Object -Unique
  $requiredTrackedReleaseValidationFiles = @(
    $focusedPlsDriverRelative,
    "scripts/bundled_validation_packages.expected.csv"
  ) + @($focusedPlsValidationFiles)
  $missingTrackedReleaseValidationFiles = @(
    $requiredTrackedReleaseValidationFiles |
      Sort-Object -Unique |
      Where-Object { $_ -notin $appFiles }
  )
  if ($missingTrackedReleaseValidationFiles.Count -gt 0) {
    throw "Release-validation file(s) are not tracked by git and would be omitted from the Electron app stage: $($missingTrackedReleaseValidationFiles -join ', ')"
  }
  foreach ($file in $appFiles) {
    $source = Join-Path $repoRoot ($file -replace "/", "\")
    $target = Join-Path $appStage ($file -replace "/", "\")
    New-Item -ItemType Directory -Path (Split-Path $target -Parent) -Force | Out-Null
    Copy-Item -LiteralPath $source -Destination $target -Force
  }

  foreach ($file in @("LICENSE", "SOURCE-OFFER.txt")) {
    $source = Join-Path $repoRoot $file
    if (Test-Path -LiteralPath $source) {
      $target = Join-Path $appStage $file
      Copy-Item -LiteralPath $source -Destination $target -Force
    }
  }
} finally {
  Pop-Location
}

if ($FineGrayComponentRoot) {
  & (Join-Path $PSScriptRoot 'stage_fine_gray_native.ps1') -ComponentRoot $FineGrayComponentRoot -AppStage $appStage
}

if (-not $SkipRuntimeCopy) {
  if (-not $RHome) {
    $hostRscript = Find-Rscript
    $RHome = (& $hostRscript -e "cat(normalizePath(R.home(), winslash='\\', mustWork=TRUE))")
  }
  if (-not (Test-Path -LiteralPath (Join-Path $RHome "bin\x64\Rscript.exe"))) {
    throw "Rscript.exe was not found under RHome: $RHome"
  }
  Write-Host "Copying R runtime from $RHome"
  Copy-Directory $RHome $runtimeStage

  $runtimeLibrary = Join-Path $runtimeStage "library"
  $dependencyScript = @"
source(file.path("$($repoRoot -replace "\\", "/")", "R", "app_bootstrap.R"), local = TRUE)
required <- required_packages
db <- installed.packages()
deps <- tools::package_dependencies(required, db = db, which = c("Depends", "Imports", "LinkingTo"), recursive = TRUE)
packages <- sort(unique(c(required, unlist(deps, use.names = FALSE))))
cat(packages, sep = "\n")
"@
  $requiredPackages = Invoke-RScriptFile (Join-Path $RHome "bin\x64\Rscript.exe") $dependencyScript
  $libraryPaths = Invoke-RScript (Join-Path $RHome "bin\x64\Rscript.exe") @("-e", "cat(.libPaths(), sep='\n')")
  $libraryPaths = @($libraryPaths | Where-Object { $_ -and (Test-Path -LiteralPath $_) })
  Write-Host ("Copying {0} required R package(s) and dependencies" -f $requiredPackages.Count)
  foreach ($package in $requiredPackages) {
    Copy-R-Package $package $libraryPaths $runtimeLibrary | Out-Null
  }
}

if (Test-Path -LiteralPath (Join-Path $runtimeStage "bin\x64\Rscript.exe")) {
  Write-Host "Validating exact bundled R package versions before validation-package staging"
  Invoke-RScript (Join-Path $runtimeStage "bin\x64\Rscript.exe") @(
    "--vanilla",
    (Join-Path $repoRoot "scripts\validate_bundled_runtime_package_versions.R"),
    "--repo-root=$repoRoot",
    "--runtime-root=$runtimeStage"
  )

  Write-Host "Staging pinned release-validation R packages"
  Invoke-RScript $gateRscript @(
    "--vanilla",
    (Join-Path $repoRoot "scripts\stage_bundled_validation_packages.R"),
    "--repo-root=$repoRoot",
    "--runtime-root=$runtimeStage",
    "--source-library=$resolvedBundledValidationLibrary",
    "--output-dir=$appStage",
    "--execute"
  )

  Write-Host "Pruning bundled R runtime packages"
  Invoke-RScript (Join-Path $runtimeStage "bin\x64\Rscript.exe") @(
    (Join-Path $repoRoot "scripts\prune_r_runtime.R"),
    "--repo-root=$repoRoot",
    "--runtime-root=$runtimeStage",
    "--output-dir=$appStage",
    "--execute"
  )

  Write-Host "Generating third-party license notices"
  Invoke-RScript (Join-Path $runtimeStage "bin\x64\Rscript.exe") @(
    (Join-Path $repoRoot "scripts\generate_oss_notices.R"),
    "--repo-root=$repoRoot",
    "--runtime-root=$runtimeStage",
    "--output-dir=$appStage"
  )

  # Keep the 1.1.1 public packaging rule: exclude R runtime documentation,
  # tests, examples, source payloads, and other non-runtime content.
  Write-Host "Pruning bundled R runtime documentation and test payloads"
  Invoke-RScript (Join-Path $runtimeStage "bin\x64\Rscript.exe") @(
    (Join-Path $repoRoot "scripts\prune_r_runtime_content.R"),
    "--runtime-root=$runtimeStage",
    "--output-dir=$appStage",
    "--execute"
  )

  Write-Host "Running bundled-runtime-only focused PLS/PLSc regressions"
  $runtimeLibrary = Join-Path $runtimeStage "library"
  $previousRuntimeRLibs = $env:R_LIBS
  $previousCsemMode = $env:STATEDU_CSEM_VALIDATION_MODE
  $previousRuntimeRLibsUser = $env:R_LIBS_USER
  $previousRuntimeRLibsSite = $env:R_LIBS_SITE
  try {
    $env:R_LIBS = $runtimeLibrary
    $env:R_LIBS_USER = $runtimeLibrary
    $env:R_LIBS_SITE = $runtimeLibrary
    $env:STATEDU_CSEM_VALIDATION_MODE = "required"
    Push-Location $appStage
    try {
      Invoke-RScript (Join-Path $runtimeStage "bin\x64\Rscript.exe") @(
        "--vanilla",
        (Join-Path $appStage "scripts\run_bundled_pls_focused_regressions.R"),
        "--repo-root=$appStage",
        "--runtime-root=$runtimeStage",
        "--timeout-seconds=900"
      )
    } finally {
      Pop-Location
    }
  } finally {
    if ($null -eq $previousRuntimeRLibs) {
      Remove-Item Env:\R_LIBS -ErrorAction SilentlyContinue
    } else {
      $env:R_LIBS = $previousRuntimeRLibs
    }
    if ($null -eq $previousRuntimeRLibsUser) {
      Remove-Item Env:\R_LIBS_USER -ErrorAction SilentlyContinue
    } else {
      $env:R_LIBS_USER = $previousRuntimeRLibsUser
    }
    if ($null -eq $previousRuntimeRLibsSite) {
      Remove-Item Env:\R_LIBS_SITE -ErrorAction SilentlyContinue
    } else {
      $env:R_LIBS_SITE = $previousRuntimeRLibsSite
    }
    if ($null -eq $previousCsemMode) {
      Remove-Item Env:\STATEDU_CSEM_VALIDATION_MODE -ErrorAction SilentlyContinue
    } else {
      $env:STATEDU_CSEM_VALIDATION_MODE = $previousCsemMode
    }
  }
  [System.IO.File]::WriteAllText((Join-Path $appStage "VERSION"), "$version`n", [System.Text.UTF8Encoding]::new($false))
} else {
  Write-Warning "R runtime was not found; third-party license notices were not generated."
}

$npmRunner = Find-NpmRunner
if ($NodePath) {
  if (-not (Test-Path -LiteralPath $NodePath)) { throw "The requested Node.js executable was not found: $NodePath" }
  $env:PATH = "$(Split-Path $NodePath);$env:PATH"
} elseif ($npmRunner.Kind -eq "npm") {
  $env:PATH = "$(Split-Path $npmRunner.Path);$env:PATH"
}
$env:CSC_IDENTITY_AUTO_DISCOVERY = "false"
$env:USE_HARD_LINKS = "false"
Push-Location $electronDir
try {
  if (-not $SkipNpmInstall) {
    if (Test-Path -LiteralPath "package-lock.json") {
      Invoke-Npm $npmRunner @("ci")
    } else {
      Invoke-Npm $npmRunner @("install")
    }
  }
  Sync-ElectronPackageMetadata
  Invoke-Npm $npmRunner @("run", "dist", "--", "--publish", "never")
} finally {
  Pop-Location
}

foreach ($devArtifact in @(".Rhistory", "builder-debug.yml")) {
  $devArtifactPath = Join-Path $distDir $devArtifact
  if (Test-Path -LiteralPath $devArtifactPath) {
    Remove-Item -LiteralPath $devArtifactPath -Force
  }
}
Remove-StaleElectronDistArtifacts

Write-Host "Electron installer output:"
Get-ChildItem -LiteralPath $distDir -Filter "*.exe" | Sort-Object LastWriteTime -Descending | Select-Object FullName, Length, LastWriteTime
