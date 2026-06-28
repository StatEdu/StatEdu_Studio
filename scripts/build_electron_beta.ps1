param(
  [string]$RHome = "",
  [switch]$SkipRuntimeCopy,
  [switch]$SkipNpmInstall
)

$ErrorActionPreference = "Stop"

$repoRoot = Resolve-Path (Join-Path $PSScriptRoot "..")
$version = (Get-Content (Join-Path $repoRoot "VERSION")).Trim()
$electronDir = Join-Path $repoRoot "packaging\electron"
$appStage = Join-Path $electronDir "app"
$runtimeStage = Join-Path $electronDir "runtime\R-4.5.3"
$runtimeRoot = Join-Path $electronDir "runtime"
$distDir = Join-Path $repoRoot "dist\electron"

function Get-ElectronReleaseProfile {
  if ($version -match "^1\.") {
    return [pscustomobject]@{
      PackageName = "statedu-studio"
      Description = "StatEdu Studio desktop installer"
      AppId = "com.statedu.studio"
      ProductName = "StatEdu Studio"
      ArtifactPrefix = "StatEdu_Studio_Setup"
      ShortcutName = "StatEdu Studio"
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
  $package.name = $profile.PackageName
  $package.version = $version
  $package.description = $profile.Description
  $package.build.appId = $profile.AppId
  $package.build.productName = $profile.ProductName
  $package.build.win.artifactName = "$($profile.ArtifactPrefix)_`${version}.`${ext}"
  $package.build.nsis.shortcutName = $profile.ShortcutName
  $json = ($package | ConvertTo-Json -Depth 20) + [Environment]::NewLine
  [System.IO.File]::WriteAllText($packagePath, $json, [System.Text.UTF8Encoding]::new($false))
  Write-Host "Electron package metadata: $($profile.ProductName), $($profile.ArtifactPrefix)_$version.exe"
}

function Find-Npm {
  $wingetNpm = Get-ChildItem "$env:LOCALAPPDATA\Microsoft\WinGet\Packages" -Recurse -Filter "npm.cmd" -ErrorAction SilentlyContinue |
    Where-Object { $_.Directory.Name -like "node-v*-win-x64" } |
    Sort-Object FullName |
    Select-Object -First 1
  if ($wingetNpm) {
    return $wingetNpm.FullName
  }
  $command = Get-Command "npm.cmd" -ErrorAction SilentlyContinue
  if ($command) {
    return $command.Source
  }
  throw "npm.cmd was not found. Install Node.js LTS with npm before building the Electron installer."
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
  & $FilePath @Arguments
  if ($LASTEXITCODE -ne 0) {
    throw "$FilePath failed with exit code $LASTEXITCODE"
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
    & $RscriptPath $tempScript
    if ($LASTEXITCODE -ne 0) {
      throw "$RscriptPath failed with exit code $LASTEXITCODE"
    }
  } finally {
    if (Test-Path -LiteralPath $tempScript) {
      Remove-Item -LiteralPath $tempScript -Force
    }
  }
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
  $artifacts = @(Get-ChildItem -LiteralPath $distDir -File -Force | Where-Object {
    (
      $_.Name -match "^StatEdu_Studio(_Beta)?_Setup_.*\.exe(\.blockmap)?$" -and
      $_.Name -notin @($currentSetupName, $currentBlockmapName)
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

$releaseProfile = Get-ElectronReleaseProfile
Write-Host "Preparing $($releaseProfile.ProductName) $version Electron installer..."
Remove-StaleElectronDistArtifacts
Remove-StaleRuntimeArtifacts

if (Test-Path -LiteralPath $appStage) {
  Remove-Item -LiteralPath $appStage -Recurse -Force
}

Push-Location $repoRoot
try {
  $appFiles = git ls-files |
    Where-Object {
      $_ -notmatch "^(packaging/|dist/)" -and
      $_ -notmatch "^R/latent_mplus_module\.R$" -and
      $_ -notmatch "^modules/latent_mplus/" -and
      $_ -notmatch "^easyflow_statistics_.*\.zip$" -and
      $_ -notmatch "^StatEdu_Studio_.*\.zip$"
    }
  $requiredUntrackedAppFiles = git ls-files --others --exclude-standard |
    Where-Object {
      $_ -match "^(R/update_check\.R|README_KO\.md|CHANGELOG_KO\.md|docs/ANALYSIS_METHODS_EN\.md|docs/ANALYSIS_REFERENCE_COMPARISON_PUBLIC(_KO)?\.md|docs/assets/user-guide/(en|ko)/|www/assets/user-guide/(en|ko)/)"
    }
  $appFiles = @($appFiles + $requiredUntrackedAppFiles) | Sort-Object -Unique
  $bootstrapText = Get-Content -LiteralPath (Join-Path $repoRoot "R\app_bootstrap.R") -Raw
  $bootstrapModules = [regex]::Matches($bootstrapText, '"([^"]+\.R)"') |
    ForEach-Object { "R/" + $_.Groups[1].Value } |
    Where-Object {
      $_ -notmatch "^R/latent_mplus_module\.R$"
    } |
    Sort-Object -Unique
  $missingTrackedModules = @($bootstrapModules | Where-Object { $_ -notin $appFiles })
  if ($missingTrackedModules.Count -gt 0) {
    throw "R module(s) referenced by app_bootstrap.R are not tracked by git and would be omitted from the Electron app stage: $($missingTrackedModules -join ', ')"
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
  $libraryPaths = & (Join-Path $RHome "bin\x64\Rscript.exe") -e "cat(.libPaths(), sep='\n')"
  $libraryPaths = @($libraryPaths | Where-Object { $_ -and (Test-Path -LiteralPath $_) })
  Write-Host ("Copying {0} required R package(s) and dependencies" -f $requiredPackages.Count)
  foreach ($package in $requiredPackages) {
    Copy-R-Package $package $libraryPaths $runtimeLibrary | Out-Null
  }
}

if (Test-Path -LiteralPath (Join-Path $runtimeStage "bin\x64\Rscript.exe")) {
  Write-Host "Pruning bundled R runtime packages"
  Invoke-Native (Join-Path $runtimeStage "bin\x64\Rscript.exe") @(
    (Join-Path $repoRoot "scripts\prune_r_runtime.R"),
    "--repo-root=$repoRoot",
    "--runtime-root=$runtimeStage",
    "--output-dir=$appStage",
    "--execute"
  )

  Write-Host "Generating third-party license notices"
  Invoke-Native (Join-Path $runtimeStage "bin\x64\Rscript.exe") @(
    (Join-Path $repoRoot "scripts\generate_oss_notices.R"),
    "--repo-root=$repoRoot",
    "--runtime-root=$runtimeStage",
    "--output-dir=$appStage"
  )
} else {
  Write-Warning "R runtime was not found; third-party license notices were not generated."
}

$npm = Find-Npm
$nodeDir = Split-Path $npm
$env:PATH = "$nodeDir;$env:PATH"
$env:CSC_IDENTITY_AUTO_DISCOVERY = "false"
$env:USE_HARD_LINKS = "false"
Push-Location $electronDir
try {
  if (-not $SkipNpmInstall) {
    if (Test-Path -LiteralPath "package-lock.json") {
      Invoke-Native $npm @("ci")
    } else {
      Invoke-Native $npm @("install")
    }
  }
  Sync-ElectronPackageMetadata
  Invoke-Native $npm @("run", "dist", "--", "--publish", "never")
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
