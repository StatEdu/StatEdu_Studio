param(
    [Parameter(Mandatory=$true)][string]$ComponentRoot,
    [string]$AppStage = '',
    [switch]$ValidateOnly
)
$ErrorActionPreference = 'Stop'
$repoRoot = Split-Path $PSScriptRoot -Parent
$component = (Resolve-Path -LiteralPath $ComponentRoot).Path
$expectedDll = 'E90C46F63FBD1ED2B60DF0CE62068A8765E149DE7C3876C67A3204D323C0CE3D'
$dll = Join-Path $component 'bin/candidate.dll'
if ((Get-FileHash -LiteralPath $dll -Algorithm SHA256).Hash -ne $expectedDll) {
    throw 'Fine-Gray component DLL is not the validated reproducible build'
}
$manifest = Get-Content -LiteralPath (Join-Path $component 'build-manifest.json') -Raw | ConvertFrom-Json
$binary = @($manifest.binaries | Where-Object { $_.name -eq 'candidate.dll' })
if ($manifest.upstream -ne 'cmprsk 2.2-12' -or $manifest.compiler -ne '14.3.0' -or
    $binary.Count -ne 1 -or $binary[0].sha256 -ne $expectedDll) {
    throw 'Fine-Gray component manifest does not match the validated build'
}
$sources = @(
    'native/fine_gray/src/crr.f', 'native/fine_gray/src/crr_cached.f',
    'native/fine_gray/generate_cached.py', 'native/fine_gray/UPSTREAM_DESCRIPTION',
    'native/fine_gray/COPYING', 'native/fine_gray/README.md',
    'native/fine_gray/tests/common.R', 'native/fine_gray/tests/verify.R',
    'native/fine_gray/tests/extra.R', 'native/fine_gray/tests/edge.R',
    'scripts/build_fine_gray_native.ps1', 'scripts/validate_fine_gray_native_build.R'
)
foreach ($relative in $sources) {
    $supplied = Join-Path $component "source/$relative"
    $expected = Join-Path $repoRoot $relative
    if ((Get-FileHash -LiteralPath $supplied -Algorithm SHA256).Hash -ne
        (Get-FileHash -LiteralPath $expected -Algorithm SHA256).Hash) {
        throw "Fine-Gray source differs from the reviewed repository file: $relative"
    }
}
foreach ($name in @('crr.f','crr_cached.f')) {
    $record = @($manifest.sources | Where-Object { $_.name -eq $name })
    $hash = (Get-FileHash -LiteralPath (Join-Path $component "source/native/fine_gray/src/$name") -Algorithm SHA256).Hash
    if ($record.Count -ne 1 -or $record[0].sha256 -ne $hash) { throw "Source manifest mismatch: $name" }
}
if ($ValidateOnly) { Write-Output 'PASS: Fine-Gray component validation'; return }
if (-not $AppStage) { throw 'AppStage is required for staging' }
$stageRoot = (Resolve-Path -LiteralPath $AppStage).Path
$destination = Join-Path $stageRoot 'native/fine_gray/component'
if (Test-Path -LiteralPath $destination) { throw 'Fine-Gray component destination already exists' }
# Copy only the files that were checked, never extra component payloads.
$files = @('bin/candidate.dll','build-manifest.json') + @($sources | ForEach-Object { "source/$_" })
foreach ($relative in $files) {
    $target = Join-Path $destination $relative
    New-Item -ItemType Directory -Force -Path (Split-Path $target -Parent) | Out-Null
    Copy-Item -LiteralPath (Join-Path $component $relative) -Destination $target
    if ((Get-FileHash -LiteralPath $target).Hash -ne (Get-FileHash -LiteralPath (Join-Path $component $relative)).Hash) {
        throw "Staged file mismatch: $relative"
    }
}
Write-Output "Staged optional Fine-Gray component: $destination"
