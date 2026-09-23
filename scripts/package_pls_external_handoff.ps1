param(
  [string]$RscriptPath = "",
  [string]$OutputArchive = "outputs\StatEdu_1.2.4_PLS_external_handoff.zip",
  [string]$Profile = "holzinger-swineford-first100-smartpls-student"
)

$ErrorActionPreference = "Stop"
Set-StrictMode -Version Latest

$profileAliases = @{
  "holzinger-swineford-first100-smartpls-student" = "holzinger-swineford-first100-smartpls-student"
  "student100" = "holzinger-swineford-first100-smartpls-student"
  "first100" = "holzinger-swineford-first100-smartpls-student"
  "hs100" = "holzinger-swineford-first100-smartpls-student"
  "holzinger-swineford-301" = "holzinger-swineford-301"
  "historical301" = "holzinger-swineford-301"
  "full301" = "holzinger-swineford-301"
  "hs301" = "holzinger-swineford-301"
}
$profileKey = $Profile.Trim().ToLowerInvariant()
if (-not $profileKey) { $profileKey = "holzinger-swineford-first100-smartpls-student" }
if (-not $profileAliases.ContainsKey($profileKey)) {
  throw "Unknown PLS external benchmark profile: $Profile"
}
$canonicalProfile = $profileAliases[$profileKey]
$profileDataFiles = @{
  "holzinger-swineford-first100-smartpls-student" = "HolzingerSwineford1939_first100_x1_x9.txt"
  "holzinger-swineford-301" = "HolzingerSwineford1939.csv"
}

$repoRoot = Split-Path -Parent $PSScriptRoot
if (-not $RscriptPath) {
  $RscriptPath = Join-Path $repoRoot "packaging\electron\runtime\R-4.5.3\bin\x64\Rscript.exe"
}
if (-not (Test-Path -LiteralPath $RscriptPath -PathType Leaf)) {
  throw "Rscript was not found: $RscriptPath"
}

$archiveFullPath = if ([System.IO.Path]::IsPathRooted($OutputArchive)) {
  [System.IO.Path]::GetFullPath($OutputArchive)
} else {
  [System.IO.Path]::GetFullPath((Join-Path $repoRoot $OutputArchive))
}
$archiveParent = Split-Path -Parent $archiveFullPath
New-Item -ItemType Directory -Path $archiveParent -Force | Out-Null

$tempRoot = [System.IO.Path]::GetFullPath([System.IO.Path]::GetTempPath())
$stagingDirectory = Join-Path $tempRoot ("statedu-pls-handoff-" + [guid]::NewGuid().ToString("N"))
New-Item -ItemType Directory -Path $stagingDirectory | Out-Null

try {
  Push-Location $repoRoot
  try {
    & $RscriptPath "scripts\prepare_pls_external_handoff.R" "--output-dir=$stagingDirectory" "--profile=$canonicalProfile"
    if ($LASTEXITCODE -ne 0) { throw "PLS external handoff generation failed with exit code $LASTEXITCODE." }
  } finally {
    Pop-Location
  }

  $runRecordPath = Join-Path $stagingDirectory "external_run.json"
  if (-not (Test-Path -LiteralPath $runRecordPath -PathType Leaf)) {
    throw "PLS external handoff is missing external_run.json."
  }
  $runRecord = Get-Content -LiteralPath $runRecordPath -Raw | ConvertFrom-Json
  $recordedProfile = [string]$runRecord.profile
  if ($recordedProfile -ne $canonicalProfile) {
    throw "PLS external handoff profile mismatch: requested $canonicalProfile; recorded $recordedProfile."
  }
  $dataFile = [string]$runRecord.data_file
  if ([string]::IsNullOrWhiteSpace($dataFile) -or
      [System.IO.Path]::IsPathRooted($dataFile) -or
      $dataFile -match '[\\/]' -or
      $dataFile -in @('.', '..') -or
      $dataFile -ne [System.IO.Path]::GetFileName($dataFile)) {
    throw "external_run.json data_file must be a safe basename."
  }
  $expectedDataFile = $profileDataFiles[$canonicalProfile]
  if ($dataFile -ne $expectedDataFile) {
    throw "PLS external handoff data/profile mismatch: expected $expectedDataFile; recorded $dataFile."
  }
  $staleProfileFiles = @($profileDataFiles.Values | Where-Object {
    $_ -ne $dataFile -and (Test-Path -LiteralPath (Join-Path $stagingDirectory $_) -PathType Leaf)
  })
  if ($staleProfileFiles.Count -gt 0) {
    throw "PLS external handoff contains stale data from another profile: $($staleProfileFiles -join ', ')"
  }

  $requiredFiles = @(
    "benchmark_manifest.json",
    "external_fit_template.csv",
    "external_fit.csv",
    "external_run.json",
    $dataFile,
    "measurement_model.csv",
    "pls_external_benchmark.stmodel",
    "RUN_EXTERNAL.md",
    "statedu_fit.csv",
    "structural_paths.csv"
  )
  $missingFiles = @($requiredFiles | Where-Object { -not (Test-Path -LiteralPath (Join-Path $stagingDirectory $_) -PathType Leaf) })
  if ($missingFiles.Count -gt 0) {
    throw "PLS external handoff is missing required file(s): $($missingFiles -join ', ')"
  }

  $manifestRows = Get-ChildItem -LiteralPath $stagingDirectory -File |
    Sort-Object Name |
    ForEach-Object {
      [pscustomobject]@{
        File = $_.Name
        Bytes = $_.Length
        SHA256 = (Get-FileHash -LiteralPath $_.FullName -Algorithm SHA256).Hash
      }
    }
  $manifestPath = Join-Path $stagingDirectory "HANDOFF_SHA256.csv"
  $manifestRows | Export-Csv -LiteralPath $manifestPath -NoTypeInformation -Encoding utf8

  Compress-Archive -Path (Join-Path $stagingDirectory "*") -DestinationPath $archiveFullPath -CompressionLevel Optimal -Force
  $archiveHash = (Get-FileHash -LiteralPath $archiveFullPath -Algorithm SHA256).Hash
  $hashPath = "$archiveFullPath.sha256"
  Set-Content -LiteralPath $hashPath -Value "$archiveHash  $([System.IO.Path]::GetFileName($archiveFullPath))" -Encoding utf8

  Write-Output "PLS external handoff archive: $archiveFullPath"
  Write-Output "Archive SHA256: $archiveHash"
  Write-Output "Archive bytes: $((Get-Item -LiteralPath $archiveFullPath).Length)"
  Write-Output "Checksum file: $hashPath"
} finally {
  $resolvedStaging = [System.IO.Path]::GetFullPath($stagingDirectory)
  if (-not $resolvedStaging.StartsWith($tempRoot, [System.StringComparison]::OrdinalIgnoreCase) -or
      -not ([System.IO.Path]::GetFileName($resolvedStaging)).StartsWith("statedu-pls-handoff-")) {
    throw "Refusing to remove unexpected staging directory: $resolvedStaging"
  }
  if (Test-Path -LiteralPath $resolvedStaging) {
    Remove-Item -LiteralPath $resolvedStaging -Recurse -Force
  }
}
