param(
    [string]$RepoRoot = (Split-Path $PSScriptRoot -Parent),
    [string]$SourceSnapshot = ''
)
$ErrorActionPreference = 'Stop'
$RepoRoot = (Resolve-Path -LiteralPath $RepoRoot).Path
$sourceRoot = if ($SourceSnapshot) { (Resolve-Path -LiteralPath $SourceSnapshot).Path } else { $RepoRoot }
$packagedApp = Join-Path $RepoRoot 'dist\electron\win-unpacked\resources\app.asar.unpacked\app'
if (-not (Test-Path -LiteralPath $packagedApp -PathType Container)) { throw "Packaged app missing: $packagedApp" }
Push-Location $RepoRoot
try {
    # Newly added analysis modules must be verified even before they are committed.
    $files = @(& git ls-files --cached --others --exclude-standard -- R www i18n)
    $documentManifestPath = 'docs/i18n/document_specs.json'
    $manifestSpec = Get-Content -LiteralPath (Join-Path $sourceRoot $documentManifestPath) -Raw -Encoding UTF8 | ConvertFrom-Json
    $files += @($documentManifestPath, 'README.md', 'README_KO.md', 'CITATION.cff')
    foreach ($languageSpec in $manifestSpec.PSObject.Properties) {
        foreach ($documentSpec in $languageSpec.Value.PSObject.Properties) { $files += $documentSpec.Value.path }
    }
    $records = foreach ($relative in ($files | Sort-Object -Unique)) {
        $source = Join-Path $sourceRoot $relative
        $target = Join-Path $packagedApp $relative
        if (-not (Test-Path -LiteralPath $target -PathType Leaf)) { throw "Packaged file missing: $relative" }
        $hash = (Get-FileHash -LiteralPath $source -Algorithm SHA256).Hash
        if ((Get-FileHash -LiteralPath $target -Algorithm SHA256).Hash -ne $hash) { throw "Packaged file differs: $relative" }
        [PSCustomObject]@{path=$relative; sha256=$hash}
    }
    $version = (Get-Content (Join-Path $packagedApp 'VERSION') -Raw).Trim()
    $prefix = if ($version -match '-dev$') { 'StatEdu_Studio_Dev_Setup' } elseif ($version -match '^\d+\.\d+\.\d+$') { 'StatEdu_Studio_Setup' } else { 'StatEdu_Studio_Beta_Setup' }
    $installer = Join-Path $RepoRoot "dist\electron\${prefix}_$version.exe"
    $installerInfo = Get-Item -LiteralPath $installer
    $installerHash = (Get-FileHash -LiteralPath $installer -Algorithm SHA256).Hash
    $workingTreeDifferences = @($records | Where-Object {
        (Get-FileHash -LiteralPath (Join-Path $RepoRoot $_.path) -Algorithm SHA256).Hash -ne $_.sha256
    } | ForEach-Object { $_.path })
    [IO.File]::WriteAllText("$installer.sha256", "$installerHash  $($installerInfo.Name)`n", [Text.UTF8Encoding]::new($false))
    $manifest = [PSCustomObject]@{
        version=$version; verifiedAtUtc=[DateTime]::UtcNow.ToString('o')
        installer=$installerInfo.Name; bytes=$installerInfo.Length; sha256=$installerHash
        verifiedFileCount=@($records).Count; files=@($records)
        sourceRoot=$sourceRoot; workingTreeDifferences=$workingTreeDifferences
    }
    $manifest | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath (Join-Path $RepoRoot 'dist\electron\packaged-source-verification.json') -Encoding utf8
    Write-Output "PASS: $(@($records).Count) packaged R/web/i18n/document files match source: $sourceRoot"
    if ($workingTreeDifferences.Count) {
        Write-Warning "Working-tree changes after this source snapshot: $($workingTreeDifferences -join ', ')"
    }
    Write-Output "$installer ($($installerInfo.Length) bytes)"
    Write-Output "SHA256 $installerHash"
} finally { Pop-Location }
