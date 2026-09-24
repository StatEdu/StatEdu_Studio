param([string]$OutputDirectory = 'output/performance-review-inventory-20260915')
$ErrorActionPreference = 'Stop'
$repoRoot = Split-Path $PSScriptRoot -Parent
$destination = Join-Path $repoRoot $OutputDirectory
New-Item -ItemType Directory -Force -Path $destination | Out-Null
$records = @(Get-ChildItem (Join-Path $repoRoot 'docs') -Filter 'PERFORMANCE*.md' | Sort-Object Name | ForEach-Object {
    $body = [IO.File]::ReadAllText($_.FullName)
    [pscustomobject]@{
        File = $_.Name
        Title = ($body -split '\r?\n' | Where-Object { $_ -match '^# ' } | Select-Object -First 1)
        Sha256 = (Get-FileHash -LiteralPath $_.FullName -Algorithm SHA256).Hash
        Body = $body
    }
})
$records | Select-Object File,Title,Sha256 | Export-Csv (Join-Path $destination 'reviews.csv') -NoTypeInformation -Encoding utf8
$modules = @(Get-ChildItem (Join-Path $repoRoot 'R') -Filter '*.R' | Sort-Object Name | ForEach-Object {
    $moduleName = $_.Name
    $hits = @($records | Where-Object { $_.Body.IndexOf($moduleName, [StringComparison]::OrdinalIgnoreCase) -ge 0 })
    [pscustomobject]@{
        Module = $moduleName
        Bytes = $_.Length
        Sha256 = (Get-FileHash -LiteralPath $_.FullName -Algorithm SHA256).Hash
        ExactFilenameMentionCount = $hits.Count
        Reviews = ($hits.File -join ';')
    }
})
$modules | Export-Csv (Join-Path $destination 'module-references.csv') -NoTypeInformation -Encoding utf8
[pscustomobject]@{ ReviewDocuments=$records.Count; RModules=$modules.Count; ModulesWithoutExactFilenameMention=@($modules | Where-Object ExactFilenameMentionCount -eq 0).Count } | Format-List
Write-Output 'Filename mentions are discovery aids, not proof of review coverage or completion.'
