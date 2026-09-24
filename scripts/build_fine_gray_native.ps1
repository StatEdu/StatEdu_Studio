param(
    [Parameter(Mandatory=$true)][string]$ToolchainRoot,
    [Parameter(Mandatory=$true)][string]$OutputDirectory
)
$ErrorActionPreference = 'Stop'
$repoRoot = Split-Path $PSScriptRoot -Parent
$sourceRoot = Join-Path $repoRoot 'native/fine_gray'
$compiler = Join-Path (Resolve-Path -LiteralPath $ToolchainRoot).Path 'bin/gfortran.exe'
if (-not (Test-Path -LiteralPath $compiler)) { throw 'gfortran.exe is missing' }
$version = (& $compiler -dumpfullversion | Out-String).Trim()
if ($LASTEXITCODE -ne 0 -or $version -ne '14.3.0') { throw 'Expected GFortran 14.3.0 from Rtools45' }
$outputRoot = [IO.Path]::GetFullPath($OutputDirectory)
foreach ($name in @('original.dll','candidate.dll','build-manifest.json')) {
    if (Test-Path -LiteralPath (Join-Path $outputRoot $name)) { throw "Build output already exists: $name" }
}
New-Item -ItemType Directory -Force -Path $outputRoot | Out-Null
$flags = @('-shared','-O2','-mfpmath=sse','-msse2','-mstackrealign','-static-libgcc','-static-libgfortran','-Wl,--no-insert-timestamp','-Wl,--image-base,0x180000000')
$previousPath = $env:PATH
try {
    $env:PATH = "$(Split-Path $compiler -Parent);$previousPath"
    foreach ($item in @(@('crr.f','original.dll'),@('crr_cached.f','candidate.dll'))) {
        & $compiler @flags (Join-Path $sourceRoot "src/$($item[0])") -o (Join-Path $outputRoot $item[1])
        if ($LASTEXITCODE -ne 0) { throw "Compilation failed: $($item[0])" }
    }
} finally {
    $env:PATH = $previousPath
}
$manifest = [ordered]@{
    upstream = 'cmprsk 2.2-12'
    compiler = $version
    compilerSha256 = (Get-FileHash -LiteralPath $compiler -Algorithm SHA256).Hash
    flags = $flags
    sources = @(Get-ChildItem -LiteralPath (Join-Path $sourceRoot 'src') -File | ForEach-Object {
        [ordered]@{name=$_.Name;sha256=(Get-FileHash -LiteralPath $_.FullName -Algorithm SHA256).Hash}
    })
    binaries = @('original.dll','candidate.dll') | ForEach-Object {
        [ordered]@{name=$_;sha256=(Get-FileHash -LiteralPath (Join-Path $outputRoot $_) -Algorithm SHA256).Hash;md5=(Get-FileHash -LiteralPath (Join-Path $outputRoot $_) -Algorithm MD5).Hash}
    }
}
$manifest | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath (Join-Path $outputRoot 'build-manifest.json') -Encoding UTF8
Write-Output "Built isolated DLLs in $outputRoot"
