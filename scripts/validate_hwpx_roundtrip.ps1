param([Parameter(Mandatory=$true)][string]$SourceFile, [Parameter(Mandatory=$true)][string]$OutputDirectory)
$ErrorActionPreference = 'Stop'
$source = (Resolve-Path -LiteralPath $SourceFile).Path
$output = (Resolve-Path -LiteralPath $OutputDirectory).Path
$folder = Join-Path ([IO.Path]::GetTempPath()) ('StatEduHwpxRoundtrip_' + [Guid]::NewGuid().ToString('N'))
[void](New-Item -ItemType Directory -Path $folder)
Copy-Item -LiteralPath $source -Destination (Join-Path $folder 'source.hwpx')
$hwp = $null
try {
    $hwp = New-Object -ComObject HWPFrame.HwpObject
    $hwp.XHwpWindows.Item(0).Visible = $false
    if (-not $hwp.Open((Join-Path $folder 'source.hwpx'), 'HWPX', '')) { throw 'HWPX reopen failed.' }
    if (-not $hwp.SaveAs((Join-Path $folder 'roundtrip.docx'), 'OOXML', '')) { throw 'DOCX round trip failed.' }
    if (-not $hwp.SaveAs((Join-Path $folder 'roundtrip.pdf'), 'PDF', '')) { throw 'PDF rendering failed.' }
    Copy-Item -LiteralPath (Join-Path $folder 'roundtrip.docx') -Destination (Join-Path $output 'roundtrip.docx')
    Copy-Item -LiteralPath (Join-Path $folder 'roundtrip.pdf') -Destination (Join-Path $output 'roundtrip.pdf')
    Write-Output 'PASS: Hancom reopened HWPX and produced DOCX/PDF for comparison.'
} finally {
    if ($null -ne $hwp) {
        try { $hwp.Clear(1) } catch {}
        try { $hwp.Quit() } catch {}
        [void][Runtime.InteropServices.Marshal]::FinalReleaseComObject($hwp)
    }
    if ([IO.Path]::GetDirectoryName($folder).TrimEnd('\') -eq [IO.Path]::GetTempPath().TrimEnd('\')) {
        Remove-Item -LiteralPath $folder -Recurse -Force -ErrorAction SilentlyContinue
    }
}
