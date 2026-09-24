param(
    [Parameter(Mandatory = $true)][string]$RequestFile,
    [switch]$Worker,
    [string]$StateFile
)
$ErrorActionPreference = 'Stop'
# Use Hancom's normal access policy. Never edit registry or suppress access dialogs.
if (-not $Worker) {
    $workFolder = Join-Path ([IO.Path]::GetTempPath()) ('StatEduHwpx_' + [Guid]::NewGuid().ToString('N'))
    [void](New-Item -ItemType Directory -Path $workFolder)
    $state = Join-Path $workFolder 'worker.json'
    $stdout = Join-Path $workFolder 'stdout.txt'
    $stderr = Join-Path $workFolder 'stderr.txt'
    $process = $null
    try {
        $request = Get-Content -LiteralPath $RequestFile -Raw -Encoding UTF8 | ConvertFrom-Json
        $source = [IO.Path]::GetFullPath([string]$request.source)
        $destination = [IO.Path]::GetFullPath([string]$request.target)
        if ([IO.Path]::GetExtension($source) -ne '.docx' -or [IO.Path]::GetExtension($destination) -ne '.hwpx') {
            throw 'Expected a DOCX source and an HWPX destination.'
        }
        Copy-Item -LiteralPath $source -Destination (Join-Path $workFolder 'source.docx')
        $stagedRequest = Join-Path $workFolder 'request.json'
        @{source=(Join-Path $workFolder 'source.docx');target=(Join-Path $workFolder 'result.hwpx')} |
            ConvertTo-Json | Set-Content -LiteralPath $stagedRequest -Encoding UTF8
        $argsText = '-NoProfile -NonInteractive -File "' + $PSCommandPath + '" -Worker -RequestFile "' + $stagedRequest + '" -StateFile "' + $state + '"'
        $process = Start-Process -FilePath (Join-Path $PSHOME 'powershell.exe') -ArgumentList $argsText -WindowStyle Hidden -PassThru -RedirectStandardOutput $stdout -RedirectStandardError $stderr
        $null = $process.Handle # Retain the handle so Windows PowerShell can read ExitCode.
        if (-not $process.WaitForExit(90000)) {
            throw 'Hancom conversion timed out. Check the Hancom installation and its file-access policy.'
        }
        if (Test-Path -LiteralPath $stdout) { Get-Content -LiteralPath $stdout }
        if ($process.ExitCode -ne 0 -or -not (Test-Path -LiteralPath (Join-Path $workFolder 'result.hwpx'))) {
            $detail = if (Test-Path -LiteralPath $stderr) { Get-Content -LiteralPath $stderr -Raw } else { '' }
            throw "Hancom conversion failed. $detail"
        }
        Copy-Item -LiteralPath (Join-Path $workFolder 'result.hwpx') -Destination $destination
    } finally {
        # Only clean up the new automation process, identified by window handle
        # and creation time, never by name or by a before/after process list alone.
        if (Test-Path -LiteralPath $state) {
            $owned = Get-Content -LiteralPath $state -Raw | ConvertFrom-Json
            $hancom = Get-Process -Id ([int]$owned.id) -ErrorAction SilentlyContinue
            if ($hancom -and $hancom.ProcessName -ieq 'Hwp' -and $hancom.StartTime.ToUniversalTime().Ticks.ToString() -eq [string]$owned.started) {
                # Quit may finish after Get-Process but before Kill. Treat only
                # that confirmed exit as successful cleanup; retain real errors.
                try {
                    if (-not $hancom.HasExited) { $hancom.Kill() }
                } catch {
                    $hancom.Refresh()
                    if (-not $hancom.HasExited) { throw }
                }
            }
        }
        if ($process -and -not $process.HasExited) { $process.Kill(); [void]$process.WaitForExit(3000) }
        # This is the unique directory created above; verify its parent before cleanup.
        if ([IO.Path]::GetDirectoryName($workFolder).TrimEnd('\') -eq [IO.Path]::GetTempPath().TrimEnd('\')) {
            Remove-Item -LiteralPath $workFolder -Recurse -Force -ErrorAction SilentlyContinue
        }
    }
    exit 0
}
$request = Get-Content -LiteralPath $RequestFile -Raw -Encoding UTF8 | ConvertFrom-Json
$ownedHwp = $null
$ownsProcess = $false
$existingIds = @(Get-Process Hwp -ErrorAction SilentlyContinue | Select-Object -ExpandProperty Id)
Add-Type @'
using System;
using System.Runtime.InteropServices;
public static class StatEduHwpWindow {
    [DllImport("user32.dll")]
    public static extern uint GetWindowThreadProcessId(IntPtr hwnd, out uint processId);
}
'@
try {
    $ownedHwp = New-Object -ComObject HWPFrame.HwpObject
    $window = $ownedHwp.XHwpWindows.Item(0)
    [uint32]$hwpProcessId = 0
    [void][StatEduHwpWindow]::GetWindowThreadProcessId([IntPtr]$window.WindowHandle, [ref]$hwpProcessId)
    if ($hwpProcessId -eq 0 -or $hwpProcessId -in $existingIds) { throw 'Could not identify an isolated Hancom process.' }
    $ownsProcess = $true
    $hancom = Get-Process -Id $hwpProcessId
    @{id=$hwpProcessId;started=$hancom.StartTime.ToUniversalTime().Ticks.ToString()} | ConvertTo-Json | Set-Content -LiteralPath $StateFile
    $window.Visible = $false
    # Explicitly configured modules remain optional; default conversion needs none.
    if ($env:STATEDU_HWP_SECURITY_MODULE) {
        if (-not $ownedHwp.RegisterModule('FilePathCheckDLL', [string]$env:STATEDU_HWP_SECURITY_MODULE)) {
            throw 'The explicitly configured Hancom file-access module could not be registered.'
        }
    }
    if (-not $ownedHwp.Open([string]$request.source, 'OOXML', '')) { throw 'Hancom could not open the captured Word document.' }
    if (-not $ownedHwp.SaveAs([string]$request.target, 'HWPX', '')) { throw 'Hancom could not save HWPX.' }
    Write-Output 'HWPX conversion completed.'
} finally {
    if ($null -ne $ownedHwp) {
        if ($ownsProcess) {
            try { $ownedHwp.Clear(1) } catch {}
            try { $ownedHwp.Quit() } catch {}
        }
        [void][Runtime.InteropServices.Marshal]::FinalReleaseComObject($ownedHwp)
    }
}
