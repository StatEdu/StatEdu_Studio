# Manual diagnostic only; requires explicit approval to temporarily replace the
# existing FilePathCheckerModule value. Never called by the application.
param(
    [Parameter(Mandatory = $true)][string]$DllPath,
    [Parameter(Mandatory = $true)][string]$SourceDocx,
    [Parameter(Mandatory = $true)][string]$OutputDirectory
)
$ErrorActionPreference = 'Stop'
$DllPath = (Resolve-Path -LiteralPath $DllPath).Path
$SourceDocx = (Resolve-Path -LiteralPath $SourceDocx).Path
$OutputDirectory = (Resolve-Path -LiteralPath $OutputDirectory).Path
if ([IO.Path]::GetExtension($SourceDocx) -ne '.docx') { throw 'Expected DOCX input.' }
$keyPath = 'Software\HNC\HwpAutomation\Modules'
$valueName = 'FilePathCheckerModule'
$key = [Microsoft.Win32.Registry]::CurrentUser.OpenSubKey($keyPath, $true)
if ($null -eq $key -or $valueName -notin $key.GetValueNames()) { throw 'Existing module value is required.' }
$oldKind = $key.GetValueKind($valueName)
$oldValue = $key.GetValue($valueName, $null, [Microsoft.Win32.RegistryValueOptions]::DoNotExpandEnvironmentNames)
$runId = [Guid]::NewGuid().ToString('N')
$backup = Join-Path $OutputDirectory "module-backup-$runId.clixml"
$target = Join-Path $OutputDirectory "module-probe-$runId.hwpx"
@{ Key = $keyPath; Name = $valueName; Kind = $oldKind.ToString(); Value = $oldValue } | Export-Clixml -LiteralPath $backup
$saved = Import-Clixml -LiteralPath $backup
if ($saved.Value -cne $oldValue -or $saved.Kind -ne $oldKind.ToString()) { throw 'Backup verification failed.' }
Write-Output "Original registry value backed up: $backup"
$ownedHwp = $null
try {
    $key.SetValue($valueName, $DllPath, [Microsoft.Win32.RegistryValueKind]::String)
    $ownedHwp = New-Object -ComObject HWPFrame.HwpObject
    $ownedHwp.XHwpWindows.Item(0).Visible = $false
    $registered = $ownedHwp.RegisterModule('FilePathCheckDLL', $valueName)
    Write-Output "Registered file-access module: $registered"
    if (-not $registered) { throw 'Registration failed; no document was opened.' }
    if (-not $ownedHwp.Open($SourceDocx, 'OOXML', '')) { throw 'DOCX open failed.' }
    if (-not $ownedHwp.SaveAs($target, 'HWPX', '')) { throw 'HWPX save failed.' }
    if (-not (Test-Path -LiteralPath $target -PathType Leaf)) { throw 'Output missing.' }
    Write-Output "Converted HWPX: $target"
} finally {
    # Restore before COM cleanup, which might block in the external application.
    try {
        $key.SetValue($valueName, $oldValue, $oldKind)
        if ($key.GetValue($valueName, $null, [Microsoft.Win32.RegistryValueOptions]::DoNotExpandEnvironmentNames) -cne $oldValue -or $key.GetValueKind($valueName) -ne $oldKind) {
            throw "Registry restoration verification failed. Backup: $backup"
        }
        Write-Output 'Original module value restored and verified.'
    } finally {
        $key.Close()
        if ($null -ne $ownedHwp) {
            try { $ownedHwp.Clear(1) } catch {}
            try { $ownedHwp.Quit() } catch {}
            [void][Runtime.InteropServices.Marshal]::FinalReleaseComObject($ownedHwp)
        }
    }
}
