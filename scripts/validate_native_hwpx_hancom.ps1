param([string]$Source,[string]$Target,[ValidateSet('HWPX','OOXML')][string]$InputFormat='HWPX')
$ErrorActionPreference='Stop'
$existingIds=@(Get-Process Hwp -ErrorAction SilentlyContinue | Select-Object -ExpandProperty Id)
Add-Type @'
using System;
using System.Runtime.InteropServices;
public static class NativeHwpxValidationWindow {
 [DllImport("user32.dll")] public static extern uint GetWindowThreadProcessId(IntPtr hwnd,out uint id);
}
'@
$hwp=$null;$owned=$false
try {
 $hwp=New-Object -ComObject HWPFrame.HwpObject
 $window=$hwp.XHwpWindows.Item(0)
 [uint32]$ownedId=0
 [void][NativeHwpxValidationWindow]::GetWindowThreadProcessId([IntPtr]$window.WindowHandle,[ref]$ownedId)
 if($ownedId -eq 0 -or $ownedId -in $existingIds){throw 'Expected isolated validation process'}
 $owned=$true;$window.Visible=$false
 if(-not $hwp.Open($Source,$InputFormat,'')){throw 'Document could not be opened'}
 if(-not $hwp.SaveAs($Target+'.docx','OOXML','')){throw 'Roundtrip save failed'}
 if(-not $hwp.SaveAs($Target+'.pdf','PDF','')){throw 'PDF rendering failed'}
 if(-not $hwp.SaveAs($Target+'.hwpx','HWPX','')){throw 'Native save failed'}
 Write-Output "PASS Hancom opened and rendered $InputFormat"
} finally {
 if($hwp -and $owned){$hwp.Clear(1);$hwp.Quit()}
 if($hwp){[void][Runtime.InteropServices.Marshal]::FinalReleaseComObject($hwp)}
}
