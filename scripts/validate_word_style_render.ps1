param([string]$Before, [string]$After, [string]$OutputDirectory)
$ErrorActionPreference = 'Stop'
$existing = @(Get-Process WINWORD -ErrorAction SilentlyContinue | Select-Object -ExpandProperty Id)
if ($existing.Count) { throw 'Close existing Word instances before running this isolated rendering check.' }
Add-Type @'
using System;
using System.Runtime.InteropServices;
public static class WordStyleValidationWindow {
 [DllImport("user32.dll")] public static extern uint GetWindowThreadProcessId(IntPtr hwnd, out uint id);
}
'@
$word = $null
$owned = $false
try {
  $word = New-Object -ComObject Word.Application
  [uint32]$wordProcessId = 0
  [void][WordStyleValidationWindow]::GetWindowThreadProcessId([IntPtr]$word.Hwnd, [ref]$wordProcessId)
  if ($wordProcessId -eq 0 -or $wordProcessId -in $existing) { throw 'Expected an isolated Word validation process.' }
  $owned = $true
  $word.Visible = $false
  $word.DisplayAlerts = 0
  $word.AutomationSecurity = 3
  [void](New-Item -ItemType Directory -Path $OutputDirectory -Force)
  $folder = (Resolve-Path -LiteralPath $OutputDirectory).Path
  foreach ($item in @(@{Name='before';Path=$Before}, @{Name='after';Path=$After})) {
    $document = $null
    try {
      $path = (Resolve-Path -LiteralPath $item.Path).Path
      $document = $word.Documents.Open($path, $false, $true, $false)
      $document.ExportAsFixedFormat((Join-Path $folder ($item.Name + '.pdf')), 17)
      Write-Output ($item.Name + ': ' + $document.ComputeStatistics(2) + ' pages; ' + $document.Tables.Count + ' tables')
    } finally {
      if ($document) { $document.Close(0); [void][Runtime.InteropServices.Marshal]::FinalReleaseComObject($document) }
    }
  }
} finally {
  if ($word -and $owned) { $word.Quit(0) }
  if ($word) { [void][Runtime.InteropServices.Marshal]::FinalReleaseComObject($word) }
}
