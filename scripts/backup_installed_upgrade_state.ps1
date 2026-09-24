param([Parameter(Mandatory=$true)][string[]]$InstallRoots,
      [Parameter(Mandatory=$true)][string[]]$ProfileRoots,
      [Parameter(Mandatory=$true)][string]$BackupRoot)
$ErrorActionPreference='Stop'
# Copies only. Never run an installer, uninstaller, or alter the source files.
$destination=[IO.Path]::GetFullPath($BackupRoot)
if(Test-Path -LiteralPath $destination){throw 'Use a new backup directory; existing backups are never overwritten.'}
$sources=@()
foreach($root in $InstallRoots){
  $resolved=(Resolve-Path -LiteralPath $root).Path
  $app=Join-Path $resolved 'resources/app.asar.unpacked/app'
  foreach($relative in @('data/StatEdu_Studio_results.json','data/EasyFlow_Statistics_results.json')){
    $file=Join-Path $app $relative
    if(Test-Path -LiteralPath $file -PathType Leaf){$sources+=Get-Item -LiteralPath $file}
  }
}
foreach($root in $ProfileRoots){
  $resolved=(Resolve-Path -LiteralPath $root).Path
  foreach($folder in @('settings','data')){
    $dir=Join-Path $resolved $folder
    if(Test-Path -LiteralPath $dir -PathType Container){
      $sources+=Get-ChildItem -LiteralPath $dir -File -Recurse | Where-Object {
        $folder -eq 'settings' -or $_.Name -in @('StatEdu_Studio_results.json','EasyFlow_Statistics_results.json')
      }
    }
  }
}
New-Item -ItemType Directory -Path $destination | Out-Null
$records=@()
foreach($source in ($sources | Sort-Object FullName -Unique)){
  $hash=(Get-FileHash -LiteralPath $source.FullName -Algorithm SHA256).Hash
  $name=('{0:D4}-{1}' -f ($records.Count+1),$source.Name)
  $target=Join-Path $destination $name
  Copy-Item -LiteralPath $source.FullName -Destination $target -ErrorAction Stop
  $copyHash=(Get-FileHash -LiteralPath $target -Algorithm SHA256).Hash
  $afterHash=(Get-FileHash -LiteralPath $source.FullName -Algorithm SHA256).Hash
  if($hash -ne $copyHash -or $hash -ne $afterHash){throw 'A source changed during backup or copy verification failed. Keep this directory for diagnosis and retry into a new directory.'}
  $records += [pscustomobject]@{source=$source.FullName;backup=$name;bytes=$source.Length;sha256=$hash}
}
ConvertTo-Json -InputObject @($records) -Depth 4 | Set-Content -LiteralPath (Join-Path $destination 'manifest.json') -Encoding utf8
Write-Output "Verified $($records.Count) files; originals unchanged. Backup: $destination"
