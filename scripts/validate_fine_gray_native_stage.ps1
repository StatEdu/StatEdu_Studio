param(
    [Parameter(Mandatory=$true)][string]$ComponentRoot,
    [Parameter(Mandatory=$true)][string]$OutputDirectory
)
$ErrorActionPreference='Stop'
$source=(Resolve-Path -LiteralPath $ComponentRoot).Path
$root=[IO.Path]::GetFullPath($OutputDirectory)
if(Test-Path -LiteralPath $root){throw 'Choose a new validation output directory'}
New-Item -ItemType Directory -Path $root | Out-Null
$stageScript=Join-Path $PSScriptRoot 'stage_fine_gray_native.ps1'
$rows=@()
foreach($case in @('valid','dll','source','license','manifest','extra')){
    $caseRoot=Join-Path $root $case
    New-Item -ItemType Directory -Path $caseRoot | Out-Null
    Copy-Item -LiteralPath $source -Destination (Join-Path $caseRoot 'component') -Recurse
    $component=Join-Path $caseRoot 'component'
    switch($case){
        'dll' {Set-Content -LiteralPath (Join-Path $component 'bin/candidate.dll') -Value 'corrupt'}
        'source' {Add-Content -LiteralPath (Join-Path $component 'source/native/fine_gray/src/crr.f') -Value 'changed'}
        'license' {Remove-Item -LiteralPath (Join-Path $component 'source/native/fine_gray/COPYING')}
        'manifest' {
            $file=Join-Path $component 'build-manifest.json'
            $manifest=Get-Content -LiteralPath $file -Raw | ConvertFrom-Json
            $manifest.upstream='unvalidated'
            $manifest | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath $file -Encoding UTF8
        }
        'extra' {Set-Content -LiteralPath (Join-Path $component 'extra.exe') -Value 'must not be staged'}
    }
    $app=Join-Path $caseRoot 'app'
    New-Item -ItemType Directory -Path $app | Out-Null
    $message=''
    try{& $stageScript -ComponentRoot $component -AppStage $app | Out-Null}catch{$message=$_.Exception.Message}
    $expectedFailure=$case -in @('dll','source','license','manifest')
    if($expectedFailure -ne [bool]$message){throw "Unexpected staging outcome: $case $message"}
    $destination=Join-Path $app 'native/fine_gray/component'
    if($expectedFailure -and (Test-Path -LiteralPath $destination)){throw 'Rejected component was partly staged'}
    if(-not $expectedFailure){
        if(Test-Path -LiteralPath (Join-Path $destination 'extra.exe')){throw 'Unlisted payload was copied'}
        $before=(Get-FileHash -LiteralPath (Join-Path $destination 'bin/candidate.dll')).Hash
        $rejected=$false
        try{& $stageScript -ComponentRoot $component -AppStage $app | Out-Null}catch{$rejected=$true}
        if(-not $rejected -or (Get-FileHash -LiteralPath (Join-Path $destination 'bin/candidate.dll')).Hash -ne $before){throw 'Existing destination was not protected'}
    }
    $rows+=[pscustomobject]@{case=$case;passed=$true;expected_rejection=$expectedFailure;message=$message}
}
foreach($file in @('build_electron_beta.ps1','stage_fine_gray_native.ps1','validate_fine_gray_native_stage.ps1')){
    $parseTokens=$null;$parseErrors=$null
    [void][Management.Automation.Language.Parser]::ParseFile((Join-Path $PSScriptRoot $file),[ref]$parseTokens,[ref]$parseErrors)
    if($parseErrors.Count){throw "PowerShell parse failed: $file"}
}
$rows | Export-Csv -LiteralPath (Join-Path $root 'validation.csv') -NoTypeInformation
$rows | Format-Table case,passed,expected_rejection
