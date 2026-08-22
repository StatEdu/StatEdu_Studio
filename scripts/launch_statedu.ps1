[CmdletBinding()]
param(
    [ValidateRange(1, 65535)]
    [int]$Port = 7894,

    [switch]$NoBrowser
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$projectRoot = Split-Path -Parent $PSScriptRoot
$baseUrl = "http://127.0.0.1:$Port/"

function Test-LocalTcpListener {
    param(
        [Parameter(Mandatory = $true)]
        [int]$TcpPort
    )

    $client = New-Object System.Net.Sockets.TcpClient
    $connect = $null
    try {
        $connect = $client.BeginConnect("127.0.0.1", $TcpPort, $null, $null)
        if (-not $connect.AsyncWaitHandle.WaitOne(750)) {
            return $false
        }
        $client.EndConnect($connect)
        return $true
    }
    catch {
        return $false
    }
    finally {
        if ($null -ne $connect) {
            $connect.AsyncWaitHandle.Close()
        }
        $client.Close()
    }
}

function Test-StatEduBackend {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Url
    )

    try {
        $response = Invoke-WebRequest `
            -Uri $Url `
            -UseBasicParsing `
            -TimeoutSec 3 `
            -Headers @{ "Cache-Control" = "no-cache" }
        $content = [string]$response.Content
        return ([int]$response.StatusCode -ge 200) -and
            ([int]$response.StatusCode -lt 400) -and
            ($content -match "(?i)StatEdu")
    }
    catch {
        return $false
    }
}

function Get-RVersionFromPath {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Path
    )

    if ($Path -match "(?i)[\\/]R-(\d+(?:\.\d+){1,3})[\\/]") {
        try {
            return [version]$Matches[1]
        }
        catch {
            return [version]"0.0"
        }
    }
    return [version]"0.0"
}

function Find-Rscript {
    $paths = New-Object System.Collections.Generic.List[string]
    $roots = @(
        (Join-Path $env:ProgramFiles "R"),
        (Join-Path $env:LOCALAPPDATA "Programs\R")
    )

    foreach ($root in $roots) {
        if (-not (Test-Path -LiteralPath $root -PathType Container)) {
            continue
        }
        foreach ($directory in Get-ChildItem -LiteralPath $root -Directory -Filter "R-*" -ErrorAction SilentlyContinue) {
            foreach ($relativePath in @("bin\x64\Rscript.exe", "bin\Rscript.exe")) {
                $candidate = Join-Path $directory.FullName $relativePath
                if (Test-Path -LiteralPath $candidate -PathType Leaf) {
                    $paths.Add($candidate)
                }
            }
        }
    }

    $pathCommand = Get-Command "Rscript.exe" -CommandType Application -ErrorAction SilentlyContinue |
        Select-Object -First 1
    if ($null -ne $pathCommand) {
        $paths.Add($pathCommand.Source)
    }

    $candidate = $paths |
        Select-Object -Unique |
        Sort-Object @{ Expression = { Get-RVersionFromPath -Path $_ }; Descending = $true } |
        Select-Object -First 1
    return $candidate
}

Set-Location -LiteralPath $projectRoot
$env:STATEDU_PORT = [string]$Port
$env:STATEDU_SINGLE_SESSION = "true"
$env:STATEDU_LAUNCH_BROWSER = if ($NoBrowser) { "false" } else { "true" }
if ($env:OS -eq "Windows_NT") {
    $env:LANG = "English_United States.utf8"
    $env:LC_ALL = "English_United States.utf8"
    $env:LC_CTYPE = "English_United States.utf8"
}

if (Test-LocalTcpListener -TcpPort $Port) {
    if (-not (Test-StatEduBackend -Url $baseUrl)) {
        throw "Port $Port is already in use, but it is not a healthy StatEdu Studio backend. The existing process was left untouched. Close it or choose another port, then try again."
    }

    Write-Host "Reusing the running StatEdu Studio backend on port $Port."
    if (-not $NoBrowser) {
        Start-Process $baseUrl
    }
    exit 0
}

$rscript = Find-Rscript
if ([string]::IsNullOrWhiteSpace($rscript)) {
    throw "Rscript was not found. Install R from https://cran.r-project.org/bin/windows/base/ and try again."
}

Write-Host "Starting StatEdu Studio..."
Write-Host "Using Rscript: $rscript"
Write-Host "Keep this window open while using the app."
Write-Host ""

& $rscript (Join-Path $projectRoot "run_app.R")
$exitCode = $LASTEXITCODE
if ($null -eq $exitCode) {
    $exitCode = 0
}

Write-Host ""
Write-Host "StatEdu Studio stopped."
exit $exitCode
