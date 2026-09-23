[CmdletBinding()]
param(
    [ValidateRange(1, 65535)]
    [int]$Port = 7894,

    [switch]$NoBrowser,

    [switch]$RestartStaleBackend
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$projectRoot = Split-Path -Parent $PSScriptRoot
$baseUrl = "http://127.0.0.1:$Port/"

function Get-NormalizedPath {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Path
    )

    return [System.IO.Path]::GetFullPath($Path).TrimEnd("\", "/")
}

function Get-BackendMetadataPath {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Root,

        [Parameter(Mandatory = $true)]
        [int]$TcpPort
    )

    $normalizedRoot = (Get-NormalizedPath -Path $Root).ToLowerInvariant()
    $sha256 = [System.Security.Cryptography.SHA256]::Create()
    try {
        $rootHashBytes = $sha256.ComputeHash([System.Text.Encoding]::UTF8.GetBytes($normalizedRoot))
        $rootHash = (([System.BitConverter]::ToString($rootHashBytes) -replace "-", "").ToLowerInvariant()).Substring(0, 12)
    }
    finally {
        $sha256.Dispose()
    }
    $runtimeDirectory = Join-Path ([Environment]::GetFolderPath("LocalApplicationData")) "StatEdu Studio\runtime"
    return Join-Path $runtimeDirectory "backend-$TcpPort-$rootHash.json"
}

function Read-BackendMetadata {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Path
    )

    if (-not (Test-Path -LiteralPath $Path -PathType Leaf)) {
        return $null
    }
    try {
        return Get-Content -LiteralPath $Path -Raw -ErrorAction Stop | ConvertFrom-Json -ErrorAction Stop
    }
    catch {
        return $null
    }
}

function Write-BackendMetadata {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Path,

        [Parameter(Mandatory = $true)]
        [System.Diagnostics.Process]$Process,

        [Parameter(Mandatory = $true)]
        [string]$Root,

        [Parameter(Mandatory = $true)]
        [int]$TcpPort,

        [Parameter(Mandatory = $true)]
        [string]$Fingerprint
    )

    $directory = Split-Path -Parent $Path
    New-Item -ItemType Directory -Path $directory -Force | Out-Null
    $metadata = [ordered]@{
        schema = 1
        process_id = $Process.Id
        process_start_utc = $Process.StartTime.ToUniversalTime().ToString("o")
        project_root = Get-NormalizedPath -Path $Root
        script_path = Get-NormalizedPath -Path (Join-Path $Root "run_app.R")
        port = $TcpPort
        fingerprint = $Fingerprint
    }
    $temporaryPath = "$Path.$($Process.Id).tmp"
    $metadata | ConvertTo-Json | Set-Content -LiteralPath $temporaryPath -Encoding UTF8
    Move-Item -LiteralPath $temporaryPath -Destination $Path -Force
}

function Remove-BackendMetadata {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Path,

        [Parameter(Mandatory = $true)]
        [int]$ProcessId
    )

    $metadata = Read-BackendMetadata -Path $Path
    $hasProcessId = $null -ne $metadata -and
        ($metadata.PSObject.Properties.Name -contains "process_id")
    if ($hasProcessId -and [int]$metadata.process_id -eq $ProcessId) {
        Remove-Item -LiteralPath $Path -Force -ErrorAction SilentlyContinue
    }
}

function Get-WorkspaceFingerprint {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Root
    )

    $sourceExtensions = @(".r", ".js", ".css", ".html", ".json")
    $sourceFiles = New-Object System.Collections.Generic.List[System.IO.FileInfo]
    foreach ($relativeDirectory in @("R", "www")) {
        $directory = Join-Path $Root $relativeDirectory
        if (-not (Test-Path -LiteralPath $directory -PathType Container)) {
            continue
        }
        foreach ($file in Get-ChildItem -LiteralPath $directory -File -Recurse -ErrorAction Stop) {
            if ($sourceExtensions -contains $file.Extension.ToLowerInvariant()) {
                $sourceFiles.Add($file)
            }
        }
    }
    foreach ($relativePath in @("app.R", "run_app.R", "VERSION")) {
        $path = Join-Path $Root $relativePath
        if (Test-Path -LiteralPath $path -PathType Leaf) {
            $sourceFiles.Add((Get-Item -LiteralPath $path))
        }
    }

    $manifest = foreach ($file in $sourceFiles | Sort-Object FullName -Unique) {
        $relativePath = $file.FullName.Substring($Root.Length).TrimStart("\", "/").Replace("\", "/")
        $fileHash = (Get-FileHash -LiteralPath $file.FullName -Algorithm SHA256).Hash.ToLowerInvariant()
        "$relativePath`t$fileHash"
    }
    $manifestText = ($manifest -join "`n") + "`n"
    $sha256 = [System.Security.Cryptography.SHA256]::Create()
    try {
        $bytes = [System.Text.Encoding]::UTF8.GetBytes($manifestText)
        $hash = $sha256.ComputeHash($bytes)
        return ([System.BitConverter]::ToString($hash) -replace "-", "").ToLowerInvariant()
    }
    finally {
        $sha256.Dispose()
    }
}

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

function Get-StatEduBackendInfo {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Url
    )

    $healthUrl = ([System.Uri]::new([System.Uri]$Url, "statedu-health/build.json")).AbsoluteUri
    try {
        $healthResponse = Invoke-WebRequest `
            -Uri $healthUrl `
            -UseBasicParsing `
            -TimeoutSec 3 `
            -Headers @{ "Cache-Control" = "no-cache" }
        $healthPayload = $healthResponse.Content | ConvertFrom-Json -ErrorAction Stop
        if (([int]$healthResponse.StatusCode -ge 200) -and
            ([int]$healthResponse.StatusCode -lt 400) -and
            ([string]$healthPayload.app -eq "StatEdu Studio")) {
            return [pscustomobject]@{
                Healthy = $true
                Fingerprint = ([string]$healthPayload.fingerprint).Trim()
                StatusCode = [int]$healthResponse.StatusCode
            }
        }
    }
    catch {
        # Backends started before the health endpoint existed are checked once
        # through the root page and treated as unversioned/stale.
    }

    try {
        $response = Invoke-WebRequest `
            -Uri $Url `
            -UseBasicParsing `
            -TimeoutSec 10 `
            -Headers @{ "Cache-Control" = "no-cache" }
        $content = [string]$response.Content
        $healthy = ([int]$response.StatusCode -ge 200) -and
            ([int]$response.StatusCode -lt 400) -and
            ($content -match "(?i)StatEdu")
        $fingerprint = ""
        $metaMatch = [regex]::Match(
            $content,
            '<meta\b[^>]*\bname=["'']statedu-build-fingerprint["''][^>]*>',
            [System.Text.RegularExpressions.RegexOptions]::IgnoreCase
        )
        if ($metaMatch.Success) {
            $contentMatch = [regex]::Match(
                $metaMatch.Value,
                '\bcontent=["'']([^"'']*)["'']',
                [System.Text.RegularExpressions.RegexOptions]::IgnoreCase
            )
            if ($contentMatch.Success) {
                $fingerprint = [System.Net.WebUtility]::HtmlDecode($contentMatch.Groups[1].Value).Trim()
            }
        }
        return [pscustomobject]@{
            Healthy = $healthy
            Fingerprint = $fingerprint
            StatusCode = [int]$response.StatusCode
        }
    }
    catch {
        return [pscustomobject]@{
            Healthy = $false
            Fingerprint = ""
            StatusCode = 0
        }
    }
}

function Get-LocalListenerProcessId {
    param(
        [Parameter(Mandatory = $true)]
        [int]$TcpPort
    )

    $connection = Get-NetTCPConnection -LocalPort $TcpPort -State Listen -ErrorAction SilentlyContinue |
        Select-Object -First 1
    if ($null -eq $connection) {
        return $null
    }
    return [int]$connection.OwningProcess
}

function Test-WorkspaceBackendProcess {
    param(
        [Parameter(Mandatory = $true)]
        [int]$ProcessId,

        [Parameter(Mandatory = $true)]
        [string]$Root,

        [Parameter(Mandatory = $true)]
        [int]$TcpPort
    )

    $process = Get-CimInstance Win32_Process -Filter "ProcessId=$ProcessId" -ErrorAction SilentlyContinue
    if ($null -eq $process) {
        return $false
    }
    $executableName = [System.IO.Path]::GetFileName([string]$process.ExecutablePath)
    $commandLine = [string]$process.CommandLine
    $expectedRoot = Get-NormalizedPath -Path $Root
    $expectedScript = Get-NormalizedPath -Path (Join-Path $Root "run_app.R")
    $hasExpectedAbsoluteScript = $commandLine.IndexOf($expectedScript, [System.StringComparison]::OrdinalIgnoreCase) -ge 0
    $hasRunAppArgument = [regex]::IsMatch($commandLine, '(?i)(?:^|[\s"])(?:[^"\r\n]*[\\/])?run_app\.R(?:[\s"]|$)')

    $metadataPath = Get-BackendMetadataPath -Root $Root -TcpPort $TcpPort
    $metadata = Read-BackendMetadata -Path $metadataPath
    $verifiedByMetadata = $false
    $requiredMetadataProperties = @(
        "process_id",
        "process_start_utc",
        "project_root",
        "script_path",
        "port"
    )
    $hasRequiredMetadata = $null -ne $metadata -and
        @($requiredMetadataProperties | Where-Object { $metadata.PSObject.Properties.Name -notcontains $_ }).Count -eq 0
    if ($hasRequiredMetadata) {
        $metadataRoot = try { Get-NormalizedPath -Path ([string]$metadata.project_root) } catch { "" }
        $metadataScript = try { Get-NormalizedPath -Path ([string]$metadata.script_path) } catch { "" }
        $metadataStart = try { [DateTime]::Parse([string]$metadata.process_start_utc).ToUniversalTime() } catch { [DateTime]::MinValue }
        $processStart = try { ([DateTime]$process.CreationDate).ToUniversalTime() } catch { [DateTime]::MaxValue }
        $verifiedByMetadata = ([int]$metadata.process_id -eq $ProcessId) -and
            ([int]$metadata.port -eq $TcpPort) -and
            ($metadataRoot -ieq $expectedRoot) -and
            ($metadataScript -ieq $expectedScript) -and
            ([Math]::Abs(($processStart - $metadataStart).TotalSeconds) -le 2)
    }

    return ($executableName -ieq "Rscript.exe") -and
        $hasRunAppArgument -and
        ($hasExpectedAbsoluteScript -or $verifiedByMetadata)
}

function Stop-VerifiedWorkspaceBackend {
    param(
        [Parameter(Mandatory = $true)]
        [int]$TcpPort,

        [Parameter(Mandatory = $true)]
        [string]$Root
    )

    $processId = Get-LocalListenerProcessId -TcpPort $TcpPort
    if ($null -eq $processId -or -not (Test-WorkspaceBackendProcess -ProcessId $processId -Root $Root -TcpPort $TcpPort)) {
        throw "The stale listener on port $TcpPort is not a verified StatEdu Studio process from this workspace. It was left untouched."
    }

    Write-Host "Stopping verified stale StatEdu Studio backend (PID $processId)..."
    Stop-Process -Id $processId -ErrorAction Stop
    $deadline = [DateTime]::UtcNow.AddSeconds(10)
    while ((Test-LocalTcpListener -TcpPort $TcpPort) -and ([DateTime]::UtcNow -lt $deadline)) {
        Start-Sleep -Milliseconds 200
    }
    if (Test-LocalTcpListener -TcpPort $TcpPort) {
        throw "The verified stale backend did not release port $TcpPort within 10 seconds."
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
$workspaceFingerprint = Get-WorkspaceFingerprint -Root $projectRoot
$backendMetadataPath = Get-BackendMetadataPath -Root $projectRoot -TcpPort $Port
$env:STATEDU_PORT = [string]$Port
$env:STATEDU_SINGLE_SESSION = "true"
$env:STATEDU_BUILD_FINGERPRINT = $workspaceFingerprint
$env:STATEDU_LAUNCH_BROWSER = if ($NoBrowser) { "false" } else { "true" }
if ($env:OS -eq "Windows_NT") {
    $env:LANG = "English_United States.utf8"
    $env:LC_ALL = "English_United States.utf8"
    $env:LC_CTYPE = "English_United States.utf8"
}

if (Test-LocalTcpListener -TcpPort $Port) {
    $backendInfo = Get-StatEduBackendInfo -Url $baseUrl
    if (-not $backendInfo.Healthy) {
        throw "Port $Port is already in use, but it is not a healthy StatEdu Studio backend. The existing process was left untouched. Close it or choose another port, then try again."
    }

    if ($backendInfo.Fingerprint -eq $workspaceFingerprint) {
        Write-Host "Reusing the current StatEdu Studio backend on port $Port (build $($workspaceFingerprint.Substring(0, 12)))."
        if (-not $NoBrowser) {
            Start-Process $baseUrl
        }
        exit 0
    }

    $runningBuild = if ([string]::IsNullOrWhiteSpace($backendInfo.Fingerprint)) {
        "unversioned"
    } else {
        $backendInfo.Fingerprint.Substring(0, [Math]::Min(12, $backendInfo.Fingerprint.Length))
    }
    $currentBuild = $workspaceFingerprint.Substring(0, 12)
    if (-not $RestartStaleBackend) {
        throw "A stale StatEdu Studio backend is running on port $Port (running: $runningBuild; current: $currentBuild). It was left untouched. Close the existing StatEdu Studio window and start again, or explicitly run StatEdu_Studio.bat -RestartStaleBackend."
    }
    Stop-VerifiedWorkspaceBackend -TcpPort $Port -Root $projectRoot
}

$rscript = Find-Rscript
if ([string]::IsNullOrWhiteSpace($rscript)) {
    throw "Rscript was not found. Install R from https://cran.r-project.org/bin/windows/base/ and try again."
}

Write-Host "Starting StatEdu Studio..."
Write-Host "Using Rscript: $rscript"
Write-Host "Build fingerprint: $($workspaceFingerprint.Substring(0, 12))"
Write-Host "Keep this window open while using the app."
Write-Host ""

$runAppPath = Join-Path $projectRoot "run_app.R"
$processStartInfo = New-Object System.Diagnostics.ProcessStartInfo
$processStartInfo.FileName = $rscript
$processStartInfo.Arguments = '"' + $runAppPath + '"'
$processStartInfo.WorkingDirectory = $projectRoot
$processStartInfo.UseShellExecute = $false
$backendProcess = [System.Diagnostics.Process]::Start($processStartInfo)
if ($null -eq $backendProcess) {
    throw "Rscript could not be started."
}
Write-BackendMetadata `
    -Path $backendMetadataPath `
    -Process $backendProcess `
    -Root $projectRoot `
    -TcpPort $Port `
    -Fingerprint $workspaceFingerprint
try {
    $backendProcess.WaitForExit()
    $exitCode = $backendProcess.ExitCode
}
finally {
    Remove-BackendMetadata -Path $backendMetadataPath -ProcessId $backendProcess.Id
}

Write-Host ""
Write-Host "StatEdu Studio stopped."
exit $exitCode
