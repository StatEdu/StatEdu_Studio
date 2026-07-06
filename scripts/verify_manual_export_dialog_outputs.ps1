param(
  [string]$RepoRoot = "",
  [string]$HtmlPath = "",
  [string]$PdfPath = ""
)

$ErrorActionPreference = "Stop"

if (-not $RepoRoot) {
  $RepoRoot = Resolve-Path (Join-Path $PSScriptRoot "..")
} else {
  $RepoRoot = Resolve-Path $RepoRoot
}

if (-not $HtmlPath) {
  $HtmlPath = Join-Path $RepoRoot "outputs\manual_qa\StatEdu_Studio_results_manual_html.html"
}

if (-not $PdfPath) {
  $PdfPath = Join-Path $RepoRoot "outputs\manual_qa\StatEdu_Studio_results_manual_pdf.pdf"
}

function Assert-ExportFile {
  param(
    [string]$Path,
    [string]$Label,
    [int]$MinimumBytes
  )

  if (-not (Test-Path -LiteralPath $Path)) {
    throw "$Label was not found: $Path"
  }

  $item = Get-Item -LiteralPath $Path
  if ($item.Length -lt $MinimumBytes) {
    throw "$Label is too small to be a valid export: $Path ($($item.Length) bytes)"
  }

  Write-Host "[ok] $Label exists: $Path ($($item.Length) bytes)"
}

Assert-ExportFile -Path $HtmlPath -Label "manual HTML export" -MinimumBytes 1024
Assert-ExportFile -Path $PdfPath -Label "manual PDF export" -MinimumBytes 1024

$html = Get-Content -LiteralPath $HtmlPath -Raw
if ($html -notmatch "(?is)<html|<!doctype|StatEdu Studio") {
  throw "manual HTML export does not look like a StatEdu Studio HTML result: $HtmlPath"
}
Write-Host "[ok] manual HTML export content looks valid"

$pdfBytes = [System.IO.File]::ReadAllBytes($PdfPath)
$pdfHeader = [System.Text.Encoding]::ASCII.GetString($pdfBytes, 0, [Math]::Min(5, $pdfBytes.Length))
if ($pdfHeader -ne "%PDF-") {
  throw "manual PDF export does not start with a PDF header: $PdfPath"
}
Write-Host "[ok] manual PDF export content looks valid"

Write-Host "Manual export-dialog output verification passed."
