param([string]$RepoRoot = (Split-Path $PSScriptRoot -Parent))
$ErrorActionPreference = 'Stop'
Add-Type -AssemblyName System.Drawing
$source = Join-Path $RepoRoot 'branding/statedu-studio/StatEdu_favicon_final_master_transparent.png'
$destination = Join-Path $RepoRoot 'packaging/electron/build/appx'
[IO.Directory]::CreateDirectory($destination) | Out-Null
$logo = [Drawing.Image]::FromFile($source)
$assets = @(
  @('StoreLogo.png',50,50), @('Square44x44Logo.png',44,44),
  @('Square150x150Logo.png',150,150), @('Wide310x150Logo.png',310,150),
  @('LargeTile.png',310,310), @('SmallTile.png',71,71),
  @('AppTile300.png',300,300)
)
try {
  foreach ($asset in $assets) {
    $bitmap = [Drawing.Bitmap]::new([int]$asset[1], [int]$asset[2], [Drawing.Imaging.PixelFormat]::Format32bppArgb)
    $graphics = [Drawing.Graphics]::FromImage($bitmap)
    try {
      $graphics.Clear([Drawing.Color]::Transparent)
      $graphics.InterpolationMode = [Drawing.Drawing2D.InterpolationMode]::HighQualityBicubic
      $graphics.PixelOffsetMode = [Drawing.Drawing2D.PixelOffsetMode]::HighQuality
      $scale = [Math]::Min($bitmap.Width / $logo.Width, $bitmap.Height / $logo.Height)
      $width = [int][Math]::Round($logo.Width * $scale)
      $height = [int][Math]::Round($logo.Height * $scale)
      $rect = [Drawing.Rectangle]::new([int](($bitmap.Width-$width)/2), [int](($bitmap.Height-$height)/2), $width, $height)
      $graphics.DrawImage($logo, $rect)
      $bitmap.Save((Join-Path $destination $asset[0]), [Drawing.Imaging.ImageFormat]::Png)
    } finally { $graphics.Dispose(); $bitmap.Dispose() }
  }
} finally { $logo.Dispose() }
$record = @{ source = 'branding/statedu-studio/StatEdu_favicon_final_master_transparent.png'; sourceSha256 = (Get-FileHash -LiteralPath $source).Hash; method = 'Deterministic export of approved artwork; aspect ratio preserved; no generative changes'; assets = $assets }
$record | ConvertTo-Json -Depth 4 | Set-Content -LiteralPath (Join-Path $destination 'provenance.json') -Encoding utf8
Write-Output "Exported $($assets.Count) PNG assets: $destination"
