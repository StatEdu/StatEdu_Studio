const { spawnSync } = require("child_process");
const path = require("path");

module.exports = async function afterPack(context) {
  if (context.electronPlatformName !== "win32") {
    return;
  }
  const iconPath = path.join(context.packager.projectDir, "build", "icon.ico");
  const rceditPath = path.join(context.packager.projectDir, "node_modules", "electron-winstaller", "vendor", "rcedit.exe");
  const exePath = path.join(context.appOutDir, "StatEdu Studio Beta.exe");
  const result = spawnSync(rceditPath, [
    exePath,
    "--set-icon", iconPath,
    "--set-version-string", "CompanyName", "StatEdu",
    "--set-version-string", "FileDescription", "StatEdu Studio Beta",
    "--set-version-string", "ProductName", "StatEdu Studio Beta",
    "--set-version-string", "InternalName", "StatEdu Studio Beta",
    "--set-version-string", "OriginalFilename", "StatEdu Studio Beta.exe",
    "--set-version-string", "LegalCopyright", "Copyright (C) 2026 StatEdu",
    "--set-file-version", context.packager.appInfo.version,
    "--set-product-version", context.packager.appInfo.version
  ], {
    windowsHide: true,
    stdio: "inherit"
  });
  if (result.status !== 0) {
    throw new Error(`rcedit failed with exit code ${result.status}`);
  }
};
