const { spawnSync } = require("child_process");
const path = require("path");

module.exports = async function afterPack(context) {
  if (context.electronPlatformName !== "win32") {
    return;
  }
  const productName = context.packager.appInfo.productName;
  const exeName = `${productName}.exe`;
  const iconPath = path.join(context.packager.projectDir, "build", "icon.ico");
  const rceditPath = path.join(context.packager.projectDir, "node_modules", "electron-winstaller", "vendor", "rcedit.exe");
  const exePath = path.join(context.appOutDir, exeName);
  const result = spawnSync(rceditPath, [
    exePath,
    "--set-icon", iconPath,
    "--set-version-string", "CompanyName", "StatEdu",
    "--set-version-string", "FileDescription", productName,
    "--set-version-string", "ProductName", productName,
    "--set-version-string", "InternalName", productName,
    "--set-version-string", "OriginalFilename", exeName,
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
