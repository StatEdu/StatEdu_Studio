const { app, BrowserWindow, dialog, ipcMain, shell } = require("electron");
const { spawn, spawnSync } = require("child_process");
const crypto = require("crypto");
const fs = require("fs");
const net = require("net");
const path = require("path");
app.setName("StatEdu Studio");

const enableHardwareAcceleration = /^(1|true|yes)$/i.test(process.env.STATEDU_ENABLE_HARDWARE_ACCELERATION || "");
const enableRendererDiagnostics = /^(1|true|yes)$/i.test(process.env.STATEDU_RENDERER_DIAGNOSTICS || "");
const STARTUP_LOG_MAX_BYTES = 5 * 1024 * 1024;
if (!enableHardwareAcceleration) {
  app.disableHardwareAcceleration();
  app.commandLine.appendSwitch("disable-gpu");
}

const DEFAULT_SHINY_STARTUP_TIMEOUT_MS = 180000;

let mainWindow = null;
let shinyProcess = null;
let isQuitting = false;
let startupLogPath = null;
let startupLogPrepared = false;
let launchStudioFile = "";
let isReloadingStudioFile = false;
let currentDataDirectory = "";
let pendingStudioFile = "";
let stoppingShiny = null;
let shutdownComplete = false;
let shutdownPending = false;
let shinyOrigin = "";

const CANVAS_FILE_EXTENSIONS = new Set([
  "streg", "stcfa", "stsem", "stpls",
  "stmmr", "stcfar", "stsemr", "stplsr"
]);
const DATA_FILE_EXTENSIONS = new Set(["sav", "sas7bdat", "xpt", "dta", "xlsx", "xls", "csv", "dat"]);

function trustedRenderer(event) {
  if (!mainWindow || !event || event.sender !== mainWindow.webContents) return false;
  const senderUrl = String(event.senderFrame && event.senderFrame.url || event.sender.getURL() || "");
  try { return !!shinyOrigin && new URL(senderUrl).origin === shinyOrigin && event.senderFrame === mainWindow.webContents.mainFrame; }
  catch (_) { return false; }
}

function normalizedDialogExtensions(values, fallback) {
  const allowed = new Set([...CANVAS_FILE_EXTENSIONS, "png"]);
  const result = (Array.isArray(values) ? values : [])
    .map((value) => String(value || "").trim().replace(/^\./, "").toLowerCase())
    .filter((value) => allowed.has(value));
  return result.length ? [...new Set(result)] : [fallback];
}

function isSessionTemporaryDirectory(directory) {
  return /(^|[\\/])Rtmp[^\\/]+([\\/]|$)/i.test(String(directory || ""));
}

function canvasDialogDirectory() {
  if (currentDataDirectory && !isSessionTemporaryDirectory(currentDataDirectory) && fs.existsSync(currentDataDirectory)) return currentDataDirectory;
  const configured = defaultSaveDirectory();
  if (configured && !isSessionTemporaryDirectory(configured) && fs.existsSync(configured)) return configured;
  return app.getPath("documents");
}

function safeSuggestedName(value, fallback) {
  const name = path.basename(String(value || "").trim());
  return name && name !== "." && name !== path.sep ? name : fallback;
}

ipcMain.on("statedu:data-file-path", (event, filePath) => {
  if (!trustedRenderer(event)) return;
  const resolved = path.resolve(String(filePath || ""));
  if (isSessionTemporaryDirectory(resolved)) return;
  const extension = path.extname(resolved).slice(1).toLowerCase();
  if (!DATA_FILE_EXTENSIONS.has(extension) || !fs.existsSync(resolved)) return;
  currentDataDirectory = path.dirname(resolved);
});

ipcMain.on("statedu:data-directory", (event, directory) => {
  if (!trustedRenderer(event)) return;
  if (isSessionTemporaryDirectory(directory)) return;
  currentDataDirectory = "";
  if (typeof directory !== "string" || !directory || !path.isAbsolute(directory)) return;
  try {
    const resolved = path.resolve(directory);
    if (fs.statSync(resolved).isDirectory()) currentDataDirectory = resolved;
  } catch (_) {}
});

ipcMain.handle("statedu:canvas-open-text", async (event, options = {}) => {
  if (!trustedRenderer(event)) throw new Error("Untrusted file-open request");
  const extensions = normalizedDialogExtensions(options.extensions, "streg").filter((extension) => extension !== "png");
  const result = await dialog.showOpenDialog(mainWindow, {
    title: String(options.title || "Open model canvas"),
    defaultPath: canvasDialogDirectory(),
    properties: ["openFile"],
    filters: [{ name: String(options.description || "StatEdu Model Canvas"), extensions }]
  });
  if (result.canceled || !result.filePaths.length) return { canceled: true };
  return { canceled: false, text: fs.readFileSync(result.filePaths[0], "utf8") };
});

ipcMain.handle("statedu:canvas-save", async (event, options = {}) => {
  if (!trustedRenderer(event)) throw new Error("Untrusted file-save request");
  const extensions = normalizedDialogExtensions(options.extensions, options.binary ? "png" : "streg");
  const fallbackName = options.binary ? "model-canvas.png" : `model-canvas.${extensions[0]}`;
  const suggestedName = safeSuggestedName(options.suggestedName, fallbackName);
  const result = await dialog.showSaveDialog(mainWindow, {
    title: String(options.title || "Save model canvas"),
    defaultPath: path.join(canvasDialogDirectory(), suggestedName),
    filters: [{ name: String(options.description || "StatEdu Model Canvas"), extensions }]
  });
  if (result.canceled || !result.filePath) return { canceled: true };
  let targetPath = result.filePath;
  const chosenExtension = path.extname(targetPath).slice(1).toLowerCase();
  if (!extensions.includes(chosenExtension)) {
    targetPath = path.join(path.dirname(targetPath), `${path.basename(targetPath, path.extname(targetPath))}.${extensions[0]}`);
  }
  const payload = options.binary ? Buffer.from(options.data || []) : String(options.text || "");
  fs.writeFileSync(targetPath, payload);
  return { canceled: false, filePath: targetPath };
});

ipcMain.handle("statedu:result-choose-open", async (event, options = {}) => {
  if (!trustedRenderer(event)) throw new Error("Untrusted analysis-result open request");
  const extensions = normalizedDialogExtensions(options.extensions, "stsemr").filter((extension) => extension !== "png");
  const result = await dialog.showOpenDialog(mainWindow, {
    title: String(options.title || "Open analysis result"),
    defaultPath: canvasDialogDirectory(),
    properties: ["openFile"],
    filters: [{ name: String(options.description || "StatEdu Analysis Result"), extensions }]
  });
  if (result.canceled || !result.filePaths.length) return { canceled: true };
  return { canceled: false, filePath: result.filePaths[0] };
});

ipcMain.handle("statedu:result-choose-save", async (event, options = {}) => {
  if (!trustedRenderer(event)) throw new Error("Untrusted analysis-result save request");
  const extensions = normalizedDialogExtensions(options.extensions, "stsemr").filter((extension) => extension !== "png");
  const suggestedName = safeSuggestedName(options.suggestedName, `analysis-result.${extensions[0]}`);
  const result = await dialog.showSaveDialog(mainWindow, {
    title: String(options.title || "Save analysis result"),
    defaultPath: path.join(canvasDialogDirectory(), suggestedName),
    filters: [{ name: String(options.description || "StatEdu Analysis Result"), extensions }]
  });
  if (result.canceled || !result.filePath) return { canceled: true };
  let targetPath = result.filePath;
  const chosenExtension = path.extname(targetPath).slice(1).toLowerCase();
  if (!extensions.includes(chosenExtension)) {
    targetPath = path.join(path.dirname(targetPath), `${path.basename(targetPath, path.extname(targetPath))}.${extensions[0]}`);
  }
  return { canceled: false, filePath: targetPath };
});

function normalizeStudioFileArg(value) {
  const raw = String(value || "").trim().replace(/^"|"$/g, "");
  if (!raw || raw.startsWith("--")) {
    return "";
  }
  const resolved = path.resolve(raw);
  if (![".studio", ".streg", ".stcfa", ".stsem", ".stpls", ".stmm"].includes(path.extname(resolved).toLowerCase())) {
    return "";
  }
  return fs.existsSync(resolved) ? resolved : "";
}

function findStudioFileArg(argv) {
  for (const arg of argv || []) {
    const studioFile = normalizeStudioFileArg(arg);
    if (studioFile) {
      return studioFile;
    }
  }
  return "";
}

launchStudioFile = findStudioFileArg(process.argv);

let requestDirectory = "";
function desktopRequestDirectory() {
  if (!requestDirectory) {
    const root = path.join(app.getPath("userData"), "desktop-sessions");
    fs.mkdirSync(root, {recursive:true, mode:0o700});
    requestDirectory = fs.mkdtempSync(path.join(root, "session-"));
    fs.chmodSync(requestDirectory, 0o700);
    require("./desktop-bridge").createDesktopBridge({app, BrowserWindow, dialog, window:() => mainWindow, directory:requestDirectory});
    app.once("will-quit", () => fs.rmSync(requestDirectory, {recursive:true, force:true}));
  }
  return requestDirectory;
}

function startupLogFile() {
  if (!startupLogPath) {
    startupLogPath = path.join(app.getPath("userData"), "logs", "startup.log");
  }
  return startupLogPath;
}

function prepareStartupLog() {
  if (startupLogPrepared) {
    return startupLogFile();
  }
  const file = startupLogFile();
  fs.mkdirSync(path.dirname(file), { recursive: true });
  try {
    if (fs.existsSync(file) && fs.statSync(file).size >= STARTUP_LOG_MAX_BYTES) {
      const rotated = `${file}.1`;
      fs.rmSync(rotated, { force: true });
      fs.renameSync(file, rotated);
    }
  } catch (error) {
    // Log rotation must never prevent startup.
  }
  startupLogPrepared = true;
  return file;
}

function appLanguageFile() {
  return path.join(app.getPath("userData"), "settings", "app-language.txt");
}

function normalizeAppLanguage(value) {
  const language = String(value || "").trim().toLowerCase();
  if (language === "english" || language === "eng") return "en";
  if (language === "korean" || language === "korea" || language === "kr") return "ko";
  return /^[a-z][a-z0-9_-]*$/.test(language) ? language : "";
}

function readAppLanguage() {
  try {
    return normalizeAppLanguage(fs.readFileSync(appLanguageFile(), "utf8").split(/\r?\n/, 1)[0]);
  } catch (error) {
    return "";
  }
}

function resultZoomFile() {
  return path.join(app.getPath("userData"), "settings", "result-zoom-percent.txt");
}

function appPreferencesFile() {
  return path.join(app.getPath("userData"), "settings", "app-preferences.json");
}

function readAppPreferences() {
  try {
    const raw = fs.readFileSync(appPreferencesFile(), "utf8");
    const parsed = JSON.parse(raw);
    return parsed && typeof parsed === "object" ? parsed : {};
  } catch (error) {
    return {};
  }
}

function defaultSaveDirectory() {
  const preferences = readAppPreferences();
  const configured = String(preferences.default_save_dir || "").trim();
  if (!configured) {
    return "";
  }
  return path.resolve(configured);
}

function configureDownloadSavePath(webContents) {
  webContents.session.on("will-download", (event, item) => {
    if (currentDataDirectory && fs.existsSync(currentDataDirectory)) {
      item.setSaveDialogOptions({ defaultPath: path.join(currentDataDirectory, path.basename(item.getFilename())) });
      return;
    }
    const directory = defaultSaveDirectory();
    if (!directory) {
      return;
    }
    try {
      fs.mkdirSync(directory, { recursive: true });
      item.setSavePath(path.join(directory, path.basename(item.getFilename())));
    } catch (error) {
      logStartup(`download save directory ignored: ${error.message}`);
    }
  });
}

function installRendererDiagnostics(webContents) {
  if (enableRendererDiagnostics) {
    webContents.on("console-message", (event, level, message, line, sourceId) => {
      logStartup(`renderer console level=${level} ${sourceId || ""}:${line || 0} ${message}`);
    });
  }
  webContents.on("did-fail-load", (event, errorCode, errorDescription, validatedURL) => {
    logStartup(`renderer did-fail-load code=${errorCode} url=${validatedURL || ""} ${errorDescription || ""}`);
  });
  webContents.on("dom-ready", () => {
    logStartup("renderer dom-ready");
  });
  webContents.on("did-finish-load", () => {
    logStartup("renderer did-finish-load");
    if (enableRendererDiagnostics) {
      logRendererSnapshot(webContents, "did-finish-load");
      setTimeout(() => logRendererSnapshot(webContents, "after-10s"), 10000);
      setTimeout(() => logRendererSnapshot(webContents, "after-30s"), 30000);
    }
  });
  webContents.on("render-process-gone", (event, details) => {
    logStartup(`renderer process gone reason=${details.reason || ""} exitCode=${details.exitCode ?? ""}`);
  });
  webContents.on("unresponsive", () => {
    logStartup("renderer unresponsive");
  });
  webContents.on("responsive", () => {
    logStartup("renderer responsive");
  });
}

function logRendererSnapshot(webContents, label) {
  if (!webContents || webContents.isDestroyed()) {
    return;
  }
  webContents.executeJavaScript(`(() => {
    const bodyText = (document.body && document.body.innerText || "").replace(/\\s+/g, " ").slice(0, 240);
    const socket = window.Shiny && window.Shiny.shinyapp && window.Shiny.shinyapp.$socket;
    return {
      url: location.href,
      title: document.title,
      readyState: document.readyState,
      bodyLength: document.body ? document.body.innerText.length : 0,
      bodyText,
      hasShiny: !!window.Shiny,
      shinySocketState: socket ? socket.readyState : null,
      inputs: document.querySelectorAll(".shiny-bound-input").length,
      outputs: document.querySelectorAll(".shiny-bound-output").length,
      reconnecting: !!document.querySelector(".shiny-reconnecting")
    };
  })()`, true).then((snapshot) => {
    logStartup(`renderer snapshot ${label}: ${JSON.stringify(snapshot)}`);
  }).catch((error) => {
    logStartup(`renderer snapshot ${label} failed: ${error.message}`);
  });
}

function logStartup(message) {
  const line = `${new Date().toISOString()} ${message}\n`;
  try {
    const file = prepareStartupLog();
    fs.appendFile(file, line, "utf8", () => {});
  } catch (error) {
    // Logging must never block app startup.
  }
}

function logStartupEnvironment() {
  logStartup(`app=${appDisplayName()} version=${appVersion()}`);
  logStartup(`electron=${process.versions.electron} chrome=${process.versions.chrome} node=${process.versions.node}`);
  logStartup(`platform=${process.platform} arch=${process.arch} osRelease=${require("os").release()}`);
  logStartup(`hardwareAcceleration=${enableHardwareAcceleration ? "enabled" : "disabled"}`);
  logStartup(`rendererDiagnostics=${enableRendererDiagnostics ? "enabled" : "disabled"}`);
  logStartup(`userData=${app.getPath("userData")}`);
  logStartup(`appPath=${app.getAppPath()}`);
}

function appBaseDir() {
  const appPath = app.getAppPath();
  if (appPath.toLowerCase().endsWith(".asar")) {
    return `${appPath}.unpacked`;
  }
  return appPath;
}

function bundledAppDir() {
  return path.join(appBaseDir(), "app");
}

function appVersion() {
  const versionPath = path.join(bundledAppDir(), "VERSION");
  try {
    return fs.readFileSync(versionPath, "utf8").trim();
  } catch (error) {
    return app.getVersion();
  }
}

function appDisplayName() { return "StatEdu Studio"; }

function windowTitle() {
  return `${appDisplayName()} v${appVersion()}`;
}

function bundledRHomePath() { return path.join(appBaseDir(), "runtime", "R.framework", "Resources"); }
function bundledRscriptPath() { return path.join(bundledRHomePath(), "bin", "Rscript"); }
function bundledRBinPath() { return path.join(bundledRHomePath(), "bin"); }
function bundledRLibraryPath() { return path.join(bundledRHomePath(), "library"); }

function shinyStartupTimeoutMs() {
  const configured = Number.parseInt(process.env.STATEDU_STARTUP_TIMEOUT_MS || "", 10);
  if (Number.isFinite(configured) && configured >= 60000) {
    return configured;
  }
  return DEFAULT_SHINY_STARTUP_TIMEOUT_MS;
}

function getFreePort() {
  return new Promise((resolve, reject) => {
    const server = net.createServer();
    server.once("error", reject);
    server.listen(0, "127.0.0.1", () => {
      const address = server.address();
      const port = address && address.port;
      server.close(() => resolve(port));
    });
  });
}

function waitForShiny(port, timeoutMs = DEFAULT_SHINY_STARTUP_TIMEOUT_MS) {
  const startedAt = Date.now();
  return new Promise((resolve, reject) => {
    const probe = () => {
      const socket = net.connect({ host: "127.0.0.1", port }, () => {
        socket.end();
        resolve();
      });
      socket.on("error", retry);
      socket.setTimeout(1500, () => {
        socket.destroy();
        retry();
      });
    };
    const retry = () => {
      if (Date.now() - startedAt > timeoutMs) {
        reject(new Error(`StatEdu Studio did not start in time after ${Math.round(timeoutMs / 1000)} seconds.`));
        return;
      }
      setTimeout(probe, 150);
    };
    probe();
  });
}

function macREnvironment() {
  const env = Object.fromEntries(Object.entries(process.env).filter(([key]) => !key.startsWith("R_") && !key.startsWith("DYLD_") && !["LD_LIBRARY_PATH", "LD_PRELOAD"].includes(key)));
  const home = bundledRHomePath();
  return {...env, RHOME: home, R_HOME: home, R_SHARE_DIR: path.join(home, "share"), R_INCLUDE_DIR: path.join(home, "include"), R_DOC_DIR: path.join(home, "doc"),
    R_LIBS: bundledRLibraryPath(), R_LIBS_SITE: bundledRLibraryPath(), R_LIBS_USER: bundledRLibraryPath(),
    PATH: `${bundledRBinPath()}${path.delimiter}/usr/bin:/bin:/usr/sbin:/sbin`};
}
function runRscriptProbe(rscript, appDir) {
  const result = spawnSync(rscript, ["--version"], {cwd: appDir, env: macREnvironment(), encoding: "utf8", timeout: 15000});
  if (result.error || result.status !== 0) throw new Error(`Bundled R probe failed: ${result.error?.message || result.stderr}`);
}

function formatStartupError(error) {
  const logPath = startupLogFile();
  return [
    error && error.message ? error.message : String(error),
    "",
    `Startup log: ${logPath}`,
    "If this happens on another PC, send this log file with the macOS version and the app version."
  ].join("\n");
}

function macPdfBrowserPath() {
  const configured = (process.env.STATEDU_CHROME || "").trim();
  if (configured) return configured;
  for (const root of ["/Applications", path.join(app.getPath("home"), "Applications")]) {
    for (const [bundle, executable] of [["Google Chrome", "Google Chrome"], ["Microsoft Edge", "Microsoft Edge"], ["Chromium", "Chromium"]]) {
      const file = path.join(root, `${bundle}.app`, "Contents", "MacOS", executable);
      try { if (fs.statSync(file).isFile()) { fs.accessSync(file, fs.constants.X_OK); return file; } } catch (_) {}
    }
  }
  return "";
}

async function startShiny() {
  const startedAt = Date.now();
  const rscript = bundledRscriptPath();
  const appDir = bundledAppDir();
  logStartup("startShiny begin");
  if (!fs.existsSync(rscript)) {
    throw new Error(`Bundled Rscript was not found: ${rscript}`);
  }
  if (!fs.existsSync(path.join(appDir, "run_app.R"))) {
    throw new Error(`Bundled StatEdu Studio app was not found: ${appDir}`);
  }
  runRscriptProbe(rscript, appDir);

  const port = await getFreePort();
  const token = crypto.randomBytes(32).toString("hex");
  const initialLanguage = normalizeAppLanguage(process.env.STATEDU_APP_LANGUAGE) || readAppLanguage() || "ko";
  const env = {
    ...macREnvironment(),
    STATEDU_PORT: String(port),
    STATEDU_APP_DIR: appDir,
    STATEDU_LAUNCH_BROWSER: "false",
    STATEDU_NO_PACKAGE_INSTALL: "true",
    STATEDU_TOKEN: token,
    STATEDU_APP_LANGUAGE: initialLanguage,
    STATEDU_STARTUP_LOG: startupLogFile(),
    STATEDU_APP_LANGUAGE_FILE: appLanguageFile(),
    STATEDU_RESULT_ZOOM_FILE: resultZoomFile(),
    STATEDU_APP_PREFERENCES_FILE: appPreferencesFile(),
    STATEDU_OPEN_STUDIO_FILE: launchStudioFile,
    STATEDU_PUBLIC_RELEASE: "1",
    STATEDU_ENABLE_LATENT_MPLUS: "0",
    STATEDU_CHROME: macPdfBrowserPath(),
    STATEDU_DESKTOP_REQUEST_DIR: desktopRequestDirectory(),
    STATEDU_USER_DATA_DIR: app.getPath("userData"),
    STATEDU_UPDATE_PAGE_URL: process.env.STATEDU_MAC_UPDATE_PAGE_URL || "https://github.com/StatEdu/StatEdu_Studio/tree/macos",
    STATEDU_ENABLE_CUSTOM_MODEL_CANVAS: process.env.STATEDU_ENABLE_CUSTOM_MODEL_CANVAS || "1",
    STATEDU_SINGLE_SESSION: "true",

  };

  if (launchStudioFile) {
    logStartup(`open studio file: ${launchStudioFile}`);
  }

  shinyProcess = spawn(rscript, ["--vanilla", "run_app.R"], {
    cwd: appDir,
    env,
    detached: true,
    stdio: ["ignore", "pipe", "pipe"]
  });

  const startedProcess = shinyProcess;
  const killDirect = startedProcess.kill.bind(startedProcess);
  startedProcess.kill = signal => {
    try { process.kill(-startedProcess.pid, signal); return true; }
    catch (error) { if (error.code === "ESRCH") return killDirect(signal); throw error; }
  };
  shinyOrigin = `http://127.0.0.1:${port}`;
  const outputTail = [];
  const recentRText = () => (outputTail.length > 0 ? `\n\nRecent R output:\n${outputTail.join("\n")}` : "");
  const rememberOutput = (prefix, data) => {
    const text = data.toString();
    text.split(/\r?\n/).filter(Boolean).forEach((line) => {
      outputTail.push(`${prefix}: ${line}`);
      while (outputTail.length > 80) {
        outputTail.shift();
      }
      logStartup(`${prefix}: ${line}`);
    });
  };

  shinyProcess.stdout.on("data", (data) => {
    process.stdout.write(data);
    rememberOutput("R stdout", data);
  });
  shinyProcess.stderr.on("data", (data) => {
    process.stderr.write(data);
    rememberOutput("R stderr", data);
  });

  let shinyReady = false;
  const exitBeforeReady = new Promise((_, reject) => {
    shinyProcess.once("error", error => reject(error));
    shinyProcess.once("exit", (code, signal) => {
      if (!shinyReady && !isQuitting) {
        reject(new Error(`StatEdu Studio R process exited before startup (code ${code ?? "null"}, signal ${signal ?? "null"}).${recentRText()}`));
      }
    });
  });

  shinyProcess.on("exit", (code, signal) => {
    logStartup(`R process exited code=${code ?? "null"} signal=${signal ?? "null"}`);
    if (shinyProcess === startedProcess) shinyProcess = null;
  });

  const timeoutMs = shinyStartupTimeoutMs();
  logStartup(`waiting for Shiny timeoutMs=${timeoutMs}`);
  const shinyReadyWait = waitForShiny(port, timeoutMs).catch((error) => {
    throw new Error(`${error.message}${recentRText()}`);
  });
  await Promise.race([shinyReadyWait, exitBeforeReady]);
  shinyReady = true;
  logStartup(`Shiny ready in ${Date.now() - startedAt}ms`);
  return `http://127.0.0.1:${port}/?token=${token}&lang=${encodeURIComponent(initialLanguage)}&t=${Date.now()}`;
}

function stopShiny() {
  if (stoppingShiny) return stoppingShiny;
  const child = shinyProcess;
  if (!child || child.exitCode !== null || child.signalCode !== null) { shinyProcess = null; return Promise.resolve(); }
  stoppingShiny = new Promise((resolve, reject) => {
    let timer;
    const finish = error => {
      clearTimeout(timer); child.removeListener("exit", exited);
      if (error) reject(error); else { if (shinyProcess === child) shinyProcess = null; resolve(); }
    };
    const exited = () => finish();
    child.once("exit", exited);
    try {
      child.kill("SIGTERM");
      timer = setTimeout(() => {
        try {
          child.kill("SIGKILL");
          timer = setTimeout(() => finish(new Error("R exit could not be confirmed")), 2000);
        } catch (error) { finish(error); }
      }, 2000);
    } catch (error) { finish(error); }
  }).finally(() => { stoppingShiny = null; });
  return stoppingShiny;
}

function focusMainWindow() {
  if (!mainWindow) {
    return;
  }
  if (mainWindow.isMinimized()) {
    mainWindow.restore();
  }
  mainWindow.focus();
}

function confirmDiscardChanges() {
  const korean = (normalizeAppLanguage(process.env.STATEDU_APP_LANGUAGE) || readAppLanguage() || "ko") === "ko";
  return dialog.showMessageBoxSync(mainWindow, {
    type: "question", title: appDisplayName(),
    message: korean ? "저장하지 않은 프로젝트 변경 사항이 있습니다." : "There are unsaved project changes.",
    detail: korean ? "변경 사항을 버리고 계속하시겠습니까?" : "Discard the changes and continue?",
    buttons: korean ? ["취소", "변경 사항 버리기"] : ["Cancel", "Discard changes"],
    defaultId: 0, cancelId: 0
  }) === 1;
}

async function reloadStudioFile(filePath) {
  const studioFile = normalizeStudioFileArg(filePath);
  if (!studioFile || isQuitting) return;
  pendingStudioFile = studioFile;
  if (!mainWindow) { launchStudioFile = studioFile; pendingStudioFile = ""; return; }
  if (isReloadingStudioFile) return;
  isReloadingStudioFile = true;
  try {
    const dirty = await mainWindow.webContents.executeJavaScript("Boolean(window.easyflowSettingsDirty)").catch(() => false);
    if (dirty) {
      if (!confirmDiscardChanges()) { pendingStudioFile = ""; return; }
      await mainWindow.webContents.executeJavaScript("window.easyflowSettingsDirty = false");
    }
    while (pendingStudioFile && !isQuitting) {
      launchStudioFile = pendingStudioFile; pendingStudioFile = "";
      await stopShiny();
      const url = await startShiny();
      if (!pendingStudioFile && !isQuitting) { await mainWindow.loadURL(url); focusMainWindow(); }
    }
  } catch (error) { dialog.showErrorBox(appDisplayName(), error.message); }
  finally { isReloadingStudioFile = false; }
}

async function createWindow() {
  isReloadingStudioFile = true;
  logStartupEnvironment();
  mainWindow = new BrowserWindow({width:1536, height:1000, minWidth:1000, minHeight:700, title:windowTitle(),
    webPreferences:{preload:path.join(__dirname,"preload.js"), contextIsolation:true, nodeIntegration:false, sandbox:true}});
  configureDownloadSavePath(mainWindow.webContents);
  mainWindow.webContents.on("will-prevent-unload", event => {
    if (confirmDiscardChanges()) event.preventDefault();
  });
  installRendererDiagnostics(mainWindow.webContents);
  mainWindow.webContents.setWindowOpenHandler(({url}) => { if (/^https?:/.test(url)) shell.openExternal(url); return {action:"deny"}; });
  mainWindow.webContents.on("will-navigate", (event,url) => {
    try { if (new URL(url).origin === shinyOrigin) return; } catch (_) {}
    event.preventDefault(); if (/^https?:/.test(url)) shell.openExternal(url);
  });
  mainWindow.on("closed", () => { mainWindow = null; });
  try {
    const url = await startShiny();
    if (!pendingStudioFile) await mainWindow.loadURL(url);
  } catch (error) { dialog.showErrorBox(appDisplayName(), formatStartupError(error)); app.quit(); }
  finally { isReloadingStudioFile = false; }
  if (pendingStudioFile) await reloadStudioFile(pendingStudioFile);
}

app.on("open-file", (event, filePath) => {
  event.preventDefault();
  if (mainWindow) {
    reloadStudioFile(filePath);
  } else {
    launchStudioFile = normalizeStudioFileArg(filePath) || launchStudioFile;
  }
});

const singleInstanceLock = app.requestSingleInstanceLock();

if (!singleInstanceLock) {
  app.quit();
} else {
  app.on("second-instance", (event, argv) => {
    const studioFile = findStudioFileArg(argv);
    if (studioFile) {
      logStartup(`open studio file second-instance: ${studioFile}`);
      reloadStudioFile(studioFile);
      return;
    }
    focusMainWindow();
  });

  app.whenReady().then(createWindow);
}

app.on("activate", () => { if (mainWindow) focusMainWindow(); else if (!isQuitting) createWindow(); });
app.on("window-all-closed", () => app.quit());
app.on("before-quit", event => {
  if (shutdownComplete) return;
  event.preventDefault();
  if (shutdownPending) return;
  // Let the renderer's unsaved-project check complete before terminating R.
  if (mainWindow && !mainWindow.isDestroyed()) { mainWindow.close(); return; }
  shutdownPending = true; isQuitting = true; pendingStudioFile = "";
  stopShiny().then(() => { shutdownComplete = true; app.quit(); }).catch(error => {
    isQuitting = false; dialog.showErrorBox(appDisplayName(), error.message);
  }).finally(() => { shutdownPending = false; });
});
