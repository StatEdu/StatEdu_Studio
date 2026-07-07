const { app, BrowserWindow, dialog, shell } = require("electron");
const { spawn, spawnSync } = require("child_process");
const crypto = require("crypto");
const fs = require("fs");
const net = require("net");
const path = require("path");

const enableHardwareAcceleration = /^(1|true|yes)$/i.test(process.env.STATEDU_ENABLE_HARDWARE_ACCELERATION || "");
if (!enableHardwareAcceleration) {
  app.disableHardwareAcceleration();
  app.commandLine.appendSwitch("disable-gpu");
}

const DEFAULT_SHINY_STARTUP_TIMEOUT_MS = 180000;

let mainWindow = null;
let shinyProcess = null;
let isQuitting = false;
let startupLogPath = null;
let launchStudioFile = "";
let isReloadingStudioFile = false;

function normalizeStudioFileArg(value) {
  const raw = String(value || "").trim().replace(/^"|"$/g, "");
  if (!raw || raw.startsWith("--")) {
    return "";
  }
  const resolved = path.resolve(raw);
  if (path.extname(resolved).toLowerCase() !== ".studio") {
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

function startupLogFile() {
  if (!startupLogPath) {
    startupLogPath = path.join(app.getPath("userData"), "logs", "startup.log");
  }
  return startupLogPath;
}

function appLanguageFile() {
  return path.join(app.getPath("userData"), "settings", "app-language.txt");
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
  webContents.on("console-message", (event, level, message, line, sourceId) => {
    logStartup(`renderer console level=${level} ${sourceId || ""}:${line || 0} ${message}`);
  });
  webContents.on("did-fail-load", (event, errorCode, errorDescription, validatedURL) => {
    logStartup(`renderer did-fail-load code=${errorCode} url=${validatedURL || ""} ${errorDescription || ""}`);
  });
  webContents.on("dom-ready", () => {
    logStartup("renderer dom-ready");
  });
  webContents.on("did-finish-load", () => {
    logStartup("renderer did-finish-load");
    logRendererSnapshot(webContents, "did-finish-load");
    setTimeout(() => logRendererSnapshot(webContents, "after-10s"), 10000);
    setTimeout(() => logRendererSnapshot(webContents, "after-30s"), 30000);
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
    const file = startupLogFile();
    fs.mkdirSync(path.dirname(file), { recursive: true });
    fs.appendFileSync(file, line, "utf8");
  } catch (error) {
    // Logging must never block app startup.
  }
}

function logStartupEnvironment() {
  logStartup(`app=${appDisplayName()} version=${appVersion()}`);
  logStartup(`electron=${process.versions.electron} chrome=${process.versions.chrome} node=${process.versions.node}`);
  logStartup(`platform=${process.platform} arch=${process.arch} windowsRelease=${require("os").release()}`);
  logStartup(`hardwareAcceleration=${enableHardwareAcceleration ? "enabled" : "disabled"}`);
  logStartup(`userData=${app.getPath("userData")}`);
  logStartup(`appPath=${app.getAppPath()}`);
}

function appBaseDir() {
  return app.getAppPath();
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

function publicReleaseFlag() {
  return /^\d+\.\d+\.\d+$/.test(appVersion()) ? "1" : "0";
}

function isPublicRelease() {
  return publicReleaseFlag() === "1";
}

function isDeveloperRelease() {
  return /^\d+\.\d+\.\d+-dev$/.test(appVersion());
}

function appDisplayName() {
  if (isPublicRelease()) {
    return "StatEdu Studio";
  }
  return isDeveloperRelease() ? "StatEdu Studio Dev" : "StatEdu Studio Beta";
}

function windowTitle() {
  return `${appDisplayName()} v${appVersion()}`;
}

function bundledRscriptPath() {
  return path.join(appBaseDir(), "runtime", "R-4.5.3", "bin", "x64", "Rscript.exe");
}

function bundledRBinPath() {
  return path.join(appBaseDir(), "runtime", "R-4.5.3", "bin", "x64");
}

function bundledRLibraryPath() {
  return path.join(appBaseDir(), "runtime", "R-4.5.3", "library");
}

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

function runRscriptProbe(rscript, appDir) {
  const result = spawnSync(rscript, ["--version"], {
    cwd: appDir,
    encoding: "utf8",
    windowsHide: true
  });
  const output = [result.stdout, result.stderr].filter(Boolean).join("\n").trim();
  if (output) {
    output.split(/\r?\n/).filter(Boolean).forEach((line) => logStartup(`R probe: ${line}`));
  }
  if (result.error) {
    throw new Error(`Bundled Rscript could not be started: ${result.error.message}`);
  }
  if (result.status !== 0) {
    throw new Error(`Bundled Rscript probe failed with exit code ${result.status ?? "null"}.`);
  }
}

function formatStartupError(error) {
  const logPath = startupLogFile();
  return [
    error && error.message ? error.message : String(error),
    "",
    `Startup log: ${logPath}`,
    "If this happens on another PC, send this log file with the Windows version and the installer filename."
  ].join("\n");
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
  const initialLanguage = process.env.STATEDU_APP_LANGUAGE || "ko";
  const env = {
    ...process.env,
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
    STATEDU_PUBLIC_RELEASE: process.env.STATEDU_PUBLIC_RELEASE || publicReleaseFlag(),
    R_HOME: path.join(appBaseDir(), "runtime", "R-4.5.3"),
    R_LIBS_USER: bundledRLibraryPath(),
    PATH: `${bundledRBinPath()};${process.env.PATH || ""}`
  };

  if (launchStudioFile) {
    logStartup(`open studio file: ${launchStudioFile}`);
  }

  shinyProcess = spawn(rscript, ["run_app.R"], {
    cwd: appDir,
    env,
    windowsHide: true,
    stdio: ["ignore", "pipe", "pipe"]
  });

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
    shinyProcess.once("exit", (code, signal) => {
      if (!shinyReady && !isQuitting) {
        reject(new Error(`StatEdu Studio R process exited before startup (code ${code ?? "null"}, signal ${signal ?? "null"}).${recentRText()}`));
      }
    });
  });

  shinyProcess.on("exit", (code, signal) => {
    logStartup(`R process exited code=${code ?? "null"} signal=${signal ?? "null"}`);
    shinyProcess = null;
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
  const processToStop = shinyProcess;
  shinyProcess = null;
  if (processToStop && processToStop.pid && !processToStop.killed) {
    if (process.platform === "win32") {
      spawnSync("taskkill", ["/pid", String(processToStop.pid), "/t", "/f"], {
        windowsHide: true,
        stdio: "ignore"
      });
    } else {
      processToStop.kill("SIGTERM");
    }
  }
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

async function reloadStudioFile(filePath) {
  const studioFile = normalizeStudioFileArg(filePath);
  if (!studioFile) {
    return;
  }
  launchStudioFile = studioFile;
  logStartup(`reload studio file: ${launchStudioFile}`);
  if (!mainWindow || isReloadingStudioFile) {
    return;
  }
  isReloadingStudioFile = true;
  try {
    stopShiny();
    const url = await startShiny();
    const loadStartedAt = Date.now();
    await mainWindow.loadURL(url);
    logStartup(`BrowserWindow reloaded Shiny URL in ${Date.now() - loadStartedAt}ms`);
    focusMainWindow();
  } catch (error) {
    logStartup(`studio file reload failed: ${error.message}`);
    dialog.showErrorBox(appDisplayName(), error.message);
  } finally {
    isReloadingStudioFile = false;
  }
}

async function createWindow() {
  logStartup("createWindow begin");
  logStartupEnvironment();
  app.setName(appDisplayName());
  mainWindow = new BrowserWindow({
    width: 1536,
    height: 1000,
    minWidth: 1120,
    minHeight: 760,
    title: windowTitle(),
    autoHideMenuBar: true,
    webPreferences: {
      contextIsolation: true,
      nodeIntegration: false,
      sandbox: true
    }
  });

  configureDownloadSavePath(mainWindow.webContents);
  installRendererDiagnostics(mainWindow.webContents);

  mainWindow.webContents.setWindowOpenHandler(({ url }) => {
    if (url.startsWith("http://127.0.0.1") || url.startsWith("http://localhost")) {
      return { action: "allow" };
    }
    shell.openExternal(url);
    return { action: "deny" };
  });

  mainWindow.webContents.on("will-navigate", (event, url) => {
    if (!url.startsWith("http://127.0.0.1") && !url.startsWith("http://localhost")) {
      event.preventDefault();
      shell.openExternal(url);
    }
  });

  mainWindow.on("page-title-updated", (event) => {
    event.preventDefault();
    mainWindow.setTitle(windowTitle());
  });

  mainWindow.on("close", () => {
    if (!isQuitting) {
      isQuitting = true;
      stopShiny();
      setTimeout(() => app.exit(0), 100);
    }
  });

  try {
    const url = await startShiny();
    const loadStartedAt = Date.now();
    logStartup("BrowserWindow loadURL begin");
    await mainWindow.loadURL(url);
    logStartup(`BrowserWindow loaded Shiny URL in ${Date.now() - loadStartedAt}ms`);
  } catch (error) {
    const message = formatStartupError(error);
    logStartup(`startup failed: ${error.message}`);
    dialog.showErrorBox(appDisplayName(), message);
    app.quit();
  }
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

app.on("window-all-closed", () => {
  isQuitting = true;
  stopShiny();
  app.quit();
});

app.on("before-quit", () => {
  isQuitting = true;
  stopShiny();
});
