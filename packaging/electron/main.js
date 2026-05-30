const { app, BrowserWindow, dialog } = require("electron");
const { spawn, spawnSync } = require("child_process");
const crypto = require("crypto");
const fs = require("fs");
const http = require("http");
const net = require("net");
const path = require("path");

let mainWindow = null;
let shinyProcess = null;
let isQuitting = false;
let startupLogPath = null;

function startupLogFile() {
  if (!startupLogPath) {
    startupLogPath = path.join(app.getPath("userData"), "logs", "startup.log");
  }
  return startupLogPath;
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

function windowTitle() {
  return `EasyFlow Statistics Beta v${appVersion()}`;
}

function bundledRscriptPath() {
  return path.join(appBaseDir(), "runtime", "R-4.5.2", "bin", "x64", "Rscript.exe");
}

function bundledRBinPath() {
  return path.join(appBaseDir(), "runtime", "R-4.5.2", "bin", "x64");
}

function bundledRLibraryPath() {
  return path.join(appBaseDir(), "runtime", "R-4.5.2", "library");
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

function waitForShiny(port, timeoutMs = 45000) {
  const startedAt = Date.now();
  return new Promise((resolve, reject) => {
    const probe = () => {
      const request = http.get(`http://127.0.0.1:${port}/`, (response) => {
        response.resume();
        if (response.statusCode && response.statusCode < 500) {
          resolve();
          return;
        }
        retry();
      });
      request.on("error", retry);
      request.setTimeout(1500, () => {
        request.destroy();
        retry();
      });
    };
    const retry = () => {
      if (Date.now() - startedAt > timeoutMs) {
        reject(new Error("EasyFlow Statistics did not start in time."));
        return;
      }
      setTimeout(probe, 150);
    };
    probe();
  });
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
    throw new Error(`Bundled EasyFlow app was not found: ${appDir}`);
  }

  const port = await getFreePort();
  const token = crypto.randomBytes(32).toString("hex");
  const env = {
    ...process.env,
    EASYFLOW_PORT: String(port),
    EASYFLOW_APP_DIR: appDir,
    EASYFLOW_LAUNCH_BROWSER: "false",
    EASYFLOW_NO_PACKAGE_INSTALL: "true",
    EASYFLOW_TOKEN: token,
    EASYFLOW_STARTUP_LOG: startupLogFile(),
    R_HOME: path.join(appBaseDir(), "runtime", "R-4.5.2"),
    R_LIBS_USER: bundledRLibraryPath(),
    PATH: `${bundledRBinPath()};${process.env.PATH || ""}`
  };

  shinyProcess = spawn(rscript, ["run_app.R"], {
    cwd: appDir,
    env,
    windowsHide: true,
    stdio: ["ignore", "pipe", "pipe"]
  });

  shinyProcess.stdout.on("data", (data) => process.stdout.write(data));
  shinyProcess.stderr.on("data", (data) => process.stderr.write(data));
  shinyProcess.on("exit", () => {
    logStartup("R process exited");
    shinyProcess = null;
  });

  await waitForShiny(port);
  logStartup(`Shiny ready in ${Date.now() - startedAt}ms`);
  return `http://127.0.0.1:${port}/?token=${token}&t=${Date.now()}`;
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

async function createWindow() {
  logStartup("createWindow begin");
  app.setName("EasyFlow Statistics Beta");
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
    logStartup(`startup failed: ${error.message}`);
    dialog.showErrorBox("EasyFlow Statistics Beta", error.message);
    app.quit();
  }
}

app.whenReady().then(createWindow);

app.on("window-all-closed", () => {
  isQuitting = true;
  stopShiny();
  app.quit();
});

app.on("before-quit", () => {
  isQuitting = true;
  stopShiny();
});
