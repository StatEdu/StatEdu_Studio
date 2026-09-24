// Real Electron IPC/preload/renderer, with only the OS dialog replaced to capture options.
const fs=require('node:fs'),path=require('node:path');
const root=path.resolve(__dirname,'../..');
const source=fs.readFileSync(path.join(root,'packaging/electron/main.js'),'utf8');
const setup=source.slice(0,source.indexOf('launchStudioFile = findStudioFileArg(process.argv);'));
new Function('require','root',setup+`
function defaultSaveDirectory(){return "";}
global.__dialogCalls=[];
dialog.showOpenDialog=async(w,options)=>{global.__dialogCalls.push(options);return {canceled:true};};
dialog.showSaveDialog=async(w,options)=>{global.__dialogCalls.push(options);return {canceled:true};};
app.setPath('userData',path.join(root,'tmp/dialog-electron-profile'));
app.whenReady().then(async()=>{
  mainWindow=new BrowserWindow({show:false,webPreferences:{preload:path.join(root,'packaging/electron/preload.js'),sandbox:true,contextIsolation:true,nodeIntegration:false,backgroundThrottling:false}});
  await mainWindow.loadURL('http://127.0.0.1:43989/?lang=en');
});
`)(require,root);
