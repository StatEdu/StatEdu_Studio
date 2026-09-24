const assert=require('node:assert/strict'),fs=require('node:fs'),path=require('node:path'),vm=require('node:vm');
const source=fs.readFileSync('packaging/electron/main.js','utf8');
const directory=path.resolve('tmp/loaded-language-regression/자료 폴더');
fs.mkdirSync(directory,{recursive:true});
const handlers={},events={},dialogs=[],downloads={};
const sender={getURL:()=> 'http://127.0.0.1:43989',session:{on:(key,fn)=>{downloads[key]=fn;}}};
const electron={app:{disableHardwareAcceleration(){},commandLine:{appendSwitch(){}},getPath:()=>directory},
 ipcMain:{on:(k,f)=>{events[k]=f;},handle:(k,f)=>{handlers[k]=f;}},
 dialog:{showOpenDialog:async(w,o)=>{dialogs.push(o);return {canceled:true};},showSaveDialog:async(w,o)=>{dialogs.push(o);return {canceled:true};}}};
const context=vm.createContext({require:n=>n==='electron'?electron:require(n),process,Buffer,console,sender});
vm.runInContext(source.slice(0,source.indexOf('launchStudioFile = findStudioFileArg(process.argv);'))+
 source.slice(source.indexOf('function configureDownloadSavePath('),source.indexOf('function installRendererDiagnostics('))+
 '\nmainWindow={webContents:sender};function defaultSaveDirectory(){return "";}configureDownloadSavePath(sender);',context);
(async()=>{
 const event={sender,senderFrame:{url:sender.getURL()}};
 events['statedu:data-directory'](event,directory);
 events['statedu:data-directory'](event,path.join(directory,'RtmpOLD123','upload-token'));
 assert.equal(vm.runInContext('currentDataDirectory',context),directory);
 for(const extension of ['streg','stcfa','stsem','stpls']){
   await handlers['statedu:canvas-open-text'](event,{extensions:[extension]});assert.equal(dialogs.at(-1).defaultPath,directory);
   await handlers['statedu:canvas-save'](event,{extensions:[extension],suggestedName:'model.'+extension});assert.equal(dialogs.at(-1).defaultPath,path.join(directory,'model.'+extension));
 }
 for(const extension of ['stmmr','stcfar','stsemr','stplsr']){
   await handlers['statedu:result-choose-open'](event,{extensions:[extension]});assert.equal(dialogs.at(-1).defaultPath,directory);
   await handlers['statedu:result-choose-save'](event,{extensions:[extension],suggestedName:'result.'+extension});assert.equal(dialogs.at(-1).defaultPath,path.join(directory,'result.'+extension));
 }
 let downloadOptions;
 downloads['will-download']({}, {getFilename:()=> 'result.html',setSaveDialogOptions:o=>downloadOptions=o,setSavePath:()=>assert.fail('Must show save dialog')});
 assert.equal(downloadOptions.defaultPath,path.join(directory,'result.html'));
 events['statedu:data-directory']({sender:{}},'');assert.equal(vm.runInContext('currentDataDirectory',context),directory);
 events['statedu:data-directory'](event,'');assert.equal(vm.runInContext('currentDataDirectory',context),'');
 events['statedu:data-directory'](event,path.join(directory,'missing'));assert.equal(vm.runInContext('currentDataDirectory',context),'');
 console.log('PASS: four canvas and four analysis result open/save dialogs, download dialog, reset, missing directory, untrusted sender');
})().catch(e=>{console.error(e);process.exitCode=1});
