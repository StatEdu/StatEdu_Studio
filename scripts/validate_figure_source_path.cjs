const vm=require('node:vm'),fs=require('node:fs'),assert=require('node:assert/strict');
let api, change, sent;
const file={name:'한글 데이터.csv'},input={id:'file',type:'file',files:[file]};
const nativePath='C:/Research/한글 데이터.csv';
const document={getElementById:id=>id==='file'?input:null,addEventListener:(name,fn)=>{if(name==='change')change=fn;}};
vm.runInNewContext(fs.readFileSync('packaging/electron/preload.js','utf8'),{
 require:()=>({contextBridge:{exposeInMainWorld:(name,value)=>api=value},ipcRenderer:{invoke(){},send(){}},webUtils:{getPathForFile:value=>{assert.equal(value,file);return nativePath;}}}),document,window:{addEventListener(){}}
});
assert.equal(api.dataFilePath(),nativePath);
const source=fs.readFileSync('www/easyflow.js','utf8');
vm.runInNewContext(source.slice(0,source.indexOf('      window.easyflowMeasurements')),{
 document,window:{stateduDesktopFiles:api,Shiny:{setInputValue:(name,payload)=>sent={name,payload}}}
});
change({target:input});assert.deepEqual(JSON.parse(JSON.stringify(sent)),{name:'desktop_data_source',payload:{name:file.name,path:nativePath}});
input.files=[];assert.equal(api.dataFilePath(),'');console.log('PASS desktop file path bridge: original Unicode path, upload metadata, cleared selection');
