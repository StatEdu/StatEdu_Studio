// Exercise the real private queue with native dialog/PDF APIs replaced by controlled adapters.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const {createDesktopBridge} = require('../packaging/macos/desktop-bridge');
const delay = ms => new Promise(resolve => setTimeout(resolve, ms));
(async () => {
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'statedu-bridge-'));
  const calls = [];
  let destroyCount=0;
  const stop = createDesktopBridge({directory, app:{getPath:()=>'/Users/한글 user/Documents'},window:()=>null,
    dialog:{showSaveDialog:async (_,options)=>{calls.push(options);return {canceled:true};},
      showOpenDialog:async (_,options)=>{calls.push(options);return {canceled:false,filePaths:['/한글 space/data.csv']};}},
    BrowserWindow:class {
      constructor(){this.webContents={setWindowOpenHandler:()=>{}, printToPDF:async options=>{assert.equal(options.preferCSSPageSize,true);return Buffer.from('%PDF-fixture');}};}
      async loadFile(file){assert.equal(file,'/report.html');}
      destroy(){destroyCount++;}
    }});
  async function request(id, data) {
    const filename=path.join(directory,`${id}.request.json`);
    fs.writeFileSync(filename,JSON.stringify(data));
    const response=filename.replace('.request.','.response.');
    const deadline=Date.now()+5000;
    while(!fs.existsSync(response)){assert.ok(Date.now()<deadline,'Queue timed out');await delay(20);}
    assert.ok(!fs.existsSync(filename));
    return JSON.parse(fs.readFileSync(response,'utf8'));
  }
  try {
    assert.equal((await request('a1',{operation:'save',defaultPath:'/한글 space/report.xlsx',extensions:['xlsx']})).canceled,true);
    assert.equal(calls.at(-1).defaultPath,'/한글 space/report.xlsx');
    assert.equal((await request('a2',{operation:'open',extensions:['csv']})).filePath,'/한글 space/data.csv');
    await request('a3',{operation:'directory'});assert.deepEqual(calls.at(-1).properties,['openDirectory','createDirectory']);
    const pdfPath=path.join(directory,'report.pdf');await request('a4',{operation:'pdf',htmlPath:'/report.html',pdfPath});
    assert.equal(fs.readFileSync(pdfPath,'utf8'),'%PDF-fixture');assert.equal(destroyCount,1);
    assert.match((await request('a5',{operation:'unknown'})).error,/Unknown/);
    fs.writeFileSync(path.join(directory,'a6.request.json'),'invalid JSON');
    await delay(150);assert.ok(JSON.parse(fs.readFileSync(path.join(directory,'a6.response.json'))).error);
    console.log('PASS: private queue, Unicode paths, cancellation, directory filters, PDF cleanup and malformed requests');
  } finally {stop();fs.rmSync(directory,{recursive:true,force:true});}
})().catch(error=>{console.error(error);process.exitCode=1;});
