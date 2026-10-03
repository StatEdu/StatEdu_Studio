'use strict';
const assert=require('node:assert/strict');
const fs=require('node:fs');
const os=require('node:os');
const path=require('node:path');
const vm=require('node:vm');
const {createSecurityScopedAccess}=require('../packaging/macos/sandbox-files');
const profile=fs.mkdtempSync(path.join(os.tmpdir(),'statedu-bookmarks-'));
let starts=[], stops=[];
const app={getPath:()=>profile,startAccessingSecurityScopedResource:value=>{starts.push(value);return ()=>stops.push(value);}};
const dialogs={showOpenDialog:async (...args)=>{
  assert.equal(args.at(-1).securityScopedBookmarks,true);
  return {canceled:false,filePaths:['/한글 space/project.studio'],bookmarks:['fixture-bookmark']};
},showSaveDialog:async (...args)=>{
  assert.equal(args.at(-1).securityScopedBookmarks,true);
  return {canceled:true};
}};
(async()=>{
  try {
    const access=createSecurityScopedAccess({app,enabled:true});
    access.restore();access.wrapDialogs(dialogs);
    const options={properties:['openFile']};
    assert.equal((await dialogs.showOpenDialog(null,options)).filePaths[0],'/한글 space/project.studio');
    assert(!options.securityScopedBookmarks,'Do not mutate caller options');
    await dialogs.showSaveDialog(null,{defaultPath:'/cancelled'});
    assert.deepEqual(starts,['fixture-bookmark']);
    const saved=path.join(profile,'settings/security-scoped-bookmarks.json');
    assert.equal(fs.statSync(saved).mode&0o777,0o600);
    access.release();access.release();
    assert.deepEqual(stops,['fixture-bookmark'],'Release each acquired permission once');
    const restarted=createSecurityScopedAccess({app,enabled:true});
    restarted.restore();assert.deepEqual(starts,['fixture-bookmark','fixture-bookmark']);
    restarted.release();
    const unchanged={showOpenDialog:async()=>{}};
    const original=unchanged.showOpenDialog;
    createSecurityScopedAccess({app,enabled:false}).wrapDialogs(unchanged);
    assert.equal(unchanged.showOpenDialog,original,'Preserve the download build behavior');
    const frontend=fs.readFileSync(path.join(__dirname,'../www/easyflow.js'),'utf8');
    const clickHelper=frontend.slice(frontend.indexOf("      // MAS file input selection"),frontend.indexOf("      // Shiny uploads retain"));
    for(const scoped of [true,false]) {
      let click,prevented=0,requests=[];
      vm.runInNewContext(clickHelper,{document:{addEventListener:(type,fn,capture)=>{assert.equal(type,'click');assert.equal(capture,true);click=fn;}},
        window:{stateduDesktopFiles:{securityScopedFiles:scoped},Shiny:{setInputValue:(...args)=>requests.push(args)}},Date});
      click({target:{id:'file',type:'file'},preventDefault:()=>prevented++});
      assert.equal(prevented,scoped?1:0);assert.equal(requests.length,scoped?1:0);
      if(scoped)assert.equal(requests[0][0],'browse_data_file');
      click({target:{id:'other',type:'file'},preventDefault:()=>prevented++});
      assert.equal(requests.length,scoped?1:0,'Other uploads retain their normal behavior');
    }
    console.log('PASS: dialog bookmarks, cancellation, private persistence, restart and balanced scope release');
  } finally {fs.rmSync(profile,{recursive:true,force:true});}
})().catch(error=>{console.error(error);process.exitCode=1;});
