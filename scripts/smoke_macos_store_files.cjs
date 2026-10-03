'use strict';
const {_electron}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
const fs=require('node:fs'),path=require('node:path'),os=require('node:os'),assert=require('node:assert/strict');
const root=path.resolve(__dirname,'..');
const executable=process.argv.find(x=>x.startsWith('--executable='))?.slice(13);
assert(executable,'Supply --executable=/absolute/path/to/MAS.app/Contents/MacOS/App');
const output=fs.mkdtempSync(path.join(root,'tmp','mas-native-'));
const fixture=path.join(root,'scripts/fixtures/survival_validation.csv');
const profile=path.join(os.homedir(),'Library/Containers/com.statedu.studio.mac/Data/Library/Application Support/StatEdu Studio',path.basename(output),'profile');
const specs=JSON.parse(fs.readFileSync(path.join(root,'docs/i18n/document_specs.json'),'utf8').replace(/^\uFEFF/,''));
const deadline=600000;
(async()=>{let app;try{
const launch=async(project)=>_electron.launch({executablePath:executable,args:[`--user-data-dir=${profile}`,...(project?[project]:[])],env:{...process.env,STATEDU_APP_LANGUAGE:'ko'},timeout:120000});
const ready=async()=>{const w=await app.firstWindow();await w.waitForURL(/^http:\/\/127\.0\.0\.1:/,{timeout:120000});await w.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1,null,{timeout:120000});return w;};
app=await launch();let w=await ready();
assert.equal(await app.evaluate(()=>process.mas),true);
await app.evaluate(({BrowserWindow})=>BrowserWindow.getAllWindows()[0].setContentSize(1280,800));
for(const language of ['en','ja','zh','es','fr','de','vi','ko']) {
 await w.locator('a[data-value="about_preferences"]').evaluate(e=>e.click());await w.locator('#app_language').selectOption(language);
 await w.locator('a[data-value="about_user_guide"]').evaluate(e=>e.click());
 await w.waitForFunction(title=>document.querySelector('#lazy_about_user_guide h1')?.textContent===title,specs[language].user_guide.title,{timeout:30000});
 await w.screenshot({path:path.join(output,`store-${language}.png`),scale:'css'});
}
await w.locator('#main_menu > li > a').first().evaluate(e=>e.click());
const saveDefault=path.join(os.homedir(),'Downloads',`StatEdu Studio sandbox validation ${path.basename(output)}.studio`);
await app.evaluate(({dialog},{fixture,saveDefault})=>{
 globalThis.nativeStoreSelections=[];
 const open=dialog.showOpenDialog.bind(dialog),save=dialog.showSaveDialog.bind(dialog);
 dialog.showOpenDialog=async(...args)=>{args[args.length-1]={...args.at(-1),defaultPath:fixture};const r=await open(...args);globalThis.nativeStoreSelections.push({operation:'open',paths:r.filePaths,canceled:r.canceled,bookmarkCount:r.bookmarks?.length||0});return r;};
 dialog.showSaveDialog=async(...args)=>{args[args.length-1]={...args.at(-1),defaultPath:saveDefault};const r=await save(...args);globalThis.nativeStoreSelections.push({operation:'save',path:r.filePath,canceled:r.canceled,bookmark:Boolean(r.bookmark)});return r;};
},{fixture,saveDefault});
assert.equal(await w.evaluate(()=>window.stateduDesktopFiles.securityScopedFiles),true);
await w.locator('#file').evaluate(input=>input.click());
console.log('Native data Open panel ready. Select survival_validation.csv and click Open.');
await w.locator('#apply_all_variable_selection').waitFor({state:'visible',timeout:deadline});
await w.locator('#apply_all_variable_selection').click();
await w.waitForFunction(()=>document.querySelector('#data_steps')?.innerText.includes('survival_validation.csv')&&!document.querySelector('#data_steps')?.classList.contains('recalculating'),null,{timeout:60000});
await w.screenshot({path:path.join(output,'external-data-loaded.png'),scale:'css'});
await w.evaluate(()=>Shiny.setInputValue('save_settings_request',{}, {priority:'event'}));
console.log('Native project Save panel ready. Save the proposed file in Downloads.');
let selected,project;
for(let start=Date.now();Date.now()-start<deadline;) {
 selected=await app.evaluate(()=>globalThis.nativeStoreSelections);
 project=selected.find(x=>x.operation==='save'&&!x.canceled)?.path;
 if(project&&await app.evaluate((_electron,p)=>{const f=process.getBuiltinModule('fs');return f.existsSync(p)&&f.statSync(p).size>100;},project))break;
 await new Promise(resolve=>setTimeout(resolve,1000));
}
assert(project,'Native save did not finish');
const bookmarks=await app.evaluate(({app})=>Object.keys(JSON.parse(process.getBuiltinModule('fs').readFileSync(app.getPath('userData')+'/settings/security-scoped-bookmarks.json','utf8')).bookmarks));
fs.writeFileSync(path.join(output,'native-selections.json'),JSON.stringify({selections:selected,bookmarkPaths:bookmarks},null,2));
assert(bookmarks.includes(fixture)&&bookmarks.includes(project));
assert.equal(selected.find(x=>x.operation==='open').bookmarkCount,1);assert(selected.find(x=>x.operation==='save').bookmark);
await w.evaluate(()=>window.easyflowSettingsDirty=false);await app.close();app=null;
app=await launch(project);w=await ready();
await w.waitForFunction(()=>document.querySelector('#data_steps')?.innerText.includes('survival_validation.csv')&&!document.querySelector('#data_steps')?.classList.contains('recalculating'),null,{timeout:60000});
await w.screenshot({path:path.join(output,'external-project-restored.png'),scale:'css'});
const result={status:'passed',version:'1.3.1',masSandbox:true,nativePowerbox:true,externalDataRead:true,externalProjectSaved:true,externalProjectAndDataRestored:true,bookmarkCount:bookmarks.length,storeScreenshotLocales:8,output};
fs.writeFileSync(path.join(output,'result.json'),JSON.stringify(result,null,2));console.log(JSON.stringify(result));
await w.evaluate(()=>window.easyflowSettingsDirty=false);
}finally{if(app)await app.close().catch(()=>{});}})().catch(e=>{console.error(e);process.exitCode=1;});
