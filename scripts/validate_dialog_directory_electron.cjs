const assert=require('node:assert/strict'),path=require('node:path');
const {_electron}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{
 const app=await _electron.launch({executablePath:process.env.STATEDU_ELECTRON_EXE||path.resolve('tmp/electron-dialog-runtime/electron.exe'),args:[path.resolve('scripts/fixtures/dialog_directory_electron.cjs')]});
 try {
  const page=await app.firstWindow();
  page.on('dialog',dialog=>dialog.accept().catch(()=>{}));
  await page.waitForFunction(()=>document.querySelector('#file.shiny-bound-input')&&!document.documentElement.classList.contains('shiny-busy'));
  const directory=path.resolve('tmp/loaded-language-regression/자료 폴더');
  await page.evaluate(()=>Shiny.addCustomMessageHandler('fixture-settings-ready',message=>{window.fixtureSettingsReady=true;}));
  for(const mode of ['settings-button','settings-separate','upload','legacy','embedded','native']){
   if(mode==='upload') await page.locator('#file').setInputFiles(path.join(directory,'자료.csv'));
   else await page.evaluate(mode=>Shiny.setInputValue('fixture_load',mode,{priority:'event'}),mode);
   if(mode==='settings-button') {
    await page.waitForFunction(()=>window.fixtureSettingsReady&&!document.documentElement.classList.contains('shiny-busy'));
    await page.locator('#browse_settings_data').click();
   }
   try {
    await page.waitForFunction(()=>document.querySelector('#data_loaded_message')?.textContent.includes('40'));
   } catch(error) {
    console.log('FAILED STATE',mode,await page.evaluate(()=>({data:document.querySelector('#data_steps')?.textContent,errors:[...document.querySelectorAll('.shiny-output-error')].map(e=>e.textContent),socket:Shiny.shinyapp.$socket?.readyState})));
    throw error;
   }
   await page.waitForTimeout(700);
   if(mode==='settings-button') assert.ok((await page.locator('#data_loaded_message').textContent()).includes('자료.csv'),'Settings restore preserves the original data filename');
   for(const extension of ['streg','stcfa','stsem','stpls']){
    await page.evaluate(extension=>window.stateduDesktopFiles.openText({extensions:[extension]}),extension);
    let actual=await app.evaluate(()=>global.__dialogCalls.at(-1).defaultPath);
    assert.equal(path.resolve(actual),directory,mode+' open '+extension);
    await page.evaluate(extension=>window.stateduDesktopFiles.save({extensions:[extension],suggestedName:'model.'+extension}),extension);
    actual=await app.evaluate(()=>global.__dialogCalls.at(-1).defaultPath);
    assert.equal(path.resolve(actual),path.join(directory,'model.'+extension),mode+' save '+extension);
   }
   console.log('PASS actual Electron preload/IPC:',mode,'four canvas open/save directories');
  }
 } finally {await app.close();}
})().catch(e=>{console.error(e);process.exitCode=1;});
