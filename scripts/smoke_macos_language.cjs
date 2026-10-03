'use strict';
const {_electron} = require(process.env.STATEDU_PLAYWRIGHT_MODULE || 'playwright');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const root = path.resolve(__dirname, '..');
const argument = process.argv.find(value => value.startsWith('--executable='));
assert(argument, 'Supply --executable=/path/to/App.app/Contents/MacOS/App');
const executable = path.resolve(argument.slice('--executable='.length));
const output = fs.mkdtempSync(path.join(root, 'tmp', 'language-smoke-'));
const profile = path.join(output, 'profile');
const env = {...process.env, STATEDU_RESULT_STORE: path.join(output, 'results.json')};
delete env.STATEDU_APP_LANGUAGE;
let application;
async function launch() {
  application = await _electron.launch({executablePath: executable, args: [`--user-data-dir=${profile}`], env, timeout:120000});
  const page = await application.firstWindow();
  await page.waitForURL(/^http:\/\/127\.0\.0\.1:/, {timeout:120000});
  await page.waitForFunction(() => window.Shiny?.shinyapp?.$socket?.readyState === 1, null, {timeout:120000});
  await page.locator('.navbar-nav a[data-value="about_preferences"]').evaluate(e=>e.click());
  await page.locator('#app_language').waitFor({state:'visible'});
  return page;
}
async function close() {
  if (!application) return;
  const page = await application.firstWindow();
  await page.evaluate(() => {window.easyflowSettingsDirty=false;});
  const closed = application.waitForEvent('close', {timeout:20000});
  await application.evaluate(({app}) => setTimeout(()=>app.quit(),0));
  await closed;
  application = null;
}
(async () => {
  try {
    let page = await launch();
    const preferred = await application.evaluate(({app}) => app.getPreferredSystemLanguages());
    const supported = ['en','ko','ja','zh','es','fr','de','vi'];
    const expected = preferred.map(x=>x.toLowerCase().split(/[-_]/)[0]).find(x=>supported.includes(x)) || 'en';
    assert.equal(new URL(page.url()).searchParams.get('lang'), expected);
    assert.equal(await page.locator('#app_language').inputValue(), expected);
    const firstInstallLanguage=expected;
    const chosen=expected==='fr' ? 'ja' : 'fr';
    await page.locator('#app_language').selectOption(chosen);
    const dataDir = await application.evaluate(({app}) => app.getPath('userData'));
    assert.equal(path.resolve(dataDir), path.resolve(profile));
    const languageFile=path.join(dataDir,'settings','app-language.txt');
    for(let attempt=0;attempt<100;attempt++) {
      if(fs.existsSync(languageFile) && fs.readFileSync(languageFile,'utf8').trim()===chosen) break;
      await page.waitForTimeout(100);
    }
    assert.equal(fs.readFileSync(languageFile,'utf8').trim(), chosen);
    await close();
    page=await launch();
    assert.equal(new URL(page.url()).searchParams.get('lang'), chosen);
    assert.equal(await page.locator('#app_language').inputValue(), chosen);
    await page.locator('.navbar-nav a[data-value="about_user_guide"]').evaluate(e=>e.click());
    const specs=JSON.parse(fs.readFileSync(path.join(root,'docs/i18n/document_specs.json'),'utf8').replace(/^\uFEFF/,''));
    await page.waitForFunction(title=>document.querySelector('#lazy_about_user_guide h1')?.textContent===title,
      specs[chosen].user_guide.title,{timeout:30000});
    await page.screenshot({path:path.join(output,'persisted-language-guide.png')});
    await close();
    const result={status:'passed',preferredLanguages:preferred,firstInstallLanguage,persistedLanguage:chosen,guideLanguage:chosen,isolatedProfile:true};
    fs.writeFileSync(path.join(output,'result.json'),JSON.stringify(result,null,2)+'\n');
    console.log(JSON.stringify({...result,output}));
  } finally {await close();}
})().catch(error=>{console.error(error);process.exitCode=1;});
