const assert=require('node:assert/strict');
const fs=require('node:fs');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE || 'playwright');
(async()=>{
 const browser=await chromium.launch({channel:'chrome',headless:true});
 try {
  const page=await browser.newPage({viewport:{width:1488,height:1000}});
  const errors=[];page.on('pageerror',e=>errors.push(e.message));
  await page.goto(process.env.STATEDU_UI_TEST_URL || 'http://127.0.0.1:3873/?lang=ko');
  await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1);
  await page.waitForFunction(()=>document.querySelector('#data_steps #file.shiny-bound-input') && !document.documentElement.classList.contains('shiny-busy'));
  // Allow deferred session restoration to finish before replacing its upload input.
  await page.waitForTimeout(1200);
  await page.locator('#file').setInputFiles({name:'language-cases.csv',mimeType:'text/csv',buffer:Buffer.from('sex,x,y\nMale,1,2\nFemale,2,4\nMale,3,5\nFemale,4,7\nMale,5,9\nFemale,6,10\n')});
  await page.locator('#apply_all_variable_selection').waitFor({state:'visible'});
  await page.locator('#apply_all_variable_selection').click();
  await page.evaluate(()=>window.__languageSessionSentinel='same-session');
  async function visit(value) {
    await page.locator(`.navbar-nav a[data-value="${value}"]`).first().evaluate(el=>el.click());
    await page.waitForTimeout(700);
  }
  await visit('data_editor_cases');
  await page.locator('[data-input-id="scope_cases_available"] [data-value="sex"]').waitFor({state:'visible'});
  await page.locator('[data-input-id="scope_cases_available"] [data-value="sex"]').dblclick();
  await page.locator('#scope_cases_values input[value="Male"]').check();
  await page.locator('#scope_cases_apply').click();
  await page.waitForFunction(()=>document.getElementById('statedu-analysis-scope-status')?.textContent.includes('3/6'));
  await visit('about_preferences');
  await page.locator('#app_language').waitFor({state:'visible',timeout:8000});
  for(const lang of ['ja','zh','es','fr','de','vi','en','ko']) {
    await page.locator('#app_language').selectOption(lang);
    await page.waitForTimeout(1200);
    assert.equal(await page.evaluate(()=>window.__languageSessionSentinel),'same-session');
    assert.equal(await page.evaluate(()=>window.easyflowAppLanguage),lang);
    await visit('Regression');
    await page.locator('#lazy_analysis_hierarchical .hierarchical-workspace-panel').waitFor({state:'visible',timeout:8000});
    assert.match(await page.locator('#statedu-analysis-scope-status').textContent(),/3\/6/);
    await visit('One-group repeated-measures ANOVA');
    await page.locator('#lazy_analysis_one_group_rm_anova').waitFor({state:'visible',timeout:8000});
    await page.locator('#run_one_group_rm_anova').waitFor({state:'visible',timeout:8000});
    assert.match(await page.locator('#statedu-analysis-scope-status').textContent(),/3\/6/);
    await visit('result');
    await page.locator('#saved_results_list .empty-message').waitFor({state:'visible',timeout:8000});
    const dictionary=JSON.parse(fs.readFileSync(`i18n/${lang}.json`,'utf8')).translations;
    // English/Korean base labels are defined in R/labels.R, not duplicated in JSON.
    const dataLabel=dictionary['ui.data'] || (lang==='ko'?'데이터':'Data');
    const oneGroupLabel=dictionary['ui.one_group_rm_anova'] || (lang==='ko'?'동일 대상 내 처치 반복측정 분산분석':'Within-subject treatment repeated-measures ANOVA');
    await page.waitForFunction(expected=>document.querySelector('.navbar-nav > li > a')?.textContent.trim()===expected,dataLabel);
    await page.waitForFunction(expected=>document.querySelector('.navbar-nav a[data-value="One-group repeated-measures ANOVA"]')?.textContent.trim()===expected,oneGroupLabel);
    const emptyExpected=dictionary['result.empty_message'] || (lang==='ko'?'분석 후 결과 추가를 클릭하면 여기에 결과를 모을 수 있습니다.':'Click Add result after an analysis to collect results here.');
    await page.waitForFunction(expected=>document.querySelector('#saved_results_list .empty-message')?.textContent.trim()===expected,emptyExpected);
    await page.locator('.navbar-nav > li > a').first().evaluate(el=>el.click());
    await page.locator('#go_step1').waitFor({state:'visible',timeout:8000});
    const stepExpected=dictionary['data.step1_load_file'] || (lang==='ko'?'Step 1. 데이터 파일 열기':'Step 1. Load data file');
    await page.waitForFunction(expected=>document.querySelector('#go_step1')?.textContent.trim()===expected,stepExpected);
    await visit('about_preferences');
    await page.locator('#app_language').waitFor({state:'visible',timeout:8000});
  }
  assert.deepEqual(errors,[]);
  console.log('PASS: 8-language round trip retains session, 3/6 case selection, regression/one-group screens; navbar and data/results text match catalogs; no page errors.');
 } finally {await browser.close();}
})().catch(e=>{console.error(e);process.exitCode=1;});
