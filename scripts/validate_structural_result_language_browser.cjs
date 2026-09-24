const assert=require('node:assert/strict'),fs=require('node:fs');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
 const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto('http://127.0.0.1:43874/');
 await page.waitForSelector('#rendered-results[data-language="ko"]');
 const main={},korean={};
 for(const type of ['cfa','cbsem','plssem']){
  main[type]=await page.locator(`#result-${type} .main-result`).innerText();
  korean[type]=await page.locator(`#result-${type}`).innerText();
  assert.ok(main[type].includes('사용자 라벨 Normality'));
 }
 for(const language of ['ja','zh','es','fr','de','vi','en','ko']){
  await page.locator('#language').evaluate((el,value)=>el.selectize.setValue(value),language);
  await page.waitForSelector(`#rendered-results[data-language="${language}"]`);
  for(const type of ['cfa','cbsem','plssem']){
   const root=page.locator('#result-'+type),text=await root.innerText();
   assert.equal(await root.locator('.main-result').innerText(),main[type],`${language}/${type}: English main table`);
   assert.ok(text.includes('Requested 사용자 <&>'),`${language}/${type}: literal group name`);
   assert.ok(text.includes('사용자 라벨 Normality'),`${language}/${type}: literal variable label`);
   const expected=language==='en'?'Reporting checklist':language==='ko'?'보고 체크리스트':JSON.parse(fs.readFileSync(`i18n/${language}.json`,'utf8')).translations['analysis.ui.reporting_checklist'];
   assert.equal(await root.locator('.structural-reporting-context > h4').innerText(),expected);
   const tables=await root.locator('[data-result-table-role="appendix"]').evaluateAll(nodes=>nodes.map(n=>n.getAttribute('data-result-table-language')));
   assert.ok(tables.length>=3&&tables.every(v=>v===language));
   if(language==='ko')assert.equal(text,korean[type],type+': complete Korean return');
   console.log('PASS:',language,type,'reactive result language, invariant English main, literal labels and group name');
  }
 }
 assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
