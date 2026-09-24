const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
const assert=require('node:assert/strict');
const labels=require('../tmp/meta-draft/labels.json');
(async()=>{
 const browser=await chromium.launch({channel:'chrome',headless:true});
 try{
  const page=await browser.newPage();await page.goto('http://127.0.0.1:8876');
  await page.locator('#meta_reset_effects').click();
  for(const lang of ['en','ja','zh','es','fr','de','vi','ko']){
   const t=require('../i18n/'+lang+'.json').translations;
   await page.evaluate(lang=>Shiny.setInputValue('test_language',lang,{priority:'event'}),lang);
   await page.waitForFunction(expected=>document.querySelector('#meta_reset_message')?.textContent===expected,t['meta.reset.confirm']);
   assert.equal(await page.locator('#meta_reset_title').textContent(),t['meta.reset.title']);
   assert.equal(await page.locator('#meta_reset_cancel').textContent(),labels[lang].cancel);
   await page.waitForFunction(expected=>document.querySelector('#meta_confirm_reset')?.textContent===expected,labels[lang].reset);
   console.log('PASS reset browser:',lang);
  }
  await page.locator('.modal-footer button[data-dismiss="modal"]').click();
  await page.locator('#meta_reset_effects').click();
  await page.locator('#meta_confirm_reset').click();
  await page.locator('#meta_reset_title').waitFor({state:'hidden'});
  console.log('PASS reset: cancel, reopen and explicit confirmation');
 }finally{await browser.close();}
})().catch(e=>{console.error(e);process.exitCode=1});
