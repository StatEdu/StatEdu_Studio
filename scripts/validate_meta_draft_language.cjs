const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
const assert=require('node:assert/strict');
const modalLabels=require('../tmp/meta-draft/labels.json');
(async()=>{
 const browser=await chromium.launch({channel:'chrome',headless:true});
 try{
  const page=await browser.newPage();
  await page.goto('http://127.0.0.1:8876');
  await page.locator('#meta_add_effect').click();
  await page.locator('#meta_field_m1').fill('17.125');
  await page.locator('#meta_field_sd1').fill('2.5');
  await page.locator('#meta_field_n1').fill('100');
  await page.locator('#meta_field_study_id').fill('User <&> 한글');
  await page.locator('#meta_field_included').uncheck();
  await page.evaluate(()=>document.querySelector('#meta_field_direction').selectize.setValue('negative'));
  await page.locator('#meta_field_m0').fill('');
  await page.locator('#meta_field_n1').blur();
  for(const lang of ['en','ja','zh','es','fr','de','vi','ko']){
   const labels=require('../i18n/'+lang+'.json').translations;
   await page.evaluate(lang=>Shiny.setInputValue('test_language',lang,{priority:'event'}),lang);
   const expected=lang==='ko'?'집단 1(실험집단) 평균 (M1)':lang==='en'?'Group 1 (treatment) mean (M1)':labels['analysis.ui.group_1_treatment_mean_m1'];
   await page.waitForFunction(expected=>document.querySelector('label[for="meta_field_m1"]')?.textContent===expected,expected);
   const m=modalLabels[lang];
   await page.waitForFunction(m=>document.querySelector('#meta_effect_modal_title')?.textContent===m.add_title && document.querySelector('#meta_save_effect')?.textContent===m.save,m);
   assert.equal(await page.locator('#meta_effect_modal_cancel').textContent(),m.cancel);
   assert.equal(await page.locator('#meta_effect_modal_help').textContent(),m.moderator_help);
   for(const field of ['study_id','study_name','publication_year','outcome','predictor','moderator_categorical','moderator_continuous']){
    assert.equal(await page.locator(`label[for="meta_field_${field}"]`).textContent(),m[field]);
   }
   assert.equal(await page.locator('#meta_input_type-label').textContent(),m.format);
   assert.equal(await page.locator('#meta_field_direction-label').textContent(),m.direction);
   assert.equal(await page.locator('#meta_field_direction').inputValue(),'negative');
   assert.equal(await page.locator('#meta_input_type').inputValue(),'means');
   assert.equal(await page.locator('#meta_field_included').isChecked(),false);
   assert.equal(await page.locator('#meta_field_moderator_categorical').getAttribute('placeholder'),labels['meta.dialog.categorical_example']);
   assert.equal(await page.locator('#meta_field_moderator_continuous').getAttribute('placeholder'),labels['meta.dialog.continuous_example']);
   assert.equal(await page.locator('#meta_field_m1').inputValue(),'17.125');
   assert.equal(await page.locator('#meta_field_sd1').inputValue(),'2.5');
   assert.equal(await page.locator('#meta_field_n1').inputValue(),'100');
   assert.equal(await page.locator('#meta_field_m0').inputValue(),'');
   assert.equal(await page.locator('#meta_field_study_id').inputValue(),'User <&> 한글');
   console.log('PASS browser draft:',lang);
  }
  await page.locator('.modal-footer button[data-dismiss="modal"]').click();
  await page.locator('#meta_add_effect').click();
  await page.waitForFunction(()=>document.querySelector('#meta_field_m1')?.value==='');
  assert.equal(await page.locator('#meta_field_study_id').inputValue(),'');
  console.log('PASS browser: cancel and reopen starts blank');
 }finally{await browser.close();}
})().catch(e=>{console.error(e);process.exitCode=1});
