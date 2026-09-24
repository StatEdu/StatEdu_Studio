const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const {pathToFileURL} = require('node:url');
const {chromium} = require(process.env.STATEDU_PLAYWRIGHT_MODULE || 'playwright');
(async () => {
  const browser = await chromium.launch({channel:'chrome', headless:true});
  try {
    const page = await browser.newPage({viewport:{width:1100,height:950}});
    await page.goto(pathToFileURL(path.resolve('tmp/result-management/index.html')).href);
    for (const css of [fs.readFileSync('tmp/canvas-export-validation/bootstrap-path.txt','utf8').trim(), 'www/style.css']) await page.addStyleTag({path:css});
    await page.evaluate(() => { window.Shiny = {setInputValue:(id,payload)=>{window.lastAction={id,payload};}}; });
    assert.equal(await page.locator('.saved-result-entry').count(), 3);
    assert.ok(await page.locator('.saved-result-entry-up').first().isDisabled());
    assert.ok(await page.locator('.saved-result-entry-down').last().isDisabled());
    await page.locator('.saved-result-entry-down').nth(1).click();
    assert.deepEqual(await page.evaluate(()=>window.lastAction), {id:'saved_result_entry_action',payload:{id:'item2',action:'down'}});
    await page.locator('.saved-result-entry-delete').nth(1).click();
    assert.deepEqual(await page.evaluate(()=>window.lastAction.payload), {id:'item2',action:'delete'});
    await page.screenshot({path:'tmp/result-management/controls.png'});
    console.log('PASS: visible entry controls, disabled boundaries, stable-ID move/delete events');
  } finally { await browser.close(); }
})().catch(error=>{console.error(error);process.exitCode=1;});
