const assert = require('node:assert/strict');
const {pathToFileURL} = require('node:url');
const path = require('node:path');
const {chromium} = require(process.env.STATEDU_PLAYWRIGHT_MODULE || 'playwright');
(async () => {
  const browser = await chromium.launch({channel: 'chrome', headless: true});
  try {
    const page = await browser.newPage();
    await page.goto(pathToFileURL(path.resolve('tmp/variable-pagination/table.html')).href);
    await page.waitForSelector('input.variable-select');
    await page.waitForTimeout(350);
    const current = () => page.evaluate(() => $('table.dataTable').DataTable().page.info().page);
    const clickPage = n => page.locator('.paginate_button').filter({hasText: new RegExp(`^${n}$`)}).first().click();
    await clickPage(3);
    await page.locator('input.variable-select').nth(0).check();
    await page.locator('input.variable-select').nth(1).check();
    await clickPage(4);
    await page.waitForTimeout(400);
    assert.equal(await current(), 3, 'one click must reach page 4 after selection');
    await clickPage(3);
    assert.equal(await page.locator('input.variable-select:checked').count(), 2);
    await page.locator('input.variable-select').nth(2).check();
    await clickPage(1);
    await page.waitForTimeout(400);
    assert.equal(await current(), 0, 'pending restoration must support page 1');
    assert.equal(await page.evaluate(() => window.easyflowVariableTableRestorePending), false);
    await clickPage(3);
    assert.equal(await page.locator('input.variable-select:checked').count(), 3);
    // A redraw without navigation still restores the saved page.
    await page.evaluate(() => {
      window.easyflowVariableTablePage = 2;
      window.easyflowVariableTableRestorePending = true;
      $('table.dataTable').DataTable().draw();
    });
    await page.waitForTimeout(400);
    assert.equal(await current(), 2);
    console.log('PASS: single-click pagination, first-page navigation, selection retention, redraw restoration');
  } finally { await browser.close(); }
})().catch(e => { console.error(e); process.exit(1); });
