const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const {pathToFileURL} = require('node:url');
const {chromium} = require(process.env.STATEDU_PLAYWRIGHT_MODULE || 'playwright');
(async () => {
  const browser = await chromium.launch({channel: 'chrome', headless: true});
  try {
    const page = await browser.newPage({viewport: {width: 1488, height: 1050}});
    for (const lang of ['ja', 'zh', 'es', 'fr', 'de', 'vi']) {
      const translations = JSON.parse(fs.readFileSync(`i18n/${lang}.json`, 'utf8')).translations;
      for (const kind of ['meta', 'survival-guide']) {
        await page.goto(pathToFileURL(path.resolve(`tmp/multilingual/${kind}-${lang}.html`)).href);
        await page.addStyleTag({path: fs.readFileSync('tmp/multilingual/bootstrap-path.txt', 'utf8').trim()});
        await page.addStyleTag({path: 'www/style.css'});
        await page.addStyleTag({content: '.tab-pane { display: block !important; }'});
        if (kind === 'survival-guide') await page.evaluate(() => {
          const fragment = document.querySelector('.survival-contract-grid-fragment');
          document.querySelector('#survival_contract_setup').append(fragment);
        });
        const expected = translations[kind === 'meta' ? 'analysis.ui.meta_analysis' : 'analysis.ui.survival_analysis_guide'];
        assert.equal(await page.locator('h1').textContent(), expected);
        const labels = await page.locator('label, h1, h3, h4, button').allTextContents();
        assert.ok(labels.every(text => !/[가-힣]/.test(text)), `${kind}/${lang}: Korean UI label`);
        if (kind === 'meta') {
          assert.equal(await page.locator('#meta_import_file').getAttribute('type'), 'file');
          assert.equal(await page.locator('input[placeholder]').first().getAttribute('placeholder'), translations['analysis.ui.no_file_selected']);
          assert.equal(await page.locator('#meta_target_family').inputValue(), 'g');
          for (const button of await page.locator('.meta-download-actions .btn').all()) {
            assert.ok(await button.evaluate(el => el.scrollWidth <= el.clientWidth + 1), `${lang}: download text overflow`);
          }
        } else {
          assert.equal(await page.locator('#survival_design_shape').inputValue(), 'single_record');
          assert.equal(await page.locator('#survival_contract_time option[value="사용자 변수"]').textContent(), '사용자 변수');
        }
        if (lang === 'ja') await page.screenshot({path: `tmp/multilingual/${kind}-ja-verified.png`, fullPage: true});
      }
      console.log(`PASS: ${lang} rendered meta/survival labels and controls`);
    }
  } finally { await browser.close(); }
})().catch(error => { console.error(error); process.exitCode = 1; });
