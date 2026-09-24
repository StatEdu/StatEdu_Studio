const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const {pathToFileURL} = require('node:url');
const {chromium} = require(process.env.STATEDU_PLAYWRIGHT_MODULE || 'playwright');

(async () => {
  const expected = fs.readFileSync('CHANGELOG.md', 'utf8').match(/^## v[^\r\n]+/gm).map(x => x.slice(3));
  const browser = await chromium.launch({channel: 'chrome', headless: true});
  try {
    const page = await browser.newPage({viewport: {width: 1280, height: 1000}});
    for (const language of ['ko', 'en', 'ja', 'zh', 'es', 'fr', 'de', 'vi']) {
      await page.goto(pathToFileURL(path.resolve(`tmp/changelog-history/${language}.html`)).href);
      assert.deepEqual(await page.locator('h2').allTextContents(), expected, language);
      const firstRelease = page.locator('h2').last();
      await firstRelease.scrollIntoViewIfNeeded();
      await page.screenshot({path: `tmp/changelog-history/${language}-first-release.png`});
      assert.equal(await page.evaluate(() => document.documentElement.scrollWidth > innerWidth), false, language);
      console.log(`PASS: ${language}: all ${expected.length} releases rendered; first release reachable without horizontal overflow`);
    }
  } finally {
    await browser.close();
  }
})().catch(error => {console.error(error); process.exit(1);});
