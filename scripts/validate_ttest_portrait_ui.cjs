const assert = require('node:assert/strict');
const path = require('node:path');
const {pathToFileURL} = require('node:url');
const {chromium} = require(process.env.STATEDU_PLAYWRIGHT_MODULE || 'playwright');
(async () => {
  const browser = await chromium.launch({channel:'chrome',headless:true});
  try {
    const page = await browser.newPage({viewport:{width:1200,height:1000}});
    for (const name of ['standard','with-df']) {
      await page.goto(pathToFileURL(path.resolve('tmp/portrait-accumulated-exports',name+'.html')).href);
      await page.evaluate(() => document.fonts.ready);
      const metrics = await page.locator('.ttest-anova-result-panel table').evaluateAll(tables => tables.map(table => ({
        orientation:table.dataset.resultTableOrientation,
        width:table.getBoundingClientRect().width,
        cssWidth:getComputedStyle(table).width, layout:getComputedStyle(table).tableLayout,
        parents:[table,table.parentElement,table.parentElement.parentElement].map(el=>({class:el.className,width:getComputedStyle(el).width,zoom:getComputedStyle(el).zoom,transform:getComputedStyle(el).transform})),
        overflow:table.scrollWidth-table.clientWidth,
        clipped:[...table.querySelectorAll('th,td')].filter(cell=>cell.scrollWidth>cell.clientWidth+2).map(cell=>cell.textContent)
      })));
      for(const m of metrics) {
        // The viewer intentionally zooms paper to 150%; compare its unzoomed layout.
        assert.equal(m.orientation,'portrait'); assert.ok(parseFloat(m.cssWidth)<=590,JSON.stringify(m));
        assert.ok(m.overflow<=2,JSON.stringify(m)); assert.deepEqual(m.clipped,[]);
      }
      await page.locator('.ttest-anova-result-panel').first().screenshot({path:path.resolve('tmp/portrait-accumulated-exports',name+'.png')});
      console.log(name+': PASS portrait width and no clipped cells');
    }
  } finally {await browser.close();}
})().catch(error=>{console.error(error);process.exitCode=1;});
