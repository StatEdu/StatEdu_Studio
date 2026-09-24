const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const {pathToFileURL} = require('node:url');
const {chromium} = require(process.env.STATEDU_PLAYWRIGHT_MODULE || 'playwright');
(async () => {
 const browser = await chromium.launch({channel:'chrome', headless:true});
 try {
  const page = await browser.newPage({viewport:{width:1500,height:1100}});
  await page.goto(pathToFileURL(path.resolve('tmp/all-analysis-notes/exports/accumulated.html')).href);
  await page.evaluate(() => document.fonts.ready);
  const notes = await page.locator('[class*="note"]').evaluateAll(nodes => nodes
    .filter(n => ['P','DIV'].includes(n.tagName) && !n.querySelector('table') && n.textContent.trim() && !n.querySelector('p[class*="note"],div[class*="note"]'))
    .map(n => {
      const table = n.closest('[data-result-table-sheet="true"]')?.querySelector('table');
      const rect = n.getBoundingClientRect();
      return {text:n.textContent.trim().slice(0,80),overflow:n.scrollWidth-n.clientWidth,
        width:rect.width,tableWidth:table?.getBoundingClientRect().width};
    }));
  fs.writeFileSync('tmp/all-analysis-notes/exports/layout.json', JSON.stringify(notes,null,2));
  assert(notes.length >= 28);
  for (const note of notes) {
    assert(note.overflow <= 2, JSON.stringify(note));
    if (note.tableWidth) assert(Math.abs(note.width-note.tableWidth) <= 3, JSON.stringify(note));
  }
  await page.screenshot({path:'tmp/all-analysis-notes/exports/notes-screen.png'});
  console.log(`PASS: ${notes.length} notes wrap at their table width`);
 } finally { await browser.close(); }
})().catch(e => {console.error(e);process.exit(1);});
