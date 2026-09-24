const assert = require('node:assert/strict');
const path = require('node:path');
const {pathToFileURL} = require('node:url');
const {chromium} = require(process.env.STATEDU_PLAYWRIGHT_MODULE || 'playwright');
(async()=>{
 const browser=await chromium.launch({channel:'chrome',headless:true});
 try {
  const page=await browser.newPage({viewport:{width:1000,height:1000}});
  await page.goto(pathToFileURL(path.resolve('tmp/regression-variable-width/accumulated.html')).href);
  await page.evaluate(()=>document.fonts.ready);
  const rows=await page.locator('table').evaluateAll(tables=>tables.map(t=>{
    const widths=[...t.querySelector('thead tr').children].map(c=>c.getBoundingClientRect().width);
    const cells=[...t.querySelectorAll('th,td')];
    return {widths,ratio:widths[0]/widths.reduce((a,b)=>a+b,0),overflow:Math.max(...cells.map(c=>c.scrollWidth-c.clientWidth))};
  }));
  assert.equal(rows.length,3);
  assert(Math.abs(rows[0].ratio-.42)<.005,JSON.stringify(rows));
  for(const row of rows){assert(row.ratio>=.279,JSON.stringify(row));assert(row.overflow<=2,JSON.stringify(row));}
  await page.locator('table').first().screenshot({path:'tmp/regression-variable-width/preview.png'});
  console.log('PASS: rendered Variable widths and no cell overflow',JSON.stringify(rows));
 }finally{await browser.close();}
})().catch(e=>{console.error(e);process.exit(1);});
