const assert=require('node:assert/strict');
const path=require('node:path');
const {pathToFileURL}=require('node:url');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{
 const browser=await chromium.launch({channel:'chrome',headless:true});
 try{
  const page=await browser.newPage({viewport:{width:1300,height:1200}});
  await page.goto(pathToFileURL(path.resolve('tmp/regression-publication-style/screen.html')).href);
  await page.evaluate(()=>document.fonts.ready);
  const styles=await page.locator('table').evaluateAll(tables=>tables.slice(0,2).map(t=>{
   const cells=[t.querySelector('th'),t.querySelector('td')];
   return cells.map(c=>{const s=getComputedStyle(c);return [s.color,s.fontFamily,s.fontSize,s.fontWeight,s.padding,s.lineHeight,s.borderBottom];});
  }));
  assert.deepEqual(styles[0],styles[1]);
  const bounds=await page.locator('.conditional-effects-table tbody tr').evaluateAll(rows=>rows.map(row=>[...row.cells].slice(0,2).map(cell=>{
   const range=document.createRange();range.selectNodeContents(cell);
   const text=range.getBoundingClientRect(),box=cell.getBoundingClientRect();
   return {lines:text.height/parseFloat(getComputedStyle(cell).lineHeight),inside:text.left>=box.left-1&&text.right<=box.right+1,overflow:cell.scrollWidth-cell.clientWidth};
  })));
  for(const row of bounds)for(const cell of row){assert(cell.inside&&cell.overflow<=1,JSON.stringify(bounds));assert(cell.lines>1.5,JSON.stringify(bounds));}
  await page.locator('.mm-conditional-effects-section').screenshot({path:'tmp/regression-publication-style/conditional.png'});
  await page.goto(pathToFileURL(path.resolve('tmp/regression-publication-style/hierarchical.html')).href);
  const actualStyles=await page.locator('table.regression-publication-table').first().evaluate(t=>[t.querySelector('th'),t.querySelector('td')].map(c=>{const s=getComputedStyle(c);return [s.color,s.fontFamily,s.fontSize,s.fontWeight,s.padding,s.lineHeight,s.borderBottom];}));
  assert.deepEqual(actualStyles,styles[1]);
  await page.locator('.hierarchical-standard-model-block').first().screenshot({path:'tmp/regression-publication-style/hierarchical.png'});
  console.log('PASS: identical regression/MM cell styles; all Path/moderator text wraps inside its cells');
 }finally{await browser.close();}
})().catch(e=>{console.error(e);process.exit(1);});
