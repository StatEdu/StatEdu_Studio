const {chromium}=require('playwright'),assert=require('node:assert/strict'),path=require('node:path');const {pathToFileURL}=require('node:url');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
 const page=await browser.newPage({viewport:{width:1250,height:1100}});await page.goto(pathToFileURL(path.resolve('tmp/bootstrap-ci/screen.html')).href);
 const checks=await page.locator('table').evaluateAll(tables=>tables.filter(t=>[...t.querySelectorAll('th')].some(x=>x.textContent.trim()==='LLCI')).map(t=>{
  const rows=[...t.tHead.rows];const grid=[];rows.forEach((r,ri)=>{grid[ri]??=[];let ci=0;[...r.cells].forEach(c=>{while(grid[ri][ci])ci++;for(let rr=ri;rr<ri+c.rowSpan;rr++){grid[rr]??=[];for(let cc=ci;cc<ci+c.colSpan;cc++)grid[rr][cc]=c;}ci+=c.colSpan;});});
  const leaves=[...t.querySelectorAll('th')].filter(c=>c.textContent.trim()==='LLCI');
  return {rows:rows.length,widths:grid.map(r=>r.length),grouped:leaves.every(c=>c.parentElement.rowIndex>0),overflow:Math.max(...[...t.querySelectorAll('th')].map(c=>c.scrollWidth-c.clientWidth))};
 }));
 for(const c of checks){assert(c.grouped,JSON.stringify(c));assert(c.widths.every(w=>w===c.widths[0]),JSON.stringify(c));assert(c.overflow<=2,JSON.stringify(c));}
 await page.locator('.result-table-with-note').first().screenshot({path:'tmp/bootstrap-ci/preview.png'});
 const wide=page.locator('table.hierarchical-coefficient-table').first();if(await wide.count())await wide.locator('..').screenshot({path:'tmp/bootstrap-ci/wide.png'});
 console.log('PASS',checks.length,'CI tables: grouped headers, aligned spans, no clipped header text');
}finally{await browser.close()}})().catch(e=>{console.error(e);process.exit(1)});
