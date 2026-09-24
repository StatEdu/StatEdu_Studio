const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
const path=require('node:path');
const fs=require('node:fs');
const {pathToFileURL}=require('node:url');
(async()=>{
 const browser=await chromium.launch({channel:'chrome',headless:true});
 const all=[];
 try {
  const page=await browser.newPage({viewport:{width:1250,height:1100}});
  for(const name of ['cross','paired','rm3','rm3binary','ancova','mixed','nonparametric','nonparametric-paired']) {
   await page.goto(pathToFileURL(path.resolve('tmp/compact-publication',name+'.html')).href);
   await page.evaluate(()=>document.fonts.ready);
   const rows=await page.locator('table[data-result-table-role="main"]').evaluateAll(tables=>tables.map(t=>({
    title:t.closest('.result-section')?.querySelector('h3')?.textContent,
    orientation:t.dataset.resultTableOrientation,width:parseFloat(getComputedStyle(t).width),
    overflow:Math.max(t.scrollWidth-t.clientWidth,t.getBoundingClientRect().right-t.parentElement.getBoundingClientRect().right),
    clipped:[...t.querySelectorAll('th,td')].filter(c=>c.scrollWidth>c.clientWidth+2).map(c=>c.textContent),
    offCenter:[...t.querySelectorAll('thead th')].filter(c=>getComputedStyle(c).textAlign!=='center').map(c=>c.textContent)
   })));
   all.push({name,tables:rows});
   await page.locator('.regression-results').last().screenshot({path:path.resolve('tmp/compact-publication',name+'.png')});
  }
 }finally{await browser.close();}
 fs.writeFileSync('tmp/compact-publication/browser-metrics.json',JSON.stringify(all,null,2));
 console.log(JSON.stringify(all,null,2));
 if(all.some(a=>a.tables.some(t=>t.clipped.length||t.offCenter.length||t.overflow>2)))process.exitCode=1;
})().catch(e=>{console.error(e);process.exitCode=1;});
