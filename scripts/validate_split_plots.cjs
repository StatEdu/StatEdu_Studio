const assert=require('node:assert/strict'),fs=require('node:fs');
const {chromium}=require('playwright');
(async()=>{
 const browser=await chromium.launch({channel:'chrome',headless:true});
 try {
  const page=await browser.newPage({viewport:{width:1450,height:1000}});
  await page.goto('http://127.0.0.1:3874');
  await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1);
  await page.locator('[data-input-id="scope_split_available"] [data-value="sex"]').dblclick();
  await page.locator('#scope_split_apply').click();
  await page.waitForFunction(()=>document.getElementById('statedu-analysis-scope-status')?.textContent.includes('총 2개 집단'));
  await page.locator('a[data-value="Regression"]').click();
  await page.locator('#run').click();
  await page.waitForFunction(()=>window.stateduSplitSnapshots?.regression_results,null,{timeout:60000});
  const groups=page.locator('#regression_results .statedu-split-group');
  assert.equal(await groups.count(),2);
  const sources=[];
  for(let i=0;i<2;i++) {
   const images=groups.nth(i).locator('.diagnostic-plots-section img');
   assert.equal(await images.count(),2,await groups.nth(i).innerText());
   sources.push(await images.evaluateAll(imgs=>imgs.map(img=>img.getAttribute('src'))));
   assert(sources[i].every(src=>src.startsWith('data:image/png;base64,')));
  }
  assert.notEqual(sources[0][0],sources[1][0],'Each group must have its own plot');
  fs.mkdirSync('tmp/split-plots',{recursive:true});
  fs.writeFileSync('tmp/split-plots/split.html',await page.evaluate(()=>window.stateduSplitSnapshots.regression_results));
  await groups.first().locator('.diagnostic-plots-section').screenshot({path:'tmp/split-plots/first.png'});
  console.log('PASS: first and second split groups retain distinct Q-Q and residual plots.');
 } finally {await browser.close();}
})().catch(e=>{console.error(e);process.exitCode=1;});
