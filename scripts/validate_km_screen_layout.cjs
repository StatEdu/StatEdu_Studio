const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE);
const path=require('path'),fs=require('fs'),assert=require('assert');
(async()=>{
 const browser=await chromium.launch({channel:'chrome',headless:true});
 try {
  const page=await browser.newPage({viewport:{width:1500,height:1050},deviceScaleFactor:1.5});
  const errors=[];page.on('pageerror',e=>errors.push(e.message));
  await page.goto('http://127.0.0.1:3879');
  await page.waitForFunction(()=>window.Shiny&&Shiny.shinyapp&&Shiny.shinyapp.$socket.readyState===1);
  await page.locator('#file').setInputFiles(path.resolve('scripts/fixtures/survival_validation.csv'));
  await page.locator('#apply_all_variable_selection').click();
  await page.locator('a[data-value="analysis_survival_km"]').evaluate(e=>e.click());
  for(const [variable,role] of [['time','time'],['status','event'],['sex','group']]) {
   await page.locator(`[data-input-id="survival_km_available"] [data-value="${variable}"]`).click();
   await page.locator(`#survival_km_${role}_move`).click();
   await page.locator(`[data-input-id="survival_km_${role}"] [data-value="${variable}"]`).waitFor();
  }
  await page.locator('#run_survival_km').click();
  const plot=page.locator('#survival_km_results .shiny-plot-output').first();
  const observations=[];
  for(const width of [1500,1100]) {
   await page.setViewportSize({width,height:1050});
   await plot.scrollIntoViewIfNeeded();
   await plot.locator('img').waitFor({state:'attached',timeout:30000});
   await page.waitForFunction(()=>{
    const e=document.querySelector('#survival_km_results .shiny-plot-output'),i=e?.querySelector('img');
    return i&&i.complete&&i.naturalWidth>0&&!e.classList.contains('recalculating');
   });
   // Let Shiny's debounced device resize finish before capturing the final bitmap.
   await page.waitForTimeout(800);
   await plot.screenshot({path:`output/km-screen-layout-20260915/gui-${width}.png`});
   observations.push({width,...await plot.locator('img').evaluate(i=>({naturalWidth:i.naturalWidth,naturalHeight:i.naturalHeight}))});
  }
  assert.deepStrictEqual(errors,[]);
  fs.writeFileSync('output/km-screen-layout-20260915/gui.json',JSON.stringify({observations,errors},null,2));
  console.log('PASS: live KM screen at two viewport widths, DPR 1.5, no page errors');
 } finally {await browser.close();}
})().catch(e=>{console.error(e);process.exitCode=1});
