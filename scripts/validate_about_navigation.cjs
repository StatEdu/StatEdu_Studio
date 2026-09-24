const assert=require('node:assert/strict'),fs=require('node:fs');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{
 const browser=await chromium.launch({channel:'chrome',headless:true});
 try {
  const page=await browser.newPage({viewport:{width:1500,height:1000}}),errors=[];
  page.on('pageerror',e=>errors.push(e.message));
  await page.goto('http://127.0.0.1:43989/?lang=ko');
  await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1&&!document.documentElement.classList.contains('shiny-busy'));
  async function visit(value){await page.locator(`.navbar-nav a[data-value="${value}"]`).first().evaluate(e=>e.click());}
  for(const language of ['ko','en','ja','zh','es','fr','de','vi']) {
   await visit('about_preferences');await page.locator('#app_language').selectOption(language);await page.waitForTimeout(400);
   for(const key of ['overview','user_guide','analysis_methods','method_notes','validation','version_history']) {
    await visit('about_'+key);
    const doc=page.locator('.about-markdown-document:visible');
    await doc.waitFor();await page.waitForTimeout(300);
    const results=await doc.evaluate(doc=>[...doc.querySelectorAll('a[href^="#"]')].map(link=>{
     const id=decodeURIComponent(link.getAttribute('href').slice(1));
     let target=[...doc.querySelectorAll('[id],a[name]')].find(e=>e.id===id||e.getAttribute('name')===id);
     if(!target)return {id,error:'missing target'};
     if(target.matches('a')&&!target.textContent.trim()) {
      const next=target.nextElementSibling||(target.parentElement.matches('p')&&target.parentElement.nextElementSibling);
      if(next&&next.matches('h1,h2,h3,h4,h5,h6'))target=next;
     }
     window.scrollTo(0,0);link.click();
     const rect=target.getBoundingClientRect(),nav=document.querySelector('.navbar').getBoundingClientRect();
     return {id,top:rect.top,bottom:rect.bottom,nav:nav.bottom,atEnd:window.scrollY+innerHeight>=document.documentElement.scrollHeight-2};
    }));
    for(const result of results) {
     assert.ok(!result.error,`${language}/${key}/${result.id}: ${result.error}`);
     assert.ok(result.top>=result.nav+10&&result.top<1000,`${language}/${key}: ${JSON.stringify(result)}`);
     if(!result.atEnd)assert.ok(Math.abs(result.top-result.nav-12)<2,`${language}/${key} heading not aligned: ${JSON.stringify(result)}`);
    }
    if(key==='analysis_methods')assert.ok(results.some(r=>r.id==='ipa'));
    if(key==='method_notes')assert.ok(results.some(r=>r.id==='method-28'));
    console.log('PASS',language,key,results.length,'local links; headings below navigation');
    if(language==='ko'&&key==='method_notes') {
     await doc.locator('a[href="#method-3"]').evaluate(e=>e.click());
     fs.mkdirSync('tmp/about-navigation',{recursive:true});
     await page.screenshot({path:'tmp/about-navigation/ko-crosstab.png'});
    }
   }
  }
  assert.deepEqual(errors,[]);
 } finally {await browser.close();}
})().catch(e=>{console.error(e);process.exitCode=1;});
