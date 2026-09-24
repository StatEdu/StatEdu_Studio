const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const {pathToFileURL} = require('node:url');
const {chromium} = require(process.env.STATEDU_PLAYWRIGHT_MODULE || 'playwright');
(async () => {
 const browser = await chromium.launch({channel:'chrome',headless:true});
 try {
  const page = await browser.newPage({viewport:{width:1488,height:1000}});
  for (const language of ['en','ja','zh','es','fr','de','vi']) {
  await page.goto(pathToFileURL(path.resolve('tmp/multilingual/ko.html')).href);
  // Reproduce a Japanese session with an old, restored Korean preferences control.
  await page.evaluate(language => {
    window.easyflowAppLanguage = language;
    const select = document.createElement('select'); select.id='app_language';
    select.innerHTML='<option value="ko" selected>Korean</option><option value="ja">Japanese</option>';
    document.body.append(select);
  }, language);
  await page.addScriptTag({path:path.resolve('www/easyflow.js')});
  await page.waitForTimeout(350);
  await page.evaluate(() => window.groupAnalysisDropdownItems?.());
  assert.equal(await page.evaluate(()=>window.easyflowAppLanguage),language);
  const heading = await page.locator('.step3-labels-section h4').textContent();
  const expected = JSON.parse(fs.readFileSync(`i18n/${language}.json`,'utf8')).translations['data.categorical_value_labels'];
  assert.equal(heading,expected || 'Categorical value labels');
  const top = await page.locator('.navbar-nav > li > a').allTextContents();
  assert.ok(top.every(text=>!/[가-힣]/.test(text)),JSON.stringify(top));
  if (language === 'ja') assert.ok(top.some(text=>text.includes('データ')));
  }
  await page.goto(pathToFileURL(path.resolve('tmp/multilingual/mm-ja.html')).href);
  for(const css of [fs.readFileSync('tmp/multilingual/bootstrap-path.txt','utf8').trim(),'www/style.css','www/model-canvas/canvas.css']) await page.addStyleTag({path:css});
  for(const script of ['state','layout','shiny-bridge','edges','nodes','dialogs','toolbar','canvas']) await page.addScriptTag({path:`www/model-canvas/${script}.js`});
  await page.evaluate(()=>document.querySelectorAll('.custom-model-edge-anchor-tools, .custom-model-edge-shape-tools').forEach(el=>el.classList.add('is-visible')));
  const metrics = await page.locator('.custom-model-edge-anchor-tools.is-visible').first().evaluate(el=>{
    const rect=x=>{const r=x.getBoundingClientRect();return {x:r.x,y:r.y,right:r.right,bottom:r.bottom};};
    return {container:rect(el),parent:rect(el.closest('.custom-model-toolbar')),children:[...el.children].map(rect)};
  });
  for(const child of metrics.children) assert.ok(child.bottom<=metrics.parent.bottom+1 && child.right<=metrics.parent.right+1,JSON.stringify(metrics));
  for(let i=0;i<metrics.children.length;i++) for(let j=i+1;j<metrics.children.length;j++) {
    const a=metrics.children[i],b=metrics.children[j];
    assert.ok(a.right<=b.x || b.right<=a.x || a.bottom<=b.y || b.bottom<=a.y,JSON.stringify(metrics));
  }
  assert.match(await page.locator('.custom-model-canvas-root').textContent(), /キャンバス上の変数を選択/);
  await page.evaluate(()=>{
    const instance=document.querySelector('.custom-model-canvas-root').__stateduModelCanvas;
    instance.state.edges=[{id:'test-edge',from:'x',to:'y',kind:'regression'}];
    window.StatEduModelCanvas.nodes.showEdgeProperties(instance,'test-edge');
  });
  assert.equal(await page.locator('.custom-model-property-title').textContent(),'パスの属性');
  await page.screenshot({path:'tmp/multilingual/mm-ja-verified.png'});
  console.log('PASS: all seven non-Korean locales retain navbar and categorical heading; Japanese property labels; anchor controls fit without overlap.');
 } finally {await browser.close();}
})().catch(e=>{console.error(e);process.exitCode=1;});
