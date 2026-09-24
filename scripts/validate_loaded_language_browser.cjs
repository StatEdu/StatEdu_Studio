const assert = require('node:assert/strict');
const {chromium} = require(process.env.STATEDU_PLAYWRIGHT_MODULE || 'playwright');
(async () => {
  const browser = await chromium.launch({channel:'chrome', headless:true});
  try {
    for (const mode of ['native', 'embedded']) {
      const page = await browser.newPage();
      const errors = [];
      page.on('pageerror', e => errors.push(e.message));
      page.on('console', m => { if (m.type()==='error' && /WebSocket/.test(m.text())) errors.push(m.text()); });
      await page.addInitScript(() => {
        window.__sessionSentinel = Math.random();
        window.stateduDesktopFiles = {setDataDirectory: d => {window.__dataDirectory=d;}};
      });
      await page.goto(process.env.STATEDU_UI_TEST_URL || 'http://127.0.0.1:43989/?lang=en');
      await page.waitForFunction(() => document.querySelector('#file.shiny-bound-input') && !document.documentElement.classList.contains('shiny-busy'));
      const sentinel = await page.evaluate(() => window.__sessionSentinel);
      await page.evaluate(mode => Shiny.setInputValue('fixture_load',mode,{priority:'event'}),mode);
      await page.locator('#apply_all_variable_selection').click();
      await page.waitForFunction(() => window.__dataDirectory?.endsWith('자료 폴더'));
      const directory = await page.evaluate(() => window.__dataDirectory);
      async function visit(menu) {
        await page.locator(`.navbar-nav a[data-value="${menu}"]`).first().evaluate(e=>e.click());
      }
      const canvases = [
        ['analysis_custom_model_canvas','custom-model-canvas-root'],
        ...['cfa','cbsem','plssem'].map(t=>['analysis_structural_'+t,'structural_'+t+'-canvas-root'])
      ];
      const snapshots = {};
      for (const [menu,id] of canvases) {
        await visit(menu);
        await page.waitForFunction(id=>!!document.getElementById(id)?.__stateduModelCanvas,id);
        snapshots[id] = await page.evaluate(id=>{
          const instance=document.getElementById(id).__stateduModelCanvas;
          const api=window.StatEduModelCanvas;
          const snapshot=api.state.snapshot(instance.state);
          snapshot.nodes=[{id:'user-node',name:'설명변수',label:'사용자 메모',role:id==='custom-model-canvas-root'?'independent':'latent',x:160,y:140,width:120,height:45}];
          api.state.restore(instance.state,snapshot);api.canvas.render(instance);api.bridge.sendState(instance);
          return api.state.snapshot(instance.state).nodes;
        },id);
      }
      for (const language of ['ko','ja','zh','es','fr','de','vi','en','ko']) {
        await visit('about_preferences');
        await page.locator('#app_language').selectOption(language);
        await page.waitForFunction(language=>document.documentElement.lang===language,language);
        await page.waitForTimeout(400);
        await page.locator('#apply_general_preferences').click();
        await page.waitForFunction(()=>document.querySelector('.shiny-notification'));
        await page.waitForTimeout(400);
        assert.equal(await page.evaluate(()=>window.__sessionSentinel),sentinel);
        assert.equal(await page.evaluate(()=>Shiny.shinyapp.$socket.readyState),1);
        for (const [menu,id] of canvases) {
          await visit(menu);
          await page.waitForFunction(({id,language})=>document.getElementById(id)?.__stateduModelCanvas?.language===language,{id,language});
          assert.deepEqual(await page.evaluate(id=>StatEduModelCanvas.state.snapshot(document.getElementById(id).__stateduModelCanvas.state).nodes,id),snapshots[id]);
        }
        await visit('Data');
        await page.waitForFunction(()=>document.getElementById('go_step1') && document.querySelector('#data_steps')?.textContent.includes('40'));
        assert.match(await page.locator('#data_steps').textContent(),mode==='native'?/자료.csv/:/embedded.csv/);
        if(language==='ko' || language==='en') {
          const label=language==='ko'?'Step 1. 데이터 파일 열기':'Step 1. Load data file';
          await page.waitForFunction(label=>document.getElementById('go_step1')?.textContent.trim()===label,label);
        }
        assert.equal(await page.evaluate(()=>window.__dataDirectory),directory);
        console.log('PASS',mode,language,'save preferences, connected session, 40 rows, four canvas models and source directory');
      }
      assert.deepEqual(errors,[]);
      await page.close();
    }
  } finally { await browser.close(); }
})().catch(e=>{console.error(e);process.exitCode=1;});
