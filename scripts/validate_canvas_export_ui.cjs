// Run Rscript scripts/validate_canvas_export_ui.R first, then this script.
// Set STATEDU_PLAYWRIGHT_MODULE when Playwright is not on the Node module path.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const {pathToFileURL} = require('node:url');
const {chromium} = require(process.env.STATEDU_PLAYWRIGHT_MODULE || 'playwright');
const fixture = path.resolve('tmp/canvas-export-validation');

(async () => {
  const browser = await chromium.launch({channel: 'chrome', headless: true});
  try {
    const page = await browser.newPage({viewport: {width: 1500, height: 900}});
    for (const menu of ['mm', 'cfa', 'sem', 'pls']) {
      await page.goto(pathToFileURL(path.join(fixture, menu + '.html')).href);
      for (const css of [fs.readFileSync(path.join(fixture, 'bootstrap-path.txt'), 'utf8').trim(), 'www/style.css', 'www/model-canvas/canvas.css']) {
        await page.addStyleTag({path: css});
      }
      for (const file of ['state', 'layout', 'shiny-bridge', 'edges', 'nodes', 'dialogs', 'toolbar', 'canvas']) {
        await page.addScriptTag({path: 'www/model-canvas/' + file + '.js'});
      }
      assert.equal(await page.locator('.structural-selection-settings').count(),1,menu+': restored sidebar settings');
      assert.equal(await page.locator('.custom-model-placement-help').count(),0,menu+': placement help stays removed');
      const fill=await page.locator('.custom-model-variable-list').evaluate(list=>{
        const panel=list.closest('.custom-model-variable-panel');
        const settings=panel.querySelector('.structural-selection-settings');
        return {height:list.getBoundingClientRect().height,panelHeight:panel.getBoundingClientRect().height,bottomGap:panel.getBoundingClientRect().bottom-settings.getBoundingClientRect().bottom,gap:settings.getBoundingClientRect().top-list.getBoundingClientRect().bottom};
      });
      assert(Math.abs(fill.height-402)<1 && fill.panelHeight>=530 && fill.bottomGap>=0 && fill.bottomGap<20 && fill.gap>=0,menu+': variable list preserved; settings grow with content '+JSON.stringify(fill));
      await page.locator('.custom-model-sidebar-save').evaluate((node, html) => {node.innerHTML = html;}, fs.readFileSync(path.join(fixture, 'saves.html'), 'utf8'));
      await page.evaluate(() => {
        const instance = document.querySelector('.custom-model-canvas-root').__stateduModelCanvas;
        instance.state.nodes = [
          {id: 'x', name: 'Predictor label', role: 'independent', x: 80, y: 180, width: 110, height: 44},
          {id: 'y', name: 'Outcome label', role: 'dependent', x: 380, y: 180, width: 110, height: 44}];
        instance.state.edges = [{id: 'xy', from: 'x', to: 'y', kind: 'path', p: .775, significant: false, dashEligible: true, resultMatched: true}];
        window.StatEduModelCanvas.canvas.render(instance);
      });
      for (const [metadata, dashed] of [
        [{p: .775, significant: false, dashEligible: true}, true],
        [{p: .05, significant: false}, true], // legacy result without eligibility flag
        [{p: .001, significant: true, dashEligible: true}, false],
        [{p: null, significant: false, dashEligible: true}, false],
        [{p: .775, significant: false, dashEligible: false}, false],
        [{p: .775, significant: false, resultMatched: false}, false]
      ]) {
        const dash = await page.evaluate(metadata => {
          const instance = document.querySelector('.custom-model-canvas-root').__stateduModelCanvas;
          instance.state.edges = [{id: 'xy', from: 'x', to: 'y', kind: 'path', ...metadata}];
          window.StatEduModelCanvas.canvas.render(instance);
          return getComputedStyle(instance.paper.querySelector('.custom-model-edge')).strokeDasharray;
        }, metadata);
        assert.equal(dash !== 'none', dashed, menu + ': significance line style');
      }
      await page.evaluate(() => {
        const instance = document.querySelector('.custom-model-canvas-root').__stateduModelCanvas;
        Object.assign(instance.state.edges[0], {label: '.21(.092)', p: .775, significant: false, dashEligible: true, resultMatched: true});
        window.StatEduModelCanvas.canvas.render(instance);
      });
      assert.equal(await page.locator('.custom-model-icon-tool svg').count(), 2);
      const snap = page.locator('.custom-model-toolbar [data-action=autoAlign]');
      await snap.click(); assert.equal(await snap.getAttribute('aria-pressed'), 'false');
      await snap.click(); assert.equal(await snap.getAttribute('aria-pressed'), 'true');
      await page.locator('.custom-model-sidebar-actions [data-action=run]').click();
      const geometry = await page.evaluate(() => {
        const launch = document.querySelector('.custom-model-sidebar-actions [data-action=run]').getBoundingClientRect();
        const confirm = document.querySelector('[data-action=runConfirm]').getBoundingClientRect();
        return {dy: Math.abs(launch.y - confirm.y), sizes: [...document.querySelectorAll('.analysis-save-button')].map(node => {
          const r = node.getBoundingClientRect(); return [Math.abs(r.width - launch.width), Math.abs(r.height - launch.height)];
        })};
      });
      assert.ok(geometry.dy < 1);
      assert.equal(geometry.sizes.length, 5);
      assert.ok(geometry.sizes.every(([w, h]) => w < 1 && h < 1), JSON.stringify({menu, geometry}));
      await page.locator('[data-action=runCancel]').click();
      for (const dpi of [300, 600]) {
        const result = await page.evaluate(async dpi => {
          const instance = document.querySelector('.custom-model-canvas-root').__stateduModelCanvas;
          instance.root.setAttribute('data-export-dpi', dpi);
          const before = JSON.stringify(instance.state), dom = instance.paper.innerHTML;
          let exportedSvg = '';
          const src = Object.getOwnPropertyDescriptor(HTMLImageElement.prototype, 'src');
          Object.defineProperty(HTMLImageElement.prototype, 'src', {...src, set(value) {
            if (value.startsWith('data:image/svg+xml')) exportedSvg = decodeURIComponent(value.split(',').slice(1).join(','));
            src.set.call(this, value);
          }});
          let blob;
          try { blob = await window.StatEduModelCanvas.dialogs.exportPng(instance); }
          finally { Object.defineProperty(HTMLImageElement.prototype, 'src', src); }
          const svgDocument = new DOMParser().parseFromString(exportedSvg, 'image/svg+xml');
          const exportedEdge = svgDocument.querySelector('.custom-model-edge');
          const bytes = new Uint8Array(await blob.arrayBuffer());
          const view = new DataView(bytes.buffer);
          let ppm;
          for (let at = 8; at < bytes.length; at += view.getUint32(at) + 12) {
            if (String.fromCharCode(...bytes.slice(at + 4, at + 8)) === 'pHYs') ppm = view.getUint32(at + 8);
          }
          const bitmap = await createImageBitmap(blob);
          const canvas = document.createElement('canvas');
          canvas.width = canvas.height = 1;
          const ctx = canvas.getContext('2d');
          ctx.drawImage(bitmap, bitmap.width - 8, bitmap.height - 8, 1, 1, 0, 0, 1, 1);
          const labelBackgrounds = [...svgDocument.querySelectorAll('.custom-model-edge-label-bg')];
          const result = {labelBackgroundCount:labelBackgrounds.length,
            labelBordersHidden:labelBackgrounds.every(node => node.style.stroke === 'none' && node.style.borderTopWidth === '0px'),
            labelHits:svgDocument.querySelectorAll('.custom-model-edge-label-hit').length,
            width: bitmap.width, height: bitmap.height, ppm, dash: exportedEdge && exportedEdge.style.strokeDasharray, alpha: ctx.getImageData(0, 0, 1, 1).data[3], unchanged: before === JSON.stringify(instance.state) && dom === instance.paper.innerHTML};
          bitmap.close(); return result;
        }, dpi);
        assert(result.width > 0 && result.width < Math.round(1123 * dpi / 96));
        assert(result.height > 0 && result.height < Math.round(794 * dpi / 96));
        assert.equal(result.ppm, Math.round(dpi / .0254));
        assert.equal(result.alpha, 0); assert.ok(result.unchanged);
        assert.ok(result.labelBackgroundCount > 0 && result.labelBordersHidden, JSON.stringify(result));
        assert.equal(result.labelHits, 0);
        assert.match(result.dash, /7(?:px)?[ ,]+5(?:px)?/);
      }
      const report = await page.evaluate(async () => {
        const instance = document.querySelector('.custom-model-canvas-root').__stateduModelCanvas;
        instance.viewingResult = true;
        const holder = document.createElement('div');
        holder.innerHTML = await window.StatEduModelCanvas.dialogs.exportReportFigure(instance);
        const image = holder.querySelector('img');
        const bitmap = await createImageBitmap(await (await fetch(image.src)).blob());
        const dimensionsMatch = image.width === bitmap.width && image.height === bitmap.height;
        bitmap.close();
        return {html:holder.innerHTML, dimensionsMatch, images: holder.querySelectorAll('img.analysis-plot-image').length, width: image.width, height: image.height, orientation: holder.firstElementChild.dataset.resultTableOrientation};
      });
      assert.equal(report.images, 1); assert.equal(report.orientation, 'landscape');
      assert(report.dimensionsMatch);
      fs.writeFileSync(path.join(fixture, menu+'-cropped.html'),report.html);
      const client = fs.readFileSync('www/easyflow.js', 'utf8');
      const start = client.indexOf('      function registerEasyflowResultSnapshotHandler()');
      const end = client.indexOf('      registerEasyflowResultSnapshotHandler();', start);
      assert.ok(start >= 0 && end > start);
      const accumulated = await page.evaluate(async source => {
        const root = document.querySelector('.custom-model-canvas-root');
        const result = document.createElement('div'); result.id = 'snapshot-test-output';
        result.innerHTML = '<table><tr><td>Result estimate .107</td></tr></table>';
        document.body.appendChild(result);
        return await new Promise(resolve => {
          let handler;
          window.easyflowResultSnapshotHandlerRegistered = false;
          window.Shiny = {addCustomMessageHandler: (name, fn) => {handler = fn;}, setInputValue: (id, payload) => resolve(payload)};
          const easyflowSnapshotHtml = async element => element.innerHTML;
          eval(source + '\nregisterEasyflowResultSnapshotHandler();');
          handler({outputId: result.id, inputId: 'add_snapshot', canvasRootId: root.id});
        });
      }, client.slice(start,end));
      assert.equal(accumulated.error, '');
      assert.match(accumulated.html, /Result estimate .107/);
      assert.match(accumulated.html, /class="analysis-plot-image"/);
      assert.match(accumulated.html, /data:image\/png;base64,/);
      console.log(menu + ': PASS actions, icons, footer alignment, 300/600dpi transparent PNG, report image');
    }
  } finally { await browser.close(); }
})().catch(error => { console.error(error); process.exitCode = 1; });
