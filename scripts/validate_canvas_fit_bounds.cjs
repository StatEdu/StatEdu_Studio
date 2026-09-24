const assert = require('node:assert/strict');
const fs = require('node:fs');
const {chromium} = require(process.env.STATEDU_PLAYWRIGHT_MODULE || 'playwright');

(async () => {
  const browser = await chromium.launch({channel: 'chrome', headless: true});
  const out = 'tmp/canvas-fit-bounds';
  fs.mkdirSync(out, {recursive: true});
  try {
    for (const menu of ['cfa', 'sem', 'pls']) {
      const page = await browser.newPage({viewport: {width: 1500, height: 1000}});
      await page.setContent(fs.readFileSync(`tmp/canvas-export-validation/${menu}.html`, 'utf8'));
      for (const css of [fs.readFileSync('tmp/canvas-export-validation/bootstrap-path.txt', 'utf8').trim(), 'www/style.css', 'www/model-canvas/canvas.css']) await page.addStyleTag({path: css});
      await page.evaluate(() => { window.Shiny = {addCustomMessageHandler() {}, setInputValue() {}}; });
      for (const file of ['state', 'layout', 'shiny-bridge', 'edges', 'nodes', 'dialogs', 'toolbar', 'canvas']) await page.addScriptTag({path: `www/model-canvas/${file}.js`});
      for (const initialZoom of [.65, 1.4]) {
        const result = await page.evaluate(async initialZoom => {
          const api = window.StatEduModelCanvas;
          const i = document.querySelector('.custom-model-canvas-root').__stateduModelCanvas;
          const s = api.state.snapshot(i.state);
          Object.assign(s.canvas, {widthPx: 1100, heightPx: 700, modelZoom: initialZoom, viewZoom: .8, paperViewMode: 'manual'});
          s.nodes = [
            {id: 'x', name: 'Predictor', role: 'latent', x: -180, y: 170, width: 100, height: 50},
            {id: 'y', name: 'Outcome', role: 'latent', x: 980, y: 530, width: 110, height: 50, resultStatsValues: {r2: 'R² = .45'}, resultStatsOffsetX: 180, resultStatsOffsetY: 80},
            {id: 'm', name: 'Item', role: 'indicator', x: 1300, y: 700, width: 100, height: 35},
            {id: 'e', name: 'ε', role: 'error', x: 1340, y: 880, width: 28, height: 28}
          ];
          s.edges = [
            {id: 'xy', from: 'x', to: 'y', label: '.45(.012)', labelOffsetX: -220, labelOffsetY: -250, labelManualPosition: true},
            {id: 'ym', from: 'y', to: 'm', label: '.82(<.001)'},
            {id: 'em', from: 'e', to: 'm', label: '1'}
          ];
          s.covariates = [];
          for (let k = 0; k < 14; k++) {
            const name = `Control_${k}`;
            s.covariates.push(name);
            s.nodes.push({id: name, name, role: 'observed', x: 1900, y: -220 + k * 100, width: 140, height: 40});
          }
          s.moderations = [];
          api.bridge.applyResult({rootId: i.root.id, source: s, result: s, show: true});
          await document.fonts.ready;
          const relative = () => i.state.nodes.map(n => [n.id, n.x - i.state.nodes[0].x, n.y - i.state.nodes[0].y]);
          const before = relative(), labels = i.state.edges.map(e => e.label);
          // Exercise the actual toolbar dispatch.
          i.root.querySelector('[data-action="fit"]').click();
          const zoom = i.state.canvas.modelZoom;
          const p = i.paper.getBoundingClientRect();
          const overflow = [...i.paper.querySelectorAll('.custom-model-node, .structural-latent-statistics, .custom-model-edge, .custom-model-edge-label')].filter(el => {
            const r = el.getBoundingClientRect();
            return r.left < p.left + 2 || r.top < p.top + 2 || r.right > p.right - 2 || r.bottom > p.bottom - 2;
          }).map(el => el.className.baseVal || el.className);
          const scroll = i.root.querySelector('.custom-model-canvas-scroll').getBoundingClientRect();
          const paperVisible = p.left >= scroll.left && p.top >= scroll.top && p.right <= scroll.right && p.bottom <= scroll.bottom;
          const after = relative();
          i.root.querySelector('[data-action="fit"]').click();
          const zoomAgain = i.state.canvas.modelZoom;
          const report = await api.dialogs.exportReportFigure(i);
          i.root.querySelector('[data-action="zoomOut"]').click();
          const zoomOut = i.state.canvas.modelZoom;
          api.state.restore(i.state, api.state.snapshot(i.state));
          api.canvas.render(i);
          const restoredZoom = i.state.canvas.modelZoom;
          return {before, after, labels, labelsAfter: i.state.edges.map(e => e.label), overflow, zoom, zoomAgain, zoomOut, restoredZoom, paperVisible, report};
        }, initialZoom);
        assert.deepEqual(result.overflow, [], menu + ': visible objects outside paper');
        assert(result.paperVisible, menu + ': paper outside viewport');
        result.before.forEach((n, index) => n.slice(1).forEach((v, axis) => assert(Math.abs(v - result.after[index][axis + 1]) < .001)));
        assert.deepEqual(result.labels, result.labelsAfter);
        assert(result.zoom < .5 && result.zoom <= initialZoom, menu + ': large model not reduced');
        assert(Math.abs(result.zoom - result.zoomAgain) < .002, menu + ': repeated fit changes zoom');
        assert(result.zoomOut < result.zoomAgain, menu + ': zoom out unexpectedly enlarges fitted model');
        assert.equal(result.restoredZoom, result.zoomOut, menu + ': snapshot loses small fitted zoom');
        fs.writeFileSync(`${out}/${menu}.html`, result.report);
        await page.screenshot({path: `${out}/${menu}-fit.png`});
        console.log('PASS', menu, initialZoom, 'full model and paper visible; layout/coefficients preserved; repeat stable', result.zoom);
      }
      await page.close();
    }
  } finally { await browser.close(); }
})().catch(error => { console.error(error); process.exit(1); });
