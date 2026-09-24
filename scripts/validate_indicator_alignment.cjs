// Generate fixtures with validate_canvas_export_ui.R before running.
const assert = require('node:assert/strict');
const path = require('node:path');
const fs = require('node:fs');
const {pathToFileURL} = require('node:url');
const {chromium} = require(process.env.STATEDU_PLAYWRIGHT_MODULE || 'playwright');

(async () => {
  const browser = await chromium.launch({channel: 'chrome', headless: true});
  try {
    const page = await browser.newPage({viewport: {width: 1500, height: 900}});
    for (const menu of ['sem', 'cfa', 'pls']) {
      await page.goto(pathToFileURL(path.resolve('tmp/canvas-export-validation', menu + '.html')).href);
      await page.addStyleTag({path: 'www/model-canvas/canvas.css'});
      for (const file of ['state', 'layout', 'shiny-bridge', 'edges', 'nodes', 'dialogs', 'toolbar', 'canvas']) {
        await page.addScriptTag({path: 'www/model-canvas/' + file + '.js'});
      }
      assert.equal(await page.locator('[data-action="alignIndicators"]').count(), 1);
      for (const action of ['alignLeft', 'alignTop', 'alignCenter', 'alignMiddle', 'distributeH', 'distributeV', 'autoLayout']) {
        assert.equal(await page.locator('[data-action="' + action + '"]').count(), 0);
      }
      for (const placement of ['top', 'bottom', 'left', 'right']) {
        const before = await page.evaluate(placement => {
          const instance = document.querySelector('.custom-model-canvas-root').__stateduModelCanvas;
          const horizontal = ['top', 'bottom'].includes(placement);
          const nodes = [{id: 'l', role: 'latent', x: 450, y: 400, width: 100, height: 50, measurementPlacement: placement},
            {id: 'unrelated', role: 'latent', x: 800, y: 600, width: 100, height: 50}];
          const edges = [];
          for (let i = 0; i < 3; i++) {
            const x = horizontal ? 300 + i * 130 : (i === 0 ? 240 : 200);
            const y = horizontal ? (i === 0 ? 160 : 200) : 150 + i * 60;
            nodes.push({id: 'i' + i, role: 'indicator', name: 'Indicator ' + i, x, y, width: 100, height: 30});
            edges.push({id: 'm' + i, from: 'l', to: 'i' + i, kind: 'measurement'});
            if (instance.analysisType !== 'plssem') {
              nodes.push({id: 'e' + i, role: 'error', x: x + (horizontal ? 37 : placement === 'left' ? -76 : 150),
                y: y + (horizontal ? placement === 'top' ? -76 : 80 : 2), width: 26, height: 26});
              edges.push({id: 'err' + i, from: 'e' + i, to: 'i' + i, kind: 'path'});
            }
          }
          Object.assign(instance.state, {nodes, edges, selectedNodeIds: nodes.filter(n => n.role !== 'latent').map(n => n.id),
            selectedNodeId: 'i0', history: [], redoStack: [], moderations: [], covariates: []});
          window.StatEduModelCanvas.canvas.render(instance);
          return JSON.parse(JSON.stringify(instance.state.nodes));
        }, placement);
        if (menu === 'sem' && placement === 'top') {
          fs.mkdirSync('tmp/indicator-alignment', {recursive: true});
          await page.locator('[data-action="alignIndicators"]').screenshot({path: 'tmp/indicator-alignment/icon.png', scale: 'css'});
          await page.locator('.custom-model-toolbar').screenshot({path: 'tmp/indicator-alignment/toolbar.png'});
        }
        await page.locator('[data-action="alignIndicators"]').click();
        const after = await page.evaluate(() => document.querySelector('.custom-model-canvas-root').__stateduModelCanvas.state.nodes);
        const axis = ['top', 'bottom'].includes(placement) ? 'y' : 'x';
        for (const old of before) {
          const current = after.find(n => n.id === old.id);
          assert.ok(current);
          const moved = old.id === 'i0' || old.id === 'e0';
          assert.equal(current.x, old.x + (moved && axis === 'x' ? -40 : 0));
          assert.equal(current.y, old.y + (moved && axis === 'y' ? 40 : 0));
        }
        await page.locator('[data-action="undo"]').click();
        const restored = await page.evaluate(() => document.querySelector('.custom-model-canvas-root').__stateduModelCanvas.state.nodes);
        for (const old of before) {
          const current = restored.find(n => n.id === old.id);
          assert.equal(current.x, old.x);
          assert.equal(current.y, old.y);
        }
        await page.evaluate(() => {
          const instance = document.querySelector('.custom-model-canvas-root').__stateduModelCanvas;
          instance.state.selectedNodeIds = ['i0'];
          window.StatEduModelCanvas.toolbar.updateButtons(instance);
        });
        assert.equal(await page.locator('[data-action="alignIndicators"]').isDisabled(), false);
        // A single selected indicator targets its whole group. Other groups stay put.
        const scopes = await page.evaluate(() => {
          const instance = document.querySelector('.custom-model-canvas-root').__stateduModelCanvas;
          const api = window.StatEduModelCanvas.canvas;
          instance.state.nodes.push(
            {id: 'u1', role: 'indicator', x: 950, y: 550, width: 100, height: 30},
            {id: 'u2', role: 'indicator', x: 970, y: 600, width: 100, height: 30},
            {id: 'u3', role: 'indicator', x: 970, y: 650, width: 100, height: 30});
          instance.state.edges.push(...['u1', 'u2', 'u3'].map(id => ({id: 'm' + id, from: 'unrelated', to: id, kind: 'measurement'})));
          instance.state.nodes.find(n => n.id === 'unrelated').measurementPlacement = 'right';
          api.alignIndicators(instance);
          const selected = instance.state.nodes.find(n => n.id === 'u1').x;
          const aligned = instance.state.nodes.filter(n => ['i0', 'i1', 'i2'].includes(n.id)).map(n => ({x: n.x, y: n.y}));
          instance.state.selectedNodeIds = [];
          instance.state.selectedNodeId = null;
          instance.state.selectedEdgeId = null;
          instance.state.selectedModerationId = null;
          window.StatEduModelCanvas.toolbar.updateButtons(instance);
          const enabled = !instance.root.querySelector('[data-action="alignIndicators"]').disabled;
          api.alignIndicators(instance);
          return {selected, all: instance.state.nodes.find(n => n.id === 'u1').x, aligned, enabled};
        });
        assert.equal(scopes.selected, 950);
        assert.equal(scopes.all, 970);
        assert.equal(scopes.enabled, true);
        assert.equal(scopes.aligned[0][axis], scopes.aligned[1][axis]);
        assert.equal(scopes.aligned[1][axis], scopes.aligned[2][axis]);
      }
    }
    console.log('PASS: unified toolbar; four directions; connected errors; unrelated nodes; undo; selected-group and no-selection full alignment across SEM/CFA/PLS.');
  } finally { await browser.close(); }
})().catch(error => { console.error(error); process.exitCode = 1; });
