const assert = require('node:assert/strict');
const fs = require('node:fs');
const {chromium} = require(process.env.STATEDU_PLAYWRIGHT_MODULE || 'playwright');

(async () => {
  const browser = await chromium.launch({channel: 'chrome', headless: true});
  const out = 'tmp/canvas-covariate-exports';
  fs.mkdirSync(out, {recursive: true});
  try {
    for (const type of ['mediation', 'cfa', 'cbsem', 'plssem']) {
      const page = await browser.newPage();
      await page.setContent(fs.readFileSync('tmp/canvas-export-validation/mm.html', 'utf8'));
      await page.addStyleTag({path: 'www/model-canvas/canvas.css'});
      await page.evaluate(type => {
        const root = document.querySelector('.custom-model-canvas-root');
        root.className = 'custom-model-canvas-root ' + (type === 'mediation' ? 'mediation-moderation-canvas-root' : 'structural-equation-canvas-root');
        root.setAttribute('data-analysis-type', type === 'mediation' ? '' : type);
        root.setAttribute('data-export-dpi', '300');
        window.saved = [];
        window.Shiny = {addCustomMessageHandler() {}, setInputValue(id, payload) { window.saved.push({id, payload}); }};
      }, type);
      for (const file of ['state', 'layout', 'shiny-bridge', 'edges', 'nodes', 'dialogs', 'toolbar', 'canvas']) {
        await page.addScriptTag({path: `www/model-canvas/${file}.js`});
      }
      const result = await page.evaluate(async () => {
        const api = window.StatEduModelCanvas;
        const root = document.querySelector('.custom-model-canvas-root');
        const i = api.canvas.init(root);
        const state = api.state.snapshot(i.state);
        Object.assign(state.canvas, {widthPx: 800, heightPx: 650});
        state.nodes = [
          {id: 'x', name: 'X', role: 'independent', x: 300, y: 100, width: 110, height: 44},
          {id: 'y', name: 'Y', role: 'dependent', x: 600, y: 200, width: 110, height: 44},
          {id: 'c1', name: 'Age', role: 'covariate', x: 40, y: 300, width: 110, height: 44},
          {id: 'c2', name: 'Sex', variable: 'sex', role: 'observed', x: 40, y: 450, width: 110, height: 44},
          {id: 'ec', name: 'eC', role: 'error', x: 180, y: 450, width: 30, height: 30}
        ];
        state.edges = [
          {id: 'xy', from: 'x', to: 'y', label: '.45(.012)'},
          {id: 'c1y', from: 'c1', to: 'y', label: '.21(.023)'},
          {id: 'c2y', from: 'c2', to: 'y', label: '-.08(.310)'},
          {id: 'cc', from: 'c1', to: 'c2', kind: 'covariance', label: '.99(<.001)'},
          {id: 'cx', from: 'c1', to: 'x', kind: 'covariance', label: '.88(<.001)'},
          {id: 'ec2', from: 'ec', to: 'c2', label: '1'}
        ];
        state.covariates = ['Age', 'sex', 'Income'];
        state.covariateEffects = [{variable: 'Income', target: 'Y', label: '.17(.041)'}];
        state.moderations = [];
        api.bridge.applyResult({rootId: root.id, source: state, result: state, show: true});
        await document.fonts.ready;
        const before = JSON.stringify(i.state), dom = i.paper.innerHTML;
        const svgs = [];
        const src = Object.getOwnPropertyDescriptor(HTMLImageElement.prototype, 'src');
        Object.defineProperty(HTMLImageElement.prototype, 'src', {...src, set(value) {
          if (value.startsWith('data:image/svg+xml')) svgs.push(decodeURIComponent(value.slice(value.indexOf(',') + 1)));
          src.set.call(this, value);
        }});
        let html;
        try {
          await api.dialogs.exportModel(i);
          html = await api.dialogs.exportReportFigure(i);
        } finally { Object.defineProperty(HTMLImageElement.prototype, 'src', src); }
        const inspect = svg => {
          const d = new DOMParser().parseFromString(svg, 'image/svg+xml');
          return {nodes: [...d.querySelectorAll('.custom-model-node')].map(n => n.dataset.nodeId),
            edges: [...d.querySelectorAll('.custom-model-edge')].map(n => n.dataset.edgeId),
            labels: [...d.querySelectorAll('[data-label-type="edge"] .custom-model-edge-label-text')].map(n => n.textContent),
            effects: [...d.querySelectorAll('[data-export-control-effect]')].map(n => n.textContent),
            badges: d.querySelectorAll('.structural-validation-badge').length};
        };
        const files = window.saved.find(item => item.id.endsWith('_figures_snapshot')).payload.files;
        window.Shiny = null;
        const fallback = [];
        window.stateduDesktopFiles = {openText() {}, async save(payload) { fallback.push(payload.suggestedName); return {canceled: false}; }};
        await api.dialogs.exportModel(i);
        return {files, html, fallback, svgs: svgs.map(inspect), unchanged: before === JSON.stringify(i.state) && dom === i.paper.innerHTML};
      });
      assert(result.unchanged, type + ': export mutated live state/DOM');
      assert.deepEqual(result.files.map(f => f.name), ['model_with_covariates.png', 'model_without_covariates.png']);
      assert.equal(result.svgs.length, 4);
      assert.equal(result.fallback.length, 2);
      assert(result.fallback[0].endsWith('_with_covariates.png'));
      assert(result.fallback[1].endsWith('_without_covariates.png'));
      for (let index = 0; index < 4; index++) {
        const figure = result.svgs[index], withControls = index % 2 === 0;
        assert.equal(figure.badges, 0);
        assert.deepEqual(figure.edges, withControls ? ['xy', 'c1y', 'c2y'] : ['xy']);
        assert.deepEqual(figure.nodes, withControls ? ['x', 'y', 'c1', 'c2'] : ['x', 'y']);
        assert.deepEqual(figure.labels, withControls ? ['.45(.012)', '.21(.023)', '-.08(.310)'] : ['.45(.012)']);
        assert.deepEqual(figure.effects, withControls ? ['Income → Y  .17(.041)'] : []);
      }
      fs.writeFileSync(`${out}/${type}.html`, result.html);
      fs.writeFileSync(`${out}/${type}.json`, JSON.stringify({files: result.files}));
      for (const file of result.files) fs.writeFileSync(`${out}/${type}_${file.name}`, Buffer.from(file.data.split(',')[1], 'base64'));
      console.log('PASS', type, 'paired PNG/report capture, control effects only, live state preserved');
      await page.close();
    }
  } finally { await browser.close(); }
})().catch(error => { console.error(error); process.exit(1); });
