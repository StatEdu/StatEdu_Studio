const assert = require('node:assert/strict');
const path = require('node:path');
const fs = require('node:fs');
const {pathToFileURL} = require('node:url');
const {chromium} = require(process.env.STATEDU_PLAYWRIGHT_MODULE || 'playwright');
(async () => {
  const browser = await chromium.launch({channel:'chrome', headless:true});
  try {
    const page = await browser.newPage({viewport:{width:1500,height:1100}});
    await page.goto(pathToFileURL(path.resolve('tmp/structural-screen-contract/screen.html')).href);
    await page.evaluate(() => document.fonts.ready);
    const geometry = await page.locator('[data-result-table-sheet="true"]').evaluateAll(nodes => nodes.map(n => ({
      orientation:n.dataset.resultTableOrientation, width:n.getBoundingClientRect().width,
      overflow:n.scrollWidth - n.clientWidth,
      tableOverflow:n.querySelector('table').scrollWidth - n.querySelector('table').clientWidth
    })));
    assert.equal(geometry[0].orientation, 'portrait');
    assert.ok(geometry[0].overflow < 2, JSON.stringify(geometry));
    for (const i of [2,3,4]) {
      assert.equal(geometry[i].orientation, 'landscape');
      assert.ok(geometry[i].width > geometry[0].width, JSON.stringify(geometry));
      assert.ok(geometry[i].overflow < 2, JSON.stringify(geometry));
    }
    await page.screenshot({path:'tmp/structural-screen-contract/screen.png', fullPage:true});
    const source = fs.readFileSync('www/easyflow.js', 'utf8');
    const capture = source.slice(source.indexOf('async function easyflowSnapshotHtml('),
      source.indexOf('function registerEasyflowResultSnapshotHandler()')).trim();
    const captured = await page.evaluate(async definition => {
      const easyflowInlineSnapshotCanvases = () => {};
      const easyflowInlineSnapshotImages = async () => {};
      const snapshot = eval('(' + definition + ')');
      const root = document.createElement('div');
      root.innerHTML = '<style>.capture-test td{border-bottom:2px solid rgb(20,30,40);white-space:normal;padding:7px}</style><table class="capture-test"><tr><td>Long label</td></tr></table>';
      document.body.append(root);
      const saved = document.createElement('div');
      saved.innerHTML = await snapshot(root);
      const style = saved.querySelector('td').style;
      return {border:style.borderBottom, wrap:style.whiteSpace};
    }, capture);
    assert.equal(captured.border, '2px solid rgb(20, 30, 40)');
    assert.equal(captured.wrap, 'normal');
    const imageHelpers = source.slice(source.indexOf('function easyflowBlobToDataUrl('),
      source.indexOf('function registerEasyflowResultSnapshotHandler()'));
    const imageCapture = await page.evaluate(async definition => {
      const snapshot = new Function(definition + '; return easyflowSnapshotHtml;')();
      const root = document.createElement('div');
      const canvas = document.createElement('canvas');
      canvas.width = 80; canvas.height = 40;
      canvas.getContext('2d').fillRect(0, 0, 80, 40);
      const expectedCanvas = canvas.toDataURL('image/png');
      const image = document.createElement('img');
      image.style.cssText = 'width:60px;height:30px';
      const url = URL.createObjectURL(await (await fetch(expectedCanvas)).blob());
      image.src = url;
      root.append(canvas, image); document.body.append(root);
      await image.decode();
      const saved = document.createElement('div');
      saved.innerHTML = await snapshot(root);
      const images = [...saved.querySelectorAll('img')];
      const result = {count:images.length, canvas:images[0].src === expectedCanvas,
        width:images[1].getAttribute('width'), height:images[1].getAttribute('height'),
        embedded:images[1].getAttribute('src').startsWith('data:image/')};
      URL.revokeObjectURL(url); root.remove();
      return result;
    }, imageHelpers);
    assert.deepEqual(imageCapture, {count:2, canvas:true, width:'60', height:'30', embedded:true});
    await page.goto(pathToFileURL(path.resolve('tmp/structural-screen-contract/print.html')).href);
    await page.evaluate(() => document.fonts.ready);
    await page.pdf({path:'tmp/structural-screen-contract/screen.pdf',preferCSSPageSize:true,printBackground:true});
    console.log('PASS: matrix and fit-table widths; PDF generated from captured display HTML');
  } finally { await browser.close(); }
})().catch(e=>{console.error(e);process.exit(1);});
