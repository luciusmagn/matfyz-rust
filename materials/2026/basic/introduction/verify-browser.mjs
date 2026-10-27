// Node 22+ and Chromium; no npm dependencies. Run after build.sh.
import { spawn } from 'node:child_process';
import { mkdtemp, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { dirname, resolve, join } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { once } from 'node:events';
import assert from 'node:assert/strict';

const options = new Set(process.argv.slice(2).filter(arg => arg.startsWith('--')));
const targetArg = process.argv.slice(2).find(arg => !arg.startsWith('--'));
const scriptDir = dirname(fileURLToPath(import.meta.url));
const target = targetArg ? resolve(process.cwd(), targetArg) : join(scriptDir, 'index.html');
const profile = await mkdtemp(join(tmpdir(), 'rust-slides-'));
const browser = spawn('chromium', [
  '--headless', '--no-sandbox', '--disable-dev-shm-usage', '--disable-gpu',
  '--remote-debugging-port=0', `--user-data-dir=${profile}`, 'about:blank'
], { stdio: ['ignore', 'ignore', 'pipe'] });
let socket;
const timeout = setTimeout(() => { console.error('Browser verification timed out'); browser.kill(); }, 60000);
try {
  const endpoint = await new Promise((resolve, reject) => {
    let output = '';
    browser.stderr.on('data', data => {
      output += data;
      const match = output.match(/DevTools listening on (ws:\/\/\S+)/);
      if (match) resolve(match[1]);
    });
    browser.on('error', reject);
    browser.on('exit', code => reject(new Error(`Chromium exited: ${code}`)));
  });
  socket = new WebSocket(endpoint);
  await new Promise((resolve, reject) => { socket.onopen = resolve; socket.onerror = reject; });
  let id = 0;
  const pending = new Map();
  socket.onmessage = ({ data }) => {
    const message = JSON.parse(data);
    if (!pending.has(message.id)) return;
    const { resolve, reject } = pending.get(message.id);
    pending.delete(message.id);
    if (message.error) reject(new Error(JSON.stringify(message.error)));
    else resolve(message.result);
  };
  function send(method, params = {}, sessionId) {
    return new Promise((resolve, reject) => {
      pending.set(++id, { resolve, reject });
      socket.send(JSON.stringify({ id, method, params, sessionId }));
    });
  }
  const { targetId } = await send('Target.createTarget', { url: 'about:blank' });
  const { sessionId } = await send('Target.attachToTarget', { targetId, flatten: true });
  const call = (method, params) => send(method, params, sessionId);
  async function evaluate(expression) {
    const result = await call('Runtime.evaluate', { expression, returnByValue: true, awaitPromise: true });
    if (result.exceptionDetails) throw new Error(JSON.stringify(result.exceptionDetails));
    return result.result.value;
  }
  await call('Page.enable');
  await call('Page.navigate', { url: pathToFileURL(target).href });
  for (let n = 0; n < 100; n++) {
    if (await evaluate('document.readyState === "complete" && typeof show === "function"')) break;
    await new Promise(resolve => setTimeout(resolve, 50));
  }
  await evaluate('document.fonts.ready.then(() => true)');
  // Fit at the logical slide size, then require the same result on every viewport.
  await call('Emulation.setDeviceMetricsOverride', { width: 1280, height: 720, deviceScaleFactor: 1, mobile: false });
  await evaluate('resize(); fitSlides()');
  const referenceScales = await evaluate(`slides.map(slide =>
    parseFloat(getComputedStyle(slide).getPropertyValue('--content-scale')) || 1)`);
  for (const [width, height] of [[1920, 1080], [1280, 720], [1024, 768], [3840, 2160]]) {
    await call('Emulation.setDeviceMetricsOverride', { width, height, deviceScaleFactor: 1, mobile: false });
    await evaluate('resize(); fitSlides()');
    const results = await evaluate(`(() => {
      const failures = [];
      const scale = Math.min(innerWidth / 1280, innerHeight / 720);
      const content = slides.map(slide => ({
        id: slide.id,
        scale: parseFloat(getComputedStyle(slide).getPropertyValue('--content-scale')) || 1
      }));
      const minContentScale = Math.min(...content.map(slide => slide.scale));
      const scaledSlides = content.filter(slide => slide.scale < .98);
      const checked = 'h2, ul, pre, figure, img, svg, .discord-qr, .title-author, .title-term';
      slides.forEach((slide, index) => {
        show(index, true);
        const rect = slide.getBoundingClientRect();
        if (rect.left < -1 || rect.top < -1 || rect.right > innerWidth + 1 || rect.bottom > innerHeight + 1)
          failures.push(slide.id + ': outside viewport');
        for (const element of slide.querySelectorAll(checked)) {
          const box = element.getBoundingClientRect();
          if (box.bottom > rect.top + 670 * scale || box.right > rect.right - 59 * scale)
            failures.push(slide.id + ': content outside safe area');
          if (element.tagName === 'PRE' && element.scrollWidth > element.clientWidth + 1)
            failures.push(slide.id + ': code wider than slide');
        }
        for (const image of slide.querySelectorAll('img'))
          if (!image.complete || image.naturalWidth === 0) failures.push(slide.id + ': image failed to load');
      });
      show(0);
      return { slides: slides.length, failures, minContentScale, scaledSlides,
        contentScales: content.map(slide => slide.scale), bodyPixels: 40 * scale, codePixels: 32 * scale };
    })()`);
    const { contentScales, ...summary } = results;
    console.log(`${width}x${height}: ${JSON.stringify(summary)}`);
    assert.deepEqual(results.failures, []);
    assert.deepEqual(contentScales, referenceScales, 'Text fitting changed with viewport scale');
    if (options.has('--no-shrink'))
      assert.equal(results.minContentScale, 1, 'Split crowded slides instead of shrinking text');
  }
  const examples = await evaluate(`(() => {
    for (const pre of document.querySelectorAll('pre.runnable')) {
      const link = pre.parentElement.querySelector('.run-code');
      if (!link || new URL(link.href).searchParams.get('code') !== pre.querySelector('code').textContent)
        throw new Error('Playground link does not contain the example');
    }
    return document.querySelectorAll('pre.runnable').length;
  })()`);
  await evaluate('show(0)');
  await call('Input.dispatchKeyEvent', { type: 'keyDown', key: 'ArrowRight', code: 'ArrowRight', windowsVirtualKeyCode: 39 });
  assert.equal(await evaluate('current'), 1);
  assert.equal(await evaluate('revealed'), 0);
  const key = async (key, code) => call('Input.dispatchKeyEvent', { type: 'keyDown', key, code: key, windowsVirtualKeyCode: code });
  const visible = () => evaluate('bullets[current].filter(li => getComputedStyle(li).visibility === "visible").length');
  const count = await evaluate('bullets[current].length');
  assert.equal(await visible(), 0);
  for (let n = 1; n <= count; n++) {
    await key('ArrowRight', 39);
    assert.equal(await evaluate('current'), 1);
    assert.equal(await visible(), n);
  }
  await key('ArrowRight', 39);
  assert.equal(await evaluate('current'), 2);
  assert.equal(await visible(), 0);
  await key('ArrowLeft', 37);
  assert.equal(await evaluate('current'), 1);
  assert.equal(await visible(), count);
  await key('ArrowLeft', 37);
  assert.equal(await visible(), count - 1);
  assert.equal(await evaluate('bullets[current].at(-1).inert'), true);
  // A fresh slide must print every bullet, including those not revealed on screen.
  await evaluate('show(1)');
  await call('Emulation.setEmulatedMedia', { media: 'print' });
  assert.equal(await visible(), count);
  await call('Emulation.setEmulatedMedia', { media: 'screen' });
  assert.equal(await visible(), 0);
  await key('End', 35);
  assert.equal(await evaluate('current'), await evaluate('slides.length - 1'));
  await call('Emulation.setEmulatedMedia', { media: 'print' });
  const printFailures = await evaluate(`slides.filter(slide => {
    const rect = slide.getBoundingClientRect();
    return [...slide.querySelectorAll('h2, ul, pre, figure, img, svg, .discord-qr, .title-author, .title-term')].some(el => el.getBoundingClientRect().bottom > rect.top + 670);
  }).map(slide => slide.id)`);
  assert.deepEqual(printFailures, []);
  await call('Emulation.setEmulatedMedia', { media: 'screen' });
  if (options.has('--screenshots')) {
    await call('Emulation.setDeviceMetricsOverride', { width: 1920, height: 1080, deviceScaleFactor: 1, mobile: false });
    const previewSlides = await evaluate(`(() => {
      const marked = [...document.querySelectorAll('section[data-preview]')];
      return (marked.length ? marked : ['programming-in-rust', 'contacts', 'course-organization'].map(id => document.getElementById(id)).filter(Boolean))
        .map(slide => slide.id);
    })()`);
    for (const id of previewSlides) {
      await evaluate(`resize(); show(slides.findIndex(slide => slide.id === ${JSON.stringify(id)}), true)`);
      const { data } = await call('Page.captureScreenshot', { format: 'png' });
      await writeFile(join(dirname(target), `.preview-${id}.png`), Buffer.from(data, 'base64'));
    }
  }
  console.log(`Verified ${examples} example links, incremental bullets, reverse navigation, and full print visibility.`);
} finally {
  clearTimeout(timeout);
  socket?.close();
  if (browser.exitCode === null) {
    const exited = once(browser, 'exit');
    browser.kill();
    await exited;
  }
  await rm(profile, { recursive: true, force: true, maxRetries: 5 });
}
