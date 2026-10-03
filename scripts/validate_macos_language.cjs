'use strict';
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const root = path.resolve(__dirname, '..');
const source = fs.readFileSync(path.join(root, 'packaging/macos/main.js'), 'utf8');
const helpers = source.slice(source.indexOf('function appLanguageFile()'), source.indexOf('function resultZoomFile()'));
const prompt = source.slice(source.indexOf('function confirmDiscardChanges()'), source.indexOf('async function reloadStudioFile('));
const languages = ['en', 'ko', 'ja', 'zh', 'es', 'fr', 'de', 'vi'];
let saved = '', preferred = [], shown;
const context = {
  path, fs: {readFileSync: () => saved}, process: {env: {}}, mainWindow: null,
  app: {getPath: () => '/isolated-profile', getPreferredSystemLanguages: () => preferred},
  appDisplayName: () => 'StatEdu Studio',
  dialog: {showMessageBoxSync: (_window, options) => {shown = options; return 0;}}
};
vm.createContext(context);
vm.runInContext(helpers + '\n' + prompt, context);
for (const [locale, expected] of Object.entries({
  'en-US':'en', 'ko-KR':'ko', 'ja-JP':'ja', 'zh-Hans-CN':'zh',
  'es-419':'es', 'fr-CA':'fr', 'de-AT':'de', 'vi-VN':'vi'
})) {
  preferred = [locale]; saved = '';
  assert.equal(context.initialAppLanguage(), expected, `First install: ${locale}`);
}
preferred = ['it-IT', 'fr-FR'];
assert.equal(context.initialAppLanguage(), 'fr', 'Choose the first supported preferred language');
preferred = ['it-IT'];
assert.equal(context.initialAppLanguage(), 'en', 'English fallback');
saved = 'ko\n';
assert.equal(context.initialAppLanguage(), 'ko', 'Preserve the selected language across launches');
saved = 'invalid';
assert.equal(context.initialAppLanguage(), 'en', 'Invalid saved locale must not reach R');
context.process.env.STATEDU_APP_LANGUAGE = 'ja-JP';
assert.equal(context.initialAppLanguage(), 'ja', 'Explicit startup override');
for (const language of languages) {
  saved = language;
  assert.equal(context.confirmDiscardChanges(), false, 'Cancel must preserve changes');
  assert.equal(shown.buttons.length, 2);
  assert.equal(shown.defaultId, 0);
  if (language !== 'en') assert.notEqual(shown.message, 'There are unsaved project changes.');
}
delete context.process.env.STATEDU_APP_LANGUAGE;
saved = '';
context.app.getPreferredSystemLanguages = () => {throw new Error('Unavailable');};
assert.equal(context.initialAppLanguage(), 'en');
const specs = JSON.parse(fs.readFileSync(path.join(root, 'docs/i18n/document_specs.json'), 'utf8').replace(/^\uFEFF/, ''));
assert.deepEqual(Object.keys(specs).sort(), languages.slice().sort());
for (const language of languages) {
  for (const document of Object.values(specs[language])) {
    assert(fs.statSync(path.join(root, document.path)).size > 0, `Missing ${language} document: ${document.path}`);
  }
}
const listing=JSON.parse(fs.readFileSync(path.join(root,'packaging/macos/app-store/localizations.json'),'utf8'));
assert.equal(listing.primary_locale,'en-US');
assert.deepEqual(listing.localizations.map(x=>x.language).sort(),languages.slice().sort());
for(const item of listing.localizations) {
  for(const [key,limit] of Object.entries({name:30,subtitle:30,keywords:100,description:4000})) {
    assert(item[key].length>0 && item[key].length<=limit,`${item.locale}: ${key} exceeds App Store limit`);
  }
  assert.equal(item.guide_path,specs[item.language].user_guide.path);
}
console.log('PASS: eight first-install languages, persisted choices, English fallback, native prompts, documentation and listing drafts');
