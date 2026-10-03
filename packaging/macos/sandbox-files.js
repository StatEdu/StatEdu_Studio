'use strict';
const fs = require('node:fs');
const path = require('node:path');

// Keep user-selected file access for the R session and restore it before R
// starts on the next launch. Bookmarks remain in the user's private container.
function createSecurityScopedAccess({app, enabled, log = () => {}}) {
  const active = new Map();
  const bookmarks = new Map();
  const filename = () => path.join(app.getPath('userData'), 'settings', 'security-scoped-bookmarks.json');
  function begin(file, bookmark) {
    if (!enabled || !file || !bookmark) return;
    const key = path.resolve(file);
    if (active.has(key)) return;
    const stop = app.startAccessingSecurityScopedResource(bookmark);
    if (typeof stop !== 'function') throw new Error('Could not acquire sandbox file access');
    active.set(key, stop);
  }
  function remember(file, bookmark) {
    if (!enabled || !file || !bookmark) return;
    const key = path.resolve(file);
    begin(key, bookmark);
    bookmarks.set(key, bookmark);
    const destination = filename();
    fs.mkdirSync(path.dirname(destination), {recursive:true, mode:0o700});
    const temporary = destination + '.tmp';
    fs.writeFileSync(temporary, JSON.stringify({schema:1, bookmarks:Object.fromEntries(bookmarks)}), {mode:0o600});
    fs.renameSync(temporary, destination);
  }
  function restore() {
    if (!enabled) return;
    let saved;
    try {saved = JSON.parse(fs.readFileSync(filename(), 'utf8'));}
    catch (error) {if (error.code !== 'ENOENT') log('Sandbox bookmark settings could not be read'); return;}
    if (saved.schema !== 1 || !saved.bookmarks || typeof saved.bookmarks !== 'object') return;
    for (const [file, bookmark] of Object.entries(saved.bookmarks)) {
      if (!path.isAbsolute(file) || typeof bookmark !== 'string' || !bookmark) continue;
      bookmarks.set(file, bookmark);
      try {begin(file, bookmark);} catch (_) {log('A saved sandbox file permission needs to be selected again');}
    }
  }
  function wrapDialogs(dialog) {
    if (!enabled) return;
    for (const method of ['showOpenDialog','showSaveDialog']) {
      const original = dialog[method].bind(dialog);
      dialog[method] = async (...args) => {
        const index = args.length - 1;
        args[index] = {...args[index], securityScopedBookmarks:true};
        const result = await original(...args);
        if (!result.canceled) {
          if (method === 'showOpenDialog') {
            (result.filePaths || []).forEach((file,i) => remember(file,result.bookmarks?.[i]));
          } else remember(result.filePath,result.bookmark);
        }
        return result;
      };
    }
  }
  function release() {
    for (const stop of active.values()) {try {stop();} catch (_) {}}
    active.clear();
  }
  return {remember, restore, wrapDialogs, release};
}
module.exports = {createSecurityScopedAccess};
