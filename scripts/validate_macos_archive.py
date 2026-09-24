import hashlib
import json
from pathlib import Path
import tempfile
import unittest
import zipfile

from archive_macos_preparation import archive_stage
from prepare_macos import verify_stage


class ArchiveTests(unittest.TestCase):
    def setUp(self):
        temp = tempfile.TemporaryDirectory()
        self.addCleanup(temp.cleanup)
        self.base = Path(temp.name)
        self.stage = self.base/'한글 Mac stage'
        self.stage.mkdir()
        package = {'devDependencies': {}, 'build': {'appId': 'com.statedu.studio.mac.dev'}}
        files = {'package.json': json.dumps(package),
                 'package-lock.json': json.dumps({'packages': {'': {'devDependencies': {}}}}),
                 'build.command': '#!/bin/sh\nset -eu\n', 'app/run_app.R': '# fixture\n'}
        for name, text in files.items():
            target = self.stage/name
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_text(text, encoding='utf-8', newline='\n')
        self.hashes = {name: hashlib.sha256((self.stage/name).read_bytes()).hexdigest() for name in files}
        self.save_manifest()
        self.zip = self.base/'handoff.zip'

    def save_manifest(self):
        (self.stage/'stage-integrity.json').write_text(json.dumps({'sha256': self.hashes}), encoding='utf-8')

    def test_roundtrip_permissions_and_exclusions(self):
        for name in ('runtime/private.txt', 'node_modules/private.txt', 'dist/private.txt', 'macos-environment-report.json'):
            target = self.stage/name
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_text('must not ship')
        self.assertEqual(archive_stage(self.stage, self.zip), 4)
        with zipfile.ZipFile(self.zip) as z:
            info = z.getinfo(self.stage.name+'/build.command')
            self.assertEqual(info.create_system, 3)
            self.assertEqual(info.external_attr >> 16, 0o100755)
            self.assertNotIn(b'\r', z.read(info))
            self.assertIn(self.stage.name+'/runtime/', z.namelist())
            self.assertFalse(any('private.txt' in name or 'environment-report' in name for name in z.namelist()))
            extracted = self.base/'extracted'
            z.extractall(extracted)
        self.assertEqual(verify_stage(extracted/self.stage.name), 4)
        self.assertEqual(self.zip.with_suffix('.zip.sha256').read_text().split()[0],
                         hashlib.sha256(self.zip.read_bytes()).hexdigest())

    def test_refuses_existing_archive_and_stage_destination(self):
        self.zip.write_bytes(b'existing')
        with self.assertRaises(ValueError):
            archive_stage(self.stage, self.zip)
        self.assertEqual(self.zip.read_bytes(), b'existing')
        with self.assertRaises(ValueError):
            archive_stage(self.stage, self.stage/'new.zip')

    def test_modified_source_is_rejected_before_writing(self):
        (self.stage/'app/run_app.R').write_text('changed')
        with self.assertRaises(ValueError):
            archive_stage(self.stage, self.zip)
        self.assertFalse(self.zip.exists())

    def test_runtime_in_manifest_is_rejected(self):
        target = self.stage/'runtime/private.txt'
        target.parent.mkdir()
        target.write_bytes(b'private')
        self.hashes['runtime/private.txt'] = hashlib.sha256(b'private').hexdigest()
        self.save_manifest()
        with self.assertRaisesRegex(ValueError, 'generated/runtime'):
            archive_stage(self.stage, self.zip)
        self.assertFalse(self.zip.exists())


if __name__ == '__main__':
    unittest.main()
