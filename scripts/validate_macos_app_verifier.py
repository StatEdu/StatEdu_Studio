"""Bundle checks with synthetic files and mocked Mac commands; no Mac certification."""
import hashlib
import json
import os
from pathlib import Path
import plistlib
import subprocess
import tempfile
import unittest
from unittest.mock import patch

from verify_macos_app import check_app, check_bundle


class BundleTests(unittest.TestCase):
    def setUp(self):
        temp = tempfile.TemporaryDirectory()
        self.addCleanup(temp.cleanup)
        root = Path(temp.name)
        self.stage = root/'stage'
        self.stage.mkdir()
        self.bundle = root/'이동 경로/My App.app'
        self.contents = self.bundle/'Contents'
        (self.contents/'MacOS').mkdir(parents=True)
        (self.contents/'MacOS/Studio').write_bytes(b'fixture')
        self.info = {'CFBundleIdentifier': 'com.statedu.studio.mac.dev',
                     'CFBundleShortVersionString': '1.3.0-dev', 'CFBundleExecutable': 'Studio'}
        self.save_info()
        (self.stage/'package.json').write_text(json.dumps({'version': '1.3.0-dev'}))
        self.unpacked = self.contents/'Resources/app.asar.unpacked'
        (self.unpacked/'app').mkdir(parents=True)
        (self.unpacked/'app/run_app.R').write_bytes(b'source')
        (self.stage/'stage-integrity.json').write_text(json.dumps({'sha256': {
            'app/run_app.R': hashlib.sha256(b'source').hexdigest()}}))
        self.home = self.unpacked/'runtime/R.framework/Resources'
        (self.home/'bin').mkdir(parents=True)
        (self.home/'bin/Rscript').write_bytes(bytes.fromhex('cffaedfe') + b'fixture')

    def save_info(self):
        with (self.contents/'Info.plist').open('wb') as handle:
            plistlib.dump(self.info, handle)

    def runner(self, args, **kwargs):
        return 'arm64' if args[0] == 'lipo' else 'Load command 0\n cmd LC_BUILD_VERSION\n minos 13.0\n'

    def test_relocated_bundle_runs_own_R_without_caller_overrides(self):
        def run(args, **kwargs):
            self.assertEqual(args[0], str(self.home/'bin/Rscript'))
            self.assertEqual(args[1], '--vanilla')
            self.assertEqual(kwargs['cwd'], self.unpacked/'app')
            self.assertEqual(kwargs['env']['R_HOME'], str(self.home))
            self.assertNotIn('DYLD_LIBRARY_PATH', kwargs['env'])
            self.assertNotIn('/opt/homebrew', kwargs['env']['PATH'])
            return subprocess.CompletedProcess(args, 0, 'PASS', '')
        with patch.dict(os.environ, {'R_HOME': '/wrong', 'DYLD_LIBRARY_PATH': '/wrong', 'PATH': '/opt/homebrew'}):
            report = check_app(self.stage, self.bundle, self.runner, run)
        self.assertEqual(report['status'], 'passed')
        self.assertEqual(report['source_files_verified'], 1)

    def test_modified_source_rejected(self):
        (self.unpacked/'app/run_app.R').write_bytes(b'stale')
        with self.assertRaisesRegex(ValueError, 'differs from stage'):
            check_bundle(self.stage, self.bundle)

    def test_packaged_file_association(self):
        package = {'version': '1.3.0-dev', 'build': {'mac': {'fileAssociations': [
            {'ext': 'studio', 'role': 'Editor', 'rank': 'Alternate'}]}}}
        (self.stage/'package.json').write_text(json.dumps(package))
        with self.assertRaisesRegex(ValueError, 'association'):
            check_bundle(self.stage, self.bundle)
        self.info['CFBundleDocumentTypes'] = [{'CFBundleTypeExtensions': ['studio'],
                                             'CFBundleTypeRole': 'Editor', 'LSHandlerRank': 'Alternate'}]
        self.save_info()
        self.assertEqual(check_bundle(self.stage, self.bundle)[2], 1)

    def test_bundle_identity_version_and_path_rejected(self):
        for key, value in [('CFBundleIdentifier', 'com.statedu.studio'),
                           ('CFBundleShortVersionString', '0.0.0'),
                           ('CFBundleExecutable', '../outside')]:
            original = self.info[key]
            self.info[key] = value
            self.save_info()
            with self.assertRaises(ValueError):
                check_bundle(self.stage, self.bundle)
            self.info[key] = original

    def test_wrong_architecture_never_executes_R(self):
        with self.assertRaisesRegex(ValueError, 'arm64'):
            check_app(self.stage, self.bundle, lambda *a, **k: 'x86_64',
                      lambda *a, **k: self.fail('R should not run'))

    def test_external_runtime_dependency_blocks_R(self):
        def runner(args, **kwargs):
            return self.runner(args, **kwargs) if args[0] == 'lipo' else (
                'Load command 0\n cmd LC_LOAD_DYLIB\n name /opt/R/lib.dylib (offset 24)\n'
                'Load command 1\n cmd LC_BUILD_VERSION\n minos 13.0\n')
        report = check_app(self.stage, self.bundle, runner,
                           lambda *a, **k: self.fail('R should not run'))
        self.assertEqual(report['status'], 'blocked')

    def test_R_failure_is_reported(self):
        report = check_app(self.stage, self.bundle, self.runner,
                           lambda *a, **k: subprocess.CompletedProcess(a[0], 1, '', 'missing package'))
        self.assertEqual(report['status'], 'blocked')
        self.assertIn('missing package', report['r_output_tail'])


if __name__ == '__main__':
    unittest.main()
