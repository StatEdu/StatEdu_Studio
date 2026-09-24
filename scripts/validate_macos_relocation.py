"""Host-independent relocation safety tests; Apple tools are mocked."""
import hashlib
from pathlib import Path
import subprocess
import tempfile
import unittest

from relocate_macos_runtime import (HOME_ASSIGNMENT, apply_plan,
                                    map_framework_dependency, patch_r_launcher,
                                    relocation_plan, map_executable_rpath)

PREFIX = '/Library/Frameworks/R.framework'
LAUNCHER = '#!/bin/sh\nR_HOME_DIR=/Library/Frameworks/R.framework/Resources\nexport R_HOME_DIR\n'


class RelocationTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.base = Path(self.temp.name).resolve()
        self.root = self.base/'한글 space/R.framework'
        (self.root/'Resources/bin').mkdir(parents=True)
        (self.root/'Resources/lib').mkdir()
        self.launcher = self.root/'Resources/bin/R'
        self.launcher.write_text(LAUNCHER, encoding='utf-8')
        self.binary = self.root/'Resources/bin/Rscript'
        self.binary.write_bytes(bytes.fromhex('cffaedfe') + b'fixture')
        (self.root/'Resources/lib/libR.dylib').write_bytes(b'fixture')

    def runner(self, args, **kwargs):
        if args[0] == 'lipo':
            return 'arm64'
        return f'Load command 0\n cmd LC_LOAD_DYLIB\n name {PREFIX}/Resources/lib/libR.dylib (offset 24)\n'

    def test_mapping_and_boundaries(self):
        def mapped(value):
            return map_framework_dependency(value, self.binary, self.root, [PREFIX])
        self.assertEqual(mapped(PREFIX+'/Resources/lib/libR.dylib'), '@loader_path/../lib/libR.dylib')
        self.assertIsNone(mapped(PREFIX+'-other/Resources/lib/libR.dylib'))
        self.assertIsNone(mapped('/opt/R/lib/external.dylib'))
        with self.assertRaises(ValueError):
            mapped(PREFIX+'/../outside')
        with self.assertRaises(FileNotFoundError):
            mapped(PREFIX+'/Resources/lib/missing.dylib')

    def test_launcher(self):
        updated = patch_r_launcher(LAUNCHER)
        self.assertIn(HOME_ASSIGNMENT, updated)
        self.assertTrue(updated.endswith('export R_HOME_DIR\n'))
        self.assertEqual(patch_r_launcher(updated), updated)
        for text in ('R_HOME_DIR=bad', '#!/bin/sh\nunknown\n', LAUNCHER+'R_HOME_DIR=second\n'):
            with self.assertRaises(ValueError):
                patch_r_launcher(text)

    def executable_header(self, kind=2):
        self.binary.write_bytes(bytes.fromhex('cffaedfe') + bytes(8) + kind.to_bytes(4, 'little'))

    def test_executable_rpath_unique_internal_target(self):
        self.executable_header()
        for rpaths in (['@loader_path/../lib'], ['@executable_path/../lib'],
                       [PREFIX+'/Resources/lib'], ['@loader_path/../lib', '@executable_path/../lib']):
            self.assertEqual(map_executable_rpath('@rpath/libR.dylib', self.binary, self.root,
                                                 [PREFIX], rpaths), '@loader_path/../lib/libR.dylib')

    def test_rpath_missing_external_and_ambiguous(self):
        self.executable_header()
        for rpaths in ([], ['@loader_path/../missing'], ['/opt/R/lib'],
                       ['@loader_path/../lib', '/opt/R/lib'], ['@loader_path/../../../../outside']):
            with self.assertRaises(ValueError):
                map_executable_rpath('@rpath/libR.dylib', self.binary, self.root, [PREFIX], rpaths)
        (self.root/'Resources/bin/libR.dylib').write_bytes(b'other')
        with self.assertRaisesRegex(ValueError, '2 found'):
            map_executable_rpath('@rpath/libR.dylib', self.binary, self.root,
                                 [PREFIX], ['@loader_path', '@loader_path/../lib'])
        with self.assertRaisesRegex(ValueError, 'Invalid'):
            map_executable_rpath('@rpath/../libR.dylib', self.binary, self.root, [PREFIX], [])

    def test_dylib_rpath_is_not_guessed(self):
        self.executable_header(kind=6)
        self.assertIsNone(map_executable_rpath('@rpath/libR.dylib', self.binary, self.root,
                                              [PREFIX], ['@loader_path/../lib']))

    def test_rpath_plan_is_readonly(self):
        self.executable_header()
        original = self.binary.read_bytes()
        def runner(args, **kwargs):
            return 'arm64' if args[0] == 'lipo' else (
                'Load command 0\n cmd LC_LOAD_DYLIB\n name @rpath/libR.dylib (offset 24)\n'
                'Load command 1\n cmd LC_RPATH\n path @loader_path/../lib (offset 12)\n')
        plan = relocation_plan(self.root, [PREFIX], runner)
        self.assertEqual(plan[0]['changes'], [['@rpath/libR.dylib', '@loader_path/../lib/libR.dylib']])
        self.assertEqual(self.binary.read_bytes(), original)

    def test_plan_is_readonly_and_rejects_universal(self):
        before = self.binary.read_bytes(), self.launcher.read_bytes()
        edits = relocation_plan(self.root, [PREFIX], self.runner)
        self.assertEqual(len(edits), 2)
        self.assertEqual(before, (self.binary.read_bytes(), self.launcher.read_bytes()))
        def universal(args, **kwargs):
            return 'x86_64 arm64' if args[0] == 'lipo' else self.runner(args, **kwargs)
        with self.assertRaisesRegex(ValueError, 'arm64-only'):
            relocation_plan(self.root, [PREFIX], universal)

    def test_apply_and_backup(self):
        edits = relocation_plan(self.root, [PREFIX], self.runner)
        calls = []
        original = self.binary.read_bytes()
        def run(args, **kwargs):
            calls.append(args)
            if args[0] == 'install_name_tool':
                Path(args[-1]).write_bytes(original + b'changed')
        backup = self.base/'backup'
        apply_plan(self.root, edits, backup, run)
        self.assertEqual([c[0] for c in calls], ['install_name_tool', 'codesign', 'codesign'])
        self.assertEqual(calls[0][1:4], ['-change', PREFIX+'/Resources/lib/libR.dylib', '@loader_path/../lib/libR.dylib'])
        self.assertEqual((backup/'Resources/bin/Rscript').read_bytes(), original)
        self.assertIn(HOME_ASSIGNMENT, self.launcher.read_text(encoding='utf-8'))

    def test_failure_restores_all_files(self):
        edits = relocation_plan(self.root, [PREFIX], self.runner)
        edits.reverse()  # Ensure a text change also occurs before the binary failure.
        originals = {e['path']: (self.root/e['path']).read_bytes() for e in edits}
        def fail(args, **kwargs):
            if args[0] == 'install_name_tool':
                Path(args[-1]).write_bytes(b'modified')
            else:
                raise subprocess.CalledProcessError(1, args)
        with self.assertRaises(subprocess.CalledProcessError):
            apply_plan(self.root, edits, self.base/'backup', fail)
        for path, content in originals.items():
            self.assertEqual((self.root/path).read_bytes(), content)

    def test_stale_plan_and_escaping_target_rejected_before_write(self):
        edits = relocation_plan(self.root, [PREFIX], self.runner)
        self.binary.write_bytes(b'changed since plan')
        backup = self.base/'backup'
        with self.assertRaisesRegex(ValueError, 'changed after planning'):
            apply_plan(self.root, edits, backup)
        self.assertFalse(backup.exists())
        outside = self.base/'outside'
        outside.write_bytes(b'original')
        edit = {'path': '../../outside', 'sha256': hashlib.sha256(b'original').hexdigest(), 'text': 'bad'}
        with self.assertRaisesRegex(ValueError, 'escapes framework'):
            apply_plan(self.root, [edit], backup)
        self.assertEqual(outside.read_bytes(), b'original')


if __name__ == '__main__':
    unittest.main()
