import tempfile
from pathlib import Path
import unittest
from macos_runtime import audit_framework, copy_framework, dependency_issue, parse_load_commands, version_tuple, runtime_environment

LOADS = '''Load command 0
 cmd LC_ID_DYLIB
 name /Library/Frameworks/R.framework/libR.dylib (offset 24)
Load command 1
 cmd LC_LOAD_DYLIB
 name /usr/lib/libSystem.B.dylib (offset 24)
Load command 2
 cmd LC_RPATH
 path @loader_path/../lib (offset 12)
Load command 3
 cmd LC_BUILD_VERSION
 platform 1
 minos 11.0
 sdk 13.3
'''


class RuntimeTests(unittest.TestCase):
    def test_runtime_environment(self):
        source = dict(R_PROFILE_USER='/outside', R_LIBS='/outside', R_ARCH='x86_64',
                      DYLD_LIBRARY_PATH='/outside', LD_PRELOAD='/outside', PATH='/opt/homebrew',
                      HOME='/Users/user', STATEDU_CHROME='/custom/Chrome')
        before = source.copy()
        home = Path('/stage/space 한글/R.framework/Resources')
        env = runtime_environment(home, source)
        self.assertEqual(source, before)
        for key in ('R_PROFILE_USER', 'R_ARCH', 'DYLD_LIBRARY_PATH', 'LD_PRELOAD'):
            self.assertNotIn(key, env)
        for key in ('R_LIBS', 'R_LIBS_SITE', 'R_LIBS_USER'):
            self.assertEqual(env[key], str(home/'library'))
        self.assertEqual(env['PATH'], str(home/'bin') + ':/usr/bin:/bin:/usr/sbin:/sbin')
        self.assertEqual(env['HOME'], source['HOME'])
        self.assertEqual(env['STATEDU_CHROME'], source['STATEDU_CHROME'])
        self.assertEqual(env['R_DOC_DIR'], str(home/'doc'))

    def test_parser_distinguishes_identity_and_dependency(self):
        deps, paths, versions = parse_load_commands(LOADS)
        self.assertEqual(deps, ['/usr/lib/libSystem.B.dylib'])
        self.assertEqual(paths, ['@loader_path/../lib'])
        self.assertEqual(versions, ['11.0'])
        self.assertEqual(version_tuple('13'), version_tuple('13.0.0'))

    def test_dependencies(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder).resolve()
            (root/'lib').mkdir(); (root/'bin').mkdir()
            (root/'lib/test.dylib').touch()
            binary = root/'bin/Rscript'
            self.assertIsNone(dependency_issue('@loader_path/../lib/test.dylib', binary, root))
            for value in ('@loader_path/../../outside', '@rpath/test.dylib', '/opt/R/lib/test.dylib'):
                self.assertIsNotNone(dependency_issue(value, binary, root))

    def test_audit_blocks_wrong_arch_os_external_and_windows(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            (root/'Rscript').write_bytes(bytes.fromhex('cffaedfe') + b'fixture')
            runner = lambda args, **kwargs: 'arm64' if args[0] == 'lipo' else LOADS
            self.assertEqual(audit_framework(root, runner)['status'], 'passed')
            wrong_arch = lambda args, **kwargs: 'x86_64' if args[0] == 'lipo' else LOADS
            self.assertEqual(audit_framework(root, wrong_arch)['status'], 'blocked')
            bad = LOADS.replace('minos 11.0', 'minos 14.0').replace('/usr/lib/', '/opt/R/')
            bad_runner = lambda args, **kwargs: 'arm64' if args[0] == 'lipo' else bad
            self.assertEqual(len(audit_framework(root, bad_runner)['errors']), 2)
            (root/'foreign.dll').write_bytes(b'MZfixture')
            self.assertEqual(audit_framework(root, runner)['status'], 'blocked')

    def test_copy_preserves_source_and_refuses_overwrite(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            source = root/'source/R.framework'
            (source/'Resources/bin').mkdir(parents=True)
            (source/'Resources/bin/Rscript').write_bytes(b'fixture')
            destination = root/'stage/R.framework'
            copy_framework(source, destination)
            self.assertEqual((destination/'Resources/bin/Rscript').read_bytes(), b'fixture')
            self.assertEqual((source/'Resources/bin/Rscript').read_bytes(), b'fixture')
            with self.assertRaises(ValueError):
                copy_framework(source, destination)

    def test_internal_and_external_symlinks(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            source = root/'source/R.framework'
            real = source/'Versions/4.5-arm64/Resources'
            (real/'bin').mkdir(parents=True)
            (real/'bin/Rscript').write_bytes(b'fixture')
            try:
                (source/'Resources').symlink_to(real, target_is_directory=True)
            except OSError:
                self.skipTest('Host cannot create symlinks')
            destination = root/'stage/R.framework'
            copy_framework(source, destination)
            self.assertEqual((destination/'Resources').resolve(), destination/'Versions/4.5-arm64/Resources')
            self.assertTrue((source/'Resources').is_symlink())
            (source/'escape').symlink_to(root, target_is_directory=True)
            with self.assertRaisesRegex(ValueError, 'External symlink'):
                copy_framework(source, root/'second/R.framework')


if __name__ == '__main__':
    unittest.main()
