"""Regression checks for the three causes of the first MAS upload rejection."""
from pathlib import Path
import plistlib
import struct
import tempfile
import unittest
from unittest.mock import patch
from prepare_macos_store_bundle import prepare_framework, validate_bundle, validate_icon_source


class StoreBundleTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.otool = patch('prepare_macos_store_bundle.subprocess.check_output', return_value='')
        self.commands = patch('prepare_macos_store_bundle.subprocess.run')
        self.otool.start(); self.commands.start()
        self.addCleanup(self.otool.stop); self.addCleanup(self.commands.stop)
        self.bundle = Path(self.directory.name) / 'Studio.app'
        resources = self.bundle / 'Contents/Resources'
        resources.mkdir(parents=True)
        (self.bundle / 'Contents/Info.plist').write_bytes(plistlib.dumps({
            'CFBundleIconFile': 'icon.icns', 'CFBundleVersion': '2'}))
        self.icon = resources / 'icon.icns'
        self.set_icon(b'ic10')
        self.framework = resources / 'app.asar.unpacked/runtime/R.framework'
        version = self.framework / 'Versions/4.5-arm64'
        home = version / 'Resources'
        (home / 'lib').mkdir(parents=True)
        self.binary = home / 'lib/libR.dylib'
        self.binary.write_bytes(bytes.fromhex('cffaedfe') + b'unchanged runtime')
        (version / 'R').symlink_to('Resources/lib/libR.dylib')
        (self.framework / 'Versions/Current').symlink_to('4.5-arm64')
        (self.framework / 'Resources').symlink_to('Versions/Current/Resources')
        (self.framework / 'R').symlink_to('Versions/Current/R')
        self.info = home / 'Info.plist'
        self.info.write_bytes(plistlib.dumps({'CFBundleIdentifier': 'org.r-project.R-framework',
                                             'CFBundlePackageType': 'FMWK', 'CFBundleVersion': '4.5.3'}))

    def set_icon(self, kind):
        entry = kind + struct.pack('>I', 12) + b'data'
        self.icon.write_bytes(b'icns' + struct.pack('>I', len(entry) + 8) + entry)

    def test_missing_framework_executable_is_rejected_then_fixed_without_binary_changes(self):
        with self.assertRaisesRegex(ValueError, 'CFBundleExecutable'):
            validate_bundle(self.bundle)
        original = self.binary.read_bytes()
        prepare_framework(self.framework)
        self.assertEqual(validate_bundle(self.bundle)['framework_executable'], 'R')
        self.assertEqual(self.binary.read_bytes(), original)
        self.assertTrue(self.binary.is_symlink())
        self.assertFalse((self.framework / 'Versions/4.5-arm64/R').is_symlink())
        self.assertEqual(plistlib.loads(self.info.read_bytes())['CFBundleVersion'], '4.5.3')

    def test_debug_payload_is_rejected_and_removed_with_runtime_preserved(self):
        debug = self.binary.with_name('libR.dylib.dSYM')
        (debug / 'Contents').mkdir(parents=True)
        (debug / 'Contents/Info.plist').write_bytes(plistlib.dumps({
            'CFBundleIdentifier': 'com.apple.xcode.dsym.org.r-project.R-framework'}))
        with self.assertRaisesRegex(ValueError, 'dSYM'):
            validate_bundle(self.bundle)
        original = self.binary.read_bytes()
        result = prepare_framework(self.framework)
        self.assertEqual(result['removed_debug_bundle_count'], 1)
        self.assertFalse(debug.exists())
        self.assertEqual(self.binary.read_bytes(), original)
        self.assertEqual(validate_bundle(self.bundle)['debug_bundle_count'], 0)

    def test_missing_retina_icon_is_rejected(self):
        prepare_framework(self.framework)
        self.set_icon(b'ic09')
        with self.assertRaisesRegex(ValueError, '512pt @2x'):
            validate_bundle(self.bundle)
        self.set_icon(b'ic10')
        self.assertTrue(validate_bundle(self.bundle)['icns_512pt_2x'])

    def test_framework_relocation_keeps_library_dependencies_inside_runtime(self):
        dependency = self.binary.parent / 'libRblas.0.dylib'
        dependency.write_bytes(b'unchanged BLAS')
        output = 'Load command 0\n cmd LC_LOAD_DYLIB\n name @loader_path/libRblas.0.dylib (offset 24)\n'
        with patch('prepare_macos_store_bundle.subprocess.check_output', return_value=output):
            report = prepare_framework(self.framework)
        self.assertEqual(report['framework_executable_dependencies'], [{
            'from': '@loader_path/libRblas.0.dylib',
            'to': '@loader_path/Resources/lib/libRblas.0.dylib'}])
        self.assertEqual(dependency.read_bytes(), b'unchanged BLAS')
        self.assertTrue((self.framework / 'Versions/4.5-arm64/R').is_file())

    def test_source_icon_must_supply_retina_resolution(self):
        icon = Path(self.directory.name) / 'source.png'
        for width in (512, 1254):
            icon.write_bytes(b'\x89PNG\r\n\x1a\n' + struct.pack('>I', 13) + b'IHDR' +
                             struct.pack('>II', width, width))
            if width == 512:
                with self.assertRaisesRegex(ValueError, '1024'):
                    validate_icon_source(icon)
            else:
                self.assertEqual(validate_icon_source(icon)['width'], 1254)


if __name__ == '__main__':
    unittest.main()
