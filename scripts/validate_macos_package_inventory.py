from pathlib import Path
import tempfile
import unittest

from check_macos_packages import check_packages, expected_packages


class InventoryTests(unittest.TestCase):
    def test_real_contract_has_72_packages(self):
        expected = expected_packages(Path(__file__).with_name('bundled_validation_packages.expected.csv'))
        self.assertEqual(len(expected), 72)
        self.assertEqual(expected['cSEM'], '0.6.1')

    def test_missing_mismatch_and_invalid_all_reported(self):
        with tempfile.TemporaryDirectory() as directory:
            library = Path(directory)/'한글 library'
            for package, text in {'good': 'Package: good\nVersion: 1.0\nDescription: test\n continuation\n',
                                  'old': 'Package: old\nVersion: 0.9\n',
                                  'bad': 'Package: another\nVersion: 1.0\n'}.items():
                folder = library/package
                folder.mkdir(parents=True)
                (folder/'DESCRIPTION').write_text(text, encoding='utf-8')
            before = {p: p.read_bytes() for p in library.rglob('DESCRIPTION')}
            report = check_packages(library, dict(good='1.0', old='1.0', bad='1.0', missing='1.0'))
            self.assertEqual(report['status'], 'blocked')
            self.assertEqual([p['status'] for p in report['packages']], ['matched', 'version_mismatch', 'invalid', 'missing'])
            self.assertEqual(before, {p: p.read_bytes() for p in before})
            self.assertEqual(check_packages(library, {'good': '1.0'})['status'], 'passed')

    def test_invalid_or_duplicate_csv_rejected(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory)/'lock.csv'
            for text in ('name,version\na,1\n', 'Package,Version\n',
                         'Package,Version\na,1\na,2\n', 'Package,Version\n../escape,1\n'):
                path.write_text(text, encoding='utf-8')
                with self.assertRaises(ValueError):
                    expected_packages(path)

    def test_duplicate_description_field_rejected(self):
        with tempfile.TemporaryDirectory() as directory:
            library = Path(directory)
            (library/'pkg').mkdir()
            (library/'pkg/DESCRIPTION').write_text('Package: pkg\nVersion: 1\nVersion: 2\n')
            self.assertEqual(check_packages(library, {'pkg': '1'})['packages'][0]['status'], 'invalid')


if __name__ == '__main__':
    unittest.main()
