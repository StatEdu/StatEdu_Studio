"""Prerequisite diagnostics with mocked Mac commands."""
import json
from pathlib import Path
import subprocess
import tempfile
import unittest

from doctor_macos import inspect_environment


class DoctorTests(unittest.TestCase):
    def setUp(self):
        temp = tempfile.TemporaryDirectory()
        self.addCleanup(temp.cleanup)
        self.stage = Path(temp.name)
        self.runtime = self.stage/'runtime/R.framework/Resources/bin/Rscript'
        self.runtime.parent.mkdir(parents=True)
        self.runtime.touch()

    def runner(self, args, **kwargs):
        output = json.dumps(dict(version='v22.12.0', arch='arm64', platform='darwin')) if args[0] == 'node' else 'available'
        return subprocess.CompletedProcess(args, 0, output, '')

    def inspect(self, **kwargs):
        defaults = dict(system='Darwin', machine='arm64', mac_version='13.0', python_version=(3, 10, 0), run=self.runner)
        defaults.update(kwargs)
        return inspect_environment(self.stage, **defaults)

    def test_supported_environment(self):
        report = self.inspect()
        self.assertEqual(report['status'], 'passed')
        self.assertTrue(all(c['passed'] for c in report['checks']))
        lock = json.loads((Path(__file__).resolve().parents[1]/'packaging/macos/package-lock.json').read_text())
        self.assertEqual(lock['packages']['node_modules/electron']['engines']['node'], '>= 22.12.0')

    def test_nonmac_executes_no_commands(self):
        report = self.inspect(system='Windows', machine='AMD64', run=lambda *a, **k: self.fail('must not run Mac tools'))
        self.assertEqual(report['status'], 'blocked')

    def test_old_or_translated_environment(self):
        for overrides in (dict(machine='x86_64'), dict(mac_version='12.7'), dict(python_version=(3, 9, 9))):
            self.assertEqual(self.inspect(**overrides)['status'], 'blocked')
        for version, arch in [('v22.11.0', 'arm64'), ('v22.12.0', 'x64')]:
            def run(args, **kwargs):
                if args[0] == 'node':
                    return subprocess.CompletedProcess(args, 0, json.dumps(dict(version=version, arch=arch, platform='darwin')), '')
                return self.runner(args, **kwargs)
            self.assertEqual(self.inspect(run=run)['status'], 'blocked')

    def test_missing_tools_are_reported_together(self):
        self.runtime.unlink()
        def run(args, **kwargs):
            if args[0] == 'node':
                raise FileNotFoundError('node unavailable')
            if args[0] == 'npm':
                raise subprocess.TimeoutExpired(args, 15)
            return self.runner(args, **kwargs)
        report = self.inspect(run=run)
        failed = [c['name'] for c in report['checks'] if not c['passed']]
        self.assertEqual(failed, ['node', 'npm', 'staged_Rscript'])
        self.assertTrue(all(c['action'] for c in report['checks'] if not c['passed']))

    def test_malformed_node_output(self):
        def run(args, **kwargs):
            return subprocess.CompletedProcess(args, 0, 'not JSON', '') if args[0] == 'node' else self.runner(args, **kwargs)
        self.assertEqual(self.inspect(run=run)['status'], 'blocked')


if __name__ == '__main__':
    unittest.main()
