"""Verify Mac staging works with no Windows packaging directory present."""
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch
import prepare_macos as prep


class SeparationTests(unittest.TestCase):
    def test_staging_without_windows_files(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary) / "repo"
            mac = root / "packaging/macos"
            (mac / "build").mkdir(parents=True)
            template = json.loads((prep.ROOT / "packaging/macos/package.json").read_text())
            (mac / "package.json").write_text(json.dumps(template))
            for name in ("main.js", "preload.js", "build/icon.png", "package-lock.json", "build.command", "PREPARATION_README_KO.md"):
                (mac / name).write_bytes((prep.ROOT / "packaging/macos" / name).read_bytes())
            (root / "scripts").mkdir()
            for name in prep.TOOLS:
                (root / "scripts" / name).write_bytes((prep.ROOT / "scripts" / name).read_bytes())
            (root / "docs/i18n").mkdir(parents=True)
            (root / "docs/i18n/document_specs.json").write_text('{}')
            for name in ("app.R", "run_app.R", "LICENSE", "SOURCE-OFFER.txt", "VERSION",
                         "docs/ANALYSIS_REFERENCE_COMPARISON_PUBLIC.md",
                         "docs/ANALYSIS_REFERENCE_COMPARISON_PUBLIC_KO.md"):
                (root / name).write_text('1.3.0' if name == 'VERSION' else 'fixture')
            stage = Path(temporary) / "Mac stage 한글"
            with patch.object(prep, "ROOT", root), patch.object(prep.subprocess, "check_output", return_value=b''):
                prep.prepare(stage)
                for name in ("main.js", "preload.js", "build/icon.png"):
                    self.assertEqual((stage / name).read_bytes(), (mac / name).read_bytes())
                actual = json.loads((stage / "package.json").read_text())
                self.assertEqual(actual['build'], template['build'])
                self.assertEqual(actual['devDependencies'], template['devDependencies'])
                self.assertFalse((root / 'packaging/electron').exists())
                self.assertGreater(prep.verify_stage(stage), 0)
                (stage/'app/app.R').write_text('changed')
                with self.assertRaisesRegex(ValueError, 'missing or changed'):
                    prep.verify_stage(stage)
                with self.assertRaises(ValueError):
                    prep.prepare(stage)
                for target in (root/'packaging/electron/new', root/'dist/electron/new'):
                    with self.assertRaises(ValueError):
                        prep.prepare(target)
                    self.assertFalse(target.exists())
                actual['build']['appId'] = 'com.statedu.studio'
                (stage/'package.json').write_text(json.dumps(actual))
                with self.assertRaisesRegex(ValueError, 'Not a StatEdu macOS'):
                    prep.check_runtime(stage)


if __name__ == '__main__':
    unittest.main()
