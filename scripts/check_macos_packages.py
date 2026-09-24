"""Compare the staged R validation library to exact versions, without executing R."""
import argparse
import csv
import json
from pathlib import Path
import re

from prepare_macos import assert_macos_destination, verify_stage


def expected_packages(lock):
    with Path(lock).open(encoding='utf-8-sig', newline='') as handle:
        reader = csv.DictReader(handle)
        if reader.fieldnames != ['Package', 'Version']:
            raise ValueError('Expected Package,Version CSV columns')
        expected = {}
        for row in reader:
            name, version = row['Package'], row['Version']
            if (not name or not re.fullmatch(r'[A-Za-z][A-Za-z0-9.]*', name)
                    or not version or version != version.strip() or name in expected or None in row):
                raise ValueError('Invalid or duplicate package/version row')
            expected[name] = version
    if not expected:
        raise ValueError('Empty validation package list')
    return expected


def check_packages(library, expected):
    library = Path(library).resolve()
    entries = []
    for name, version in expected.items():
        description = library/name/'DESCRIPTION'
        row = dict(package=name, expected=version, installed=None, status='missing')
        if description.is_file():
            try:
                if library not in description.resolve().parents:
                    raise ValueError('Package resolves outside staged library')
                fields = {}
                for line in description.read_text(encoding='utf-8-sig').splitlines():
                    if not line or line[0].isspace():
                        continue
                    key, separator, value = line.partition(':')
                    if separator and key in {'Package', 'Version'}:
                        if key in fields:
                            raise ValueError(f'Duplicate DESCRIPTION field: {key}')
                        fields[key] = value.strip()
                if fields.get('Package') != name or not fields.get('Version'):
                    raise ValueError('Package identity/version missing or mismatched')
                row.update(installed=fields['Version'], status='matched' if fields['Version'] == version else 'version_mismatch')
            except (OSError, ValueError) as error:
                row.update(status='invalid', detail=str(error))
        entries.append(row)
    return dict(schema=1, status='passed' if all(e['status'] == 'matched' for e in entries) else 'blocked',
                packages=entries, scope='Validation package versions only; no R loading, architecture, numerical or full dependency verification')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--stage', required=True)
    args = parser.parse_args()
    stage = assert_macos_destination(args.stage)
    verify_stage(stage)
    expected = expected_packages(stage/'tools/bundled_validation_packages.expected.csv')
    library = stage/'runtime/R.framework/Resources/library'
    if stage not in library.resolve().parents:
        raise ValueError('Library must remain within Mac preparation')
    report = check_packages(library, expected)
    target = stage/'macos-validation-packages.json'
    target.write_text(json.dumps(report, indent=2), encoding='utf-8')
    for entry in report['packages']:
        if entry['status'] != 'matched':
            print(f'{entry["package"]}: {entry["status"]} (expected {entry["expected"]}, installed {entry["installed"]})')
    print(f'{report["status"]}: {target}')
    raise SystemExit(0 if report['status'] == 'passed' else 1)


if __name__ == '__main__':
    main()
