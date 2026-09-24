"""Report Mac build prerequisites without installing or modifying system tools."""
import argparse
import json
from pathlib import Path
import platform
import re
import subprocess
import sys

from prepare_macos import assert_macos_destination, verify_stage


def inspect_environment(stage, system=None, machine=None, mac_version=None,
                        python_version=None, run=subprocess.run):
    checks = []
    def record(name, passed, detail, action):
        checks.append(dict(name=name, passed=bool(passed), detail=detail,
                           action='' if passed else action))
    system = platform.system() if system is None else system
    machine = platform.machine() if machine is None else machine
    record('macOS', system == 'Darwin', system, 'Run on an Apple Silicon Mac.')
    record('native_python_arch', machine == 'arm64', machine, 'Use native arm64 Python and terminal, without Rosetta.')
    version = python_version or sys.version_info[:3]
    record('python', tuple(version) >= (3, 10, 0), '.'.join(map(str, version)), 'Install Python 3.10 or newer.')
    if system != 'Darwin':
        return {'schema': 1, 'status': 'blocked', 'checks': checks,
                'scope': 'Mac-only checks were not run on this host.'}
    mac_version = platform.mac_ver()[0] if mac_version is None else mac_version
    parts = tuple(map(int, mac_version.split('.'))) if re.fullmatch(r'\d+(?:\.\d+)*', mac_version) else ()
    record('macOS_version', parts >= (13,), mac_version, 'Use macOS 13 or newer (target; not yet certified).')
    def command(name, args, action, validator=None):
        try:
            result = run(args, capture_output=True, text=True, timeout=15)
            output = (result.stdout + result.stderr).strip()
            passed = result.returncode == 0 and bool(output)
            if passed and validator:
                passed = validator(result.stdout.strip())
            record(name, passed, output[-1000:], action)
        except (OSError, subprocess.TimeoutExpired, ValueError, KeyError, TypeError) as error:
            record(name, False, str(error)[:1000], action)

    def valid_node(output):
        info = json.loads(output)
        version = tuple(map(int, info['version'].lstrip('v').split('.')))
        return info['arch'] == 'arm64' and info['platform'] == 'darwin' and version >= (22, 12, 0)
    command('node', ['node', '-p', 'JSON.stringify({version:process.version,arch:process.arch,platform:process.platform})'],
            'Install native arm64 Node.js >=22.12.0 (required by the pinned Electron package).', valid_node)
    command('npm', ['npm', '--version'], 'Make npm available in the same terminal.')
    command('developer_tools', ['xcode-select', '-p'], 'Install/select Xcode Command Line Tools.')
    for tool in ('lipo', 'otool', 'install_name_tool'):
        command(tool, ['xcrun', '--find', tool], 'Install/select Xcode Command Line Tools.')
    command('codesign', ['/usr/bin/codesign', '--version'], 'Verify macOS codesign is available.')
    runtime = Path(stage)/'runtime/R.framework/Resources/bin/Rscript'
    record('staged_Rscript', runtime.is_file(), 'present' if runtime.is_file() else 'missing',
           'Prepare and copy R 4.5.3 arm64 with tools/macos_runtime.py; then resolve audit findings.')
    return {'schema': 1, 'status': 'passed' if all(c['passed'] for c in checks) else 'blocked',
            'checks': checks, 'scope': 'Prerequisites only; no R package, runtime relocation, GUI, export or release certification.'}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--stage', required=True)
    args = parser.parse_args()
    stage = assert_macos_destination(args.stage)
    verify_stage(stage)
    report = inspect_environment(stage)
    report_path = stage/'macos-environment-report.json'
    report_path.write_text(json.dumps(report, indent=2), encoding='utf-8')
    for check in report['checks']:
        print(f'{"PASS" if check["passed"] else "BLOCKED"}: {check["name"]}'
              + (f' — {check["action"]}' if check['action'] else ''))
    print(f'Report: {report_path}')
    raise SystemExit(0 if report['status'] == 'passed' else 1)


if __name__ == '__main__':
    main()
