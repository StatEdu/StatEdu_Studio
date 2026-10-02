"""Build and verify signed, notarized StatEdu Studio 1.3.1 DMG/ZIP artifacts."""
import argparse
import json
import os
from pathlib import Path
import subprocess
import sys
from prepare_macos import assert_macos_destination, check_runtime, verify_stage
from verify_macos_app import check_app


def release(stage):
    stage = assert_macos_destination(stage)
    verify_stage(stage)
    identity = os.environ.get('STATEDU_MAC_SIGN_IDENTITY', '')
    profile = os.environ.get('APPLE_KEYCHAIN_PROFILE', '')
    if not identity or not profile:
        raise RuntimeError('Set STATEDU_MAC_SIGN_IDENTITY and APPLE_KEYCHAIN_PROFILE. '
                           'Import a Developer ID Application certificate and store notarytool credentials in Keychain first.')
    identities = subprocess.check_output(['security','find-identity','-v','-p','codesigning'],text=True)
    if identity not in identities or 'Developer ID Application' not in identity:
        raise RuntimeError('The requested valid Developer ID Application identity is unavailable in Keychain.')
    # Validate the profile without printing credentials or submitting artifacts.
    subprocess.run(['xcrun','notarytool','history','--keychain-profile',profile],check=True,capture_output=True)
    check_runtime(stage)
    package = json.loads((stage/'package.json').read_text())
    if package['version'] != '1.3.1' or (stage/'app/VERSION').read_text().strip() != '1.3.1':
        raise ValueError('Expected the 1.3.1 public release source')
    base = package['build']
    override = json.loads((stage/'release.json').read_text())
    config = {**base, **override, 'mac': {**base['mac'], **override['mac'], 'identity':identity},
              'forceCodeSigning': True, 'directories': {**base['directories'], 'output':'release-dist'}}
    config_path = stage/'release-effective.json'
    config_path.write_text(json.dumps(config,indent=2)+'\n')
    env = dict(os.environ)
    env.pop('CSC_IDENTITY_AUTO_DISCOVERY',None)
    subprocess.run(['npm','ci','--no-audit','--no-fund'],cwd=stage,env=env,check=True)
    subprocess.run(['npx','--no-install','electron-builder','--config',str(config_path),'--mac','--arm64','--publish','never'],
                   cwd=stage,env=env,check=True)
    bundle=stage/'release-dist/mac-arm64/StatEdu Studio.app'
    report=check_app(stage,bundle)
    if report['status'] != 'passed':
        raise RuntimeError('Signed application runtime preflight failed: '+str(report.get('errors')))
    subprocess.run(['codesign','--verify','--deep','--strict',str(bundle)],check=True)
    subprocess.run(['xcrun','stapler','validate',str(bundle)],check=True)
    subprocess.run(['spctl','--assess','--type','execute','--verbose',str(bundle)],check=True)
    dmgs=list((stage/'release-dist').glob('*.dmg'))
    if len(dmgs) != 1:
        raise RuntimeError('Expected exactly one release DMG')
    result=subprocess.check_output(['xcrun','notarytool','submit',str(dmgs[0]),'--keychain-profile',profile,
                                    '--wait','--output-format','json'],text=True)
    notarization=json.loads(result)
    (stage/'release-notarization.json').write_text(json.dumps(notarization,indent=2)+'\n')
    if notarization.get('status') != 'Accepted':
        raise RuntimeError('DMG notarization was not accepted; inspect release-notarization.json')
    subprocess.run(['xcrun','stapler','staple',str(dmgs[0])],check=True)
    subprocess.run(['xcrun','stapler','validate',str(dmgs[0])],check=True)
    report.update(signing_verified=True,app_staple_verified=True,gatekeeper_assessment=True,dmg_notarization=notarization)
    (stage/'release-verification.json').write_text(json.dumps(report,indent=2)+'\n')
    return dmgs[0]


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--stage',required=True)
    args=parser.parse_args()
    print(release(Path(args.stage)))


if __name__ == '__main__':
    try:
        main()
    except (ValueError, RuntimeError) as error:
        print(str(error),file=sys.stderr)
        raise SystemExit(1)
