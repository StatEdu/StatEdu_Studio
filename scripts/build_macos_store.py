"""Build isolated MAS development/distribution or local sandbox validation artifacts."""
import argparse
import datetime
import hashlib
import json
import os
from pathlib import Path
import plistlib
import re
import subprocess
from prepare_macos import assert_macos_destination, check_runtime, verify_stage
from verify_macos_app import check_bundle
from prepare_macos_store_runtime import prepare as prepare_store_runtime


def valid_identity(team, prefixes, policy='codesigning'):
    text = subprocess.check_output(['security','find-identity','-v','-p',policy],text=True)
    identities = re.findall(r'^\s*\d+\)\s+([0-9A-Fa-f]{40})\s+"([^"]+)"\s*$',text,re.MULTILINE)
    matches = [(digest,name) for digest,name in identities
               if any(name.startswith(prefix + ': ') for prefix in prefixes) and name.endswith(f'({team})')]
    if len(matches) != 1:
        raise ValueError(f'Expected one valid {" / ".join(prefixes)} certificate with private key for team {team}; found {len(matches)}')
    return matches[0]


def validate_profile(profile, team, bundle_id, identity_hash, mode, now=None):
    now = now or datetime.datetime.now(datetime.timezone.utc).replace(tzinfo=None)
    if profile.get('TeamIdentifier') != [team]:
        raise ValueError('Provisioning profile belongs to a different team')
    if not isinstance(profile.get('ExpirationDate'),datetime.datetime) or profile['ExpirationDate'] <= now:
        raise ValueError('Provisioning profile has expired or lacks an expiration date')
    entitlements = profile.get('Entitlements',{})
    if entitlements.get('com.apple.application-identifier') != f'{team}.{bundle_id}':
        raise ValueError('Provisioning profile App ID differs from Studio (Sync profiles are not accepted)')
    if entitlements.get('com.apple.developer.team-identifier') != team:
        raise ValueError('Provisioning profile entitlement team differs')
    if 'OSX' not in profile.get('Platform',[]):
        raise ValueError('A macOS provisioning profile is required')
    certificates = [hashlib.sha1(data).hexdigest().upper() for data in profile.get('DeveloperCertificates',[])]
    if identity_hash.upper() not in certificates:
        raise ValueError('Provisioning profile does not authorize the selected signing certificate')
    development = bool(entitlements.get('get-task-allow') or entitlements.get('com.apple.security.get-task-allow'))
    if (mode == 'development') != development:
        raise ValueError('Provisioning profile development/distribution type differs from build mode')
    if mode == 'development' and not profile.get('ProvisionedDevices'):
        raise ValueError('Development provisioning profile must include registered Macs')
    if mode == 'distribution' and (profile.get('ProvisionedDevices') or profile.get('ProvisionsAllDevices')):
        raise ValueError('App Store distribution profile required')
    return {'name':profile['Name'],'uuid':profile['UUID'],'expires':profile['ExpirationDate'].isoformat()}


def build(stage, mode, profile_path, team, check_only=False):
    stage = assert_macos_destination(stage)
    verify_stage(stage)
    package = json.loads((stage/'package.json').read_text())
    if package['version'] != '1.3.1' or (stage/'app/VERSION').read_text().strip() != '1.3.1':
        raise ValueError('Only the official public 1.3.1 release is supported')
    bundle_id = package['build']['appId']
    prefixes = {'distribution':['Apple Distribution','3rd Party Mac Developer Application'],
                'development':['Apple Development','Mac Developer'],
                'sandbox':['Developer ID Application']}[mode]
    identity_hash, identity_name = valid_identity(team,prefixes)
    profile_info = None
    if mode != 'sandbox':
        if not profile_path:
            raise ValueError('Supply --profile for the exact Studio App ID')
        profile_path = Path(profile_path).resolve(strict=True)
        profile = plistlib.loads(subprocess.check_output(['security','cms','-D','-i',str(profile_path)]))
        profile_info = validate_profile(profile,team,bundle_id,identity_hash,mode)
    qualifier = identity_name.split(': ',1)[1]
    installer = None
    if mode == 'distribution':
        installer = valid_identity(team,['3rd Party Mac Developer Installer'],'basic')
        if installer[1].split(': ',1)[1] != qualifier:
            raise ValueError('Application and installer certificates must have the same team qualifier')
    if check_only:
        return {'status':'passed','mode':mode,'identity':identity_name,'profile':profile_info,
                'installer_identity':installer[1] if installer else None}
    openmp=prepare_store_runtime(stage)
    check_runtime(stage)
    base = package['build']
    override = json.loads((stage/'mas.json').read_text())
    target = 'mas-dev' if mode == 'development' else 'mas'
    config = {**base,**override,'mac':{**base['mac'],**override['mac'],'target':[{'target':target,'arch':['arm64']}]},
              'mas':{**base['mac'],**override['mas'],'identity':qualifier,'provisioningProfile':str(profile_path) if profile_path else None},
              'directories':{**base['directories'],'output':f'mas-{mode}-dist'},'forceCodeSigning':mode != 'sandbox'}
    config['mas'].pop('target',None)
    config['mas'].pop('notarize',None)
    config['mas']['extendInfo'] = {**base['mac'].get('extendInfo',{}),'ElectronTeamID':team}
    if mode == 'development':
        config['mas']['type']='development'
        config['masDev']={**config['mas'],'type':'development'}
    elif mode == 'sandbox':
        config['mas']['identity']=None
        entitlements = plistlib.loads((stage/'build/entitlements.mas.plist').read_bytes())
        entitlements['com.apple.security.application-groups']=[f'{team}.{bundle_id}']
        (stage/'build/entitlements.mas.local.plist').write_bytes(plistlib.dumps(entitlements))
    config_path=stage/f'mas-{mode}-effective.json'
    config_path.write_text(json.dumps(config,indent=2)+'\n')
    env=dict(os.environ,STATEDU_MAC_SIGN_IDENTITY=identity_hash)
    subprocess.run(['npm','ci','--no-audit','--no-fund'],cwd=stage,env=env,check=True)
    subprocess.run(['npx','--no-install','electron-builder','--config',str(config_path),'--mac',target,'--arm64','--publish','never'],cwd=stage,env=env,check=True)
    bundle=stage/config['directories']['output']/f'{target}-arm64/StatEdu Studio.app'
    if mode == 'sandbox':
        subprocess.run(['node',str(stage/'mas-local-sign.cjs'),str(bundle),str(stage)],cwd=stage,env=env,check=True)
    check_bundle(stage,bundle)
    subprocess.run(['codesign','--verify','--deep','--strict',str(bundle)],check=True)
    effective_entitlements=plistlib.loads(subprocess.check_output(['codesign','-d','--entitlements',':-',str(bundle)],stderr=subprocess.DEVNULL))
    if effective_entitlements.get('com.apple.security.app-sandbox') is not True:
        raise ValueError('MAS app sandbox entitlement is missing')
    if mode == 'distribution' and effective_entitlements.get('com.apple.security.get-task-allow'):
        raise ValueError('Debug entitlement in distribution package')
    result={'status':'built','mode':mode,'version':'1.3.1','app_id':bundle_id,'identity':identity_name,
            'profile':profile_info,'sandbox_entitlements':effective_entitlements,
            'openmp_runtime':openmp,'strict_signature_verified':True,'gui_verified':False,'app_store_uploaded':False,'app':str(bundle)}
    if mode=='distribution':
        pkgs=list(bundle.parent.glob('*.pkg'))
        if len(pkgs)!=1: raise ValueError('Expected one signed MAS installer package')
        check=subprocess.check_output(['pkgutil','--check-signature',str(pkgs[0])],text=True)
        if installer[1] not in check: raise ValueError('Installer signature does not match the expected identity')
        result.update(package=str(pkgs[0]),package_sha256=hashlib.sha256(pkgs[0].read_bytes()).hexdigest())
    (stage/f'mas-{mode}-verification.json').write_text(json.dumps(result,indent=2)+'\n')
    return result


if __name__ == '__main__':
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--stage',required=True)
    parser.add_argument('--mode',choices=['development','distribution','sandbox'],required=True)
    parser.add_argument('--profile')
    parser.add_argument('--team',required=True)
    parser.add_argument('--check-only',action='store_true')
    args=parser.parse_args()
    try:
        print(json.dumps(build(Path(args.stage).resolve(),args.mode,args.profile,args.team,args.check_only),indent=2))
    except (ValueError,FileNotFoundError) as error:
        parser.exit(1,str(error)+'\n')
