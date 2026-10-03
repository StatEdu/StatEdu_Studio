"""Validate MAS profile boundaries with fixtures; actual sandbox execution is separate."""
import datetime
import hashlib
import plistlib
from pathlib import Path
import unittest
from build_macos_store import validate_profile

ROOT=Path(__file__).resolve().parents[1]


class StoreProfileTests(unittest.TestCase):
    def setUp(self):
        self.cert=b'fixture certificate'
        self.digest=hashlib.sha1(self.cert).hexdigest().upper()
        self.profile={'Name':'Studio fixture','UUID':'fixture','TeamIdentifier':['TEAM'],
                      'ExpirationDate':datetime.datetime(2030,1,1),'Platform':['OSX'],
                      'DeveloperCertificates':[self.cert],'Entitlements':{
                          'com.apple.application-identifier':'TEAM.com.statedu.studio.mac',
                          'com.apple.developer.team-identifier':'TEAM'}}

    def check(self, mode='distribution'):
        return validate_profile(self.profile,'TEAM','com.statedu.studio.mac',self.digest,mode,
                                datetime.datetime(2026,10,3))

    def test_exact_distribution_profile(self):
        self.assertEqual(self.check()['name'],'Studio fixture')

    def test_sync_profile_rejected(self):
        self.profile['Entitlements']['com.apple.application-identifier']='TEAM.com.statedu.sync'
        with self.assertRaisesRegex(ValueError,'App ID'): self.check()

    def test_expired_team_platform_certificate_rejected(self):
        for key,value in [('ExpirationDate',datetime.datetime(2020,1,1)),('TeamIdentifier',['OTHER']),
                          ('Platform',['iOS']),('DeveloperCertificates',[b'other'])]:
            original=self.profile[key];self.profile[key]=value
            with self.assertRaises(ValueError): self.check()
            self.profile[key]=original

    def test_development_is_not_distribution(self):
        self.profile['Entitlements']['get-task-allow']=True
        self.profile['ProvisionedDevices']=['fixture-Mac']
        with self.assertRaisesRegex(ValueError,'type'): self.check()
        self.assertEqual(self.check('development')['uuid'],'fixture')

    def test_distribution_cannot_authorize_arbitrary_devices(self):
        self.profile['ProvisionsAllDevices']=True
        with self.assertRaisesRegex(ValueError,'App Store'): self.check()

    def test_sandbox_entitlements_are_scoped(self):
        app=plistlib.loads((ROOT/'packaging/macos/build/entitlements.mas.plist').read_bytes())
        child=plistlib.loads((ROOT/'packaging/macos/build/entitlements.mas.inherit.plist').read_bytes())
        self.assertTrue(app['com.apple.security.app-sandbox'])
        self.assertTrue(app['com.apple.security.network.server'])
        self.assertTrue(app['com.apple.security.files.user-selected.read-write'])
        self.assertTrue(child['com.apple.security.inherit'])
        for key in ['com.apple.security.get-task-allow','com.apple.security.cs.disable-library-validation',
                    'com.apple.security.files.downloads.read-write','com.apple.security.device.camera']:
            self.assertNotIn(key,app)


if __name__=='__main__': unittest.main()
