// Extend the immutable installed-source review with separately identified build inputs.
const fs = require('node:fs');
const path = require('node:path');
const crypto = require('node:crypto');
const root = path.resolve(__dirname, '..');
const base = path.join(root, 'output/microsoft-store/StatEdu_Studio_1.3.0_source-review');
const out = path.join(root, 'output/microsoft-store/StatEdu_Studio_1.3.0_source-review-v2');
const sha = b => crypto.createHash('sha256').update(b).digest('hex').toUpperCase();
if (fs.existsSync(out)) throw new Error('Preserve existing review v2; do not overwrite.');
const old = JSON.parse(fs.readFileSync(path.join(base, 'MANIFEST.json'), 'utf8'));
for (const r of old.records) if (sha(fs.readFileSync(path.join(base, r.path))) !== r.sha256) throw new Error(`Snapshot changed: ${r.path}`);
fs.cpSync(base, out, {recursive:true, errorOnExist:true});
const records = [];
const missing = [];
const queue = ['scripts/build_electron_beta.ps1', 'scripts/build_electron_release.ps1', 'scripts/validate_installer_regressions.ps1'];
const visited = new Set();
const alerts = [];
function add(rel, origin) {
  const source = path.join(root, rel);
  if (!fs.existsSync(source)) { missing.push(rel); return null; }
  const bytes = fs.readFileSync(source);
  const dest = path.join(out, 'build-inputs', rel);
  fs.mkdirSync(path.dirname(dest), {recursive:true});
  fs.writeFileSync(dest, bytes);
  records.push({path:'build-inputs/'+rel, sha256:sha(bytes), origin});
  const text = bytes.toString('utf8');
  if (/-----BEGIN (?:RSA |EC |OPENSSH )?PRIVATE KEY-----|ghp_[A-Za-z0-9]{30,}|github_pat_[A-Za-z0-9_]{40,}|AKIA[A-Z0-9]{16}|sk-proj-[A-Za-z0-9_-]{30,}/.test(text)) alerts.push(rel);
  return text;
}
add('packaging/electron/package.json', 'working-tree-build-config-not-shipped');
add('packaging/electron/package-lock.json', 'working-tree-build-lock-not-shipped');
while (queue.length) {
  const rel = queue.shift();
  if (visited.has(rel)) continue;
  visited.add(rel);
  // Prefer the shipped script for reference discovery; retain current missing inputs separately.
  const shipped = path.join(base, 'app', rel);
  let text;
  if (fs.existsSync(shipped)) text = fs.readFileSync(shipped, 'utf8');
  else text = add(rel, 'working-tree-missing-build-input');
  if (text === null) continue;
  for (const m of text.matchAll(/scripts[\\/]+([A-Za-z0-9_.-]+\.(?:ps1|R|cjs|js|py|csv))/g)) queue.push('scripts/'+m[1]);
}
const packed = JSON.parse(fs.readFileSync(path.join(base,'electron/package.json'),'utf8'));
const build = JSON.parse(fs.readFileSync(path.join(root,'packaging/electron/package.json'),'utf8'));
if (packed.version !== build.version || packed.name !== build.name) throw new Error('Build identity differs');
const report = {version:packed.version, originalInstalledAppFiles:old.records.filter(x=>x.origin==='installed-app').length, supplementalFiles:records, scannedScriptReferences:[...visited].sort(), missingLiteralScriptReferences:missing, credentialPatternAlerts:alerts, limitations:['Literal scripts/path references only; dynamic paths and fixtures not comprehensively resolved','Private SmartPLS evidence is excluded and original release gate remains mandatory','R runtime and upstream third-party sources are not included','No clean build or installer certification has been run'],readyToPublish:false};
fs.writeFileSync(path.join(out,'BUILD-INPUTS-REVIEW.json'),JSON.stringify(report,null,2));
fs.writeFileSync(path.join(out,'BUILD-REVIEW-KO.md'),`# 1.3.0 소스 빌드 준비 검토\n\n이 묶음은 검토본이며 재현 빌드 완료본이 아닙니다.\n\n## 보완 내용\n\n설치본에서 추출한 electron/package.json은 실행 정보만 포함합니다. 전체 빌드 설정은 build-inputs/packaging/electron/package.json에 별도 추가했습니다. 앞선 REVIEW-README의 '추출한 package.json에 builder 버전이 있다'는 설명은 이 문서로 정정합니다. 이 설정은 현재 작업 트리에서 가져온 보조 자료이며 역사적 빌드 시점과 동일함을 보증하지 않습니다.\n\napp/와 electron/main.js·preload.js는 이전 검토본의 설치본 원문을 유지했습니다. 누락된 문자 그대로의 scripts/경로 참조를 따라 검증 스크립트를 build-inputs/scripts에 추가했습니다. 파일별 출처·해시는 BUILD-INPUTS-REVIEW.json을 참조하세요.\n\n## 복원할 구조\n\n별도의 빈 Windows 작업 폴더에 app/ 내용을 루트로 복사하고, electron/main.js·preload.js·build/를 packaging/electron/에 복사합니다. build-inputs/의 파일은 같은 상대 경로로 추가합니다. 원본 검토 폴더에서 직접 빌드를 실행하지 마세요. 배포 스크립트는 git 파일 목록을 사용하므로 공개할 파일을 검토하고 별도 저장소에 추적해야 합니다.\n\nR 4.5.3과 app/license_report.csv 및 bundled_validation_packages.lock.csv의 패키지 버전이 필요합니다. Node.js는 lockfile의 엔진 조건에 맞춰 준비하고, 별도 작업 폴더의 packaging/electron에서 npm ci를 실행하여 잠긴 Electron 43.4.0과 electron-builder 26.15.3을 준비합니다. 실행 전 실제 파일의 값도 확인하세요. 이 단계의 다운로드·설치는 이번 작업에서 실행하지 않았습니다.\n\n기존 scripts/build_electron_beta.ps1의 공개판 경로를 사용합니다. -Developer는 사용하지 않습니다. 검증 게이트를 우회하지 않습니다. 기존 SmartPLS 비교 자료와 정확한 검증 라이브러리가 필요하며, 해당 비공개 자료는 ZIP에 포함하지 않았습니다. 따라서 현재 묶음만으로 동일한 공식 릴리스 빌드가 끝난다고 주장할 수 없습니다.\n\n## 남은 조건\n\n- 동적 경로와 검증용 fixture의 누락 확인\n- 제3자 소스 및 정확한 버전의 런타임 준비 절차 검토\n- 격리된 복원 작업 폴더에서 기존 검증 게이트와 빌드 실행\n- 설치·업데이트·내보내기 검증 및 새 해시 기록\n- 소스 안내 주소 수정 및 공개 자료 검토 후 별도 게시\n\nGitHub 태그나 기존 설치본은 변경하지 않았습니다.\n`);
console.log(JSON.stringify({directory:out,supplementalFiles:records.length,scriptReferences:visited.size,missing,credentialPatternAlerts:alerts.length,readyToPublish:false},null,2));
if (alerts.length || missing.length) process.exitCode = 2;
