# StatEdu Studio 1.0 배포/라이센스/업데이트 계획

작성일: 2026-06-07  
초기 작성 기준: 0.9.33 beta
최근 검토: 2026-06-24, 0.9.42 안정화 단계
검증 태그: statedu-release-plan-reviewed-0.9.42; distribution; license; update
목표 버전: 1.0 정식 배포

이 문서는 1.0 정식 버전으로 전환하기 전에 검토할 배포, 업데이트, 라이센스, 제품 등급 정책 후보를 정리한다.
0.9.42 안정화 단계에서는 새 분석 기능을 추가하지 않고, 1.0 전환 전에 각 배포/라이센스/업데이트 항목을 구현할지 명시적으로 유예할지 결정하는 기준 문서로 다시 확인했다.

## 현재 상태

이 문서는 0.9.42 안정화 단계의 planning/reference document only 문서이다. 아래의 Free/Pro/Latent 권한 분리, 라이센스 서버, activation 관리, 업데이트 확인, installer 배포 인프라는 `docs/RELEASE_1_0_DECISION_LOG.md`에서 구현 완료 또는 명시적 유예로 확정되기 전까지 1.0 공개 기능으로 주장하지 않는다.

Do not claim gated editions, license activation, in-app updates, or public installer infrastructure until the decision log marks them implemented or deferred.

## 전환 리마인더

현재 0.9.42 안정화 단계에서는 1.0 정식 배포 전환 여부를 기능 추가가 아니라 안정화, 검증, 패키징, 라이센스/업데이트 정책 확정 여부로 판단한다. Free/Pro/Latent 권한 분리, 라이센스 서버, activation 관리, 업데이트 확인 기능, installer 배포 인프라는 `docs/RELEASE_1_0_DECISION_LOG.md`에 구현 또는 유예 결정을 기록하기 전까지 구현 대상으로 확정하지 않는다.

릴리스 준비 중 다음 신호가 보이면 1.0 전환 회의를 진행한다.

- 주요 통계분석 기능이 더 이상 큰 구조 변경 없이 안정화됨
- 결과표/내보내기 형식이 논문/보고서 작성에 충분히 안정화됨
- 신규 기능 추가보다 배포, 라이센스, 업데이트, 문서화가 병목이 됨
- 0.9.x beta 꼬리표를 계속 유지할 실익이 줄어듦
- 사용자에게 무료판과 Pro판의 차이를 명확히 설명할 수 있음

## 제품 등급 정책

이 절은 1.0 후보 제품 등급 정책이다. 최종 1.0 정책은 `docs/RELEASE_1_0_DECISION_LOG.md`와 공개 릴리스 노트에서 확정한다. 정책이 채택되는 경우, 1.0 정식 버전은 분석 기능 자체를 유료화하지 않고 무료 배포판에서도 통계분석 기능을 기간 제한 없이 사용할 수 있게 한다. 유료 버전은 더 넓은 데이터 import, 고급 export, 결과 누적/저장 workflow를 제공하는 방향으로 검토한다.

### Free

- 기간 제한 없음
- 통계분석 기능 전체 사용 가능
- CSV 데이터 불러오기 가능
- Stata 데이터 불러오기 가능
- HTML 결과 저장 가능
- Figure 저장 가능
- PDF, Excel, Word 내보내기 불가
- Add Result 기능 불가
- Result collection 저장/불러오기 불가

### Personal Pro

예상 가격:

- 연간 약 120,000원
- 해외 가격 약 90 USD

포함 기능:

- Free 기능 전체 포함
- Excel 데이터 불러오기
- SPSS 데이터 불러오기
- SAS 데이터 불러오기
- PDF 결과 내보내기
- Excel 결과 내보내기
- Word 결과 내보내기
- Add Result로 분석 결과 누적
- Result collection 저장/불러오기

### Latent Add-in

예상 가격:

- 연간 약 50,000원
- 해외 가격 약 30 USD

정책:

- Personal Pro 기반 add-in으로 운영한다.
- Mplus 자체는 포함하지 않는다.
- 사용자는 별도의 정품 Mplus 라이센스가 필요하다.

포함 기능:

- Mplus input 생성
- Mplus 실행 연동
- Mplus output parsing
- LCA/LTA/mixture 관련 결과 정리
- transition/result table 자동화

권장 문구:

> Latent Add-in은 Mplus 연동, 입력파일 생성, 실행 자동화, 결과 정리 기능을 제공합니다. Mplus 프로그램 및 Mplus 라이센스는 별도로 필요합니다.

## 배포 정책

이 절은 1.0 후보 배포 정책이다. 정책이 채택되는 경우, 1.0 배포판 installer는 공개 다운로드로 배포하고 구매자 전용 파일로 숨기지 않는다. 유료 여부는 앱 내부 라이센스 권한으로 제어하는 방향으로 검토한다.

설치파일 크기는 약 400MB로 예상한다. 일반 웹호스팅에서 직접 전송하면 다운로드 트래픽 비용과 제한이 커질 수 있으므로, 실제 파일 전송은 대용량 파일 배포용 저장소/CDN이 담당한다.

권장 구조:

```text
studio.statedu.com/download
  -> 다운로드 페이지

download.statedu.com
  -> Cloudflare R2 public bucket 또는 동급 object storage/CDN
  -> StatEdu_Studio_Setup_1.0.0.exe
  -> checksum / manifest / release notes

license.statedu.com
  -> 라이센스 인증
  -> Pro/Latent 권한 확인
  -> activation 관리
```

1차 권장 배포 저장소:

- Cloudflare R2
- custom domain: `download.statedu.com`
- R2 public bucket + Cloudflare cache 사용
- egress 비용이 없는 구조를 우선 검토한다.

보조 배포 채널:

- GitHub Releases는 beta 또는 mirror 용도로 사용할 수 있다.
- 정식 상용 배포의 주 채널은 R2 또는 동급 object storage/CDN으로 둔다.

배포 파일 예:

```text
/windows/StatEdu_Studio_Setup_1.0.0.exe
/windows/StatEdu_Studio_Setup_1.0.0.exe.sha256
/windows/latest.json
/release-notes/1.0.0.html
```

중요 원칙:

- 홈페이지 서버가 400MB installer를 직접 전송하지 않는다.
- 홈페이지는 다운로드 버튼과 release notes만 제공한다.
- 실제 installer 파일은 `download.statedu.com`에서 전송한다.
- installer는 공개되어도 된다. Pro/Latent 기능은 라이센스 권한으로만 열린다.

## 업데이트 정책

이 절은 후보 업데이트 정책이다. 업데이트 기능을 1.0에 포함하기로 확정하는 경우, 정식 배포판에는 업데이트 확인 기능과 업데이트 실행 기능을 포함한다.

앱 메뉴 예:

```text
Help > Check for Updates
Help > About StatEdu Studio
```

업데이트 확인 흐름:

```text
1. 앱 실행 시 또는 사용자가 Check for Updates 클릭
2. latest.json 확인
3. 현재 버전과 latest_version 비교
4. 새 버전이 있으면 release notes와 업데이트 버튼 표시
5. 필수 업데이트이면 실행 전 업데이트 요구
```

`latest.json` 예:

```json
{
  "latest_version": "1.0.3",
  "minimum_supported_version": "1.0.0",
  "download_url": "https://download.statedu.com/windows/StatEdu_Studio_Setup_1.0.3.exe",
  "sha256": "...",
  "size_mb": 412,
  "release_notes_url": "https://studio.statedu.com/releases/1.0.3",
  "mandatory": false
}
```

업데이트 실행 흐름:

```text
1. update manifest에서 download_url 확인
2. installer 또는 update package 다운로드
3. SHA256 checksum 검증
4. 가능하면 코드 서명 검증
5. 현재 앱 종료
6. installer/updater 실행
7. 새 버전 실행
```

1차 구현은 전체 설치파일 재다운로드 방식으로 충분하다.

```text
1차: StatEdu_Studio_Setup_1.0.3.exe 전체 다운로드
2차: StatEdu_Studio_Update_1.0.2_to_1.0.3.exe 패치 다운로드 검토
```

## 라이센스 서버 정책

무료판은 항상 실행 가능해야 한다. 라이센스 서버는 앱 실행 허가가 아니라 Pro/Latent 기능 권한을 관리한다.

권장 서버 구조:

```text
StatEdu Studio app
  -> license.statedu.com
    -> License API server
    -> PostgreSQL DB
```

권장 초기 스택:

- FastAPI 또는 Node.js
- PostgreSQL
- Render, Railway, Fly.io, Lightsail, DigitalOcean, Cloudtype 중 하나
- HTTPS 필수

필요 API:

```text
POST /license/activate
POST /license/validate
GET  /license/status
GET  /license/activations
POST /license/deactivate
POST /license/deactivate-current
GET  /updates/latest
```

서버 DB 핵심 테이블:

```text
licenses
- id
- license_key_hash
- email
- plan
- has_latent_addin
- issued_at
- expires_at
- max_activations
- status

activations
- id
- license_id
- device_id_hash
- fingerprint_hash
- device_label
- os
- app_version
- activated_at
- last_seen_at
- status
```

라이센스 키 원본은 가능하면 DB에 저장하지 않고 hash로 저장한다.

## Device Activation 정책

Personal Pro는 기본적으로 1 라이센스당 최대 2대 활성화를 허용한다.

기기 식별은 초기에는 random device id 기반으로 시작한다.

초기 방식:

```text
1. 앱 최초 실행 시 random device_id 생성
2. 로컬에 암호화 저장
3. 서버에는 device_id_hash 등록
4. max_activations = 2 기준으로 활성화 제한
```

추후 보강:

```text
device_id + hardware fingerprint 보조 정보
```

MAC address 단독 기반은 사용하지 않는다. 네트워크 어댑터 변경, VPN, 가상 어댑터, 개인정보 이슈, 변경 가능성 때문에 안정성이 낮다.

사용자 표시 정보:

- 기기 이름
- OS
- 앱 버전
- 최초 활성화일
- 마지막 사용일
- 현재 기기 여부

사용자에게 표시하지 않을 정보:

- raw device_id
- fingerprint 원본
- fingerprint hash
- license_key 원본

## 사용자 Activation 관리 기능

사용자는 앱 안에서 현재 활성화된 기기를 확인하고, 특정 기기를 비활성화할 수 있어야 한다.

앱 메뉴 예:

```text
License > Enter License Key
License > License Status
License > Manage Activations
License > Deactivate This Device
```

화면 예:

```text
StatEdu Studio Personal Pro
License: active
Expires: 2027-06-07
Activations: 2 / 2

Activated devices

1. DESKTOP-AB12
   Windows 11
   Last used: 2026-06-07
   This device
   [Deactivate]

2. LAPTOP-HOME
   Windows 11
   Last used: 2026-05-28
   [Deactivate]
```

새 컴퓨터에서 2대 제한이 찬 경우:

```text
This license is already activated on 2 devices.
Please deactivate one device below to continue.
```

원격 비활성화 정책:

- 사용자가 특정 activation을 비활성화하면 서버에서 해당 activation status를 `deactivated`로 변경한다.
- 비활성화된 컴퓨터는 다음 validate 때 Pro 권한을 잃고 Free 모드로 전환한다.
- 오프라인 grace period가 남아 있으면 즉시 차단되지 않고 최대 grace period 종료 후 반영될 수 있다.

남용 방지:

- 현재 기기 비활성화는 허용한다.
- 원격 기기 비활성화는 월 2-3회 정도 제한하는 방안을 검토한다.
- 초과 시 관리자 reset 또는 고객지원으로 처리한다.

## Offline Grace 정책

인터넷 연결이 없어도 최근 인증 이후 일정 기간은 Pro 기능을 사용할 수 있게 한다.

권장값:

```text
offline_grace_days = 14
```

흐름:

```text
1. 앱 실행
2. 로컬 signed license token 확인
3. 최근 validate가 14일 이내이면 Pro 기능 임시 허용
4. 인터넷 연결되면 서버 validate 수행
5. 만료, revoke, deactivated 상태이면 Free 모드로 전환
6. 14일 초과 시 validate 전까지 Free 모드로 전환
```

무료 기능은 서버 연결 여부와 관계없이 항상 사용 가능해야 한다.

## Feature Flag 정책

Pro 기능은 UI뿐 아니라 실제 실행 함수에서도 권한을 확인한다.

예:

```text
UI:
- PDF export 버튼 비활성화 또는 Pro 표시

Server/action:
- PDF export 실행 직전에도 license_check("export_pdf") 확인
```

기능 플래그 예:

```json
{
  "plan": "personal_pro",
  "expires_at": "2027-06-07",
  "features": {
    "import_csv": true,
    "import_stata": true,
    "import_excel": true,
    "import_spss": true,
    "import_sas": true,
    "export_html": true,
    "export_figure": true,
    "export_pdf": true,
    "export_word": true,
    "export_excel": true,
    "add_result": true,
    "save_result_project": true,
    "latent_addin": false
  }
}
```

Free 기본 권한:

```text
import_csv = true
import_stata = true
export_html = true
export_figure = true
all_analysis = true
```

Pro 권한:

```text
import_excel = true
import_spss = true
import_sas = true
export_pdf = true
export_word = true
export_excel = true
add_result = true
save_result_project = true
```

Latent 권한:

```text
latent_addin = true
```

## 1.0 구현 체크리스트

### 앱 내부

- Free/Pro/Latent edition state 관리 모듈 추가
- feature flag 조회 함수 추가
- 데이터 import 기능을 Free/Pro 권한으로 분기
- PDF/Excel/Word export를 Pro 권한으로 분기
- Add Result와 Result collection 저장/불러오기를 Pro 권한으로 분기
- Latent/Mplus 메뉴를 Latent Add-in 권한으로 분기
- License Status UI 추가
- Manage Activations UI 추가
- Check for Updates UI 추가
- updater download/checksum/installer 실행 흐름 추가
- offline grace 처리 추가

### 서버/API

- license server scaffold 작성
- license activation API 구현
- license validation API 구현
- activations list API 구현
- deactivate current device API 구현
- deactivate selected device API 구현
- update manifest API 또는 static latest.json 배포
- 관리자용 license 발급/만료/activation reset 기능 구현

### 배포 인프라

- `download.statedu.com` 준비
- R2 bucket 또는 동급 object storage 준비
- `license.statedu.com` 준비
- HTTPS 적용
- installer 파일, checksum, latest.json 업로드 절차 정리
- release notes 페이지 준비

### 운영/정책

- 가격표 확정
- 무료/Pro/Latent 기능 비교표 확정
- Mplus 별도 라이센스 필요 문구 명시
- 개인정보처리방침 준비
- 이용약관 준비
- 라이센스 환불/기기 reset 정책 준비
- 코드 서명 인증서 검토

## 1.0 성공 기준

- 누구나 installer를 다운로드하여 기간 제한 없이 Free 기능을 사용할 수 있다.
- Free 사용자는 모든 분석을 실행할 수 있다.
- Free 사용자는 CSV/Stata import, HTML 저장, figure 저장을 사용할 수 있다.
- Free 사용자는 Pro import/export/result-management 기능을 실행할 수 없다.
- Pro 라이센스를 입력하면 Excel/SPSS/SAS import, PDF/Excel/Word export, Add Result, Result 저장/불러오기가 열린다.
- 사용자는 활성화된 기기 목록을 확인하고 직접 비활성화할 수 있다.
- 2대 활성화 제한이 서버에서 동작한다.
- 마지막 인증 후 14일 동안 offline grace가 동작한다.
- 앱에서 최신 버전 확인과 업데이트 다운로드가 가능하다.
- installer 다운로드 트래픽은 일반 웹호스팅이 아니라 object storage/CDN에서 처리된다.
