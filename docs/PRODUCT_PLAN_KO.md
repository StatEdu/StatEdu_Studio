# **StatEdu Studio** 제품 계획

**StatEdu Studio**는 Windows 로컬 환경에서 실행되는 Shiny 기반 통계 분석 도구이다. 사용자는 R 또는 RStudio를 직접 다루지 않고도 데이터 불러오기, 변수 검토, 분석 실행, 결과 저장까지 하나의 흐름으로 진행할 수 있다.

## 제품 방향

- 통계 초보자도 데이터 열기, 변수 확인, 분석 실행, 결과 저장까지 자연스럽게 진행할 수 있게 한다.
- 변수의 measurement level을 중심으로 분석 가능성과 방법 선택을 안내한다.
- 분석이 불가능한 조합은 전체 실행을 중단하지 않고 경고 또는 skipped result로 분리한다.
- 결과표는 연구 보고서나 논문 작성에 옮기기 쉬운 형태로 정리한다.
- 1.0 정식 버전 전까지는 새 분석 기능보다 안정화, UI 일관성, 검증 자동화, 배포 품질을 우선한다.

## 0.9.x 안정화 범위

- 제품명을 **StatEdu Studio**로 통일하고, visible UI와 문서에서 이전 명칭을 제거한다.
- Data Editor와 주요 분석 메뉴의 블럭, 패널, 버튼 위치를 공통 UI 규칙에 맞춘다.
- Wide to Long, recode, rename, missing values 같은 데이터 편집 흐름을 분석 메뉴와 같은 3블럭 구조로 정리한다.
- 설정 파일은 `.studio` 형식으로 단순화하고, 저장/불러오기 동작을 검증한다.
- Shiny startup, Electron release, version metadata, UI layout, data IO, 주요 분석 결과를 자동 검증한다.

## 1.0 이전 점검 기준

- `scripts/validate_stabilization.ps1 -Full`이 통과해야 한다.
- `scripts/smoke_shiny_app.ps1`와 `scripts/smoke_electron_release.ps1`를 실행해 앱 시작과 배포 산출물을 확인한다.
- UI layout contract와 release checklist가 실제 구현과 맞아야 한다.
- public release에 필요한 source, license, third-party notices, validation notes를 포함한다.
- Free/Pro/Latent 배포 정책과 라이선스 계획은 `RELEASE_1_0_DISTRIBUTION_LICENSE_PLAN_KO.md`에서 최종 검토한다.

## 이후 개선 후보

- 예제 데이터와 튜토리얼 시나리오를 보강한다.
- 주요 분석별 해석 문장과 보고 예시를 추가한다.
- 결과표 스타일과 저장 문서 형식을 더 고도화한다.
- 한국어와 영어 문서를 함께 정비한다.
