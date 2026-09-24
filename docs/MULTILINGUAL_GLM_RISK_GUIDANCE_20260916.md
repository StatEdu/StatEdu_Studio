# GLM 위험 진단 안내 다국어 보완

2026-09-16. 소스만 변경했으며 설치본은 만들지 않았다.

## 변경 범위

일본어·중국어·스페인어·프랑스어·독일어·베트남어에 보조표 설명 12개를 추가했다. 완전/준완전 분리, 희소 범주 및 셀, 영향 관측치, Cook의 거리와 레버리지, VIF, 과산포 대응, EPV 및 불안정한 추정치의 대응 안내를 포함한다. 기존 한국어 설명은 유지했다.

통계 계산, 선택 옵션의 내부 값, 본표의 영어, 사용자 변수명과 라벨은 변경하지 않았다. 기존 번역표 조회 경로를 사용한다.

## 검증

- `scripts/fixtures/glm_i18n_risk_guidance.json`의 12개 문장을 기존 GLM 검증에 추가했다.
- `scripts/validate_glm_multilingual.R`: 8개 언어 통과. 보조표 번역, 동일한 문장을 사용자 Variable 값으로 쓴 경우의 원문 보존, Gaussian/binomial/count 실제 결과의 영어 본표 유지 및 옵션 값 보존을 확인했다.
- `scripts/validate_i18n_contract.R`: 통과.
- 일본어 보조표 fixture의 현재/누적 HTML·Word·HWPX·Excel 텍스트 비교 및 PDF 생성 통과.
- `scripts/validate_multilingual_pdf.py tmp/glm-multilingual`: 현재/누적 PDF 실제 텍스트 비교 통과.
- 합성 카운트 데이터의 기존 음이항 추정 반복 한도 경고는 발생했으나 출력 계약 검사는 통과했다.

## 잔여 범위

GLM 변수 코딩, 결측자료, 수치가 포함된 동적 설명, 원고 문장 등은 추가 점검 대상이다. 모든 위험 상황을 실제 데이터로 재현한 검증은 아니며, 위험 문장은 보조표 fixture로 검사했다. 이번 저장 검증의 언어는 일본어이다.
