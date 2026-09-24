# GLM 진단 설명 다국어 보완

2026-09-16. 설치본 생성 없이 소스만 수정.

## 변경

- GLM 보조표의 관측치 독립성, 분포 선택 확인, 잔차 진단, 적합확률 분리 징후, 희소 범주 확인, VIF/산포비 추정 실패, 과산포 선별, 민감도 분석 안내 등 10개 설명을 일본어·중국어·스페인어·프랑스어·독일어·베트남어 카탈로그에 추가했다.
- 기존 한국어 설명과 영어 본표는 유지한다. 모델 계산과 옵션 값은 변경하지 않았다.
- `scripts/fixtures/glm_i18n_diagnostics.json`의 설명을 기존 GLM 검증에 포함했다. 사용자 Variable 열에 동일한 문장이 있어도 번역하지 않는 것을 검사한다.

## 검증

- `scripts/validate_glm_multilingual.R`: 8개 언어 통과. Gaussian/binomial/count 실제 분석 본표의 셀·제목·주석 영어 유지, 옵션 값 보존, 설명 번역과 사용자 변수명 보존 확인.
- `scripts/validate_i18n_contract.R`: 통과.
- 일본어 보조표 fixture로 현재/누적 HTML·PDF·Word·HWPX·Excel 저장 확인. HTML/Word/HWPX/Excel 텍스트 비교 통과.
- `scripts/validate_multilingual_pdf.py tmp/glm-multilingual`: 두 PDF의 실제 텍스트 비교 통과.
- 카운트 모형의 합성 검증 데이터에서 기존 음이항 추정 반복 한도 경고가 발생했다. 해당 검증은 통계적 추정 성능이 아닌 출력 계약을 검사한다.

## 남은 범위

전체 GLM 번역 완료를 의미하지 않는다. 완전/준완전 분리, 희소 셀, 영향 관측치, 변수 코딩, 결측자료, 동적 수치 포함 설명 및 원고 문장 등에 대한 추가 점검이 남아 있다. 이번 내보내기 검사는 일본어 fixture 기준이다.
