# GLM 상세 진단·보고 안내 다국어 보완

2026-09-16. 설치본은 생성하지 않았다.

## 변경

6개 언어에 고정 설명 10개를 추가했다. 반복·군집·대응 자료의 대안 분석, 편차 잔차 Shapiro–Wilk 검토의 의미, 모형기반 공분산, 카운트 분포 절차, 계수표 구성, 산포/영점 진단, 카운트 미적용, 주석 제공, 원고 제안 및 노출량 오프셋 안내를 포함한다.

통계적 의미를 원문에 맞추었으며 계산 로직·본표의 영어·사용자 라벨은 변경하지 않았다.

## 검증

- `scripts/fixtures/glm_i18n_reporting.json`에 10개 원문을 기록하고 기존 GLM 검증에 추가했다.
- `scripts/validate_glm_multilingual.R`: 8개 언어 보조표 번역, 사용자 Variable 원문, 실제 Gaussian/binomial/count 본표 영어 및 옵션 값 보존 검사.
- `scripts/validate_i18n_contract.R`: 통과.
- 합성 count 분석의 기존 음이항 반복 한도 경고가 발생했으나 출력 계약 검사는 통과했다.
- 8개 언어 GLM 검사 통과. 일본어 현재/누적 HTML·Word·HWPX·Excel 내용 비교 및 PDF 생성 통과. `scripts/validate_multilingual_pdf.py`의 두 PDF 실제 텍스트 비교도 통과했다.

## 남은 범위

일부 결측/IPW 동적 설명과 안내, 다중대치 표 주석 등은 추가 점검 대상이다. 전체 앱의 다국어 작업 완료를 의미하지 않는다.
