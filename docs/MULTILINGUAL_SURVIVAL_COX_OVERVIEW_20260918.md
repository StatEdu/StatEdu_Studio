# 생존분석 Cox 모형 개요 표제 번역

## 변경

- Excluded rows, Events / parameter, Ties, LR chi-square (df), LR p, Concordance (95% CI)의 6개 표제를 8개 언어 사전에 반영했다.
- 프랑스어 Variance와 Package는 영어와 철자가 같은 정상 번역으로 구분했다.
- 통계 추정과 결과 표·저장 구현은 수정하지 않았다.

## 검증

- `scripts/validate_survival_cox_overview_i18n.R`: 기존 survival_validation.csv로 실제 Cox 분석을 수행하고 21행 모형 개요 표를 생성했다. 수정 전 5개 표제의 비영어 누락을 재현했고, 추가로 LR p 표제도 함께 보완했다. 최종 검사는 6개 항목을 포함한 개요 표 전체 표제에 적용했다.
- 8개 언어에서 수치, LR 검정 결과, 일치도·신뢰구간, 패키지 버전과 원본 표 보존을 확인했다. 동일 표를 main 역할로 렌더링했을 때 영어 유지도 확인했다.
- `scripts/validate_i18n_contract.R`: 통과.
- `scripts/validate_survival_followup_table_i18n.R`: KM·Cox·경쟁위험의 8개 언어 조합 24건 통과. 실제 안내 패널의 추적기간 표와 수치가 유지됨을 확인했다.
- 공통 저장 검증기 `scripts/validate_longitudinal_count_exports.R`에 캡처한 생존분석 HTML을 입력해 한국어·일본어 현재·누적 결과의 HTML/PDF/DOCX/HWPX/XLSX 내용 보존 검사를 통과했다. 검사기 이름과 달리 이번 입력은 Cox 표이며, 저장 시 분석을 다시 실행하지 않았다.
- `scripts/validate_longitudinal_error_pdf.py`: 한국어·일본어 현재 각 3쪽, 누적 각 5쪽의 전체 캡처 텍스트 보존 검사 통과.
- 산출물: `tmp/survival-cox-overview-i18n/exports`.

## 범위

이번 변경은 Cox 모형 개요의 표제에 한정한다. 다른 생존분석 부록과 동적 안내는 계속 확인해야 하며 전체 다국어 작업 완료를 의미하지 않는다. 문서 앱별 수동 시각 검토와 설치파일 빌드는 수행하지 않았다.
