# 보정 생존곡선 개요 번역

## 변경

- 표준화 모집단, 신뢰구간 방법, 부트스트랩 요청·성공 횟수, 유효 비율, 신뢰구간 산출 가능 여부, 난수 시드, 주변 표준화, 시점별 백분위 부트스트랩 신뢰구간, Cox 완전사례 분석표본, N/A 등 11개 문구를 8개 언어 사전에 반영했다.
- 스페인어 No는 영어와 철자가 같은 정상 번역으로 구분했다.
- 추정 알고리즘·기본 반복 횟수·표시 및 저장 구현은 변경하지 않았다.

## 검증

- `scripts/validate_survival_adjusted_overview_i18n.R`: 기존 검증 자료의 sex를 범주형으로 지정해 실제 보정 Cox 분석을 실행했다. 부트스트랩 0회(미요청), 1회(신뢰구간 산출에 부족), 25회(신뢰구간 산출 가능)의 개요 표를 각각 생성했다. 이 횟수는 표시 분기 검사용이며 기본값 변경이나 분석 권장값이 아니다.
- 수정 전 누락을 재현하고 수정 후 세 개요 표의 8개 언어 검증을 통과했다. 변수명, 반복 횟수, 비율, 난수 시드 및 원본 표 보존을 확인했다.
- `scripts/validate_survival_cox_overview_i18n.R`, `scripts/validate_survival_cox_diagnostic_labels_i18n.R`: 기존 Cox 개요·진단 표의 번역과 수치·사용자 값 보존 검사 통과. Cox 개요의 영어 main 역할 렌더링 보존도 확인했다.
- `scripts/validate_i18n_contract.R`: 통과.
- 공통 저장 검증기 `scripts/validate_longitudinal_count_exports.R`에 캡처한 생존분석 개요 HTML을 입력해 한국어·일본어 현재·누적 결과의 HTML/PDF/DOCX/HWPX/XLSX 내용 보존 검사 통과. 저장 시 분석을 다시 실행하지 않았다.
- `scripts/validate_longitudinal_error_pdf.py`: 전체 캡처 텍스트 보존 검사 통과. 한국어 현재 2쪽·누적 3쪽, 일본어 현재 3쪽·누적 4쪽이다.
- 산출물: `tmp/survival-adjusted-overview-i18n/exports`.

## 범위

이번 변경은 보정 생존곡선의 개요 표에 한정한다. 곡선 이미지나 시점별 추정·대비 표를 변경하거나 이번 저장 검사에 포함한 것은 아니다. 전체 다국어 작업 완료를 의미하지 않는다. 문서 앱별 수동 시각 검토와 설치파일 빌드는 수행하지 않았다.
