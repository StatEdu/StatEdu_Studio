# Cox 층별·범주형 검정 표제 번역

## 변경

- Stratum, Records, Levels, Wald chi-square, Estimable의 5개 문구를 8개 언어 사전에 반영했다.
- 프랑스어·스페인어 Estimable과 Variable, 독일어 Variable/Status 등은 원문과 철자가 같은 정상 번역으로 구분했다. 신규 문구는 단순 문자열 변경 여부뿐 아니라 사전값과 실제 출력의 일치도 확인했다.
- 모형 추정·진단 계산·저장 코드는 변경하지 않았다.

## 검증

- `scripts/validate_survival_cox_diagnostic_labels_i18n.R`: 기존 검증 자료에서 sex를 범주형으로 지정하고 층화 Cox 분석을 실제 실행했다. 층별 사건 수 표와 범주형 전체효과 검정 표를 생성해 8개 언어로 검증했다.
- 추정 불가 상태는 결과 복사본의 Estimable 플래그를 변경해 표시 경로만 검사했다. 실제 비추정 모형을 적합하거나 해당 검정 수치를 검증한 것으로 집계하지 않는다.
- 수정 전 번역 누락을 재현하고 수정 후 통과했다. 사용자 층 이름 Warning/Value, 변수명, 통계값·표시 정밀도 및 원본 표가 유지됨을 확인했다.
- `scripts/validate_survival_cox_overview_i18n.R`: 기존 Cox 개요의 번역·수치·영어 main 역할 렌더링 보존 검사 통과.
- `scripts/validate_i18n_contract.R`: 통과.
- 공통 검증기 `scripts/validate_longitudinal_count_exports.R`에 생존분석 표의 캡처 HTML을 입력해 한국어·일본어 현재·누적 결과의 HTML/PDF/DOCX/HWPX/XLSX 내용 보존 검사 통과. 저장 시 분석을 다시 실행하지 않았다.
- `scripts/validate_longitudinal_error_pdf.py`: 한국어·일본어 현재 각 4쪽, 누적 각 7쪽의 전체 캡처 텍스트 보존 검사 통과.
- 산출물: `tmp/survival-cox-diagnostic-labels-i18n/exports`.

## 범위

이번 변경은 층별 사건 수와 범주형 전체효과 검정의 표제·상태에 한정한다. 전체 다국어 번역 완료를 의미하지 않는다. 문서 앱별 수동 시각 검토와 설치파일 빌드는 수행하지 않았다.
