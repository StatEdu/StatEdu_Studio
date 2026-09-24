# 대응표본 제외 진단표의 검정명 번역

## 변경

실제 대응표본·비모수 대응표본 분석의 제외 진단표에 표시되는 `Wilcoxon signed-rank test`, `Paired categorical test`가 일본어·중국어·스페인어·프랑스어·독일어·베트남어에서 영어로 남는 것을 재현했다. 두 검정명을 8개 언어 사전에 반영했다. 영어와 기존 한국어 표기는 유지했다. 계산 및 렌더링 코드는 변경하지 않았다.

## 검증

- `validate_paired_skipped_methods_i18n.R`: 실제 유효 연속형 쌍, 차이가 모두 0인 쌍, 관측 범주가 하나뿐인 이분형 쌍으로 분석했다. 보완 전 6개 언어에서 실패하고 보완 후 통과했다.
- 두 분석 패널 × 8개 언어에서 제외 진단표 검정명을 확인했다. 영어 본표의 언어 간 동일성, `Review - Normality` 등 사용자 변수명, 원본 결과 객체 보존을 확인했다.
- `validate_paired_guards.R`, `validate_i18n_contract.R`: 통과.
- 캡처된 한국어·일본어 결과를 현재 및 누적 모드로 HTML/PDF/DOCX/HWPX/XLSX에 저장했다. 저장 시 분석을 재실행하지 않았다. HTML·Word·HWPX·Excel 내용 검사 및 PDF 캡처 텍스트 보존 검사 통과. PDF는 두 언어 모두 현재 5쪽, 누적 9쪽이다.
- 재사용 저장 검증기: `validate_longitudinal_count_exports.R`, PDF 검사기: `validate_longitudinal_error_pdf.py`. 공용 검증기의 표지 제목은 Longitudinal이며 실제 캡처는 이번 대응표본 결과이다. 본표 동일성은 전용 검사에서 별도로 확인했다.
- 산출물: `tmp/paired-skipped-methods-i18n/exports`.

문서 앱별 수동 시각 검토 및 설치본 빌드는 수행하지 않았다. 기존 매개·조절효과 화면은 제외했다. 이 결과는 프로그램 전체 번역 완료를 의미하지 않는다.
