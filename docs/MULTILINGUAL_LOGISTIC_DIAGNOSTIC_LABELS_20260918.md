# 로지스틱 회귀 진단 항목·요약 경고 번역

## 변경

진단 항목 Convergence, EPV / sparse, Separation과 요약 경고 EPV/sparse warning, Separation warning의 5개 문구를 8개 언어 사전에 반영했다. 통계 계산·렌더링 코드는 변경하지 않았다.

## 검증

`validate_logistic_diagnostic_labels_i18n.R`는 8명의 이분형 결과와 범주형 예측변수가 완전히 분리되는 자료를 실제로 분석한다. 희소·분리 경고가 실제 생성되고 개요·진단 표에 번역되어 표시되는지 확인한다. 이 검사용 모형은 분리 문제를 의도적으로 유발하며, 추정의 타당성을 보증하는 검사가 아니다.

- 보완 전 6개 외국어의 사전 누락 및 영어 경고 잔여 재현.
- 보완 후 8개 언어 표시 및 영어 경고 잔여 제거 검사 통과.
- 프랑스어 Convergence와 독일어 Separation은 영어와 철자가 같은 올바른 용어로 인정했다.
- 영어 본표의 언어 간 동일성, 전체 결과 객체 및 진단 문구와 같은 사용자 변수명 보존 확인.
- 실제 적합에서 확률이 수치상 0/1이라는 경고가 발생했다. 의도한 분리 자료의 경고이며 번역 검사 실패가 아니다.
- `validate_logistic_instability_i18n.R`, `validate_logistic_sparse_i18n.R`, `validate_i18n_contract.R` 및 변경 공백 검사 통과.
- 공통 16개 부록 변환 함수와 8개 결과 렌더러의 8개 언어 회귀 검사 통과.
- 한국어·일본어 현재/누적 HTML/PDF/DOCX/HWPX/XLSX 내용 및 PDF 텍스트 보존 검사 통과. 두 언어 모두 현재 4쪽·누적 7쪽이다.
- 저장 검증은 `validate_longitudinal_count_exports.R`와 `validate_longitudinal_error_pdf.py`를 재사용했다. 공용 표지 제목은 Longitudinal이고 본문은 실제 로지스틱 진단 결과 캡처다. 저장 시 분석을 재실행하지 않았다. 산출물: `tmp/logistic-diagnostic-labels-i18n/exports`.

기존 매개·조절효과 화면은 제외했다. 설치본 빌드 및 문서 앱별 수동 시각 검토는 수행하지 않았다.
