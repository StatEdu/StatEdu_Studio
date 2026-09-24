# 회귀 Excel 제외 모형 검증 복구 — 2026-09-14

## 원인과 수정

기존 `scripts/validate_regression_coefficients.R`는 Excel에 이름이 정확히 `Skipped models`인 시트가 있어야 한다고 검사했다. 현재 스냅샷 기반 저장은 화면의 `Warnings / skipped models` 제목을 사용하고 Excel 시트명에 허용되지 않는 `/`를 제거하므로 실제 시트명은 `Warnings skipped models`다.

실제 파일을 읽어 제외된 종속변수 `Constant outcome`, 유형 `Skipped`, 표본 수 6, 제외 사유 `The dependent variable has no variance after complete-case filtering.`가 같은 행에 저장된 것을 확인했다. 분석 내용이나 제외 사유의 누락은 아니었다.

검증 코드를 시트명 일치 검사에서 저장된 내용 검사로 변경했다.

- 첫 열에 실제 경고 표 제목이 있는 시트를 찾고 정확히 하나인지 검사한다.
- 같은 행에 제외 유형, 종속변수 표시 이름, 표본 수, 실제 분석 결과의 정확한 제외 사유가 함께 있는지 검사한다.

단순히 새로운 시트명을 하드코딩하거나 실패 검사를 제거하지 않았다. 제품의 분석 코드와 Excel 저장 코드는 변경하지 않았다.

## 검증 결과

번들 R 4.5.3에서 `scripts/validate_regression_coefficients.R` 전체가 통과했다. 이전에 중단되던 제외 모형 검사를 넘어 저장 결과/Word 표 일치, 부트스트랩 계수 표 간격, 위계적 회귀의 공통 완전 관측치 표본 검사도 실행됐다. 완전 적합 합성 자료에 대한 기존 `summary.lm` 경고는 발생했으며 숨기지 않았다.

직전 VIF 재사용 및 효과크기 경량 적합 보고서에 기록했던 기존 Excel 검사 실패는 이 검증 코드 수정으로 해소됐다. 당시 기록은 해당 시점의 결과로 유지한다.

재현 자료는 `output/regression-skipped-excel-check-20260914/inspect.R`와 `current.xlsx`이며, 지속 검증은 수정된 `scripts/validate_regression_coefficients.R`다. 표시·저장 내용 변경이 없는 테스트 수정이므로 전체 내보내기 형식 재생성이나 빌드·배포는 수행하지 않았다.
