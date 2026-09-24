# GLM 실제 MI 분기 및 진단표

2026-09-16. 설치본을 만들지 않았다.

## 발견과 변경

실제 MI를 실행해 대치별 진단표를 확인했다. Imputation, Residual df, Converged, Poisson dispersion, SE method, Term signature 헤더 6개와 분포·링크의 내부 문자열이 남아 있어 번역을 보완했다. 분포·링크 변환은 대치별 진단표 구조에 한정한다.

모형 항 목록은 사용자 변수명을 포함하므로 원문을 보호한다. 수치와 논리형 수렴 상태, 영어 본표는 그대로 유지한다. 계산 알고리즘은 변경하지 않았다.

## 검증

- 새 `scripts/validate_glm_mi_multilingual.R`에서 100행 합성 자료로 실제 MI를 실행했다. 2개 대치 자료·2회 반복으로 종속변수 결측을 제외하는 observed 방식과 대치값을 포함하는 impute 방식을 검사했다.
- 분석 행 수가 각각 92, 100임을 확인했다. 8개 언어에서 진단표 수치·논리값, 통합표 계수 항, 본표 셀과 주석이 유지되는지 검사해 통과했다.
- `Model-based`, `Intercept only`와 같은 모형 항 목록이 번역되지 않는지 확인했다.
- `scripts/validate_i18n_contract.R`: 통과.
- 일본어 실제 대치별 진단표와 통합 진단표를 현재/누적 내보내기 fixture로 사용했다.
- 현재/누적 HTML·Word·HWPX·Excel 내용 비교와 PDF 생성 통과. `scripts/validate_multilingual_pdf.py`의 두 PDF 실제 텍스트 비교도 통과했다.

## 범위

이 검증의 적은 대치 횟수는 출력 회귀 검사용이며 실제 연구의 대치 횟수를 권고하는 것이 아니다. Gaussian MI 두 종속변수 처리 방식을 검증했으며 모든 분포·MI 옵션이나 전체 앱의 다국어 완료를 의미하지 않는다.
