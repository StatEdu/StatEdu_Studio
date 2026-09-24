# 종단분석 과산포·이분산·확률효과 진단 번역

2026-09-18

## 변경

`longitudinal_check_random_effect_normality`, `longitudinal_check_overdispersion`, `longitudinal_check_heteroskedasticity`의 실제 출력에서 설명·권고문 14개의 번역 누락을 확인했다. 추가 6개 언어뿐 아니라 일부 한국어 문구도 영어로 남았다. `scripts/fill_longitudinal_variance_i18n.py`로 대응하는 `analysis.ui.*` 항목을 8개 언어 사전에 추가했다. 분석 계산, 판정 기준, 결과표 구성은 변경하지 않았다.

## 진단 경로 검증

`scripts/validate_longitudinal_variance_i18n.R`는 다음 12개 경로를 확인한다.

- 확률효과 정규성 4개: 군집 부족, 상수 확률효과의 검사 계산 실패, 정규성 위반 근거 없음, 정규성 위반. 합성 자료에 실제 `lme4::lmer`를 적합하고 `ranef`와 Shapiro-Wilk 결과를 사용한다. 상수 확률효과 모형의 singular-fit 메시지는 의도한 시험 상황이다.
- 과산포 3개: 잔차 없음, 낮은 산포, 높은 산포. 실제 선형모형의 자유도와 잔차를 사용하며, 높은 산포 경로에는 명시적으로 10배 잔차를 입력한다. 실제 Poisson 모형 전체 실행을 검증한 것은 아니다.
- 이분산 5개: 비정규 분포, 선택 패키지 없음, 잘못된 모형식의 검사 실패, 등분산, 이분산. Breusch-Pagan 검사는 실제 실행한다. 패키지 없음은 복사한 함수의 격리 환경에서 `lmtest` 탐지만 false로 바꾸며 설치 패키지나 제품 함수 환경은 변경하지 않는다.

수정 전 누락을 재현했고 최종 12경로 × 8언어 검사를 통과했다. Check/Result/Interpretation/Recommendation의 번역, Statistic/p/Issue 값, 직렬화한 원본 결과 및 동일 문구를 가진 사용자 변수명 보존을 확인했다.

## 공통 검사

`validate_i18n_contract.R`는 Windows UTF-8 로캘에서 통과했다. 변경 사전의 `git diff --check`도 통과했다. 저장 검증용 원본 표는 `tmp/longitudinal-variance-i18n/tables.rds`에 보관했다.

## 저장 검증 완료

`validate_longitudinal_count_exports.R tmp/longitudinal-variance-i18n/tables.rds tmp/longitudinal-variance-i18n/exports`를 실행해 이미 계산한 12개 진단 표를 재사용했다. 한국어/일본어 × 현재/누적 결과를 HTML, PDF, Word, HWPX, Excel로 생성하고 HTML/Word/HWPX/Excel의 예상 표 문구 보존을 확인했다. 영어 본표는 8개 언어에서 동일했다.

`validate_longitudinal_error_pdf.py tmp/longitudinal-variance-i18n/exports`도 통과했다. 네 PDF에서 예상 표 문구를 모두 추출했다. 현재 결과는 표지 포함 14쪽, 두 항목을 누적한 결과는 27쪽이다. 출력물은 `tmp/longitudinal-variance-i18n/exports`에 있다. 이는 내용/구조 검사이며 Word/Hancom에서 별도 시각적 페이지 검수를 수행한 것은 아니다.

## 남은 범위

이번 작업은 세 진단 함수의 문구 번역과 관련 결과 저장 검증이다. 다른 종단 진단, 모든 실제 자료/모형 조합, 원어민 검수 및 전체 앱 다국어 완료를 뜻하지 않는다. 설치본을 생성하거나 설치 앱을 변경하지 않았다.
