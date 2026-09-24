# 규제 회귀 분포 셀 번역 및 고급 옵션 점검

## 변경

규제 회귀 부록의 교차검증 설정표에서 자동 생성하는 `Family = Gaussian` 셀이 모든 언어에 영어로 표시되는 것을 실제 분석으로 확인했다. 규제 회귀 전용 키 `analysis.penalized.family_gaussian`을 8개 언어 사전에 추가하고, 해당 분포 열의 Gaussian 값에만 적용했다. 한국어는 정규분포로 표시한다. 통계 계산 코드는 변경하지 않았다.

## 검증 범위

- 실제 90명 자료의 Ridge·LASSO·Elastic Net 분석: 연속형 예측변수와 3수준 범주형 예측변수, 부트스트랩 2회, 표본 분할 20회, 검증 반복 2회. 반복 횟수는 출력 검사 목적이며 통계적 안정성을 보증하지 않는다.
- `validate_penalized_remaining_review.R`에서 기존 고급 옵션 검사를 실행하고 8개 언어의 실제 전체 결과를 캡처했다. 분포 셀의 번역, 영어 본표 동일성, 사용자 결과변수 라벨 `Tested`와 전체 결과 객체 보존을 확인했다.
- 부록의 영어와 동일한 열 제목·셀 목록을 `tmp/penalized-remaining-review/unchanged-appendix-candidates.csv`에 저장했다. 동일 표기가 곧 번역 누락이라는 뜻은 아니다. 사용자 라벨 Tested, 모형명, lambda 규칙, RMSE·MAE 등의 지표, 프랑스어·독일어 Minimum/Maximum 등이 포함된다. `(Intercept)` 등의 보존된 계수항 표기는 별도 검토 대상으로 남긴다.
- 정적 후보 `Stability`는 현재 분석 생성 코드에 없는 호환용 열 매핑이다. 이번에 새 열이나 값을 만들지 않았다.

## 저장 검증

한국어·일본어 각각 현재/누적 결과의 HTML·PDF·DOCX·HWPX·XLSX 생성과 표·제목 텍스트 보존 검사를 통과했다. PDF 텍스트 검사도 모두 통과했으며 각 언어 현재 29쪽, 누적 57쪽이다. 산출물은 `tmp/penalized-remaining-review/exports`에 저장했다. 내보내기는 캡처한 결과를 사용하며 모델을 재적합하지 않았다.

공용 `validate_longitudinal_count_exports.R`를 재사용했으므로 검증용 표지 제목은 Longitudinal이지만 본문은 이번 세 규제 회귀 결과이다. 본표 동일성은 전용 고급 옵션 검사에서 따로 확인했다. `validate_i18n_contract.R`와 변경 경로의 `git diff --check`도 통과했다. 최초 계약 검사 실행의 로캘 오류는 UTF-8 로캘을 지정해 재실행하여 해소했다.

기존 매개·조절효과 화면은 범위에서 제외했다. 프로그램 전체 번역 완료를 의미하지 않는다. 설치본 빌드와 문서 앱별 수동 시각 검토는 수행하지 않았다.
