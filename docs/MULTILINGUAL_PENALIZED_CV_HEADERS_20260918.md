# 규제 회귀 교차검증 부록 열 제목 번역

## 변경

실제 LASSO 결과의 부록에 표시되는 CV folds, CV MSE, CV SE, CV RMSE, CV MAE, CV R², Apparent R², N complete의 8개 열 제목을 8개 언어 사전에 반영했다. 영어·기존 한국어 표현과 통계 계산·렌더링 코드는 유지했다. lambda 같은 모수 기호는 이번 대상에서 제외했다.

## 검증

`validate_penalized_cv_headers_i18n.R`는 60명·연속형 예측변수 2개로 실제 LASSO 분석을 수행한다. 검증 속도를 위해 선택 안정성 부트스트랩은 2회, 검증 반복은 1회로 설정했다. 이는 표시 검증용 설정이며 충분한 통계적 안정성 검증을 의미하지 않는다.

- 보완 전 6개 외국어 사전 누락 재현, 보완 후 실제 부록의 8개 열 제목이 8개 언어로 표시되는지 확인.
- 영어 본표의 언어 간 동일성과 전체 결과 객체 보존 확인.
- 기존 `validate_penalized_i18n.R`의 Ridge·LASSO·Elastic Net × 8개 언어 검사와 `validate_i18n_contract.R` 통과. 번역 사전의 `git diff --check` 통과.
- 한국어·일본어 각각 현재 결과와 누적 결과를 HTML·PDF·DOCX·HWPX·XLSX로 저장했다. HTML 및 문서·스프레드시트 내부 텍스트에서 캡처한 표·제목 내용 보존을 확인했고, PDF 텍스트 검사도 모두 통과했다. PDF는 언어별 현재 8쪽, 누적 15쪽이다.

저장 검사는 실제 LASSO 결과 캡처를 재사용하며 내보내기 중 모델을 다시 적합하지 않았다. 공용 `validate_longitudinal_count_exports.R`를 재사용하여 검증 문서의 표지 제목은 Longitudinal이지만, 본문은 이번 규제 회귀 캡처이다. 규제 회귀 본표의 언어 간 동일성은 전용 검사에서 별도로 확인했다. 산출물은 `tmp/penalized-cv-headers-i18n/exports`에 있다.

기존 매개·조절효과 화면은 제외했다. 설치본 빌드 및 문서 앱별 수동 시각 검토는 수행하지 않았다. 이번 검증은 표 제목·내용에 초점을 두며 그래프 렌더링 전체를 검증한 것은 아니다.
