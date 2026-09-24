# 상관분석 잠재변수 부록 제목 호환성 보완

## 변경

`correlation_results_ui()`가 결과의 `latent` 하위 목록을 함께 렌더링하는 경로에서, 부록 제목의 Latent-variable 접두어가 한국어 외 언어에서 영어로 남았다. 전용 키 `analysis.correlation.latent_variable_prefix`를 8개 언어 사전에 추가하고 해당 접두어를 번역했다. 영어 본표의 제목과 계수는 그대로 유지한다. 한국어의 기존 잠재변수 표기도 유지한다.

현재 `prepare_correlation_results()`는 `latent = NULL`을 반환한다. 따라서 이번 보완은 현재 기본 분석 화면의 새 누락이 아니라, 별도 잠재변수 결과를 포함한 객체의 표시 호환 경로를 대상으로 한다.

## 검증

`validate_correlation_latent_prefix_i18n.R`는 기존 상관방법 검사를 실행해 실제 관측 상관 및 잠재반응 상관을 계산한다. 그런 다음 두 결과를 `latent` 하위 목록 구조로 결합하고 직렬화 왕복하여 호환성 사례를 구성했다. 실제 과거 저장 파일을 검사한 것은 아니다.

- 8개 언어에서 잠재변수 부록 제목의 번역 확인.
- 두 영어 본표의 제목·행렬 내용 및 전체 결과 객체 보존 확인.
- 기존 `validate_correlation_methods_i18n.R`의 Kendall·점이연·phi·Cramer's V·eta·polyserial·polychoric·tetrachoric 결과 및 사용자 라벨 검사 통과.
- 기존 `validate_correlation_i18n.R`의 Pearson/Spearman·정규성·제외 변수·행렬 사용자 라벨 검사 통과.

## 저장 검사

한국어·일본어 각각 현재/누적 HTML·PDF·DOCX·HWPX·XLSX 생성과 표·제목 텍스트 보존 검사를 통과했다. PDF 텍스트 검사도 통과했으며 두 언어 모두 현재 5쪽, 누적 8쪽이다. 산출물은 `tmp/correlation-latent-prefix-i18n/exports`에 있다. 위 호환성 사례의 화면 캡처를 저장에 사용했으며 내보내기 중 분석을 재실행하지 않았다.

공용 `validate_longitudinal_count_exports.R`를 재사용하여 검증용 표지 제목은 Longitudinal이지만 본문은 상관분석 호환성 사례이다. 본표 동일성은 전용 검사에서 확인했다. `validate_i18n_contract.R`와 변경 경로의 `git diff --check`도 통과했다.

기존 매개·조절효과 화면은 제외했다. 설치본 빌드 및 문서 앱별 수동 검토는 수행하지 않았다. 프로그램 전체 번역 완료를 의미하지 않는다.
