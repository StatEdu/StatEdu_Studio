# 상관분석 빈 결과 안내 번역

## 변경

`correlation_results_ui()`가 비어 있는 결과 객체를 받을 때 영어로만 표시하던 `No correlation results to show.`를 전용 키 `analysis.correlation.no_results`로 연결하고 8개 언어에 반영했다. 한국어는 `표시할 상관분석 결과가 없습니다.`이다.

결과가 아직 없는 `NULL` 입력은 기존처럼 화면을 생성하지 않는다. 통계 계산·본표·그래프는 변경하지 않았다.

## 검증 범위

`validate_correlation_empty_i18n.R`에서 빈 목록과 0×0 상관행렬을 가진 객체를 각 언어로 렌더링해 안내를 확인했다. 이는 빈 객체를 직접 입력한 분기 검증이며 실제 분석 실패를 발생시킨 검사는 아니다. `NULL` 입력의 비표시 동작과 원본 객체 보존도 확인했다.

기존 `validate_correlation_i18n.R`의 실제 Pearson/Spearman 분석, 영어 본표, 네 부록, 사용자 라벨 및 10개 번역 문구 검사도 8개 언어에서 통과했다. 저장 검증용 캡처는 이 실제 결과와 빈 결과 안내를 함께 담았다.

## 저장 검사

한국어·일본어 현재/누적 HTML·PDF·DOCX·HWPX·XLSX 저장 검사를 통과했다. 공용 표·제목 검사에 더해 `validate_correlation_empty_exports.py`가 빈 결과 안내 문장 자체를 HTML 및 DOCX/HWPX/XLSX 내부 텍스트에서 확인하고 PDF 기대 문구에도 추가했다. PDF 텍스트 검사까지 통과했으며 두 언어 모두 현재 3쪽, 누적 4쪽이다.

산출물은 `tmp/correlation-empty-i18n/exports`에 있다. 화면 캡처를 저장에 사용하며 내보내기 중 분석을 재실행하지 않았다. 공용 `validate_longitudinal_count_exports.R` 재사용으로 검증용 표지 제목은 Longitudinal이지만 본문은 상관분석 결과와 빈 결과 안내이다. 본표 동일성은 기존 상관분석 검사에서 별도로 확인했다. `validate_i18n_contract.R`와 변경 경로의 `git diff --check`도 통과했다.

기존 매개·조절효과 화면은 제외했다. 설치본 빌드 및 문서 앱별 수동 시각 검토는 수행하지 않았다. 프로그램 전체 번역 완료를 의미하지 않는다.
