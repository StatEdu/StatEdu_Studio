# 상관분석 결과표 일괄 생성 — 2026-09-13

`correlation_pair_rows_and_matrices()`가 변수 쌍마다 한 행짜리 데이터프레임을 만든 뒤 합치던 부분을 변경했다. 기존 쌍 순서대로 같은 함수로 각 셀을 포맷한 뒤 열을 모아 데이터프레임을 한 번 생성한다. 계수·p값·신뢰구간 계산과 행렬 생성 함수는 변경하지 않았다.

## 성능

동봉 R 4.5.3, `statedu_apply_preferences()`로 앱 세션과 같이 설정을 적용한 2,000행 합성 자료. 변경 전후 순서를 교대로 5회 측정한 `prepare_correlation_results()` 전체 실행시간 중앙값이다. 이전 방법명 변환 최적화까지 적용된 소스와 비교했다.

| 변수 수 | 방법 | 변경 전 | 변경 후 |
|---|---|---:|---:|
| 20 | Pearson | 0.09초 | 0.05초 |
| 20 | Spearman | 0.14초 | 0.11초 |
| 50 | Pearson | 0.62초 | 0.30초 |
| 50 | Spearman | 0.97초 | 0.66초 |

50변수의 1,225개 쌍에서 데이터프레임 생성을 1,225회에서 1회로 줄였다. 해당 시험의 시간 감소는 Pearson 약 52%, Spearman 약 32%다. 실제 화면 렌더링·내보내기 시간과 프로그램 로딩은 측정에 포함하지 않는다.

## 검증

- 변경 직전 실제 소스와 44개 전체 결과·속성·경고·메시지·오류·난수 상태 정확 일치(`validate_correlation_vector_reuse.R`에 이전 파일 지정).
- `validate_correlation_table_build.R`: 소수 0~5자리, 두 p값 표기 방식, 중복 표시명·한글 이름, 빈 결과·단일 쌍·다중 쌍, 결측 계수 및 CI, 정수/실수 N 혼합 등 72개 표·행렬·열 자료형·포맷 비교 통과.
- 관측상관·잠재상관 각각 화면 HTML과 저장 HTML의 정확 일치, Excel 시트 이름 및 모든 시트 내용 일치 확인.
- `validate_correlation_auto.R` 통과.
- `git diff --check` 통과.

표시·저장 내용 구성은 유지했다. 명시한 시험 범위의 정확 일치 검증이며 모든 입력에 대한 수학적 불변 증명은 아니다. 설치 파일은 재빌드하지 않았다.

재현 자료: `output/correlation-table-performance-20260913/`의 `baseline.R`, `benchmark.R`, `benchmark.csv`, `verify-display.R`, 변경 전후 HTML·Excel 파일.
