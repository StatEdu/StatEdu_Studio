# 프로파일링과 상관방법 이름 변환 제거 — 2026-09-13

초기 패키지/모듈 로딩과 상관분석을 Rprof로 조사했다. 초기 패키지/모듈 로딩의 단일 관측은 약 1.25초였으며 실제 창 표시·Shiny 연결·데이터 로딩을 포함하지 않는다. 로딩 코드는 변경하지 않았다.

설정 적용 전 독립 실행에서는 표시 형식 조회가 설정 파일을 반복 읽었다. 실제 앱의 세션은 `statedu_apply_preferences()`를 실행하므로, 이 조건을 맞춘 뒤 다시 프로파일링했다. 설정 적용 후 수동 상관분석의 샘플링 프로파일에서 `tools::toTitleCase()`가 상당한 비용을 차지했다. 샘플링 비율은 전체 벽시계 시간의 정밀 분해가 아니다.

## 구현

`correlation_method_for_pair()`는 수동 연속형 상관방법을 이미 Pearson·Spearman·Kendall의 세 값으로 검증한다. 이 경로에서 `tools::toTitleCase(method)`를 두 번 호출하던 것을 세 개의 고정 표시 문자열을 선택하는 방식으로 바꿨다. 표에 쓰이는 이름과 선택 이유 문장은 그대로다. 통계 계산, 자동 방법 선택, 잠재상관 경로, 자료와 설정은 변경하지 않았다.

## 검증

앱처럼 설정을 적용한 2,000행·20변수 연속형 합성 자료에서, 변경 전후 순서를 교대로 5회 측정한 전체 `prepare_correlation_results()` 중앙값은 다음과 같다. 초기 모듈 로딩은 제외했다.

| 방법 | 변경 전 | 변경 후 |
|---|---:|---:|
| 수동 Pearson | 0.14초 | 0.10초 |
| 수동 Spearman | 0.20초 | 0.16초 |
| 수동 Kendall | 7.20초 | 7.14초 |
| 자동 선택 | 0.12초 | 0.12초 |

Pearson은 약 29%, Spearman은 20% 감소했다. Kendall의 작은 차이는 측정 변동을 고려해야 한다. 자동 선택 경로에는 이번 변경이 적용되지 않는다. 원자료는 `method-label-benchmark.csv`에 있다.

- `validate_correlation_method_labels.R`: 192개 측정 수준·방법·정규성 조합의 방법명과 설명문 정확 일치.
- `validate_correlation_vector_reuse.R`에 변경 직전 실제 파일을 전달하여 44개 전체 결과·속성·경고·메시지·오류·난수 상태 정확 일치. `identical(..., num.eq=FALSE)` 사용.
- `validate_correlation_auto.R`: 자동 선택 및 잠재상관 추론 검증 통과.
- `git diff --check` 통과.

프로파일링 실행 중 두 검증 프로세스가 같은 임시 모듈 캐시를 만들면서 한 프로세스가 불완전 캐시를 읽었다. 기존 개별 소스 재로딩 경로로 복구했다. 통계 비교는 통과했으며 성능 측정은 모듈 로딩 후 실행했다. 재현 시 프로세스별 캐시 디렉터리를 분리하거나 순차 실행해야 한다.

설치 파일은 재빌드하지 않았다. 재현 자료: `output/performance-profile-next-20260913/`의 `profile.R`, `profile-session.R`, 프로파일 CSV, `correlation-baseline.R`, `benchmark-labels.R`.
