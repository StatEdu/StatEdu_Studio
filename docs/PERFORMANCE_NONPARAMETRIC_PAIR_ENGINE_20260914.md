# 2회 대응 비모수 검정 엔진 재사용 — 2026-09-14

## 적용

`R/analysis_nonparametric_paired.R`의 `nonparametric_paired_analyze_pair()`에서 Wilcoxon 호출 한 곳을 기존 `paired_rm_wilcox_engine()`에 연결했다. 반복측정 사후검정에서 검증한 엔진을 재사용하며 계산 코드를 새로 복제하지 않았다. 인수 `paired=TRUE`, `exact=FALSE`, 경고 억제·오류 처리, 효과크기, 0 차이·동순위 안내, 요약표 생성은 그대로다.

엔진의 원본 코드 서명 검증과 불일치 시 stats 함수 복귀도 그대로 사용한다. 이번에는 일반 대응 분석의 `R/analysis_paired.R` 호출이나 UI·내보내기 코드를 수정하지 않았다.

## 실제 적용 코드의 전체 실행 시간

20,000행 × 16열로 8개 대응쌍을 분석했다. 연속형은 정규 난수, 순서형은 1~5 균등 표본이다. 데이터 seed 939, capture seed 718. 결측률은 셀별 독립 확률 0 또는 10%이고 각 대응쌍의 완전 사례를 사용하는 기존 동작을 유지한다. 중앙값/IQR 옵션을 켠 `prepare_nonparametric_paired_results()` 전체를 측정했다.

R 4.5.3 번들 런타임으로 새 프로세스 3개를 순차 실행했다. 각 조건에서 기존/현재를 예열하고 측정했으며 두 번째 프로세스에서는 수준 및 기존/현재 실행 순서를 뒤집었다. 단위는 초다.

| 수준 | 셀별 결측률 | 기존 1/2/3 | 현재 1/2/3 | 중앙값 기존→현재 |
|---|---|---|---|---|
| 연속형 | 0% | 0.41 / 0.39 / 0.42 | 0.06 / 0.04 / 0.03 | 0.41→0.04 |
| 연속형 | 10% | 0.30 / 0.31 / 0.33 | 0.05 / 0.03 / 0.03 | 0.31→0.03 |
| 순서형 | 0% | 0.17 / 0.15 / 0.17 | 0.04 / 0.06 / 0.05 | 0.17→0.05 |
| 순서형 | 10% | 0.13 / 0.14 / 0.13 | 0.04 / 0.03 / 0.05 | 0.13→0.04 |

12회 모두 단축됐다. 결측 없는 조건의 중앙값 기준 단축은 연속형 약 90%, 순서형 약 71%다. 짧은 실행의 타이머 해상도 및 환경 변동이 있으므로 모든 자료에서 동일한 비율을 보장하지 않는다. 메모리와 앱 로딩 시간은 측정하지 않았다.

초기 후보도 별도 3개 프로세스에서 12회 모두 개선됐다. 연속형 무결측 기존 0.41/0.37/0.39→후보 0.05/0.04/0.05초, 결측 0.31/0.31/0.31→0.04/0.03/0.04초; 순서형 무결측 0.17/0.16/0.15→0.05/0.06/0.04초, 결측 0.12/0.14/0.13→0.04/0.03/0.03초였다. 초기 후보와 제품 측정값을 섞어 중앙값을 계산하지 않았다.

## 동일성 검증

`scripts/validate_nonparametric_pair_engine.R`에서 현재 호출 한 곳을 원래 stats 호출로 복원한 독립 기준 함수를 사용한다. 복원 개수가 정확히 한 곳인지 검사한다.

- 전체·통합 결과 48조건: 연속형/순서형 × 일반/결측/0 차이/Inf/일정 차이/factor × 평균·표준편차 또는 중앙값·IQR × 일반 결과/통합 결과. 통합 결과는 `add_mean_sd=TRUE`, `effect_size=TRUE`도 포함한다.
- 대규모 원시 검정 32개: 두 수준 × 두 결측률 × 8쌍 모두의 `htest` 전체를 비교했다. 데이터 이름을 포함한 원시 통계량·p값·클래스도 일치한다.
- 전체 및 원시 비교는 `identical(..., num.eq=FALSE)`로 수행했다. 경고·메시지와 오류의 클래스·문구, 표준출력, RNG 상태도 일치했다. 객체 정규화나 수치 허용 오차를 사용하지 않았다.
- 정상·결측 전체 분석이 오류 반환이 아님도 확인했다. 0 차이 등의 기존 제외·안내 결과는 원래 결과와 비교했다.
- 초기 후보와 실제 적용 경로의 예열·실측 전체 반환값도 정확히 일치했다. 실제 수정 함수와 후보의 실행 코드 텍스트가 일치하는지 검사했다.
- 변경 직전 파일과 직접 diff하여 이번 제품 변경이 호출 한 줄임을 확인했고 `git diff --check`를 통과했다.

엔진 구현은 변경하지 않았으므로 이전 `validate_wilcoxon_ties.R`의 엔진 자체 검증을 불필요하게 반복하지 않았다. 결과 표시·내보내기 구조는 변경하지 않았고 문서 변환이나 설치 파일 빌드는 수행하지 않았다.

## 재현

기준본과 연구/제품 측정 스크립트 및 CSV는 `output/nonparametric-pair-engine-20260914/`에 있다. `timing-*.csv`는 후보, `product-timing-*.csv`는 실제 적용 코드다.

```powershell
$env:LC_ALL='English_United States.utf8'
$env:LANG=$env:LC_ALL
$env:STATEDU_NO_PACKAGE_INSTALL='true'
$env:STATEDU_MODULE_CACHE_DIR='output/startup-gzip-level1-20260914/cache-candidate-hit'
& ./packaging/electron/runtime/R-4.5.3/bin/Rscript.exe scripts/validate_nonparametric_pair_engine.R
1..3 | ForEach-Object {
  & ./packaging/electron/runtime/R-4.5.3/bin/Rscript.exe output/nonparametric-pair-engine-20260914/check.R $_ product
  if ($LASTEXITCODE -ne 0) { throw 'Product validation failed' }
}
```
