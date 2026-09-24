# Wilcoxon 사후검정 동순위 계산 최적화 — 2026-09-14

## 적용 결과

Friedman 최적화가 이미 적용된 현재 기준본에서, 20,000행 × 6회 측정의 비모수 반복측정 전체 분석 중앙 시간이 연속형 **1.05→0.29초(약 72%)**, 순서형 **0.52→0.28초(약 46%)**로 줄었다. 실제 제품 경로의 각 3회 비교가 모두 빨라졌다. 전체에는 Friedman, Wilcoxon 사후검정 15쌍, 요약·표시표 생성이 포함된다. 다른 데이터에서도 같은 비율을 보장하는 수치는 아니다.

## 변경 범위

`R/analysis_paired_rm.R`의 `paired_rm_posthoc_scale()`에서 Wilcoxon 검정 호출만 지연 생성하는 `paired_rm_wilcox_engine()`으로 연결했다. 대응 2회 분석, 독립표본 분석, UI·내보내기 코드는 변경하지 않았다.

`paired_rm_build_wilcox_engine()`은 설치된 `stats::wilcox.test.default`의 코드 텍스트 SHA256이 검증된 번들과 일치할 때만 함수 복사본을 만든다. `NTIES <- table(r)` 두 문장을 찾았는지도 확인한다. 복사본에서는 `paired=TRUE`일 때 평균 순위의 두 배를 정수 인덱스로 삼아 `tabulate()`로 빈도를 세고 0 빈도를 제거한다. 독립표본 경로는 원래 `table()`을 유지한다.

동순위 빈도의 오름차순·정수 저장형, `NTIES^3 - NTIES` 및 합산 순서를 보존한다. 순위 계산, 0 차이 제거, 연속성 보정, 표준편차, z값, p값, 효과크기, 다중비교 조정은 변경하지 않았다. 신뢰구간 내부의 `NTIES.CI` 계산도 그대로다. 엔진을 반환받아 원래 인수로 직접 호출하므로 검정의 데이터 이름 표현도 보존한다.

코드 서명이나 교체 문장 개수가 다르면 원래 `stats::wilcox.test()`를 반환한다. 엔진은 첫 호출에 생성하여 재사용하고 `stats` 네임스페이스는 수정하지 않는다.

## 시간 측정

R 4.5.3 번들 런타임, 데이터 seed 939, capture seed 718. 연속형은 정규 난수, 순서형은 1~5 균등 표본이며 결측치는 없다. 별도 R 프로세스 3개를 순차 실행하고 각 입력에서 양쪽을 예열한 후 전체 실행을 측정했다. 두 번째 프로세스는 입력 순서와 기존/개선 순서를 뒤집었다. 단위는 초다.

| 입력 | 기존 1/2/3 | 실제 적용 1/2/3 | 중앙값 기존→적용 |
|---|---|---|---|
| 연속형 | 1.06 / 0.94 / 1.05 | 0.33 / 0.29 / 0.28 | 1.05→0.29 |
| 순서형 | 0.55 / 0.52 / 0.52 | 0.28 / 0.40 / 0.28 | 0.52→0.28 |

초기 연구 후보 측정은 연속형 1.09/0.97/1.03→0.32/0.30/0.28초, 순서형 0.53/0.55/0.52→0.30/0.40/0.26초였다. 초기 후보와 실제 제품 경로를 별도로 측정했으며 두 세트를 섞어 중앙값을 계산하지 않았다. 첫 연구 프로세스 이후 다음 시작 시 다른 파일 `R/result_penalized_ui.R`의 일시적 구문 오류로 중단됐다. 해당 파일을 수정하지 않았고 이후 정상 상태에서 2·3번 프로세스를 다시 실행했다. 최종 제품 경로 3회는 모두 정상 종료했다.

메모리, 앱 로딩 시간, 첫 엔진 생성 비용은 별도로 측정하지 않았다. 예열 실행에서는 첫 엔진 생성도 수행되며 결과 동일성을 확인한다.

## 정확성 검증

`scripts/validate_wilcoxon_ties.R` 통과:

- 원시 검정 198조건: 기본 경계 150조건과 신뢰구간/정확검정/대응 여부/동순위/단측·양측/연속성 보정 조합 48조건. 빈 입력, 작은 표본, 0 차이, NA, Inf 등을 포함한다.
- 원시 동순위 빈도 순서 및 보정항 합 12조건. 기존 table의 메타데이터만 제거하고 정수 빈도와 보정항을 정확히 비교했다.
- 대규모 30쌍: 시간 측정에 사용한 두 자료의 모든 사후검정 쌍에서 원시 `htest` 전체가 정확히 일치했다.
- 전체 분석 32조건: 일반/비모수 반복측정 × 연속형/순서형 × 정상/결측/변화 없음/마지막 셀 변화 × Holm/Bonferroni. 정상·결측은 오류가 아님도 확인했다. 기존 변화 없음 오류는 그대로 보존되는지 별도로 비교했다.
- 서명 불일치 시 원래 함수 복귀, 실행 후 엔진 코드 유지, 원래 stats 함수가 변경되지 않았음을 확인했다.

원시 `htest`와 전체 분석은 `identical(..., num.eq=FALSE)`로 비교했다. 통계량·p값·이름·클래스뿐 아니라 경고/메시지·오류의 클래스와 문구, 표준출력, RNG 상태도 비교했다. 반환 결과에 정규화나 수치 허용 오차를 적용하지 않았다. 호출 스택 전체의 동일성을 주장하는 검증은 아니다.

초기 연구 및 제품 경로 측정의 예열·실측 결과도 정확히 일치했다. 제품 엔진의 실행 코드 텍스트가 연구 후보와 같은지 확인했다. `scripts/validate_friedman_ties.R` 재실행도 원시 135조건·보정항 24조건·전체 48조건을 통과했다. 반복 실행한 같은 사례를 새로운 사례로 합산하지 않았다. 수정 파일 `git diff --check` 통과.

결과 구조나 표시·내보내기를 변경하지 않았으며 문서 변환 및 설치 빌드는 재실행하지 않았다.

## 재현

변경 직전 소스, 후보 및 측정 스크립트와 CSV는 `output/wilcoxon-ties-review-20260914/`에 있다. `timing-*.csv`는 초기 후보, `product-timing-*.csv`는 실제 제품 경로다.

```powershell
$env:LC_ALL='English_United States.utf8'
$env:LANG=$env:LC_ALL
$env:STATEDU_NO_PACKAGE_INSTALL='true'
$env:STATEDU_MODULE_CACHE_DIR='output/startup-gzip-level1-20260914/cache-candidate-hit'
& ./packaging/electron/runtime/R-4.5.3/bin/Rscript.exe scripts/validate_wilcoxon_ties.R
& ./packaging/electron/runtime/R-4.5.3/bin/Rscript.exe scripts/validate_friedman_ties.R
1..3 | ForEach-Object {
  & ./packaging/electron/runtime/R-4.5.3/bin/Rscript.exe output/wilcoxon-ties-review-20260914/check.R $_ product
  if ($LASTEXITCODE -ne 0) { throw 'Validation failed' }
}
```
