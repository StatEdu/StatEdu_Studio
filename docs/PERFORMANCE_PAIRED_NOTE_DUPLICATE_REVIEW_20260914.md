# 대응분석 동순위 안내 검사 검토 — 2026-09-14

## 결정

`paired_wilcoxon_note()`에서 고유값 전체를 생성하는 조건을 `anyDuplicated(nonzero_abs) > 0L`로 바꾸는 후보를 시험했다. 검증한 안내 문구와 전체 결과는 같았지만 로그정규 자료의 전체 실행이 세 번 모두 느려져 **반영하지 않았다**. 제품 코드는 유지했다.

## 후보 및 검증

연구 환경에 로드한 함수 복사본에서 `length(nonzero_abs) > length(unique(nonzero_abs))` 조건만 바꿨다. 유한값·0 차이 처리, 안내 문구와 순서, 검정·효과크기는 그대로다.

- 원시 안내 15조건: 빈 벡터, NA/NaN/Inf, 부호 있는 0, 양·음 동일 절댓값, 인접 double, 중복 없음, 처음/끝 중복, 여러 크기의 연속·반올림 자료.
- 전체 분석 9회: 세 분포 × 독립 프로세스 3회. 예열·실측 반환값과 경고/메시지 및 오류의 클래스·문구, 표준출력, RNG를 `identical(..., num.eq=FALSE)`로 비교했다. 모든 비교를 통과했고 정상 분석이 오류 반환이 아님도 확인했다.
- 반환 객체 정규화나 수치 허용 오차를 사용하지 않았다. 같은 원시 15조건의 반복 실행을 별도 사례로 합산하지 않았다. 수치 검정 내부를 변경한 후보는 아니므로 원시 검정 통계량을 별도로 추적하지 않았다.

## 전체 시간

번들 R 4.5.3, 10만 행 × 16열의 8개 대응쌍, 결측 없음. 정규/로그정규 난수 또는 1~5 균등 순서형 자료. 데이터 seed 939, capture seed 718. `prepare_paired_results()`를 가정검사·중앙값/IQR 옵션을 켜서 측정했다. 새 R 프로세스 3개를 순차 실행하고 양쪽을 예열했다. 두 번째 프로세스는 분포 및 기존/후보 순서를 뒤집었다. 단위는 초다.

| 자료 | 기존 1/2/3 | 후보 1/2/3 | 중앙값 기존→후보 |
|---|---|---|---|
| 로그정규 | 0.39 / 0.35 / 0.39 | 0.47 / 0.37 / 0.48 | 0.39→0.47 |
| 정규 | 0.22 / 0.24 / 0.22 | 0.23 / 0.23 / 0.23 | 0.22→0.23 |
| 순서형 | 0.18 / 0.29 / 0.15 | 0.17 / 0.19 / 0.16 | 0.18→0.17 |

순서형은 두 번 빨라지고 한 번 느려졌지만, 로그정규는 세 번 모두 느려졌고 정규는 두 번 느려졌다. 단순한 중복 존재 검사라는 이유만으로 전체 분석이 빨라지는 것은 아니었다. 환경 변동이 있으므로 이 측정으로 모든 입력에서 `anyDuplicated()` 자체가 느리다고 일반화하지 않는다. 메모리·앱 로딩 시간은 측정하지 않았다.

## 재현

기준본, 연구 스크립트, CSV는 `output/paired-note-duplicate-20260914/`에 있다. 제품·UI·내보내기 변경이나 빌드는 수행하지 않았다.

```powershell
$env:LC_ALL='English_United States.utf8'
$env:LANG=$env:LC_ALL
$env:STATEDU_NO_PACKAGE_INSTALL='true'
$env:STATEDU_MODULE_CACHE_DIR='output/startup-gzip-level1-20260914/cache-candidate-hit'
1..3 | ForEach-Object {
  & ./packaging/electron/runtime/R-4.5.3/bin/Rscript.exe output/paired-note-duplicate-20260914/check.R $_
  if ($LASTEXITCODE -ne 0) { throw 'Validation failed' }
}
```
