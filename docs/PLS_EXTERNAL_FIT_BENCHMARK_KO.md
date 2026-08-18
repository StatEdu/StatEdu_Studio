# PLS/PLSc 외부 적합도 수치 비교 절차

## 목적

StatEdu Studio와 SmartPLS 또는 ADANCO의 SRMR, d_G, d_ULS를 비교할 때 자료·모형·추정 설정 차이를 수치 오류로 오인하지 않도록 동일 조건의 결과를 기계적으로 대조한다. 이 비교는 특정 소프트웨어를 절대적 정답으로 간주하지 않으며 구현 동등성과 차이 원인을 추적하기 위한 검증이다.

## 비교 조건

- 동일 원자료, 행 순서, 결측 처리와 표준화 방식
- 동일한 반영형/형성형 및 공통요인/composite 명세
- 동일한 구조경로와 측정 블록
- PLS와 PLSc를 구분하고, 혼합모형이면 PLSc가 보정한 블록 범위를 함께 기록
- saturated/estimated model 중 어느 값을 비교했는지 기록
- 프로그램명·버전, 알고리즘 설정과 실행일을 별도 분석기록에 보존

## CSV 형식

StatEdu와 외부 프로그램 결과를 각각 다음 열의 CSV로 준비한다.

```text
Model,srmr,d_G,d_ULS
pls,0.041,0.012,0.020
plsc,0.038,0.009,0.017
```

표시용 반올림 값이 아니라 가능한 최대 정밀도의 원값을 사용한다. `Model` 값은 대소문자를 구분하지 않지만 중복될 수 없다.

## 자동 비교

```powershell
& "C:\Program Files\R\R-4.5.3\bin\Rscript.exe" scripts/compare_pls_fit_external.R `
  --statedu=statedu_fit.csv `
  --external=smartpls_fit.csv `
  --absolute-tolerance=1e-6 `
  --relative-tolerance=1e-4 `
  --report=fit_comparison.csv
```

각 지표는 절대오차 또는 상대오차 중 하나가 허용범위 안이면 통과한다. 초과 행이 있으면 스크립트는 실패 종료코드를 반환한다. 차이가 발견되면 먼저 지표 정의, 상관행렬 구성 범위, PLSc 보정 범위, 반올림 전 원값과 saturated/estimated model 설정을 확인한다.
