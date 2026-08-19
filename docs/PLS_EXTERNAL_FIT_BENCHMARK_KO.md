# PLS/PLSc 외부 적합도 수치 비교 절차

## 목적

StatEdu Studio와 SmartPLS 또는 ADANCO의 SRMR, d_G, d_ULS를 비교할 때 자료·모형·추정 설정 차이를 수치 오류로 오인하지 않도록 동일 조건의 결과를 기계적으로 대조한다. 이 비교는 특정 소프트웨어를 절대적 정답으로 간주하지 않으며 구현 동등성과 차이 원인을 추적하기 위한 검증이다. SmartPLS 공식 문서도 saturated/estimated 구분과 PLS 적합도 사용에 관한 연구가 아직 충분하지 않다고 명시하므로, 서로 다른 적합대상을 임의로 같은 값으로 취급하지 않는다.

## 비교 조건

- 동일 원자료, 행 순서, 결측 처리와 표준화 방식
- 동일한 반영형/형성형 및 공통요인/composite 명세
- 동일한 구조경로와 측정 블록
- PLS와 PLSc를 구분하고, 혼합모형이면 PLSc가 보정한 블록 범위를 함께 기록
- saturated/estimated model 중 어느 값을 비교했는지 기록. 현재 StatEdu 진단은 모든 구성개념 상관을 자유롭게 둔 **saturated 측정모형 근사값만** 제공하며, 구조경로 제약을 반영한 SmartPLS `estimated model` 값과 비교하지 않는다.
- 프로그램명·버전, 알고리즘 설정과 실행일을 별도 분석기록에 보존

## 고정 benchmark 묶음 생성

다음 명령은 301명 Holzinger-Swineford 자료의 x1-x9, 세 개의 반영형 공통요인(visual, textual, speed), `visual -> textual`, `visual -> speed`, `textual -> speed` 경로를 사용하는 고정 benchmark를 생성한다.

```powershell
& "C:\Program Files\R\R-4.5.3\bin\Rscript.exe" scripts/generate_pls_external_benchmark.R `
  --output-dir=outputs/pls_external_benchmark
```

생성 파일은 다음과 같다.

- `statedu_fit.csv`: 반올림 전 StatEdu PLS/PLSc saturated 기준값
- `external_fit_template.csv`: SmartPLS/ADANCO 원값 입력용 템플릿
- `benchmark_manifest.json`: 자료·모형 SHA-256, 구성개념/경로, StatEdu·R·seminr 버전과 알고리즘 설정

외부 실행자에게 전달할 완전한 핸드오프 묶음은 다음 명령으로 생성한다.

```powershell
& "C:\Program Files\R\R-4.5.3\bin\Rscript.exe" scripts/prepare_pls_external_handoff.R `
  --output-dir=outputs/pls_external_handoff
```

이 묶음에는 원자료와 StatEdu 모형 스냅샷의 사본, `measurement_model.csv`, `structural_paths.csv`, `statedu_fit.csv`, 값 입력용 `external_fit.csv`, 사전 해시가 기록된 `external_run.json`, 실행 안내문이 포함된다.

전달용 ZIP과 파일별/ZIP SHA-256은 다음 명령으로 새 임시 staging에서 생성한다. 기존 작업 폴더의 오래된 파일은 ZIP에 섞이지 않는다.

```powershell
& scripts/package_pls_external_handoff.ps1
```

기본 산출물은 `outputs/StatEdu_1.2.3_PLS_external_handoff.zip`과 동일 이름의 `.sha256` 파일이다. ZIP 내부 `HANDOFF_SHA256.csv`로 개별 입력 파일의 크기와 해시를 확인한다.

외부 프로그램에는 `sample/HolzingerSwineford1939.csv`와 `sample/pls_external_benchmark.stmodel`에 기록된 동일 모형을 사용한다. SmartPLS 4에서는 standardized results, path weighting, 초기 외부가중치 +1, 고정 stop criterion 10^-7을 사용하고 saturated model 결과를 기록한다. SmartPLS 4의 최대 반복은 3,000회로 고정되어 있지만 StatEdu/seminr는 300회이므로, 양쪽 모두 300회 이전에 수렴했는지 확인한다. benchmark 자료에는 결측값이 없다. PLS와 consistent PLS(PLSc)를 각각 실행하고 외부 프로그램 버전·실행일을 manifest 사본에 기록한다.

## CSV 형식

StatEdu와 외부 프로그램 결과를 각각 다음 열의 CSV로 준비한다.

```text
Model,Fit,srmr,d_G,d_ULS
pls,saturated,0.0840405399973689,0.125566775738875,0.317826556337221
plsc,saturated,0.0789513151368562,0.119832386033816,0.280498957282763
```

표시용 반올림 값이 아니라 가능한 최대 정밀도의 원값을 사용한다. `Model` 값은 대소문자를 구분하지 않는다. `Fit`은 `saturated` 또는 `estimated`여야 하며, `Model`과 `Fit`의 조합은 중복될 수 없다. 현재 고정 benchmark는 양쪽 CSV에 PLS/PLSc `saturated` 행만 둔다. StatEdu가 제공하지 않는 `estimated` 행을 외부 CSV에만 추가하면 비교기는 의미가 다른 행의 혼합을 막기 위해 중단한다.

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

외부 값을 `external_fit.csv`에 입력한 후에는 다음 명령으로 비교표와 증거 해시를 확정한다. `--converged-before-300=true`는 실제 반복기록을 확인한 경우에만 지정한다.

```powershell
& "C:\Program Files\R\R-4.5.3\bin\Rscript.exe" scripts/finalize_pls_external_evidence.R `
  --evidence-dir=outputs/pls_external_handoff `
  --software=SmartPLS `
  --software-version=4.x.x `
  --run-date=2026-08-19 `
  --converged-before-300=true `
  --reported-decimal-places=3
```

`--reported-decimal-places`에는 화면이 아니라 실제 비교에 사용한 Excel/HTML 내보내기 값의 소수 자릿수를 기록한다. 확정 스크립트는 기본 수치 허용오차와 마지막 보고 자릿수의 반 단위 중 큰 값을 절대 허용오차로 사용한다. 따라서 결과는 ‘외부 출력 정밀도 내 일치’로 해석하며 원시 내부값의 완전 동일성을 주장하지 않는다. 스크립트는 여섯 행이 이 기준을 모두 통과한 경우에만 `comparison.csv`를 보존하고 `external_run.json`의 외부 결과·비교표 SHA-256을 갱신한다. 프로그램명, 버전, 실행일, 보고 자릿수 또는 300회 이전 수렴 확인이 빠지면 실패한다.

SmartPLS 공식 참고: [Model Fit](https://www.smartpls.com/documentation/algorithms-and-techniques/model-fit/), [Consistent PLS-SEM](https://www.smartpls.com/documentation/algorithms-and-techniques/consistent-pls/), [PLS-SEM Algorithm](https://smartpls.com/documentation/algorithms-and-techniques/core-algorithm/pls/).
