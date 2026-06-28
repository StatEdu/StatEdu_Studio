# StatEdu Studio

**StatEdu Studio**는 가정 검토를 기반으로 통계분석을 수행하고 논문/보고서용 결과표를 생성하는 로컬 Shiny 애플리케이션입니다.

앱은 사용자의 Windows PC에서 실행되며 로컬 브라우저 세션으로 열립니다. 데이터는 사용자의 PC에서 분석되고 외부 서버로 전송되지 않습니다.

모든 통계분석은 CRAN 패키지만 사용합니다.

## 현재 버전

현재 공개 버전: `1.0.1`

버전 1.0.1은 로컬 데이터 불러오기, 데이터 편집, 가정 검토 기반 통계분석, 표본수/효과크기 계산기, HTML/PDF 결과 출력 기능을 안정화한 공개 패치 릴리스 라인입니다. 자세한 개발 이력은 **About > Version History**에서 확인할 수 있습니다.

Public 1.0에서는 라이센스 활성화, 유료 edition gating, Excel/Word 결과 저장, Mplus/latent add-on, 종단/패널 분석 workflow를 노출하지 않습니다. 이 항목들은 개발 이력이나 계획 문서에 나타날 수 있지만 public 1.0 제공 기능으로 해석하지 않습니다.

## 현재 범위

- `StatEdu_Studio.bat`를 통한 로컬 Windows 실행
- SPSS SAV, SAS, Stata, Excel XLS/XLSX, CSV, DAT 파일 불러오기
- 클라우드 동기화 폴더의 파일을 임시 로컬 경로로 복사한 뒤 불러오는 파일 처리
- 파일 불러오기, 변수 선택, measurement level 검토, 변수 라벨, 범주값 라벨을 포함한 데이터 작업 흐름
- 코딩 오류 점검, Likert 변환, 결측값 처리, 역코딩, 계산 변수, 수식 기반 변수 변환, 리코딩, 이름 변경을 위한 Data Editor 도구
- 범주형 및 연속형 변수의 빈도분석/기술통계
- 이분형, 순서형, 범주형 변수의 교차표 분석
- 정규성 검토, 등분산성 검토, 사후분석 옵션, 효과크기, 추세 옵션, 비모수 대체 방법을 포함한 t-test / ANOVA
- 표준 ANCOVA, Robust ANCOVA(HC3), Ranked ANCOVA, Interaction ANCOVA 중 자동 선택 또는 경고 표시를 지원하는 ANCOVA. Levene / Brown-Forsythe / Breusch-Pagan / fitted-value White 방식 분산 검토, 기울기 동질성 진단, complete-case 보고, 선형성 그림, 영향점 민감도 분석 포함
- Mann-Whitney U와 Kruskal-Wallis 흐름을 사용하는 독립 비모수 검정
- 두 시점 또는 세 시점 이상 반복측정을 위한 paired test 및 repeated-measures paired workflow
- Wilcoxon signed-rank와 Friedman 흐름을 사용하는 독립 비모수 paired test
- 자동 방법 선택, p 값/신뢰구간 출력, 방법 노트, 선택 이유 노트, 산점도 행렬, heatmap을 포함한 상관분석
- Pearson 및 polychoric matrix 옵션, 진단, 그림, score 저장 보조 기능을 포함한 요인분석 및 주성분분석
- 척도 및 문항 수준 요약을 위한 신뢰도 분석
- OLS, HC3 robust standard errors, bootstrap 신뢰구간, HC3와 bootstrap 결합 출력을 가정 검토에 따라 제공하는 선형회귀
- 블록별 모형 비교를 포함한 위계적 회귀
- 이분형, 순서형, 범주형 종속변수를 위한 로지스틱 회귀
- 독립 관측자료의 Gaussian, binary logistic, Gamma, count outcome을 위한 일반화선형모형(GLM). Poisson 대 negative-binomial 선별, robust standard error 옵션, 결측 민감도 분석, offset/exposure 처리, SCI 스타일 진단, publication notes, reporting checklist, suggested manuscript text 포함
- 심한 다중공선성 상황을 위한 penalized regression 보조 분석
- 방법 노트와 참고문헌을 포함한 독립 표본수, power, 효과크기 계산기
- HTML, PDF, 그림, 누적 Result collection 저장
- Result collection의 HTML 및 PDF 출력

전체 공개 분석 방법 목록은 [docs/ANALYSIS_METHODS_KO.md](docs/ANALYSIS_METHODS_KO.md)를 참고하십시오.

## 실행 환경

- 테스트 개발 환경: Windows의 R 4.5.3
- 앱 프레임워크: Shiny 로컬 앱
- 패키지 출처: 명시된 런타임 및 분석 의존성은 CRAN 패키지
- 실행 방식: `127.0.0.1`의 로컬 브라우저 세션. 데이터는 사용자의 PC에 남아 있음

일부 설치된 패키지 바이너리는 런타임 R보다 최신 patch-level R에서 빌드되었을 수 있습니다. 이러한 빌드 경고는 패키지 로딩이나 검증이 실패하지 않는 한 정보성 경고입니다.

## R 패키지

| 영역 | 패키지 | **StatEdu Studio**에서의 역할 |
|---|---|---|
| 앱 UI | `shiny`, `DT`, `htmltools`, `markdown` | Shiny 앱 shell, interactive table, HTML helper, About 문서 렌더링 |
| 데이터 불러오기 | `haven`, `readr`, `readxl`, `openxlsx` | SAV, SAS, Stata, CSV, DAT, XLS, XLSX 불러오기 |
| 설정 및 데이터 보조 | `jsonlite`, `xml2`, `rvest`, `callr` | 설정 직렬화, HTML/XML 처리, background R process 지원 |
| 회귀 진단 | `car`, `lmtest`, `sandwich`, `nortest`, `boot` | Type II/III ANCOVA table, Levene 방식 분산 검토, Breusch-Pagan test, HC3 robust SE, Lilliefors normality test, bootstrap inference |
| 선형/일반화 모형 | `MASS`, `nnet`, `lmtest`, `sandwich`, `geepack`, `mice`, `lme4`, `lmerTest`, `plm` | GLM robust inference, ordered logistic, multinomial, 모형 지원 utility |
| Penalized regression | `glmnet` | Ridge, LASSO, Elastic Net 보조 분석 |
| 사후분석 및 집단 비교 | `agricolae` | ANOVA 계열 workflow에서 사용하는 multiple-comparison 절차 |
| 신뢰도, 요인분석, 상관 | `psych`, `polycor` | 신뢰도 계수, factor/PCA helper, polychoric/polyserial/tetrachoric correlation 지원 |
| 표본수 및 power | `longpower`, `WebPower`, `TOSTER` | Cluster trial / SEM power, exact TOST equivalence 계산 |
| 보고서 출력 | `officer`, `flextable`, `openxlsx` | 보고서 표 지원 |

## 로컬 실행

1. R을 설치합니다.
2. **StatEdu Studio** 폴더의 압축을 풉니다.
3. `StatEdu_Studio.bat`를 더블클릭합니다.

앱은 기본 브라우저의 `127.0.0.1`에서 열립니다. Launcher는 `Rscript.exe`를 찾고, 필요한 런타임 패키지가 없으면 R을 통해 설치한 뒤 로컬 PC에서 Shiny 앱을 시작합니다.

## 검증

버전 1.0.1은 안정화 검증 suite를 이어받고, 최종 릴리스용 Electron metadata 검사를 추가합니다. 공개 검증 범위에는 계산기, 데이터 불러오기, 데이터 편집, 교차표, 상관분석 자동 선택, 요인분석/PCA, 로지스틱 분석과 UI, paired guard 처리, p 값 형식, 회귀계수 출력, GLM 출력, t-test / ANOVA guard 처리가 포함됩니다. 효과크기 비교는 정의가 앱 계산과 일치하는 경우 `effectsize`를 검증 기준으로 사용합니다. `effectsize`는 런타임 필수 패키지가 아닙니다.

병합 또는 패키징 전에는 repository root에서 안정화 검증 suite를 실행합니다.

```powershell
powershell -ExecutionPolicy Bypass -File scripts\validate_stabilization.ps1
powershell -ExecutionPolicy Bypass -File scripts\validate_stabilization.ps1 -Full
```

Release candidate 준비 전에는 Shiny 및 Electron smoke check를 실행합니다.

```powershell
powershell -ExecutionPolicy Bypass -File scripts\release_preflight.ps1
powershell -ExecutionPolicy Bypass -File scripts\smoke_shiny_app.ps1
powershell -ExecutionPolicy Bypass -File scripts\smoke_electron_release.ps1 -SkipUnpackedChecks
```

Electron packaging이 완료되면 full packaged-output preflight를 실행합니다.

```powershell
powershell -ExecutionPolicy Bypass -File scripts\release_preflight.ps1 -FullElectronSmoke
```

자동 검사 통과 후에는 [docs/RELEASE_MANUAL_QA.md](docs/RELEASE_MANUAL_QA.md)를 완료하고, 완료된 QA 기록을 release notes 및 validation artifacts와 함께 보관합니다.

개별 `scripts/validate_*.R` 파일은 특정 모듈을 수정하는 중간 단계에서만 사용하고, 최종적으로는 stabilization suite를 실행합니다.

## 인용

연구에서 **StatEdu Studio**를 사용한 경우 다음과 같이 인용해 주십시오.

LEE, I. H. (2026). **StatEdu Studio** (Version 1.0.1) [Computer software].
https://doi.org/10.22934/statedu.studio

## 개발 모델

이 프로젝트는 비공개로 개발되고 검증 후 공개 릴리스됩니다. 공개 릴리스에는 source code, documentation, example data, validation notes가 포함되어야 합니다.
