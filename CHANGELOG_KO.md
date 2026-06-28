# 변경 이력

## v1.0.1 - 2026-06-28

### 변경

- 결과표는 영어로 유지하면서, 주요 메뉴, 설정 화면, 계산기, 문서 화면, 알림 메시지의 한글/영문 UI 전환을 안정화했습니다.
- 데이터 편집과 분석 메뉴를 범주별로 정리하고, 한글 라벨과 버튼/탭 배치를 정돈했습니다.
- t-test / ANOVA의 왜도/첨도 정규성 기준 옵션, 평균순 post-hoc 표시, 교차분석 표 레이아웃과 내보내기 동작을 보완했습니다.
- EQ-5D와 ASCVD10 계산기에 출력 변수명 입력 기능을 추가하고 계산기 패널 레이아웃을 개선했습니다.
- 업데이트 확인 기능, `.studio` 파일 연결용 패키징 메타데이터, `.studio` 파일 아이콘을 추가했습니다.
- 버그 신고, 기능 개선 요청, 분석기법 요청, Q&A, 업데이트 확인을 묶은 도움말 메뉴를 추가했습니다.
- 도움말 요청 항목을 StatEdu Studio 홈페이지 글쓰기 폼으로 연결하고, Q&A는 UI 언어에 맞춰 연결되도록 했습니다.
- 1.0 사용자 가이드 캡처와 한/영 문서 자산을 갱신했습니다.

## v1.0.0 - 2026-06-25

### 변경

- 안정화된 릴리스 라인을 공개 1.0.0 버전 metadata로 승격했습니다.
- Electron package metadata를 beta 이름에서 최종 **StatEdu Studio** 릴리스 이름으로 변경했습니다.
- 공개 1.0에서 미루는 기능, DOI 검증, 홈페이지 검증, packaged QA gate를 릴리스 문서에 명확히 유지했습니다.
- ordered post-hoc marker가 평균 순서에서 직접 유의한 비교만 표시하도록 수정했습니다. 한 쌍이 유의하지 않은데도 `b>a>c`처럼 transitive chain으로 보이는 표시를 피합니다.
- 교차표 PDF 출력에 너비 기반 방향 선택을 추가했습니다. 넓은 primary table은 landscape로 출력하고, 좁은 표와 보조 표는 portrait를 유지합니다.
- t-test / ANOVA 설정에 왜도/첨도 정규성 기준 선택 옵션을 추가했습니다: 2/5, 2/7, 3/7. 기본값은 2/7입니다.
- 설정 저장 대화상자가 데이터 파일 경로를 알 수 있을 때 해당 데이터 파일 폴더에서 열리도록 수정했습니다.
- 시작 시 유효한 pending Excel file path가 없으면 Excel import review panel이 나타나지 않도록 수정했습니다.

## v0.9.42 - 2026-06-23

### 추가

- 반복측정 열을 longitudinal / panel analysis 전에 long-format data로 변환하는 **Data Editor > Wide to Long** workflow를 추가했습니다.
- Wide-to-long reshaping 검증과 data-editor recoding 검증 범위를 확장했습니다.
- Release-candidate 검증을 위한 Shiny startup 및 Electron release smoke checks를 추가했습니다.
- Release-candidate 문서 검사를 위한 tracked documentation UTF-8 validation을 추가했습니다.

### 변경

- Data Editor panel, button, spacing, data-viewer layout을 t-test / ANOVA dialog pattern에 맞춰 표준화했습니다.
- Missing-value handling이 user-missing marking과 system NA conversion을 모두 지원하도록 업데이트했습니다.
- Recode와 rename workflow를 queued-variable removal, aligned target panel, 더 명확한 rule setup으로 개선했습니다.
- Settings files를 `.studio` 형식으로 단순화하고 save/load dialog를 업데이트했습니다.
- Longitudinal / panel model layout과 categorical predictor reference-category reporting을 정교화했습니다.
- Local-only files, generated artifacts, version metadata, release documentation에 대한 release hygiene check를 강화했습니다.
- 현재 1.0 안정화 우선순위를 반영한 Korean product plan 문서를 복원했습니다.
- 현재 Korean user/method documentation reference를 0.9.42로 업데이트하고 오래된 current-version reference 검증을 추가했습니다.
- 1.0 distribution, license, update plan을 0.9.42 안정화 단계에서 검토 완료로 표시했습니다.
- Core stabilization suite에 release hygiene validation을 추가하여 generated Electron staging files와 local artifacts가 실수로 추적되지 않도록 했습니다.
- Packaging, DOI, website, 1.0 deferral decisions를 위한 release readiness status tracking과 validation을 추가했습니다.
- Full stabilization validation, Shiny startup smoke, Electron release smoke checks를 실행하는 release preflight script를 추가했습니다.
- Packaging, DOI, website, edition gating, license, update, public-release note decisions를 기록하는 1.0 decision log를 추가했습니다.
- 표준 3-block Data Editor button placement에 대한 UI layout contract와 validation을 강화했습니다.
- Release-candidate visual, data, analysis, export, packaged Electron check를 위한 manual QA protocol을 추가했습니다.
- Manual QA protocol을 Electron release smoke checks에 연결했습니다.
- Passing release preflight check를 release readiness status에 기록했습니다.
- Unresolved public 1.0 blockers에 대한 release metadata validation을 강화했습니다.
- Release-candidate QA evidence를 위한 manual QA record template과 validation을 추가했습니다.
- `outputs/`에 추적되던 generated comparison artifacts를 제거하고 root output artifacts를 release hygiene validation에서 차단했습니다.
- README와 version metadata validation에 full packaged-output Electron preflight command를 문서화했습니다.
- DOI target landing URL이 `https://studio.statedu.com`임을 release-gate documentation에 명확히 했습니다.
- 1.0 distribution, license, update plan이 planning material이며 gated editions, license activation, updater, public installer infrastructure가 구현되었다는 주장으로 읽히지 않도록 정리했습니다.
- 공개 1.0 citation announcement 전 intended DOI가 resolve되어야 한다는 README 및 release-readiness warning을 추가했습니다.
- Public release source availability, third-party notices, license reports, bundled license text references에 대한 source/license notice validation을 강화했습니다.
- Completed QA evidence를 release notes 및 validation artifacts와 함께 보관하도록 release checklist, README, manual QA guidance를 정렬했습니다.
- Public 1.0 installer 전 0.9.x beta Electron package name을 교체해야 하는 release gate를 문서화했습니다.
- Latent Mplus settings dialogs를 `.studio` 전용 settings file contract에 맞췄습니다.
- 1.0 distribution/license/update plan에서 historical beta-version notes가 현재 릴리스 기준처럼 읽히지 않도록 업데이트했습니다.
- Public release notes와 사용자 표시 텍스트가 구현되지 않은 gated editions, license activation, in-app updates, public installer infrastructure를 주장하지 않도록 manual QA gate를 추가했습니다.
- Latent Mplus builder placeholder notifications를 명시적인 not-enabled release messages와 validation으로 교체했습니다.
- 사용할 수 없는 계산기가 future feature promises처럼 읽히지 않도록 effect-size fallback subtitle을 수정했습니다.
- 메뉴 assembly code에서 사용하지 않는 Analysis placeholder tab helper를 제거했습니다.
- Legacy settings formats는 public settings dialogs에 노출하지 않고 internal compatibility identifiers는 문서에 유지하도록 release checklist를 명확히 했습니다.

## v0.9.41 - 2026-06-20

### 변경

- Analysis, Sample Size, Effect Size navigation menus를 일관된 1단계 통계 범주로 묶었습니다.
- Cross-tabulation variable transfer controls에서 선택한 column 또는 row variables가 available variable list로 안정적으로 돌아가도록 수정했습니다.
- Column 및 row target panels의 cross-tabulation transfer-button placement를 조정했습니다.

## v0.9.40 - 2026-06-20

### 변경

- 0.9.39 beta release 이후 development metadata를 갱신했습니다.
- 앱 header, About text, launcher naming, installer metadata, favicon, logo assets, default export filenames 등 product-facing surfaces를 **StatEdu Studio**로 rebrand했습니다.
- DOI, environment, path, backward-compatible data lookup 이유로 남아 있는 legacy identifiers를 문서화하는 brand compatibility release checks를 추가했습니다.
- Shiny client binding 준비 전 `Shiny.setInputValue` 오류가 발생하지 않도록 client startup 중 Shiny input calls를 보호했습니다.
- Transitive `undici` development dependency가 npm audit findings 없이 resolve되도록 Electron package-audit lock metadata를 업데이트했습니다.
- Electron package author metadata를 추가하고 local development 및 Electron staging에서 `StatEdu_Studio_*.zip` release artifacts를 ignore했습니다.

## v0.9.39 - 2026-06-18

### 추가

- GEE, LMM, GLMM, panel fixed-effects, panel random-effects models를 위한 별도 `Analysis > Longitudinal / Panel Models` workflow를 추가했습니다.
- Longitudinal / panel model results에 model-specific assumption checks, recommended alternatives, automated sensitivity comparisons, publication-ready estimates, manuscript text, SCI reporting checklist, software-version reporting을 추가했습니다.
- Longitudinal / panel Missing options tab을 추가하여 primary missing-data handling, 실제 MI/IPW/WGEE sensitivity engines, report-level missing-data method tracking을 지원했습니다.
- Longitudinal analysis weights를 추가하여 single weight-variable target, sampling/longitudinal/IPW/combined weight types, trimming, normalized final weights, effective sample size reporting을 지원했습니다.
- Longitudinal count/rate models를 위한 optional exposure / offset handling을 추가하고 primary 및 sensitivity fits에 `log(exposure)` offsets를 포함했습니다.
- Observed zero proportions와 Poisson-expected zero proportions를 비교하는 count-model zero-inflation screening details를 추가했습니다.
- Gaussian, binary logistic, Gamma, count GLMs를 위한 active `Analysis > GLM` workflow를 추가하고 GEE count-family screening approach를 재사용하여 Poisson vs negative-binomial model selection을 수행했습니다.
- GLM SCI-oriented reporting details를 추가하여 complete-case missing-data handling, count-family Poisson/negative-binomial selection, logistic EPV/separation/sparse-cell screening, independent-observation review, influence diagnostics, publication table notes, reporting checklists, suggested manuscript text를 포함했습니다.
- Complete-case, multiple imputation, inverse-probability weighted missing-data handling을 위한 Missing tab이 포함된 tabbed GLM options를 추가했습니다.
- Korean User Guide, Analysis Methods, Method Notes에 GLM documentation을 추가하여 family/link selection, missing-data sensitivity, robust SE options, count overdispersion, SCI reporting expectations를 설명했습니다.
- GLM outputs에 HTML, PDF, Excel, saved Result collection support를 추가하고 publication notes, SCI checklists, manuscript text, software-version sheets를 포함했습니다.
- Longitudinal / panel model outputs에 HTML, PDF, Excel, saved Result collection support를 추가했습니다.
- Longitudinal / panel model fitting, GLM fitting, setup UI structure, assumption-check catalogs, HTML export, Excel export, sensitivity comparisons, SCI reporting sections에 대한 validation coverage를 추가했습니다.

### 변경

- Longitudinal / Panel Models setup screen을 기존 t-test / ANOVA-style transfer layout에 맞추고 model-relevant options만 조건부로 표시하도록 조정했습니다.
- Longitudinal / panel Model과 Terms options를 병합하여 model type, fixed-time terms, random-effect terms를 한 tab에서 설정하도록 했습니다.
- 기본 GEE working correlation을 exchangeable로 변경했고 AR(1) fits는 subject/time wave ordering을 `geepack::geeglm`에 전달하도록 했습니다.
- GEE의 negative-binomial count fits를 marginal negative-binomial GLM with subject-cluster robust SE로 relabel했습니다. geepack이 native negative-binomial GEE를 제공하지 않기 때문입니다.
- Optional Cluster ID는 LMM/GLMM에서 추가 random-intercept grouping variable로 사용되며 선택한 GEE/panel primary fit에는 적용되지 않음을 명확히 했습니다.
- LMM/GLMM missing-data handling을 available repeated measures를 사용하는 likelihood-based MAR analysis로 설명하고 MI/IPW는 기본 primary fit이 아니라 sensitivity analyses로 유지했습니다.
- Heavy optional package dependencies를 추가하지 않기 위해 zero-inflated 및 hurdle count models를 default longitudinal / panel module에서 제외하고, excess-zero findings는 screening guidance로 보고하도록 했습니다.
- 기존 Generalized scaffold를 working GLM setup, run handler, coefficient table, fit statistics, robust SE option, overdispersion check, optional VIF diagnostics로 교체했습니다.
- Launcher-time package installation이 app runtime과 같은 required-package list를 사용하도록 `run_app.R`을 `R/app_bootstrap.R`과 정렬했습니다.
- Electron runtime pin을 39.8.6으로 업데이트하고 package-audit review 후 lockfile을 갱신했습니다.
- Electron beta build script가 PATH 밖의 Rscript를 찾을 수 있고 packaged app에서 제외되는 optional latent Mplus module 때문에 실패하지 않도록 강화했습니다.
- 새 longitudinal / panel 및 GLM workflows에 맞춰 README, Korean User Guide, Analysis Methods, Method Notes documentation을 업데이트했습니다.

## v0.9.38 - 2026-06-15

### 추가

- .80 power에 대한 Fritz & MacKinnon (2007) empirical mediation sample-size table estimates를 추가했습니다.
- Fritz & MacKinnon, Monte Carlo, bootstrap, Sobel mediation sample-size calculations에 대한 method-specific mediation references를 추가했습니다.

### 변경

- 1280px-width displays에서 기존 three-block layout을 유지하면서 큰 창에서는 Sample Size results block을 넓혔습니다.

## v0.9.37 - 2026-06-12

### 추가

- 기존 5-point detection sets에 대응하는 4-point Korean and English Likert detection dictionaries를 추가했습니다.
- Bundled guide images를 사용한 animated action-overlay walkthrough를 Korean in-app user guide에 추가했습니다.

### 변경

- Likert custom detection dictionary UI를 개선하여 registered dictionaries를 button에서 열고 list box에 표시하며 selected dictionary details를 side panel에 보여주도록 했습니다.
- 4-point responses가 4-point scales로 감지되도록 compatible superset matches보다 exact-level Likert dictionary matches를 우선했습니다.
- Data-loading, t-test / ANOVA, result-review walkthrough의 user guide overlay timing과 box positions를 조정했습니다.

### 수정

- Registered Likert detection dictionaries를 열거나 선택할 때 scroll position을 보존했습니다.
- Select 및 detection-name columns의 Likert detection table column widths를 개선했습니다.

## v0.9.36 - 2026-06-11

### 추가

- Registered dictionary review, detail display, editing, deletion이 가능한 editable custom Likert detection dictionary manager를 추가했습니다.
- Logistic analysis, data editor recoding, factor/PCA, correlation, paired tests, data I/O, result history에 대한 validation coverage를 확장했습니다.

### 변경

- Logistic regression, factor analysis, PCA, reliability, paired/repeated tests, correlation, saved result views 전반에서 B5-oriented result-table rendering을 정교화했습니다.
- Sheet preview를 main review panel로 옮기고 import controls를 단순화하여 Excel import review flow를 개선했습니다.
- Converted variables가 요청한 post-conversion measurement type을 갖도록 Likert conversion을 업데이트했습니다.

### 수정

- Excel import 후 data editor variable lists의 select-button movement를 수정했습니다.
- Conversion/import 후 automatic missing-value detection 및 Likert detection refresh paths를 수정했습니다.
- Hierarchical logistic regression table placement, reference cells, VIF display, confidence-interval option handling을 수정했습니다.
- B5 portrait output에 맞춰 factor/PCA loading column order와 compact table headers를 수정했습니다.

## v0.9.35 - 2026-06-10

### 추가

- Developer Latent Mplus workflow를 Data, Setup, Results 단계가 있는 optional EasyFlow module로 추가했습니다.
- Latent role saving/loading, subset-condition handling, selected-order preservation, Results-based progress message review를 추가했습니다.
- Loaded data file folder 아래에 output, Mplus temporary files, run logs, Excel tables, 600 dpi figures를 포함하는 latent output routing을 추가했습니다.
- Selected Mplus native plot display와 color indicator-profile figure variants를 추가했습니다.

### 변경

- Latent Results tables and figures를 B5-oriented display frame에 맞춰 left-aligned output, compact table rendering, B5 portrait figure scaling으로 정렬했습니다.
- Data-tab load/settings/reset timing paths를 개선하고 latent tab이 열릴 때까지 latent server registration을 지연했습니다.
- LPA output에서는 LCA-only latent result tables를 숨기고 Results display에서 internal BCH class key tables를 제거했습니다.
- Public beta staging에서 developer-only Latent Mplus module을 제외하도록 Electron beta packaging을 업데이트했습니다.

### 수정

- Assumption-check output이 채워지도록 t-test / ANOVA assumption review 및 model overview tables를 수정했습니다.
- 명시적으로 복원된 YAML settings는 보존하면서 새 data file이 load될 때 latent roles/results를 reset했습니다.
- Latent variable-table scroll position을 roles assignment 중 보존했습니다.
- Analysis runs 중 latent run-progress messages를 Setup에서 Results로 이동했습니다.

## v0.9.34 - 2026-06-09

### 변경

- Summary display options, statistic alignment, effect-size labels, warnings, assumption-review output에 맞춰 paired 및 repeated-measures result tables를 정교화했습니다.
- Displayed pairs가 사용자의 selection order를 따르도록 paired repeated-measures variable ordering을 수정했습니다.
- Paired setup option tabs를 계속 표시하면서 repeated-variable labels는 three or more repeated measures에서만 활성화했습니다.
- Repeated-measures method notes에서 Wilks' lambda와 Greenhouse-Geisser details가 combined method처럼 보고되지 않도록 명확히 했습니다.
- Registered EasyFlow Statistics DOI를 사용하도록 citation metadata를 업데이트했습니다.

## v0.9.33 - 2026-06-06

### 변경

- ANCOVA assumption diagnostics를 확장하여 Levene을 default variance check로 사용하고 optional Brown-Forsythe / Breusch-Pagan / White test checks, slope-homogeneity detail tables, complete-case reporting, residual-linearity plots, influence sensitivity analysis를 추가했습니다.
- ANCOVA automatic-method controls를 추가하여 사용자가 automatic selection을 유지하거나 assumption warnings를 보고하면서 standard ANCOVA model을 유지할 수 있도록 했습니다.
- Shared result-table rendering을 9 pt table fonts, fixed-width portrait / landscape tables, 1.5x on-screen preview scaling, common post-hoc markers, ES column labels, two-line post-hoc headers에 맞춰 정교화했습니다.
- ANCOVA Assumptions / Model / Output options를 재구성하고 option spacing, indentation, result notes를 정렬했습니다.

## v0.9.32 - 2026-06-03

### 변경

- Tighter table width handling과 right-aligned test statistics를 포함한 ANCOVA result presentation refinements를 추가했습니다.
- Omnibus F/df partial eta squared와 covariance-based pairwise dz에 대한 SPSS-style LMM effect-size conversion을 추가했습니다.
- Binary logit, count log-link, Gaussian fixed-effect outputs에 대한 GLMM effect-size conversion을 추가했습니다.
- 새 ANCOVA, LMM, GEE, GLMM effect-size workflows에 맞춰 Korean User Guide, Analysis Methods, Method Notes를 업데이트했습니다.
- LMM omnibus 및 pairwise calculations가 사용 가능한 어느 input set에서도 실행될 수 있도록 Effect Size input handling을 정교화했습니다.

## v0.9.31 - 2026-06-02

### 변경

- Dedicated `.efs-result` type marker가 있는 Result history save/open files를 추가했습니다.
- Saved settings files를 `.efs-settings`로 분리하고 type validation을 추가했습니다.
- Add result가 output을 다시 생성하지 않고 현재 rendered analysis result snapshot을 보존하도록 변경했습니다.
- Analysis output, Result history, HTML, PDF, Word 전반에서 result table widths와 landscape export rules를 표준화했습니다.
- Correlation, paired tests, factor analysis, reliability, logistic regression의 Model overview 및 warning table layouts를 정교화했습니다.
- Saved Result documents가 methods and results로 바로 시작하도록 Word export cover page를 제거했습니다.
- Automatic standard, HC3 robust, ranked, interaction-model selection과 HTML, PDF, Excel, Result history export support가 포함된 ANCOVA를 추가했습니다.

## v0.9.30 - 2026-06-02

### 변경

- Effect-size menus에서 non-effect-size workflows를 제거하여 sample-size 및 effect-size calculators를 정교화했습니다.
- Progress reporting이 있는 stoppable background sample-size calculations를 추가했습니다.
- LMM unstructured correlation inputs와 SEM/CFA model-count degrees-of-freedom estimation을 추가했습니다.
- Explicit `n` labels와 0.95 default power로 required-sample-size result emphasis를 표준화했습니다.
- Sample-size, power, effect-size workflows에 대해 formulas and references를 포함하여 Korean User Guide, Analysis Methods, Method Notes를 확장했습니다.
- T-test/ANOVA, paired tests, nonparametric paired tests, correlation의 Model overview behavior를 포함하여 0.9.30 analysis outputs에 맞춰 Korean Analysis Methods documentation을 갱신했습니다.
- Method Notes에서 offline formula rendering을 지원하도록 local MathJax assets를 bundled 처리했습니다.

## v0.9.29 - 2026-06-01

### 추가

- Analysis 뒤에 standalone Sample Size 및 Effect Size top-level menus를 추가했습니다.
- T-test, ANOVA / ANCOVA, GEE, LMM, nonparametric, proportion, chi-square, McNemar, regression, survival, additional planning workflows를 위한 reference-backed sample size, power, effect size calculators를 추가했습니다.
- Sample size, achieved power, effect-size calculation wrappers에 대한 focused validation coverage를 추가했습니다.

### 변경

- Sample size 및 effect size screens를 analysis setup panels와 같은 shared three-block workflow로 재작업했습니다.
- Study-design family별로 Sample Size 및 Effect Size menu order를 재구성했습니다.
- Selected-method results가 강조되고 convertible effect sizes가 non-effect-size intermediate values 없이 표시되도록 t-test effect-size output을 업데이트했습니다.

## v0.9.28 - 2026-05-30

### 수정

- Native R file picker 대신 topmost WinForms owner를 사용하여 Windows data/settings open dialogs가 foreground로 오도록 했습니다.
- Factor Analysis와 PCA loading tables를 on-screen, HTML/PDF, Excel output에서 overview tables 바로 뒤로 이동했습니다.
- Generated third-party notices가 bundled app path에서 resolve되고 license metadata가 direct EFS packages, bundled dependencies, R base/recommended packages, R runtime별로 그룹화되도록 About > Open Source Licenses를 수정했습니다.
- Port 7894에서 기존 app process를 닫는 동안 startup이 멈추지 않도록 Windows launcher port-cleanup step을 `netstat` / `taskkill` path로 교체했습니다.

## v0.9.27 - 2026-05-29

### 변경

- Result, data, settings, generated export files의 user-facing default export filenames를 `EasyFlow_Statistics_...`에서 `EFS_...` prefix로 단축했습니다.

### 추가

- Bundled changelog를 desktop app 안에서 검토할 수 있도록 About > Version History를 추가했습니다.

## v0.9.26 - 2026-05-29

### 추가

- Sheet selection, A1-style start cell selection, header-row control, dataset loading 전 preview가 포함된 two-step Excel import flow를 추가했습니다.
- Reopened Excel data files를 위해 selected Excel import options를 saved settings에 보존했습니다.

## v0.9.25 - 2026-05-29

### 수정

- Categorical reference rows, value labels, single-line model fit summary를 보존하여 Regression Add result / Word export를 on-screen coefficient table과 정렬했습니다.
- Wide regression coefficient tables에 landscape Word section을 사용하여 saved Word output이 displayed result table과 더 잘 맞도록 했습니다.

## v0.9.24 - 2026-05-29

### 수정

- Footnote marker가 같은 trailing digit을 사용할 때도 `.008` 같은 p-values와 `.022` 같은 effect sizes가 세 자리 소수점을 모두 유지하도록 t-test / ANOVA 및 nonparametric result table rendering을 수정했습니다.

## v0.9.23 - 2026-05-29

### 수정

- Installed Electron app에서 Open data file이 안정적으로 나타나도록 PowerShell-based desktop data-file picker를 R's native Windows `choose.files()` dialog로 교체했습니다.

## v0.9.22 - 2026-05-29

### 수정

- File dialog가 Electron 뒤에서 열리거나 나타나지 않는 경우를 줄이기 위해 desktop data-file picker를 Tcl/Tk fallback 전에 Windows-native dialog로 전환했습니다.
- Data-file picker에서 Excel, SAS, Stata, CSV, DAT, SPSS filters가 보이도록 유지했습니다.

## v0.9.21 - 2026-05-29

### 추가

- Legacy Excel `.xls`, SAS `.sas7bdat` / `.xpt`, Stata `.dta` files에 대한 data import support를 추가했습니다.
- 확장된 import formats에 맞춰 data-file picker, Data tab copy, data IO validation coverage를 업데이트했습니다.

## v0.9.20 - 2026-05-29

### 변경

- Data Editor, Calculator, Analysis, About tab bodies를 해당 tab이 열릴 때만 rendering하여 initial Shiny page payload를 줄였습니다.

## v0.9.19 - 2026-05-29

### 변경

- Initial app boot 중 Shiny와 DT만 attach하여 installed desktop startup time을 줄였습니다.
- Electron startup 중 redundant bundled-runtime package scans를 생략했습니다. Package availability는 build 및 release smoke checks에서 계속 다룹니다.
- Electron의 Shiny readiness polling interval을 줄이고 별도 BrowserWindow load timing diagnostics를 추가했습니다.

## v0.9.18 - 2026-05-29

### 추가

- Logistic Regression results에 standard five-slot save control row를 추가했습니다.
- Logistic Regression export support를 HTML, PDF, Excel, saved Result collection에 추가했습니다.

## v0.9.17 - 2026-05-29

### 수정

- Correlation > Advanced correlations에서 latent-variable correlations가 별도 duplicate result set으로 rendering되지 않고 primary method set을 대체하도록 변경했습니다.
- Latent-variable correlations가 활성화되면 eligible continuous-ordinal/binary pairs가 main Methods table에 Polyserial로 직접 표시되도록 했습니다.

## v0.9.16 - 2026-05-29

### 수정

- T-test / ANOVA 및 standalone nonparametric result tables에서 p-value 및 effect-size footnote wrapping을 수정했습니다.

## v0.9.15 - 2026-05-29

### 수정

- Leading-zero suppression을 유지하면서 effect-size values가 정렬되도록 t-test / ANOVA inline footnote marker styling을 수정했습니다.

## v0.9.14 - 2026-05-29

### 변경

- Bundled desktop source/license notices를 위한 GPL application licensing, source-code offer text, About pages를 추가했습니다.
- Electron/R installer builds를 위한 generated OSS notices, license report, bundled license text collection, release smoke checks를 추가했습니다.
- Bundled R runtime prune reporting과 exact Electron/electron-builder version pins를 추가했습니다.
- Installed desktop startup overhead를 줄이고 startup timing diagnostics를 추가했습니다.
- Installed desktop app이 disabled grey screen에 남을 수 있는 Shiny startup session close guard를 제거했습니다.
- Version 0.9.14용 Windows beta installer를 다시 빌드했습니다.

## v0.9.13 - 2026-05-28

### 변경

- Supported Word result export를 활성화하고 Word, PDF, Excel 전반의 export rules를 표준화했습니다.
- Cover and methods pages, main-table-only selection, table notes, superscript markers, B5 portrait default 및 wide tables에만 landscape 적용을 포함한 publication-oriented Word output을 추가했습니다.
- PDF/Word output에 맞춰 paired, repeated-measures, t-test/ANOVA, correlation, regression, hierarchical regression, logistic result table widths, headers, post-hoc columns, footer statistics를 정교화했습니다.
- Titles, two-level headers, borders, merged notes, fixed column widths가 displayed result table structure를 보존하도록 Excel export를 개선했습니다.
- Electron desktop startup window size를 키우고 results render 후 regression setup action-row alignment를 안정화했습니다.
- Version 0.9.13용 Windows beta installer를 다시 빌드했습니다.

## v0.9.12 - 2026-05-27

### 변경

- Paired, repeated-measures paired, nonparametric paired, factor analysis, PCA, logistic regression, t-test / ANOVA outputs의 PDF 및 publication result layouts를 정교화했습니다.
- Exported reports의 result table spacing, model overview alignment, beta-watermark sizing을 개선했습니다.
- Compact summaries가 methods, N, assumptions, concise decision notes에 집중되도록 result display wording을 강화했습니다.
- Displayed table alignment rules를 보존하면서 result tables가 printable page width 안에 맞도록 PDF export는 A4, Word export는 B5로 표준화했습니다.
- Repeated-measures paired-test effect-size columns와 hierarchical regression coefficient tables에 대한 landscape PDF table fitting을 확장했습니다.
- Two-level headers, title rows, table border lines, fixed column widths, merged note rows가 displayed result-table layout과 맞도록 Excel table export rules를 표준화했습니다.
- Result export를 지원하는 editions에서 Result tab Word export button을 활성화했습니다.
- Word table header/body font styling을 맞추고 visible Word save button style을 활성화했으며 paired summaries는 two-decimal means/standard deviations를 사용하고 mixed paired-test overview/assumption/diagnostic tables를 결합했습니다.
- Word export에서 two-level table headers를 보존하고 report logos를 Word body output에서 제외했으며 wide repeated-measures 및 hierarchical tables에 landscape Word sections를 추가하고 paired, repeated-measures, hierarchical result tables의 PDF margins/column widths를 조정했습니다.
- T-test / ANOVA model overviews가 N, analysis method, reason을 direct columns로 표시하도록 변경하고 multi-model regression overviews를 two-level dependent/model headers로 rendering했습니다.
- Repeated-measures paired PDF tables를 landscape page width로 강제하고 hierarchical tables에 first-level header rules를 추가했으며 Word landscape sections를 scope 처리하여 default document는 B5 portrait를 유지했습니다.
- Post-hoc 및 tolerance columns를 넓히고 PDF report covers를 키웠으며 paired-test effect sizes를 two-level header 아래에 묶고 Word superscript note markers와 table notes를 복원하며 repeated regression footer rows를 Word output에서 병합했습니다.
- Word table-note font size를 줄이고 모든 rendered table notes를 Word output에 반영했으며 hierarchical model header markers를 superscript 처리하고 hierarchical footer statistics를 model별로 한 번만 병합했습니다.
- Hierarchical regression screen에서 Block 1-only runs를 saved Result collection에 추가할 때 ordinary regression으로 표시했습니다.
- Word result export가 paper-ready main tables only를 포함하고 각 table을 자체 page에서 시작하며 figures를 upscaling 없이 rendered size로 유지하고, wide paired/hierarchical tables와 at least 10 variables인 correlation matrices에만 landscape를 사용하도록 변경했습니다.
- Word에서 regression footer statistics를 가운데 정렬하고 F(p) 위에 strong top rule을 적용했으며 residual homoscedasticity를 single x²(p) footer item으로 표시했습니다.
- Electron desktop startup window size를 키우고 Word cover and analysis-method pages를 추가했으며 regression figures를 page당 두 개 배치하고 Word paper-table export에서 t-test/ANOVA post-hoc detail tables를 제외했습니다.
- Word table export spacing, frequency/descriptive column widths, regression summary rows, landscape section transitions, figure sizing을 정교화하여 wrapping, excess whitespace, blank pages를 줄였습니다.
- Results render 후 regression action-row alignment를 안정화하고 Word post-hoc table filtering을 강화했으며 combined n(%)/M±SD 및 IQR columns를 넓히고 wide correlation table sizing을 조정했습니다.

## v0.9.11 - 2026-05-27

### 변경

- Paired tests, t-test / ANOVA, regression, logistic regression outputs에 compact model-overview 및 assumption-review summaries를 추가했습니다.
- Detailed assumption diagnostics를 dedicated review tables로 이동하고 model overviews는 N, analysis method, concise reasons에 집중하도록 했습니다.
- Factor analysis, PCA, t-test / ANOVA에 tabbed option panels를 추가하고 tab-state preservation과 refined option spacing을 적용했습니다.
- Regression effect-size defaults를 f2가 기본 표시되고 sr2는 unchecked 상태로 남도록 업데이트했습니다.

## v0.9.10 - 2026-05-27

### 변경

- Bundled R runtime support, desktop window launcher, installer metadata, EasyFlow icon assets가 포함된 Electron beta packaging workflow를 추가했습니다.
- Common encodings를 시도하고 imported names와 character values를 normalize하여 Korean CSV 및 Excel import handling을 개선했습니다.
- Settings가 save/load될 때 reviewed binary, categorical, ordinal measurements를 보존하여 Analysis menus가 Data review와 같은 variable types를 사용하도록 했습니다.
- Frequencies / Descriptives result columns를 selected variable types에 맞는 statistics로 제한했습니다.
- Result export를 위한 beta-watermark와 Word-export placeholder refinements를 추가했습니다.

## v0.9.9 - 2026-05-27

### 변경

- Same-variable recode workflow를 queued `Add` step과 final `Apply` step 중심으로 재설계하여 data 변경 전 recode rules를 검토할 수 있도록 했습니다.
- Row selection, delete controls, automatic variable-type defaults, automatic output measurement inference가 있는 editable queued recode rules를 추가했습니다.
- Observed category values, missing-value markers, unmatched-value notices, `NA`로 또는 `NA`에서 recoding하는 single-value recoding support를 추가했습니다.
- Categorize-value recoding controls, range operators, panel alignment, action button placement, Recode Variable layout spacing을 정교화했습니다.

## v0.9.8 - 2026-05-26

### 변경

- Overview, User Guide, Analysis Methods, Method Notes, application information pages가 있는 About menu를 추가했습니다.
- User guidance, implemented analysis methods, method notes, package/runtime overview, criteria, references에 대한 Korean documentation을 확장했습니다.
- Regression residual homoscedasticity wording과 cross-tabulation trend method labels를 명확히 했습니다.
- 20,000을 documented bootstrap resampling option으로 추가하고 50,000을 recommended option으로 유지했습니다.
- **EasyFlow Statistics**가 full name으로 일관되게 강조되어 쓰이도록 documentation naming을 표준화했습니다.

## v0.9.7 - 2026-05-26

### 변경

- Normal continuous pairs에는 Pearson, non-normal 또는 ordinal pairs에는 Spearman을 사용하는 automatic correlation method selection을 추가했습니다.
- Correlation, paired tests, t-test / ANOVA, regression, logistic regression에서 invalid variables or models가 full analysis를 중단하지 않고 skip되도록 guard conditions를 추가했습니다.
- Low sample size, zero variance, all ties, sparse cells, separation risk, rank deficiency, VIF thresholds에 대한 warning 및 skipped-result output을 추가했습니다.
- Ordinal-data guidance와 sample-size warnings를 포함하여 factor analysis 및 PCA에 Pearson / polychoric matrix options를 추가했습니다.
- Result screens와 Excel exports 전반에서 warning 및 skipped-output helpers를 통합했습니다.

## v0.9.6 - 2026-05-25

### 변경

- Wilcoxon signed-rank와 Friedman tests를 사용하는 standalone Nonparametric Paired Test menu를 추가했습니다.
- Paired menus의 default를 Bonferroni로 두고 Bonferroni 및 Holm-Bonferroni paired post-hoc options를 추가했습니다.
- Nonparametric paired results에 median, Q1~Q3 output 및 Wilcoxon effect-size notes를 추가했습니다.
- Paired 및 nonparametric paired result table headers, footnote markers, export output, action-button layout을 정렬했습니다.

## v0.9.5 - 2026-05-25

### 변경

- Mann-Whitney U 및 Kruskal-Wallis tests를 사용하는 standalone Nonparametric Tests analysis menu를 추가했습니다.
- Standalone nonparametric tests에 median 및 quartile summary output을 추가했습니다.
- Mann-Whitney U results에 Cliff's delta effect sizes를 추가했습니다.
- Shared non-significant groups가 combined letters를 받도록 compact post-hoc lettering을 수정했습니다.
- Stable table alignment를 위해 p-value 및 effect-size footnote markers를 narrow adjacent columns에 rendering했습니다.
- Nonparametric Tests와 Paired test option-panel spacing을 정교화했습니다.

## v0.9.4 - 2026-05-25

### 변경

- Horizontal EasyFlow 및 StatEdu branding을 사용하여 development-only HTML/PDF watermarks를 정교화했습니다.
- Scatter plot matrices와 correlation heatmaps에 대한 correlation figure export를 활성화했습니다.

## v0.9.3 - 2026-05-25

### 변경

- Reliability columns는 제외하면서 h², complexity, eigenvalue, variance, cumulative variance, KMO / Bartlett diagnostics rows를 포함하도록 principal component analysis loading table을 factor analysis table style과 정렬했습니다.
- Matrix choice, cumulative-variance component selection, aligned component-selection number fields에 맞춰 principal component analysis setup controls를 정교화했습니다.
- Factor analysis diagnostics table styling을 개선하고 KMO / Bartlett summary row placement를 마무리했습니다.
- Development builds에서 all five analysis save controls를 계속 표시하고 remaining analysis modules에 PDF / Add result coverage를 추가했습니다.
- PDF cover decoration 및 internal filename/date print labels를 제거하고 bottom-right page numbering을 추가했습니다.
- StatEdu logo와 StatEdu 통계연구소 name을 포함하여 edition-aware PDF cover identity handling을 추가했습니다.
- Report cover의 saved date 아래에 PDF output date를 추가했습니다.
- Exported HTML 및 PDF reports에 development-only watermarks를 추가했습니다.

## v0.9.1 - 2026-05-24

### 변경

- Sorted loading matrices, optional small-loading filtering, problem-value highlighting, communalities, complexity, eigenvalue and variance summaries, oblique structure matrices를 포함하여 exploratory factor analysis output을 정교화했습니다.
- Factor loading matrix 바로 옆에 optional subfactor reliability summaries를 추가했습니다.
- Normality-driven extraction method selection, high fixed-factor counts, missing or infinite values, item-level reliability issues에 대한 factor analysis diagnostics를 개선했습니다.
- 모든 options가 standard three-column setup block 안에 맞도록 factor analysis option panel layout을 조정했습니다.

## v0.9.0 - 2026-05-24

### 변경

- Supported analysis outputs를 Add result에서 순서대로 수집할 수 있도록 Result tab accumulation을 추가했습니다.
- Result collection export를 HTML, PDF, Excel, Word에 추가했습니다.
- Final Result table format이 결정될 때까지 factor analysis 및 principal component analysis를 Add result에서 제외했습니다.

## v0.8.12

### 변경

- Principal axis factoring 및 maximum likelihood extraction, Varimax 및 Oblimin rotation, eigenvalue 또는 fixed-factor selection, normality-driven method selection, KMO / Bartlett diagnostics, scree plots, export support를 포함한 exploratory factor analysis를 추가했습니다.
- Correlation 또는 covariance matrix input, eigenvalue/fixed count/cumulative variance에 따른 component selection, optional rotation, scree and component plots, diagnostics, export support가 포함된 principal component analysis를 추가했습니다.
- Factor analysis 및 PCA calculations and exports에 대한 validation coverage를 추가했습니다.

## v0.8.11

### 변경

- Logo를 icon과 HTML text로 조합하지 않고 horizontal EasyFlow Statistics logo image를 사용하도록 navbar brand를 복원했습니다.

## v0.8.10

### 변경

- Light header에서 EasyFlow Statistics logo text와 version이 보이도록 navbar brand contrast를 수정했습니다.
- `3, 2>1` 및 `3>2, 1`을 포함한 shared comparison patterns가 일관되게 rendering되도록 ordered post-hoc significance notation을 표준화했습니다.
- Dependent 또는 independent lists에서 variables를 다시 이동할 때 t-test / ANOVA variable transfer behavior를 개선했습니다.
- Decimal-valued numeric variables가 low unique counts만으로 categorical로 분류되지 않도록 automatic measurement inference를 정교화했습니다.
- 새 EasyFlow Statistics session 시작 전 launcher cleanup을 app port로 제한했습니다.
- Reviewed conversion to `NA`가 있는 automatic missing-value detection을 추가했습니다.
- Numeric, text, statistical, date, conditional expressions에서 new variables를 만들 수 있는 formula-based variable transformation을 추가했습니다.
- Data Editor commands를 재구성하고 recoding을 same-variable 및 new-variable targets가 있는 single Recode variable workflow로 통합했습니다.

## v0.8.7

### 변경

- Imported survey data를 위한 automatic Likert text detection 및 batch conversion을 추가했습니다.
- Item text, original labels, numeric values, reverse coding, post-conversion variable type에 대한 grouped Likert review controls를 추가했습니다.
- Missing observed response levels가 있는 items도 full detected scale에 맞춰 유지되도록 partial Likert-level handling을 개선했습니다.
- Table scanning이 쉽도록 compact hierarchical regression statistic columns를 좁혔습니다.

## v0.8.6

### 변경

- Value labels, variable labels, measurement-type changes를 위한 Labels / Variables views와 unified Apply workflow가 있는 Step 3 variable review를 추가했습니다.
- Apply 후 Step 3 measurement-type edits가 analysis menus에 propagate되도록 했습니다.
- Step 3 review controls를 current Data workflow layout과 일관되게 유지했습니다.

## v0.8.4

### 변경

- Frequencies, t-test / ANOVA notes, correlation p-value / CI output, reliability item analysis, regression Durbin-Watson spacing, hierarchical regression method annotations, unstable logistic regression output의 result-table layout을 개선했습니다.
- Current Data page에서 checked variables를 대상으로 하는 Step 2 bulk measurement-type editing을 추가했습니다.
- Automatic reverse coding이 new variables를 만들거나 existing variables를 overwrite할 때 source variable measurement levels를 보존했습니다.

## v0.8.3

### 변경

- Coding error checks, automatic reverse coding, different-variable recoding, row-wise variable calculation을 위한 Data editor workflows를 추가했습니다.
- Correction apply controls, generated-variable previews, save-data support after variable creation, recoding and copied CSV / DAT reads에 대한 validation coverage를 추가했습니다.
- Shared button placement와 selected-data viewer fallback behavior를 포함하여 Data editor, Calculator, Analysis menus 전반의 setup/result layout behavior를 표준화했습니다.
- Reliability ordinal alpha default handling과 t-test / ANOVA spacing refinements를 포함하여 analysis option defaults 및 nonparametric post-hoc controls를 업데이트했습니다.
- Cloud-synced data file handling을 개선하여 SAV, CSV, DAT files를 import 전에 temporary read location으로 복사했습니다.
- Stable Shiny input synchronization을 유지하면서 Ctrl / Shift / Ctrl+A에 대한 transfer-list multi-selection behavior를 복원했습니다.

## v0.8.2

### 변경

- Paired tests, frequencies, correlation, reliability, logistic regression, t-test / ANOVA setup screens 전반에서 commonly used analysis options를 기본 활성화했습니다.
- Kruskal-Wallis follow-up comparisons를 위한 independent nonparametric post-hoc correction choices를 추가했으며 Bonferroni correction을 default로 선택하고 Holm Bonferroni를 사용할 수 있게 했습니다.
- Post-hoc 및 effect-size controls가 setup panel 안에 맞도록 t-test / ANOVA option panel spacing을 정교화했습니다.
- Data editor에 same-variable recoding support를 추가했습니다.

## v0.8.1

### 변경

- Paired test (2)와 Paired test (3+)를 하나의 Paired test setup으로 결합하여 repeated-measure count에 따라 적절한 analysis로 dispatch하도록 했습니다.
- Hierarchical regression workflow를 Regression으로 이름 변경하고 separate regression menu를 제거하면서 single-block regression 및 multi-block hierarchical behavior는 유지했습니다.
- Unified regression workflow에 bootstrap progress 및 stop controls를 추가했습니다.
- Paired-test layout sizing과 Data tab branding을 업데이트했습니다.

## v0.8.0

### 변경

- Binary, ordinal, multinomial dependent variables를 위한 Logistic Regression setup and results를 추가하고 hierarchical block models, OR / CI output, pseudo R2 options, VIF, model-fit rows, warning notes를 포함했습니다.
- Analysis assignment block에 variables가 있을 때만 활성화되는 shared Reset setting controls를 analysis setup screens 전반에 추가했습니다.
- Analysis menus 전반에서 selected-data viewer access, transfer-list double-click removal, three-panel grid spacing을 표준화했습니다.
- Models 실행 전 empty leading blocks를 compact하도록 regression 및 hierarchical regression block handling을 업데이트했습니다.

## v0.7.11

### 변경

- Column variables가 row variables 위에 배정되도록 Cross-tabulation setup을 변경하고 column 및 row panels를 expected variable counts에 맞게 sizing했습니다.
- Cross-tabulation PDF export를 추가하고 development builds에서 모든 save actions를 기본 활성화했습니다.
- Top-aligned statistics, centered column headers, left-aligned row values, numbered effect-size notes를 포함하여 Cross-tabulation result layout을 표준화했습니다.
- Result outputs 전반에서 effect-size number formatting을 leading zero 없이 세 자리 소수점으로 표준화했습니다.
- T-test / ANOVA results에 numbered p-value, effect-size, trend notes를 추가하고 note markers를 superscripts로 rendering했습니다.
- T-test / ANOVA note rendering에 대한 validation coverage를 추가하고 Cross-tabulation validation coverage를 확장했습니다.

## v0.7.10

### 변경

- Binary, ordered, categorical variables를 위한 Cross-tabulation Analysis를 추가하고 Pearson chi-square, Fisher exact / Monte Carlo fallback, trend analysis를 지원했습니다.
- Ordering controls, column-grouped result tables, row/column/total percent display options, optional split n / percent cells가 있는 multi-variable row and column assignment를 추가했습니다.
- Cross-tabulation results에 p-value method footnotes, p for trend with method-specific notes, effect-size notes, HTML / Excel export support를 추가했습니다.
- Cross-tabulation statistics, rendering, variable ordering, export helpers에 대한 validation coverage를 추가했습니다.

## v0.7.9

### 변경

- EQ-5D, metabolic syndrome, metabolic severity calculators 전반에서 calculator panel spacing을 조이고 정렬했습니다.
- Custom criteria가 선택되면 metabolic syndrome default criterion table을 숨겼습니다.
- Metabolic severity Formula panel을 다른 calculator reference panels와 맞도록 재작업하고 Output section을 분리했습니다.
- HINT8, EQ-5D, metabolic syndrome, FRS, ASCVD10, metabolic severity에 대한 calculator validation coverage를 추가했습니다.

## v0.7.7

### 변경

- HINT8 calculator initial values display를 compact item-by-level matrix로 변경했습니다.
- Ordered variables가 없어도 data loading 후 HINT8 setup panels가 보이도록 유지했습니다.
- HINT8 initial-value panel spacing을 조정했습니다.

## v0.7.6

### 변경

- EQ-5D calculator initial values display를 long reference list에서 compact dimension-by-level matrix로 변경했습니다.

## v0.7.5

### 변경

- Variable labels, value labels, measurement types가 한 번의 click으로 적용되고 saved settings에 유지되도록 Data Step 3 label application을 수정했습니다.
- Free mode에서 HTML 및 figure export는 사용 가능하게 유지하면서 PDF, Excel, Add result에 paid-export gating을 추가했습니다.
- Report cover, mixed portrait/landscape print layout, scaled wide tables, two-column plot pages가 포함된 regression 및 hierarchical regression PDF report export를 추가했습니다.
- Original on-screen layout을 보존하면서 horizontal table scrolling이 있는 viewer로 saved HTML output을 개선했습니다.
- Regression 및 hierarchical regression save-button layout을 표준화하고 sr2, f2, VIF options를 기본 활성화했습니다.
- Cancel 후 save-dialog prompts가 반복되는 문제를 수정하고 analysis menu tab activation errors를 줄였습니다.

## v0.7.4

### 변경

- Top navigation을 Data, Data Editor, Calculator, Analysis, Result, About으로 재구성했습니다.
- Nested Paired test 및 Regression menus를 포함하여 Data Editor와 Analysis menu groupings를 추가했습니다.
- Analysis 및 Calculator submenus가 normal Shiny tab activation을 사용하도록 nested menu handling을 개선했습니다.
- Menu navigation 중 unnecessary Data Step 3 table flushing과 repeated variable-info summaries를 피하여 analysis setup delays를 줄였습니다.

## v0.7.3

### 변경

- HINT8, EQ5D, Metabolic Syndrome, Metabolic Severity, FRS, ASCVD10에 대한 calculator menu modules를 추가했습니다.
- Calculated calculator outputs를 current loaded data에 다시 추가하여 analysis menus에서 사용할 수 있도록 했습니다.
- Obsolete Data Step 4/5 workflow remnants를 제거하고 Step 3에서 variable-label editing을 마무리했습니다.
- Analysis outputs 전반에서 notes가 table width와 맞도록 result table note rendering을 표준화했습니다.

## v0.7.2

### 변경

- Total-row summaries, combined item analysis output, full-item item-deleted diagnostics가 있는 Reliability subfactor blocks를 추가했습니다.
- Omega option이 enabled일 때만 omega statistics가 표시되고 validation되도록 Reliability option handling을 정교화했습니다.
- Reliability list sizing, result-table widths, header alignment를 조정했습니다.

## v0.7.1

### 변경

- Paired test (3+) repeated-measures labels, grouped result headers, post-hoc notes, effect-size annotations를 정교화했습니다.
- Reliability, Frequencies, Paired test, t-test/ANOVA, Correlation, Regression, Hierarchical workflows 전반에서 analysis transfer-list heights와 transfer-button alignment를 표준화했습니다.

## v0.7.0

### 추가

- Three or more repeated measurements를 위한 Paired test (3+) tab을 추가하고 RM ANOVA, Friedman, Cochran's Q routing with assumption checks and post-hoc comparisons를 포함했습니다.
- Partial eta squared, Kendall's W, Hedges' g, Wilcoxon r 등 repeated-measures effect sizes를 추가했습니다.

### 변경

- Paired test output tables, post-hoc notation, effect-size placement, HTML/Excel export layouts를 정교화했습니다.
- Paired test selection panels가 grouped repeated-measures rows와 더 compact한 target list heights를 사용하도록 조정했습니다.

## v0.6.8

### 추가

- Two repeated measurements를 위한 Paired test tab을 추가하고 paired t-test/Wilcoxon routing, binary pairs용 McNemar/exact McNemar routing, categorical pairs용 Stuart-Maxwell/Bowker options를 포함했습니다.
- Shapiro-Wilk 또는 skewness/kurtosis diagnostics와 3*IQR outlier screening이 포함된 optional paired-difference assumption checks를 추가했습니다.
- Paired test tables 및 assumption-check notes에 대한 HTML 및 Excel export support를 추가했습니다.

## v0.6.7

### 변경

- Shared downward offset을 제거하고 two-button regression/t-test layouts를 target block rows에 맞춰 analysis transfer buttons를 다시 가운데 정렬했습니다.

## v0.6.6

### 변경

- Regression 및 hierarchical regression에서 categorical coefficients를 `variable:level`로 표시하고 Data tab에 explicit reference value가 없어도 default reference row를 포함하도록 했습니다.

## v0.6.5

### 변경

- 0.5.7 setup geometry의 analysis transfer-button alignment를 복원하고 새 Reliability tab에 적용했습니다.
- Hierarchical regression save-button placement를 0.5.7 action-row position으로 복원했습니다.

## v0.6.4

### 변경

- Significance levels option이 선택되면 correlation coefficient matrix에 significance-level stars를 적용했습니다.

## v0.6.3

### 변경

- Analysis tabs로 이동할 때 labels와 함께 current measurement selections가 flush되도록 Step 3 measurement-level changes를 수정했습니다.
- Server-side direct input collection path에 Step 3 category-label measurement selectors를 포함했습니다.
- Hierarchical regression save-button block을 다시 third setup block 아래로 이동했습니다.

## v0.6.2

### 변경

- Continuous dependent variables 선택 시 Step 3 measurement-level overrides가 반영되도록 regression 및 hierarchical regression dependent-variable filtering을 수정했습니다.
- Shared setup-panel geometry changes 후 analysis variable-transfer buttons를 다시 정렬했습니다.

## v0.6.1

### 변경

- Step 3 variable type changes가 Reliability, Frequencies, t-test/ANOVA, Correlation, Regression, Hierarchical setup lists에 즉시 적용되도록 measurement-level propagation을 수정했습니다.

## v0.6.0

### 변경

- Same-level item selection, automatic KR-20/Cronbach's alpha/omega routing, ordinal alpha/omega support, item diagnostics, normality-aware method notes가 포함된 Reliability analysis를 추가했습니다.
- Edition-aware HTML/figure/Excel/add-result buttons가 있는 result tabs 전반의 analysis save controls를 표준화했습니다.
- Table notes가 matching table widths와 readable Excel column sizing으로 저장되도록 HTML 및 Excel result exports를 개선했습니다.
- Alpha, omega, polychoric-based ordinal coefficients를 위한 reliability-analysis engine으로 `psych`를 추가했습니다.

## v0.5.7

### 변경

- Previous/next block navigation을 사용하면서 Block 1/2/3 variable state를 보존하도록 hierarchical regression setup을 Dependent Variables와 one active Block at a time 표시 방식으로 재작업했습니다.
- 더 compact하고 aligned된 layout을 위해 hierarchical setup panel heights, list sizes, block navigation placement를 조정했습니다.
- Hierarchical regression result table에서 coefficient rows와 model fit rows 사이 separators를 정교화했습니다.
- Effect-size columns가 있는 wide three-model outputs에 맞춰 hierarchical regression result table column widths와 padding을 조정했습니다.

## v0.5.6

### 변경

- Analysis result tabs 전반에 shared HTML export를 추가하고 saved HTML table styling을 in-app regression-style tables와 정렬했습니다.
- Measurement-level automatic method selection, optional latent-variable correlations, method/reason matrices, p-value and 95% CI matrix output, larger scatter/heatmap figures가 포함되도록 correlation analysis를 확장했습니다.
- Two-level headers, numeric alignment, notes, save dialog behavior를 포함하여 regression 및 hierarchical regression tables의 Excel/HTML export styling을 정교화했습니다.
- Bootstrap workflows 중 regression 및 hierarchical regression option controls를 안정화했습니다.
- Non-hierarchical analysis tabs의 setup panel block geometry를 표준화했습니다.

## v0.5.5

### 변경

- Pairwise correlations, optional normality checks, p-values, confidence intervals, significance markers, matrix tables, plots가 있는 Correlation analysis run workflow를 구현했습니다.
- T-test / ANOVA results에 대한 Excel table export를 추가했습니다.
- Hierarchical regression results에 대한 Excel table export 및 residual diagnostic figure export를 추가했습니다.
- 새로 사용되는 analysis dependencies에 맞춰 local run package requirements를 업데이트했습니다.

## v0.5.4

### 변경

- T-test / ANOVA outputs에 effect size, trend analysis, ordered significance notation, expanded post-hoc handling을 추가했습니다.
- T-test / ANOVA normality option behavior, model overview labels, statistic labels, p-value notes, result table layout을 정교화했습니다.
- agricolae를 통한 Duncan multiple range test support를 추가하고 required package loading을 업데이트했습니다.
- Frequencies / Descriptives optional statistic columns를 수정하고 result tables가 compact regression-style widths를 사용하도록 조정했습니다.
- Hierarchical regression table spacing, separator lines, chi-square statistic labeling을 정교화했습니다.

## v0.5.2

### 변경

- Bootstrap regression을 사용할 때 regression model overview에 bootstrap sample count와 seed number를 추가했습니다.
- First-item Shift selection에서 regression variable transfer behavior를 안정화하고 selection-triggered scroll resets를 줄였습니다.
- Regression available variable list height를 20 variables가 보이도록 늘렸습니다.
- EasyFlow Statistics logo concept SVG assets를 추가하고 정교화했습니다.

## v0.5.1

### 변경

- Multi-select, Ctrl+A selection, movement direction, order preservation을 포함하여 regression variable transfer controls를 안정화했습니다.
- Regression setup layout, fixed list heights, move button placement, option checkbox persistence를 정교화했습니다.
- Analysis outputs에 대한 shared table 및 figure export behavior를 추가했습니다.
- Shared variable transfer UI가 있는 Frequencies / Descriptives setup and output scaffolding을 추가했습니다.
- EasyFlow Statistics citation metadata를 업데이트했습니다.

## v0.5.0

### 추가

- One dependent variable과 Block 1/2/3 predictor organization이 있는 hierarchical multiple regression용 Hierarchical tab scaffold를 추가했습니다.
- Future hierarchical regression setup을 위한 Block 2 to Block 3 variable transfer controls를 추가했습니다.
- Future generalized regression models를 위한 Generalized tab scaffold를 추가했습니다.

### 변경

- OLS-only bootstrap 및 sr2/f2 options를 제거하여 GLM-style models에 맞게 Generalized setup options를 업데이트했습니다.
- Count models를 Poisson / Negative binomial / Zero-inflated로 그룹화하고 positive continuous outcomes에는 Gamma를 유지했습니다.
- Generalized reporting options가 exp(B)를 IRR / ratio로 사용하도록 업데이트했습니다.

## v0.4.1

### 변경

- Regression tab과 page headings를 EasyFlow Statistics에서 Regression으로 이름 변경했습니다.

### 수정

- `missing value where TRUE/FALSE needed`가 표시될 수 있던 empty VIF warning text handling을 수정했습니다.

## v0.4.0

### 추가

- Excel table export와 figure folder selection을 위한 Windows-native save dialogs를 추가했습니다.
- Coefficient tables, model fit rows, diagnostics, notes가 있는 journal-table style Excel workbook export를 추가했습니다.
- Severe VIF values에 대한 guidance가 포함된 VIF-based multicollinearity warnings를 추가했습니다.
- Cross-validation을 사용하는 severe multicollinearity cases용 Ridge, LASSO, Elastic Net analyses를 추가했습니다.
- Model performance, OLS/penalized coefficient comparison, retained predictors를 위한 SCI-style penalized regression tables를 추가했습니다.

### 변경

- Shared independent-variable cell merging, wrapping, compact widths를 사용하여 Model overview Excel formatting을 개선했습니다.
- Penalized regression results가 표시될 때 residual diagnostics와 Durbin-Watson output을 숨겼습니다.
- Dependent variable labels 또는 names를 사용하도록 regression result sheet names를 업데이트했습니다.

### 수정

- Categorical variables가 선택되지 않았을 때 settings save errors를 수정했습니다.
- Blank measurement names가 measurement overrides에 저장되지 않도록 했습니다.

## v0.3.1

### 변경

- Multiple dependent variables 전반에서 Model overview를 하나의 table로 통합했습니다.
- Regression output 순서를 모든 coefficient tables 먼저, diagnostic plots 나중으로 재정렬했습니다.
- Dependent variables 전반에서 assumption checks와 Durbin-Watson results를 각각 하나의 table로 통합했습니다.
- Labels가 있으면 dependent variables를 label로만 표시하고, 없으면 variable name으로 표시했습니다.
- Coefficient tables 뒤에 effect size guidelines를 한 번만 표시했습니다.

## v0.2.0

### 추가

- Multiple dependent variables에 대한 sequential regression output을 추가했습니다.
- Regression setup panel에 bootstrap progress 및 stop controls를 추가했습니다.
- Optional sr2, f2, VIF/collinearity diagnostics output을 추가했습니다.
- Sr2 및 Cohen's f2에 대한 effect size guideline references를 추가했습니다.
- Side-by-side residual diagnostic plots를 추가했습니다.

### 변경

- Variables, Dependent Variables, Independent Variables, bootstrap controls가 있는 regression setup layout으로 재작업했습니다.
- Dependent variable, independent variables, N, R2(adj. R2), F(p), selected method를 보고하도록 Model overview를 업데이트했습니다.
- Residual homoscedasticity plots와 outlier boundary display를 표준화했습니다.
- Regression output에서 superscript/subscript notation을 개선했습니다.

### 수정

- Text input이 typed character마다 reset되지 않도록 label editing을 수정했습니다.
- Saved settings loading과 variable labels, measurements, references, value labels의 propagation을 steps 전반에서 수정했습니다.
- Bootstrap stop handling과 progress display placement를 수정했습니다.

## v0.1.2

### 추가

- Regression setup screen의 Dependent Variables 아래에 Up/Down controls를 추가했습니다.
- Saved settings와 summary displays에서 dependent variable order를 보존했습니다.

## v0.1.1

### 수정

- Step 3 `selected` header button이 active role에 대해 모든 visible variables를 select 또는 clear할 수 있도록 했습니다.
- Step 3 apply button이 현재 DataTables checkbox state를 submit하도록 변경했습니다.
- DataTables redraw 중 `var_label`, `reference`, `value`, `label` edits를 보존하고 sync했습니다.

## v0.1.0

### 추가

- Initial Shiny app prototype을 추가했습니다.
- CSV upload 및 variable selection을 추가했습니다.
- Multiple regression analysis를 추가했습니다.
- Lilliefors corrected Kolmogorov-Smirnov residual normality test를 추가했습니다.
- Breusch-Pagan homoscedasticity test를 추가했습니다.
- HC3 robust standard errors를 추가했습니다.
- Bootstrap confidence intervals를 추가했습니다.
- `C:/StatEdu/easyflow_statistics/easyflow_statistics_3.0.xlsx`를 사용한 Durbin-Watson dL/dU lookup을 추가했습니다.
