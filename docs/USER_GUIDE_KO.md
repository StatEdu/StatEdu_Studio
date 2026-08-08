# **StatEdu Studio** 사용자 안내서

이 문서는 **StatEdu Studio** 버전 1.2.0을 실제로 사용하는 절차를 설명합니다. 앱 실행, 데이터 열기, 변수 선택, 분석 실행, 결과 저장처럼 사용자가 화면에서 따라 해야 하는 작업 흐름을 다룹니다. 구현된 분석 기법 목록은 `ANALYSIS_METHODS_KO.md`, 방법 선택 기준과 해석상 주의점은 `METHOD_NOTES_KO.md`를 참고합니다.

## 1. 앱 실행

**StatEdu Studio**는 Windows PC에서 로컬로 실행되는 Shiny 앱입니다. 데이터는 사용자의 PC에서 분석되며 외부 서버로 전송되지 않습니다.

1. **StatEdu Studio** 폴더를 엽니다.
2. `StatEdu_Studio.bat`을 더블클릭합니다.
3. 브라우저가 열리면 `127.0.0.1:7894` 주소에서 앱을 사용합니다.

같은 포트에서 이전 **StatEdu Studio** 세션이 실행 중이면 런처가 해당 세션을 정리한 뒤 새 세션을 시작합니다.

![데이터 작업 흐름](docs/assets/user-guide/ko/data-workflow.png)

## 2. 화면 액션 오버레이로 따라 하기

아래 예시는 데이터 파일을 불러오고, 분석 변수를 선택하고, `t-test / ANOVA`를 실행한 뒤 논문용 결과표를 확인하는 과정을 순서대로 보여줍니다. 초록색 플래시 박스는 실제로 사용자가 클릭하거나 확인해야 하는 위치입니다.

<style>
.efs-guide-demo{max-width:1080px;margin:18px 0 24px}
.efs-guide-shell{border:1px solid #dce5ee;border-radius:8px;background:#fff;box-shadow:0 16px 42px rgba(16,25,35,.12);overflow:hidden}
.efs-guide-viewport{position:relative;aspect-ratio:1533/978;background:#eef5f8;overflow:hidden}
.efs-guide-shot,.efs-guide-nav-fixed{position:absolute;inset:18px 22px 22px;width:calc(100% - 44px);height:calc(100% - 40px);object-fit:cover;object-position:top;border:1px solid #d6e2ea;border-radius:6px;background:#f6f8fb;box-shadow:0 18px 42px rgba(16,25,35,.12)}
.efs-guide-shot{opacity:0;animation:efsGuideShot 40s linear infinite}
.efs-guide-shot.s01{animation-delay:0s}.efs-guide-shot.s02{animation-delay:4s}.efs-guide-shot.s03{animation-delay:8s}.efs-guide-shot.s04{animation-delay:12s}.efs-guide-shot.s05{animation-delay:16s}.efs-guide-shot.s06{animation-delay:20s}.efs-guide-shot.s07{animation-delay:24s}.efs-guide-shot.s08{animation-delay:28s}.efs-guide-shot.s09{animation-delay:32s}.efs-guide-shot.s10{animation-delay:36s}
.efs-guide-nav-fixed{z-index:20;clip-path:inset(0 0 92.15% 0);pointer-events:none;opacity:0;animation-duration:40s;animation-timing-function:linear;animation-iteration-count:infinite}
.efs-guide-nav-data{animation-name:efsGuideNavData}.efs-guide-nav-menu{animation-name:efsGuideNavMenu}.efs-guide-nav-analysis{animation-name:efsGuideNavAnalysis}
.efs-guide-action-layer{position:absolute;z-index:30;inset:18px 22px 22px;pointer-events:none}
.efs-guide-action{position:absolute;border:2px solid rgba(10,166,166,.95);border-radius:7px;background:rgba(10,166,166,.12);box-shadow:0 0 0 4px rgba(10,166,166,.08);opacity:0;animation:efsGuideAction 40s linear infinite}
.efs-guide-action b{position:absolute;left:50%;top:100%;transform:translate(-50%,8px);white-space:nowrap;background:#0d1724;color:#fff;border-radius:5px;padding:6px 9px;font-size:12px;font-weight:900;line-height:1.1}
.efs-guide-action.label-top b{top:auto;bottom:100%;transform:translate(-50%,-8px)}
.efs-guide-action.a01 b,.efs-guide-action.a05 b{left:100%;top:50%;transform:translate(8px,-50%)}
.efs-guide-action.a03b b,.efs-guide-action.a04c b,.efs-guide-action.a07c b{top:auto;bottom:100%;transform:translate(-50%,-8px)}
.efs-guide-action.a01{left:7.8%;top:30.3%;width:10.8%;height:4.7%;animation-delay:0s}
.efs-guide-action.a02{left:34.2%;top:58.2%;width:2.2%;height:26.7%;animation-delay:4s}
.efs-guide-action.a03a{left:34.2%;top:57.8%;width:2.2%;height:26.2%;animation-delay:8s}
.efs-guide-action.a03b{left:7.5%;top:54.0%;width:13.2%;height:5.2%;animation-delay:9.45s}
.efs-guide-action.a04a{left:55.2%;top:69.2%;width:8.5%;height:4%;animation-delay:12s}
.efs-guide-action.a04b{left:64.2%;top:58.4%;width:20.2%;height:4.6%;animation-delay:13.15s}
.efs-guide-action.a04c{left:7.4%;top:71.7%;width:13.2%;height:5.4%;animation-delay:14.15s}
.efs-guide-action.a05{left:38.6%;top:18%;width:14.9%;height:4.9%;animation-delay:16s}
.efs-guide-action.a06a{left:9.3%;top:51.7%;width:22%;height:11.1%;animation-delay:20s}
.efs-guide-action.a06b{left:31.9%;top:40.2%;width:3.1%;height:4.8%;animation-delay:21.55s}
.efs-guide-action.a07a{left:36.6%;top:37.2%;width:22.3%;height:8.9%;animation-delay:24s}
.efs-guide-action.a07b{left:36.6%;top:58.9%;width:22.4%;height:25.7%;animation-delay:25.2s}
.efs-guide-action.a07c{left:9%;top:94.0%;width:23.1%;height:4.4%;animation-delay:26.25s}
.efs-guide-action.a10{left:8.8%;top:17.6%;width:59.8%;height:48.5%;animation-delay:36s}
.efs-guide-action-note{margin:10px 2px 0;color:#5f6f83;font-size:14px;line-height:1.55}
.efs-guide-steps{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:14px;margin:18px 0 24px;max-width:1080px}
.efs-guide-step{background:#fff;border:1px solid #dce5ee;border-radius:8px;padding:18px}
.efs-guide-step strong{display:block;margin-bottom:7px;font-size:16px}.efs-guide-step span{color:#5f6f83;font-size:14px;line-height:1.5}
@keyframes efsGuideShot{0%{opacity:0}.5%,9.8%{opacity:1}10.8%,100%{opacity:0}}
@keyframes efsGuideAction{0%,.8%{opacity:0;transform:scale(.98)}1.2%,4.45%{opacity:1;transform:scale(1)}5.15%,100%{opacity:0;transform:scale(.98)}}
@keyframes efsGuideNavData{0%,39.2%{opacity:1}40.5%,100%{opacity:0}}
@keyframes efsGuideNavMenu{0%,39.2%{opacity:0}40.5%,49.2%{opacity:1}50.5%,100%{opacity:0}}
@keyframes efsGuideNavAnalysis{0%,49.2%{opacity:0}50.5%,100%{opacity:1}}
@media (max-width:900px){.efs-guide-shot,.efs-guide-nav-fixed{inset:14px 12px 14px;width:calc(100% - 24px);height:calc(100% - 28px)}.efs-guide-action-layer{inset:14px 12px 14px}.efs-guide-action b{display:none}.efs-guide-steps{grid-template-columns:1fr}}
</style>
<div class="efs-guide-demo">
  <div class="efs-guide-shell">
    <div class="efs-guide-viewport">
      <img class="efs-guide-nav-fixed efs-guide-nav-data" src="docs/assets/user-guide/ko/2.png" alt="" aria-hidden="true">
      <img class="efs-guide-nav-fixed efs-guide-nav-menu" src="docs/assets/user-guide/ko/5.png" alt="" aria-hidden="true">
      <img class="efs-guide-nav-fixed efs-guide-nav-analysis" src="docs/assets/user-guide/ko/6.png" alt="" aria-hidden="true">
      <img class="efs-guide-shot s01" src="docs/assets/user-guide/ko/1.png" alt="데이터 파일 열기 화면">
      <img class="efs-guide-shot s02" src="docs/assets/user-guide/ko/2.png" alt="변수 선택 화면">
      <img class="efs-guide-shot s03" src="docs/assets/user-guide/ko/3.png" alt="변수 선택 적용 화면">
      <img class="efs-guide-shot s04" src="docs/assets/user-guide/ko/4.png" alt="변수 라벨 화면">
      <img class="efs-guide-shot s05" src="docs/assets/user-guide/ko/5.png" alt="분석 메뉴 화면">
      <img class="efs-guide-shot s06" src="docs/assets/user-guide/ko/6.png" alt="종속변수 선택 화면">
      <img class="efs-guide-shot s07" src="docs/assets/user-guide/ko/7.png" alt="분석 실행 화면">
      <img class="efs-guide-shot s08" src="docs/assets/user-guide/ko/8.png" alt="모형 개요 화면">
      <img class="efs-guide-shot s09" src="docs/assets/user-guide/ko/8_2.png" alt="모형 개요 상세 화면">
      <img class="efs-guide-shot s10" src="docs/assets/user-guide/ko/9.png" alt="결과표 화면">
      <div class="efs-guide-action-layer">
        <div class="efs-guide-action a01"><b>Open data file</b></div>
        <div class="efs-guide-action a02"><b>Checkbox</b></div>
        <div class="efs-guide-action a03a"><b>Selected checkbox</b></div>
        <div class="efs-guide-action a03b"><b>Apply variable selection</b></div>
        <div class="efs-guide-action a04a label-top"><b>age: continuous</b></div>
        <div class="efs-guide-action a04b"><b>job labels</b></div>
        <div class="efs-guide-action a04c"><b>Apply</b></div>
        <div class="efs-guide-action a05"><b>t-test / ANOVA</b></div>
        <div class="efs-guide-action a06a"><b>QoL - x3</b></div>
        <div class="efs-guide-action a06b"><b>Select</b></div>
        <div class="efs-guide-action a07a label-top"><b>Dependent variables</b></div>
        <div class="efs-guide-action a07b"><b>Independent variables</b></div>
        <div class="efs-guide-action a07c"><b>Run analysis</b></div>
        <div class="efs-guide-action a10"><b>QoL table</b></div>
      </div>
    </div>
  </div>
  <div class="efs-guide-action-note">초록색 플래시 박스는 각 단계에서 눌러야 할 위치와 확인해야 할 영역을 순서대로 강조합니다.</div>
</div>

<div class="efs-guide-steps">
  <article class="efs-guide-step"><strong>1. 데이터 준비</strong><span>파일을 불러오고 분석에 사용할 변수를 선택합니다.</span></article>
  <article class="efs-guide-step"><strong>2. 분석 선택</strong><span>Analysis 메뉴에서 t-test / ANOVA를 선택하고 종속변수와 독립변수를 지정합니다.</span></article>
  <article class="efs-guide-step"><strong>3. 결과 확인</strong><span>분석 가정과 변수 유형에 맞는 결과를 논문용 표 형태로 확인합니다.</span></article>
</div>

## 3. 데이터 열기

Data 메뉴에서 SPSS SAV, Excel, CSV, DAT 파일을 불러옵니다. 파일을 연 뒤에는 원자료 표, 변수명, 변수 라벨, 값 라벨을 확인합니다.

데이터를 불러온 직후에는 다음을 먼저 확인하는 것이 좋습니다.

- 변수명이 분석에 사용할 수 있는 형태인지 확인합니다.
- 값 라벨이 의도한 범주와 맞는지 확인합니다.
- 결측값 코드가 실제 결측으로 처리되어야 하는지 확인합니다.
- 숫자로 저장된 범주형 변수의 measurement level을 확인합니다.

## 4. 데이터 편집과 전처리

데이터 편집 메뉴에는 분석 전 정리 작업을 위한 기능이 모여 있습니다.

![데이터 편집 메뉴](docs/assets/user-guide/ko/data-editor-menu.png)

주요 기능은 다음과 같습니다.

- 자동 코딩 오류 확인: 범위 밖 값이나 정수로 입력되어야 하는 변수의 오류를 확인합니다.
- Likert 자동 변환: 텍스트 Likert 응답을 숫자형 점수로 변환합니다.
- 결측값 자동 처리: 결측값으로 보이는 코드를 찾아 `NA`로 처리합니다.
- 역코딩 자동 처리: 역문항을 새 변수로 생성합니다.
- 변수 자동 계산: 여러 변수의 행 단위 합계나 평균을 계산합니다.
- 변수 변환: 빠른 공식 또는 사용자 식으로 새 변수를 만듭니다.
- 변수 리코딩: 기존 값을 다른 값으로 리코딩합니다.
- 변수 이름 변경: 변수명과 라벨을 정리합니다.

## 5. 변수 속성 확인

분석 전 Step 3에서 measurement level을 반드시 확인합니다. **StatEdu Studio**는 measurement level을 바탕으로 가능한 분석 방법을 자동 또는 반자동으로 선택합니다.

- `continuous`: 평균 비교, 상관, 회귀 등에 사용합니다.
- `ordered`: 순서형 범주 또는 ordinal 문항으로 사용합니다.
- `binary`: 두 수준의 범주형 변수로 사용합니다.
- `category`: 순서가 없는 범주형 변수로 사용합니다.

## 6. 분석 메뉴 사용

분석 메뉴에서 분석 종류를 선택합니다.

public 1.2 분석 메뉴에는 빈도분석/기술통계, 교차표 분석, t-test / ANOVA, paired test, ANCOVA, 비모수 검정, 비모수 대응검정, 상관분석, 신뢰도, 평가자간 일치도, 요인분석, 주성분분석, 회귀분석, 매개·조절, 매개·조절 사용자 정의 모델, GLM, 로지스틱 회귀, 반복측정 ANOVA, 종단/패널 모형, 복합표본분석이 포함됩니다.

분석 화면의 기본 흐름은 대체로 같습니다.

1. 왼쪽 변수 목록에서 변수를 선택합니다.
2. 종속변수, 독립변수, 그룹 변수, 반복측정 변수 등 필요한 영역으로 변수를 이동합니다.
3. 옵션을 선택합니다.
4. 분석 실행 버튼으로 분석을 실행합니다.
5. 결과표, 경고, 실행되지 않은 분석 또는 실행되지 않은 모형을 확인합니다.


### 6.1 매개·조절 분석

`분석 > 회귀 / 모형 > 매개·조절`에서는 독립변수, 종속변수, 매개변수, 조절변수, 공변량을 지정해 PROCESS 방식의 주요 매개·조절 모형을 실행한다.

사용 절차:

1. 데이터 탭에서 분석에 사용할 변수를 선택한다.
2. `매개·조절` 메뉴를 연다.
3. 종속변수와 독립변수를 지정한다.
4. 매개변수가 있으면 매개변수 목록에 넣고, 병렬 매개 또는 순차 매개 구조를 선택한다.
5. 조절변수가 있으면 조절변수를 지정하고 조절 경로를 선택한다.
6. 필요하면 공변량, 평균중심화, bootstrap 반복 수, 신뢰구간 방법, 단순기울기, Johnson-Neyman 옵션을 조정한다.
7. `분석 실행`을 누르고, 모형 요약, 경로계수, 직접효과, 간접효과, 조건부 효과, 조건부 간접효과, 그림을 확인한다.

지원 모형은 조절 모형, 단순 매개, 순차 매개, 직접경로 조절, 1단계 조절된 매개, 2단계 조절된 매개, 전체 경로 조절된 매개를 포함한다. 결과의 bootstrap 신뢰구간은 간접효과와 조건부 간접효과 해석에서 우선 확인한다.

`매개·조절 사용자 정의 모델` 메뉴에서는 캔버스에 변수를 배치하고 화살표로 연결해 같은 분석 엔진에 전달한다. 캔버스 모형은 현재 지원하는 매개·조절 모형 번호와 일치해야 분석할 수 있으며, 실행 후에는 결과 모형 그림과 계수 라벨을 확인할 수 있다.

### 6.2 평가자간 일치도

두 명 이상의 평가자, 판정자, 코더 또는 측정도구가 같은 사례를 평가한 자료는 `분석 > 신뢰도 > 평가자간 일치도`를 사용합니다.

사용 절차:

1. 데이터 탭에서 평가자 변수들을 선택합니다.
2. `평가자간 일치도` 메뉴를 엽니다.
3. 같은 measurement level의 평가자 변수들을 분석 목록으로 이동합니다. 연속형 평정과 범주형 평정을 한 번에 섞어 분석하지 않습니다.
4. 순서형 평정은 Step 3에서 범주 순서가 연구 설계와 맞는지 확인합니다.
5. 필요하면 weighted kappa/Gwet 가중 방식, ICC 모형/유형/단위, bootstrap 신뢰구간, 정규성 검토 옵션을 선택합니다.
6. 분석을 실행한 뒤 권장 일치도 지표를 먼저 보고, 보조 지표, 결측 처리 안내, 경고를 함께 확인합니다.

연속형 평정은 ICC 결과를 우선 확인합니다. 이분형, 순서형, 명목형 평정은 자료 구조에 따라 Cohen/weighted kappa, Fleiss 또는 Light kappa, Gwet AC1/AC2, Krippendorff alpha 같은 chance-corrected agreement 지표를 제공합니다.

### 6.3 혼합 반복측정 ANOVA

wide-format 사전-사후 또는 다시점 outcome 열을 집단 변수에 따라 비교할 때는 `분석 > 집단 비교 > 반복측정 ANOVA`를 사용합니다.

사용 절차:

1. 데이터 탭에서 집단 변수, 반복측정 outcome 변수, 선택적 공변량을 선택합니다.
2. `반복측정 ANOVA` 메뉴를 엽니다.
3. 두 개 이상의 반복측정 outcome 열을 시간 순서대로 `반복측정 변수` 영역에 이동합니다.
4. 하나의 집단 변수를 `집단 변수` 영역에, 필요한 baseline 또는 설계 공변량을 `공변량` 영역에 이동합니다.
5. PP 또는 available-case ITT 경로, 가정 검토, 사후비교, 보정 방법, 시점 라벨을 선택합니다.
6. 분석을 실행한 뒤 모형 개요, 시점별/집단별 요약, time/group/interaction 검정, 구형성 및 분산 검토, 사후비교, 혼합모형 대안 안내를 확인합니다.

이 workflow는 wide-format 반복 outcome용입니다. 자료가 이미 long format이거나 GEE, LMM, GLMM, panel FE/RE 모형이 필요한 경우에는 `종단 / 패널 모형`을 사용합니다.

### 6.4 Generalized Linear Model (GLM) 사용 절차

독립 관측자료에서 연속형, 이분형, 양수 편향형, count outcome을 회귀분석할 때는 `분석 > 일반화 선형모형(GLM)`을 사용합니다. 반복측정, 군집자료, 패널자료용 종단/패널 분석은 public 1.2에서 공개 범위에 맞게 제공합니다.

1. 왼쪽 변수 목록에서 종속변수를 선택한 뒤 종속변수 영역으로 이동합니다. 종속변수는 하나만 지정합니다.
2. 설명변수는 독립변수 영역으로 이동합니다. 순서는 위로, 아래로 버튼으로 조정할 수 있습니다.
3. rate 또는 person-time 분석처럼 노출량 보정이 필요하면 하나의 양수 변수를 노출/오프셋 영역에 지정합니다.
4. 옵션의 모형 탭에서 종속변수 분포와 링크 함수를 선택합니다. 자동을 선택하면 프로그램이 변수 유형과 값 구조로 Gaussian, Binary, Gamma, Count 후보를 판정합니다.
5. Count outcome은 Poisson과 negative binomial을 따로 고르지 않고 `Count`로 선택합니다. 프로그램이 Poisson dispersion, zero screen, 가능한 AIC/BIC 정보를 확인해 Poisson 또는 negative binomial 결과를 보고합니다.
6. 옵션의 결측 탭에서 complete-case, MI, IPW 중 결측 처리 또는 결측 민감도 분석 방식을 선택합니다.
7. 옵션의 검토 탭에서 family/link, 잔차 또는 과분산, separation/sparse cell, 영향점, VIF 등 필요한 가정 검토를 선택합니다.
8. GLM 실행 버튼을 눌러 분석을 실행하고, 결과에서 의사결정 요약, 계수표, 가정 검토, 결측자료 요약, 소프트웨어 버전을 확인합니다.

### 6.5 종단 / 패널 분석

반복측정, 군집자료, 패널자료처럼 한 대상 또는 군집에서 여러 시점의 관측값이 있는 long-format 데이터는 public 1.2에서 종단/패널 및 혼합 반복측정 workflow의 공개 범위에 맞게 제공합니다.

1. 데이터 탭에서 Step 2 변수 선택을 적용합니다.
2. 왼쪽 변수 목록에서 변수를 선택한 뒤 `>` 버튼으로 모형 변수 영역에 이동합니다.
3. 종속변수, 대상자 ID, 군집 ID(선택), 시간 변수는 각각 하나씩만 지정합니다.
4. 설명변수는 독립변수 영역에 여러 개 지정할 수 있습니다.
5. 옵션의 모형 탭에서 분석기법을 선택합니다. GEE와 GLMM은 outcome family와 관련 옵션을 표시합니다. Count outcome은 Poisson과 negative binomial을 따로 고르지 않고 `Count: Poisson or negative binomial / log`로 선택하면, 프로그램이 과분산 screening 후 최종 family를 결정합니다. LMM / GLMM은 random slope 옵션을 Terms 탭에 표시합니다. Panel FE / RE는 패널모형에 필요한 옵션만 표시합니다.
6. 옵션의 결측 탭에서 결측 처리 방식을 선택합니다. LMM / GLMM의 기본값은 `Likelihood-based MAR: available repeated measures`이며, 관측된 반복측정 행을 사용해 mixed-model likelihood를 적합합니다. 한 방문의 outcome 결측 때문에 대상자 전체를 제거하지는 않지만, 해당 모델 행의 outcome, 공변량, ID, time 결측은 대체하지 않습니다. 공변량 결측이나 dropout 메커니즘이 중요하면 MI 또는 IPW를 민감도 분석으로 추가합니다.
7. 옵션의 검토 탭에서 가정 검토를 켜고, 선택한 분석기법에 맞는 세부 가정 항목을 선택합니다.
8. 모형 실행 버튼을 눌러 분석을 실행합니다.
9. 결과에서 모형 개요, 자료 구조, 결측자료, 출판용 추정표, 가정 검토, 권장 분석, 민감도 분석 결과, 원고용 문장, SCI 보고 체크리스트를 확인합니다.

모형 선택은 연구 질문에 맞춰 결정합니다. GEE는 population-averaged effect, LMM / GLMM은 subject-specific effect, Panel FE는 within-unit change와 time-invariant confounding 통제, Panel RE는 unit effect가 predictors와 독립이라는 가정이 타당할 때 사용합니다.


### 6.6 복합표본분석

복합표본분석은 `분석 > 복합표본분석` 아래에 있다. 먼저 `복합표본 설계변수` 메뉴에서 층화변수, 집락/PSU 변수, 가중치 변수, 유한모집단 보정(FPC), 복제 가중치, 단일 PSU 층 처리 방식을 지정한다. 이 설정은 이후 복합표본 분석 메뉴에서 자동으로 불러온다.

복합표본 설계변수 사용 절차:

1. 데이터 탭에서 분석 데이터와 변수를 준비한다.
2. `복합표본분석 > 복합표본 설계변수`를 연다.
3. 설계변수 입력 블록에서 층화, 집락/PSU, 가중치, 부모집단 변수를 지정한다.
4. 설계 옵션 블록에서 분산추정 방법, FPC, 단일 PSU 층 처리, 복제 가중치 사용 여부를 지정한다.
5. 자주 쓰는 설계는 `설정 저장`으로 저장하고, 같은 데이터 구조를 다시 사용할 때 `설정 불러오기`로 복원한다.

복합표본 분석 메뉴:

- `복합표본 빈도분석 / 기술통계분석`: 가중 빈도, 비율, 평균, 표준오차, 신뢰구간, 결측 요약.
- `복합표본 교차분석`: 행/열/전체 퍼센트, 설계기반 검정, 추세검정, 퍼센트 신뢰구간.
- `복합표본 t-test / ANOVA`: 설계기반 평균 비교, 사후분석, 가중 N, 설계 df, 효과크기.
- `복합표본 상관분석`: Pearson 또는 Spearman 상관, 설계기반 표준오차, p값 보정, 상관행렬.
- `복합표본 회귀분석`: survey-weighted 선형회귀, 설계기반 Wald/F 검정, 모형 적합 요약.
- `복합표본 로지스틱 회귀분석`: survey-weighted 로지스틱 회귀, 오즈비, Wald 검정, pseudo R-squared.

복합표본 자료는 단순 무작위표본으로 간주하면 표준오차와 p값이 달라질 수 있다. 표와 회귀계수의 점추정뿐 아니라 설계 df, 표준오차, 신뢰구간, 단일 PSU 처리 방식, 복제 가중치 사용 여부를 함께 보고한다.

## 7. 결과 확인

결과 탭에서는 실행한 분석 결과를 모아 봅니다. 결과는 분석별 표, 경고, 진단 결과, 저장 옵션으로 구성됩니다.

결과를 해석할 때는 p 값만 보지 말고 다음을 함께 확인합니다.

- 어떤 분석 방법이 선택되었는가
- 경고가 있는가
- 실행되지 않은 분석 또는 실행되지 않은 모형이 있는가
- 효과크기와 신뢰구간이 결론과 같은 방향인가
- 표본 수와 결측 처리 방식이 충분한가

## 8. 결과 저장

결과 탭에서 public 1.2는 HTML, PDF 형식 저장을 제공합니다. Excel, Word 결과 저장은 public 1.2에서는 숨겨져 있으며 이후 Pro 기능으로 분리할 예정입니다. 저장 결과에는 화면에 표시된 분석표와 주요 경고가 포함됩니다.

보고서나 논문에 사용할 때는 저장된 표를 그대로 붙이기보다, 분석 방법과 가정 진단 결과를 함께 서술하는 것이 좋습니다.

## 9. 정보와 문서

정보 메뉴에는 프로그램 정보와 문서가 분리되어 있습니다.

![정보 메뉴](docs/assets/user-guide/ko/about-menu.png)

- 정보: 버전, 개발자, 이메일, 실행 환경, 인용 정보를 확인합니다.
- 개요: 프로젝트 개요, R 버전, 사용 R 패키지 정보를 확인합니다.
- 사용자 안내서: 실제 앱 조작 절차를 확인합니다.
- 분석: 구현된 분석 메뉴와 출력 항목을 확인합니다.
- 방법론 노트: 기준값, 가정 진단, 참고문헌, 해석상 주의점을 확인합니다.

## 10. 표본수, 검정력, 효과크기 메뉴

버전 1.2.0 기준으로 표본수와 효과크기 메뉴가 별도 상위 메뉴로 제공된다. 이 메뉴는 연구계획 단계에서 필요한 최소 표본 수 `n`, 이미 정한 표본 수에서의 검정력, 그리고 표본수 계산에 넣을 효과크기를 계산하기 위한 도구다.

### 표본수 사용 절차

1. 상단 메뉴에서 `표본수`를 선택한다.
2. 분석 계열을 선택한다. 예: `t-test`, `ANOVA`, `ANCOVA / MANOVA`, `GEE`, `LMM`, `Regression`, `Survival / Cox`, `ROC AUC`, `SEM / CFA`.
3. 왼쪽 계산 영역에서 `최소 표본 수` 또는 `검정력`을 선택한다. 단, Reliability / Agreement처럼 정밀도 기반 표본 수만 제공되는 메뉴는 `최소 표본 수`만 표시된다.
4. 가운데 입력 영역에 효과크기, 유의수준, 목표 검정력, 배정비, 탈락률 같은 가정을 입력한다.
5. 계산 버튼을 누르면 오른쪽 결과 영역에 계산 결과가 표시된다.
6. 시간이 오래 걸리는 시뮬레이션 기반 계산은 진행률 막대와 `Stop` 버튼이 표시된다. 중단하면 현재 계산을 종료하고 결과 영역에 중단 상태가 표시된다.

### 결과 읽는 법

- 최종 최소 표본 수는 결과 표에서 굵게 표시되는 `n (...)` 행을 먼저 확인한다.
- `n (... with dropout)`이 있으면 탈락률을 반영한 표본 수다.
- `Estimated power`는 산출된 표본 수에서 다시 계산한 검정력이다.
- `Formula / approximation`은 앱이 사용한 수식 또는 근사 방식을 요약한다.
- `References`는 해당 계산의 근거 문헌이다.

### 효과크기 사용 절차

1. 상단 메뉴에서 `효과크기`를 선택한다.
2. 분석 계열을 선택한다.
3. 가능한 입력 방식 중 하나를 선택한다. 예: 평균과 표준편차에서 Cohen's d 계산, t 통계량에서 Pearson r 계산, ANCOVA F 통계량에서 partial eta squared 계산, SPSS LMM 출력에서 partial eta squared 또는 paired dz 계산, GLMM fixed effect에서 OR/IRR 또는 latent-scale d 계산.
4. 계산 버튼을 누르면 선택한 효과크기와 변환 가능한 보조 효과크기가 함께 표시된다.

효과크기 메뉴는 실제 효과크기 또는 표본수 계획에 직접 들어가는 효과크기를 중심으로 정리되어 있다. 동등성/비열등성 margin distance, 일반 신뢰구간 정밀도, SEM/CFA 복잡도 규칙처럼 효과크기라기보다 계획 기준에 가까운 항목은 표본수 또는 관련 메뉴에서 다룬다.

### 입력값 선택 팁

- 목표 검정력 기본값은 `.95`다. 연구 분야에서 `.80`을 요구하면 직접 바꿀 수 있다.
- 회귀, 포아송, 음이항, 감마 회귀의 `Regression coefficient B`는 log link 모형의 계수다. 비율 효과는 `ratio = exp(B)`로 해석한다.
- LMM과 GEE의 unstructured correlation은 시간점 사이의 pairwise correlation을 입력한다. 세 시점이면 `r12, r13, r23` 순서로 입력한다.
- SPSS LMM 출력에서 omnibus 효과는 `F`, numerator df, denominator df로 partial eta squared를 계산하고, 시간점 간 비교는 평균차와 공분산 행렬의 분산/공분산으로 paired dz를 계산한다. 둘 중 한 종류만 입력해도 계산할 수 있다.
- GLMM/GEE의 logit 또는 log link 계수는 평균차가 아니라 log odds 또는 log rate 척도다. GEE는 population-average 효과, GLMM은 subject-specific 효과로 해석한다.
- SEM/CFA는 model degrees of freedom을 직접 넣거나, latent variables, measured variables, structural paths로 근사 df를 계산하게 할 수 있다.
