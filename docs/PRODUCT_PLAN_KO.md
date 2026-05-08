# EasyFlow Regression 제품 계획

## 기본 방향

EasyFlow Regression은 Windows 로컬 실행형 Shiny 앱으로 개발한다. 사용자는 R 또는 RStudio를 직접 열지 않고 `EasyFlow_Regression.bat`을 더블클릭하여 앱을 실행한다.

앱 UI와 결과표는 영어로 작성하고, 매뉴얼은 영어와 한국어 두 가지로 제공한다.

## 핵심 기능

- 데이터 파일 열기
- 설정 불러오기 및 저장하기
- EasyFlow Regression 분석 메뉴
- 분석 결과 불러오기 및 저장하기
- 다중회귀분석
- 위계적 회귀분석
- PROCESS-style 매개 및 조절 모형 직접 구현
- 논문용 결과표 및 해석문 출력

## 회귀분석 자동 분기

잔차 정규성 검정과 등분산성 검정을 기준으로 다음 네 가지 방식 중 하나를 적용한다.

| Residual normality | Homoscedasticity | Method |
|---|---|---|
| satisfied | satisfied | OLS |
| satisfied | violated | HC3 robust SE |
| violated | satisfied | Bootstrap CI and bootstrap p |
| violated | violated | HC3 robust SE plus bootstrap CI and bootstrap p |

## Durbin-Watson

Durbin-Watson 임계값은 `EasyFlow_Statistics_3.0.xlsx`의 `Durbin-Watson` 시트를 사용한다.

- dU range: `DP1:EI2000`
- dL range: `EL1:FE2000`
- lookup rule: `INDEX(range, n, p)`

범주형 변수가 더미변수로 확장된 경우 `p`는 원 변수 수가 아니라 실제 회귀설계행렬의 절편 제외 열 수로 계산한다.

## 배포 전략

- Private repository: 개발용
- Public repository: 안정판 공개 및 릴리스용
- Public release에는 소스코드, 문서, 검증 노트, 예제 데이터를 포함한다.

