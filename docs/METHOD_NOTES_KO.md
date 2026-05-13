# 방법론 노트

EasyFlow Statistics은 회귀분석 후 가정 진단 결과에 따라 추론 방식을 선택합니다.

## 정규성

잔차 정규성은 Lilliefors 보정 Kolmogorov-Smirnov 검정으로 확인합니다.

## 등분산성

등분산성은 Breusch-Pagan 검정으로 확인합니다.

## Robust Standard Errors

이분산성이 확인된 경우 HC3 robust standard errors를 보고합니다.

## Bootstrap

잔차 정규성 가정이 지지되지 않는 경우 bootstrap confidence intervals와 bootstrap p values를 보고합니다.

