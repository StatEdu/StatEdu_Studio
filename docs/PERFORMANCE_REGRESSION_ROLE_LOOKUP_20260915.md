# 회귀 설정 변수 목록의 역할 조회 개선

## 변경

`roles_for_variables()`를 R/data_roles.R에 추가하고 R/data_regression_setup.R의 회귀 변수 목록에서 사용한다. 일반 문자형 벡터의 역할을 일괄 분류해 각 변수마다 긴 역할 목록을 다시 검색하던 비용을 줄였다. 통제→독립→종속 순서로 덮어써 기존 종속 우선, 다음 독립, 다음 통제의 우선순위를 유지한다.

벡터 이름도 기존 vapply와 같이 유지한다. factor, 숫자, 행렬, 객체 등은 기존 vapply와 role_for_variable로 복귀한다. 기존 scalar 함수와 범주 라벨 표·일반 변수 표의 다른 호출처는 이번에 바꾸지 않았다. 분석 수식, 변수 역할 지정값과 최종 정렬 규칙은 변경하지 않았다.

## 검증

`scripts/fixtures/regression_role_lookup_reference.R`에 변경 전 회귀 목록 함수를 보존하고 `scripts/validate_regression_role_lookup.R`를 추가했다.

- 전체 반환 표·속성·진단·표준 출력·RNG 80개 조건 정확 일치: 빈 선택, 중복, 빈 문자열, NA, 이름 있는 벡터, factor/숫자, 역할 중복, 메타데이터의 중복 이름과 라벨 override 포함.
- 새 helper의 직접 역할 벡터·이름·진단/RNG 40개 비교: 빈/NULL, NA, 이름 있는 벡터, factor, 숫자, 행렬, 리스트 포함.
- 중복 선택과 역할 중복을 포함한 고정 elementId의 변수 목록 DT HTML 1개 정확 일치.
- 기존 데이터 IO 및 사례 선택·분할 변수 제외 검사 통과. 후자는 빈도·상관·PCA·회귀·위계 회귀·GLM의 수동 제외 비교와 scope reset/canvas 경로 정리를 검사한다.

정확 비교에는 `identical(..., num.eq=FALSE)`를 사용했다. 실제 브라우저 메뉴 조작과 저장 형식별 신규 검사는 이번 범위에 포함하지 않는다.

## 성능

메타데이터 info=NULL인 변수 목록에서 일반 문자 이름을 종속/독립/통제 세 집합에 대략 1/3씩 배분했다. 변경 전후 함수는 동일하게 바이트 컴파일하고 준비 비교 후 각 조건을 5회, 순서를 교대해 측정했다. 독립 프로세스 3개는 순차 실행했다. 시간에는 반환 표 생성과 진단 캡처가 포함되고 HTML/브라우저는 제외된다.

|변수 수|기존 중앙값 범위(ms)|개선 중앙값 범위(ms)|
|---|---:|---:|
|1,000|3.06~3.18|0.45~0.48|
|5,000|39.89~40.70|1.12~1.30|
|10,000|145.15~148.41|2.02~2.03|

큰 역할 집합에서 반복 membership 조회를 제거한 효과다. 역할 집합이 작거나 다른 자료형인 조건의 같은 개선율을 보장하지 않는다. 회귀 계산이나 전체 앱 로딩의 개선율도 아니다.

초기 system.time 기반 측정에서 후보가 0.00초로 반올림돼 Sys.time/difftime으로 세 프로세스를 다시 측정했다. 최종 근거는 refined.R, refined-1/2/3.log 및 refined-1/2/3.csv다. 초기 run-*.log를 무시간 처리의 근거로 사용하지 않는다.

관련 자료는 `output/regression-role-lookup-20260915` 아래 baseline.R, compare.R, product-validation.log, data-io.log, scope.log에 있다. 제품 적용 후 최종 비교를 다시 실행했다.

최종 SHA256:

- data_roles.R: `75A8A5BE6135D404BCA6B5D2F321BEC84068A87CB05896C87CD7CE623ECB8810`
- data_regression_setup.R: `74888455EFDA6B749F8C93941292443664C633F84423C53432D7FC75CD07648F`
