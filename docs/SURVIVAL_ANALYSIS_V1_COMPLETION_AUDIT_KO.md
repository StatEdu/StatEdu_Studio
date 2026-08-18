# StatEdu Studio 생존분석 v1 완료 감사

상태: 구현·전용 회귀검증·핵심 UI 스모크 테스트 완료
감사일: 2026-08-16

## 1. v1 실행 지원 범위

- 단일 사건 Kaplan–Meier, 생명표, log-rank/Breslow/Tarone–Ware
- RMST 추정과 집단 간 차이·비율
- 표준 Cox 비례위험 회귀와 PH·잔차·영향력 검토
- 지연 진입 Kaplan–Meier 및 Cox (`Surv(entry, exit, event)`)
- start–stop 시간의존 Cox와 대상자 군집-강건 표준오차
- 누적발생함수(CIF), Gray 검정
- 원인별 Cox와 Fine–Gray 회귀
- Cox 보정 생존곡선(단일행/지연 진입 구조)

## 2. 공통 계약 및 보고 지원

- 연구 목적·자료 형태·사건 구조 기반 분석 추천
- 시간 원점·시간 단위 기록
- 관측된 모든 사건 코드의 명시적 역할 매핑과 사용자 확인
- 분석 행 및 대상자 수, 제외 사유, 사건·경쟁사건·검열 수 감사
- start–stop 구간 중첩, 시간 역전, 복수 사건, 사건 이후 구간 검사
- 사건/모수 비율, 희소 사건, 군집 수, 검열률 및 말단 위험집단 검토
- 역 Kaplan–Meier 잠재 추적기간 요약
- KM/CIF 곡선과 number-at-risk 표 결합
- 방법 문장, 보고 체크리스트, 해석 가이드 및 CSV/TXT 감사 내보내기

## 3. 명시적으로 차단하는 범위

- 생존 예측모형 및 외부/내부 검증
- interval/left-censored 전용 모형
- 반복사건 모형
- multi-state 모형
- frailty 모형
- 지연 진입 경쟁위험 CIF/Fine–Gray
- 지연 진입 생명표
- start–stop Cox의 marginal adjusted survival

차단 항목은 지원되는 분석으로 자동 변환하지 않으며 추천 카드에 미지원 또는 확인 필요 상태로 표시한다.

## 4. 완료 감사에서 수정한 정확성 항목

- 설계 화면에서 확정한 `exclude`, `competing_event`, 라벨을 KM·Cox·경쟁위험 엔진까지 보존한다.
- 지연 진입 경쟁위험을 일반 CIF로 잘못 실행하지 않고 `U06`으로 차단한다.
- 지연 진입 자료에서 생명표 선택을 차단한다.
- start–stop Cox에서 행 단위 marginal standardization을 실행하지 않는다.
- 기존 생존 검증은 누락된 외부 CSV 대신 `survival::lung`을 재현 가능한 대체 데이터로 사용한다.
- 데이터 탭 선택 상태가 비어 전달되는 경우 현재 데이터 열과 변수 메타데이터에서 생존분석 변수 목록을 복구한다.
- 동적으로 생성되는 사건코드 역할 입력은 값 변경이 안정적으로 반영되는 기본 선택상자를 사용한다.
- 경쟁위험 추천 후 화면 전환 시 `time`, `event`, `group`, 공변량, 사건코드 및 회귀 목표량을 다음 렌더 주기까지 보존한다.

## 5. 검증 결과

- `scripts/validate_survival_preflight.R`: 통과
- `scripts/validate_survival.R`: 통과
- 전체 R 모듈 로딩: 통과
- 생존 보고 CSV/TXT 및 KM/CIF 결합 그림 생성: 통과
- `git diff --check` 생존 관련 파일: 통과

실제 Shiny UI 스모크 테스트 결과:

- 분석 설계의 변수 목록·시간 원점·시간 단위·사건코드 매핑: 통과
- 추천 규칙과 분석 화면 자동 전달: `G01`, `A01`, `A05` 통과
- Kaplan–Meier, log-rank, number-at-risk 출력: 통과
- Cox 회귀, 모형검정, PH·잔차·영향력 진단 출력: 통과
- CIF, Gray 검정, 원인별 Cox, Fine–Gray 및 안정성 진단 출력: 통과
- HTML·PDF·Excel 공통 저장은 현재 결과 컬렉션 미등록 상태에서 비활성임을 확인했으며, 실제 저장 파일 생성 검증은 후속 작업으로 보류했다.

이번 UI 결함에 대한 자동 회귀검증:

- 선택 상태·현재 데이터·변수 메타데이터의 변수 범위 복구 우선순위
- 사건코드 역할 입력의 비-Selectize 렌더링 계약
- 경쟁위험 전달값의 `time/status/group`, `Both estimands`, 다중 공변량 선택 유지

전체 `validate_stabilization.ps1 -Full`은 생존분석 테스트 실행 전에 SEM 검증 스크립트 10개가 안정화 매니페스트에 등록되지 않은 작업트리 상태로 중단되었다. 이는 본 생존분석 변경의 테스트 실패가 아니며, unrelated SEM 범위를 임의로 수정하지 않았다.

## 6. 후속 버전 후보

- v1.1: 층화 Cox, 사용자 지정 cluster Cox, spline/시간상호작용 보조 UI
- v1.2: 반복사건 분석
- v1.2+: multi-state 분석
- 별도 모듈: interval-censored survival, 예측 성능평가, frailty
