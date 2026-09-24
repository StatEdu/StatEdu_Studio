# 패키지 상관·생존분석 GUI 점검

2026-09-15. 현재 1.3.0 unpacked 실행 패키지를 격리 프로필로 실행하고 `scripts/fixtures/survival_validation.csv`의 72행을 사용했다. 제품 및 설치 파일은 변경하지 않았다.

## 실행 확인

| 분석 | 설정 | 결과 표 | 로딩 확인한 그림 |
| --- | --- | ---: | ---: |
| 상관 | time, age | 4 | 2 |
| Kaplan–Meier | time, status=1, sex 집단 | 12 | 4 |
| Cox | time, status=1, age 공변량 | 14 | 3 |

화면의 실제 변수 이동/실행 버튼으로 세 분석을 실행했다. 표 생성·필수 변수명·그림 9개의 이미지 로딩·누적 결과 저장·JavaScript pageerror 없음·창 닫기 후 R 종료 로그를 확인했다. 같은 임시 프로필로 다시 실행해 누적 결과 3개의 ID·순서·HTML이 저장본과 정확히 일치함을 확인했다. 이는 GUI 동작 및 저장/복원 검사이며 수치 엔진을 독립 재계산한 비교나 파일 형식별 내보내기 검사는 아니다.

최종 실행의 초기 준비 3.508초, 복원 재실행 준비 2.858초는 각 1회 관측이다. 분석별 기록 시간에는 계산뿐 아니라 스크롤·그림 로딩·스크린샷·누적 저장까지 들어가므로 분석 자체 성능 개선율로 사용하지 않는다.

## 시각 점검에서 남은 문제

최초 검사는 아직 img가 없는 화면 밖 지연 그래프를 성공으로 간주할 수 있었다. `scripts/smoke_packaged_correlation_survival.cjs`를 보완해 각 `.shiny-plot-output`으로 스크롤하고 이미지가 생성되고 재계산 표시가 사라질 때까지 기다린 뒤 최종 실행을 다시 검증했다. 최초 결과 디렉터리 `tmp/packaged-smoke-eTqak0`는 최종 그림 로딩 근거로 사용하지 않는다.

최종 개별 그림을 시각 확인했다. 상관 산점도/히트맵과 Cox 위험비 그림은 표시됐으나 Kaplan–Meier 화면용 그림에서는 위험집단 표의 두 행이 가까이 붙고 긴 하단 주석의 오른쪽이 잘려 보였다. 화면 고정 메뉴가 그림 상단에 겹치는 캡처 현상도 있어 상단 캡처만으로 원본 누락을 단정하지 않았다.

누적 저장 HTML에 포함된 첫 KM PNG를 URI/base64 디코딩해 별도로 확인한 결과, 저장본에서는 위험집단 두 행·범례·주석 전체가 정상 표시됐다. 따라서 현재 확인된 문제는 화면용 그림과 저장본의 크기/배치 차이다. 화면의 KM 그림 레이아웃 재현·보완이 다음 작업이며, 전체 GUI 시각 검증을 통과했다고 보고하지 않는다. 이번에는 출력 코드를 변경하지 않았다.

## 증거

- 실행: `scripts/smoke_packaged_correlation_survival.cjs`
- 재실행: `scripts/smoke_packaged_isolated.cjs --restore=D:/Program/Studio/tmp/packaged-smoke-1KsaLR`
- 최종 증거: `tmp/packaged-smoke-1KsaLR/`의 result.json, restore-result.json, 분석별 HTML/PNG, 그림별 PNG, 저장 HTML, saved-results.json, profile/logs/startup.log.
- 원본 확인: 같은 폴더 `km-embedded-first.png` (저장 HTML에서 추출, 이미지 수정 없음).

실제 기존 설치·사용자 설정 변경·외부 배포는 하지 않았다.
