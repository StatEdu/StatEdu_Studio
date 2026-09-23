# StatEdu Studio 1.3.0 — SmartPLS HS100 재검증

SmartPLS 4.1.1.8 Student에서 HS100 모형을 새로 구성하고 PLS와 PLSc를 실행했다. 각 추정법의 saturated SRMR, d_ULS, d_G 총 6개 값이 소수 셋째 자리 표시 오차 범위(절대오차 ≤ .0005)에서 StatEdu 1.3.0과 일치한다. 원시 정밀도의 완전 일치를 의미하지 않는다.

| 추정법 | 지표 | StatEdu | SmartPLS 표시값 | 판정 |
|---|---|---:|---:|---|
| PLS | SRMR | .1121530 | .112 | 일치 |
| PLS | d_ULS | .5660237 | .566 | 일치 |
| PLS | d_G | .1756490 | .176 | 일치 |
| PLSc | SRMR | .1226683 | .123 | 일치 |
| PLSc | d_ULS | .6771381 | .677 | 일치 |
| PLSc | d_G | .2817688 | .282 | 일치 |

HolzingerSwineford1939의 원본 순서 첫 100행, x1–x9, 결측 0개를 사용했다. 세 반영형 구성개념은 visual(x1–x3), textual(x4–x6), speed(x7–x9)이며 SmartPLS 캔버스 이름은 각각 x, t, s다. 경로는 visual→textual, visual→speed, textual→speed다.

Path 가중, 표준화 결과, 초기 가중치 +1, 중단 기준 10⁻⁷이다. SmartPLS의 DEFAULT 설정에서도 보고서 초기 가중치 1.0과 설정 파일의 9개 +1을 확인했다. 두 실행 모두 26회 반복으로 종료했다. SmartPLS 최대 3,000회와 StatEdu 최대 300회 차이는 실제 반복 수에 영향을 주지 않았다.

실행일은 external_run.json에 보존한다. 데이터·모형·설정·계산 완료·적합도 화면 등 14개 비공개 원본을 SHA-256과 크기로 봉인했다. 내보내기 기능이 잠겨 있어 표시값을 전사했다. 기존 1.2.4 기록을 덮어쓰지 않았다. 과거 TAM 보조 비교는 원본 근거가 없어 1.3.0 재검증에 포함하지 않는다. 이 검증은 HS100 saturated fit 범위이며 모든 PLS 분석의 정확성을 보증하지 않는다.

Ringle, C. M., Wende, S., and Becker, J.-M. (2024). SmartPLS 4. Bönningstedt: SmartPLS GmbH. https://www.smartpls.com .

SmartPLS Terms §8.1 citation retained. SmartPLS Terms §3.4: vendor screenshots and UI/project/settings artifacts remain private and are excluded from this public evidence directory.
