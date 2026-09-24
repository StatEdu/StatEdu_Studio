# 구조방정식 다집단 설정 다국어

- CFA·SEM·PLS 다집단 탭의 제목, 모형별 측정불변성/경로 비교 안내, 분석 활성화 라벨, 집단 선택, 경로 비교 범위, MICOM 횟수/시드 등 16문구를 번역했다.
- 사용자 제공 집단명과 동적으로 생성되는 경로명은 번역하지 않는다. 순열 횟수·기본값·실행 조건 및 계산 코드는 유지했다.
- 자료: `scripts/fill_structural_multigroup_ui_i18n.py`.

## 검증

- `validate_structural_multigroup_ui_i18n.R`: CFA·SEM·PLS × 8언어 통과. 라벨/선택지/설명/입력값/기본값/조건부 표시식 검사.
- 집단 선택지 `사용자 집단 [g]`, `Normality` 및 내부 값 `g`, `g2`의 보존을 확인했다.
- 실제 이름 resolver와 경로 선택지 생성 함수로 `Normality → 사용자 요인` 및 `user_path`가 모든 언어에서 유지됨을 확인했다. 측정경로와 공분산 경로는 비교 목록에서 제외된다.
- 수정 전/후 영어 HTML은 임시 탭 ID 정규화 후 동일하다. 공통 사전 검사도 통과했다.
- 근거: `tmp/structural-multigroup-ui-i18n.log`, `tmp/structural-multigroup-ui-i18n/*.html`, `tmp/structural-multigroup-ui-coverage.log`.

## 제한

- UI 설정만 수정했다. 분석 결과표·계산·저장 내용은 변경하지 않아 이번 수정에는 5형식 결과 저장 검사가 해당하지 않는다.
- 실제 브라우저에서 다집단 설정을 바꾸고 언어를 왕복하는 동작, 모형별 다집단 분석 실행/결과는 이번 검사에 포함하지 않는다.
- 타당도·진단·공통방법 설정과 실제 모델 검증은 남아 있다. 이전 복합표본 사용자 모형의 HWPX 시간 초과도 미해결이다. 설치본을 만들지 않았다.
