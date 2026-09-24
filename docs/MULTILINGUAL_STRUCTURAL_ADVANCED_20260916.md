# 구조방정식 고급 옵션 다국어

- CFA·SEM·PLS의 고급 옵션 탭에서 표본수/검정력 근거, 분석 근거 입력 힌트, ML/Wishart 관례, 결측 민감도, 잠재변수 척도, RMSEA 신뢰수준과 잠재조절 방법의 표시를 번역했다.
- 고정 영문 선택지였던 Delta/pattern-mixture와 CB-SEM 곱지표 방법도 표시 이름을 분리했다. 내부 선택값과 계산 코드는 그대로다. RMSEA의 90%/95%/99% CI 표기는 유지한다.
- 자료: `scripts/fill_structural_advanced_i18n.py`.

## 검증

- `validate_structural_advanced_i18n.R`: 3분석 × 8언어 검사 통과. 추가 6언어의 라벨/선택지/설명/입력 힌트가 사전 값으로 출력됨을 검사했다.
- 전체 옵션 입력의 ID, 값, 기본 선택, textarea 크기와 조건부 표시식을 보존했다. 수정 전/후 영어 HTML은 Shiny 임시 탭 ID만 정규화한 후 동일하다.
- 기존 부트스트랩 검사 및 공통 다국어 사전 검사 통과. 근거: `tmp/structural-advanced-i18n.log`, `tmp/structural-advanced-i18n/*.html`, `tmp/structural-advanced-coverage.log`.

## 범위

- 설정 UI만 수정했다. 분석 계산·결과표·저장 내용은 변경하지 않아 이번 수정에는 5형식 내보내기 재검사가 해당하지 않는다.
- 실제 브라우저의 옵션 변경/언어 왕복과 실제 모형별 계산은 이번 렌더링 검사의 범위 밖이다. 다집단·타당도·진단·공통방법 탭은 별도 점검이 필요하다.
- 이전 복합표본 사용자 모형의 HWPX 변환 시간 초과도 미해결이다. 설치본을 만들지 않았다.
