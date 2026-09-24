# 구조방정식 부트스트랩 설정 다국어

- CFA·SEM·PLS의 부트스트랩 탭에 있는 모형별 기본 실행 설명, 효과/신뢰도/적합도/판별타당도 제목, 재표집 종류 라벨, 난수 시드, CI 방법, 조건/제한 안내를 번역 경로에 연결했다.
- BC·백분위수·BCa 선택지의 표시는 UI 언어를 따르며 내부 값 `bias_corrected`, `percentile`, `bca`는 유지한다. BCa의 느린 계산 안내도 번역했다.
- 자료: `scripts/fill_structural_bootstrap_i18n.py`. Bootstrap·CI·HTMT·PLS 등의 기술 약어는 필요한 위치에 유지한다.

## 검증

- `validate_structural_bootstrap_i18n.R`: 3분석 × 8언어 통과. 부트스트랩 탭의 라벨/선택지/제목/설명을 확인했다. 추가 6언어는 각 원문의 사전 값이 렌더링에 나타나는지도 검사했다.
- 수정 전/후 영어 HTML은 Shiny 임시 탭 ID만 정규화하여 동일함을 확인했다. 모든 입력 ID·값·기본 선택과 조건부 표시식도 동일하다.
- 기존 추정 탭/재표집 횟수 검사 및 공통 사전 검사를 재실행하여 통과했다.
- 근거: `tmp/structural-bootstrap-i18n.log`, `tmp/structural-bootstrap-i18n/*.html`, `tmp/structural-bootstrap-coverage.log`.

## 제한

- 이번 수정은 UI 설정 문구에 한정된다. 통계 계산·결과표·저장 내용에 변경이 없어 결과 5형식 저장 검사는 해당하지 않는다.
- 실제 브라우저에서 옵션별 실행과 언어 왕복은 검증하지 않았다. 고급 옵션·다집단·타당도·진단·공통방법 탭과 실제 모형별 결과 검증은 남아 있다.
- 복합표본 사용자 모형 HWPX 시간 초과는 미해결이며, 설치본을 만들지 않았다.
