# 종단 MI/IPW 실제 반환 경로 — 2026-09-17

실제 결측 민감도 함수에서 반환하는 다섯 경로를 검사했다: MI 관측 ID/시점 행 부족, 결측이 없어 MI 불필요, 대치별 가중치 오류, WGEE의 비-GEE 모형 제한, IPW 결합 가중치 오류. 대치별 오류는 mice를 실제 실행한 뒤 가중치 검증에서 발생시켰으며 모의 반환값을 사용하지 않았다.

행 부족·대치 불필요·WGEE 모형 제한의 세 문장이 영어로 남아 있어 8언어 카탈로그와 보조표 표시 함수를 연결했다. 계산 함수와 원본 결과는 바꾸지 않았다. 영어 본표와 사용자 변수명/라벨 보존 원칙을 유지한다.

- `scripts/validate_longitudinal_actual_mi_ipw.R`: 5경로 × 7개 비영어 언어의 실제 반환 표 35개 검사 통과. 숫자 열 보존도 확인했다.
- `scripts/validate_longitudinal_error_exports.R`: 실제 반환 표를 기존 오류·상태 시험 표와 함께 한국어/일본어 현재·누적 HTML, PDF, Word, 직접 생성 HWPX, Excel로 저장했다. 저장 텍스트와 원본 보존 검사를 통과한 뒤 생성되는 expected JSON 네 개를 확인했다.
- `scripts/validate_longitudinal_error_pdf.py`: PDF 네 개의 캡처 텍스트 검사 통과. 각 언어 현재 10쪽, 누적 19쪽.
- `scripts/validate_longitudinal_structural_i18n.R`: 기존 실제 GEE/LMM/패널 고정효과 결과의 8언어 회귀 검사 통과. 본표 영어, 진단 번역, 사용자 이름 보존을 확인했다. 변경 파일의 `git diff --check`도 통과했다.

산출물은 `tmp/longitudinal-error-exports`에 있다. 작은 합성 자료의 대표 경로 검사이며 MI/IPW의 모든 옵션·실패 조합, 브라우저 조작, 페이지별 시각 검토를 완료했다는 뜻은 아니다. 메타분석은 제외했고 설치본은 만들지 않았다.
