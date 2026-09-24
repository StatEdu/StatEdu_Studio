# 종단 보조표 상태와 패키지 경고 — 2026-09-17

Warning·Skipped 상태를 UI 언어에 연결하고, R package warning 접두어를 8개 언어로 번역한다. 접두어 이후의 패키지 오류 상세·변수명·숫자·특수문자는 그대로 유지한다. 사용자 데이터 열의 Warning·Skipped 문자열도 원문을 보존한다.

`validate_longitudinal_error_exports.R`에 상태 시험표와 경고 진단표를 추가했다. 실제 가중치 오류 skipped 표와 함께 8개 언어 렌더링, 사용자 이름/숫자/영어 본표 유지, 한국어·일본어 현재/누적 HTML·PDF·Word·HWPX·Excel 내용 검사를 수행한다. PDF는 `validate_longitudinal_error_pdf.py`로 텍스트를 검사한다. 기존 실제 GEE/LMM/패널 고정효과 및 종단/구조 보조표 회귀 검사도 수행한다.

여기서 추가한 패키지 경고는 표시 경로용 시험 데이터다. 실제 패키지 경고를 발생시킨 검증, 브라우저 조작, 페이지별 시각 검토는 포함하지 않는다. 복합 MI/IPW 실패 상세를 모두 번역한 것은 아니다. 계산·원본 결과는 바꾸지 않았고 메타분석은 제외했다. 설치본은 만들지 않았다.

위 자동 검사는 모두 통과했다. PDF는 한국어·일본어 각각 현재 5쪽, 누적 9쪽이다. 산출물: `tmp/longitudinal-error-exports`.
