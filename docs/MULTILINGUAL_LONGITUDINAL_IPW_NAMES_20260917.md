# 종단 IPW 관측모형 변수명 보존 — 2026-09-17

관측모형의 유일한 예측변수 이름이 `Warning`일 때 한국어 보조표에서 이름이 번역되는 문제를 실제 IPW 계산으로 재현했다. IPW 진단 표 생성 시 관측모형 변수 셀에 공통 `result_user_cells` 메타데이터를 부여하고, 가중치 요약 표와 표시 표를 거칠 때 해당 위치 정보를 전달하도록 수정했다. 수치 계산식은 변경하지 않았다.

`Intercept only`는 실제 사용자 변수명일 수도 있고 앱의 절편 전용 상태 안내일 수도 있다. 문자열 자체가 아니라 생성 시점의 `model_terms` 존재 여부로 구분한다. 실제 변수인 경우 원문을 보존하고, 예측변수가 없어서 생긴 안내는 UI 언어로 번역한다.

`validate_longitudinal_ipw_names_i18n.R`에서 실제 IPW 계산의 `Warning` 변수, 진단 표 생성 함수의 `Intercept only` 변수, 예측변수 없는 안내의 3경로 × 8언어, 24개 검사가 통과했다. 두 후자의 경우 진단 표 생성 함수에 직접 입력하는 시험이며 실제 모형 적합 시험과 구분한다.

실제 반환 표를 `validate_longitudinal_error_exports.R`에 추가해 기존 가중치·MI/IPW 검사와 함께 한국어/일본어 현재·누적 HTML/PDF/Word/직접 생성 HWPX/Excel을 검증한다. PDF 텍스트는 `validate_longitudinal_error_pdf.py`로 확인한다. 산출물은 `tmp/longitudinal-error-exports`다.

기존 저장 스냅샷을 재작성하지 않았다. 전체 브라우저·모형 조합·페이지별 시각 검토는 별도 범위다. 메타분석은 제외했고 설치본은 만들지 않았다.

위 검사와 5종 저장 검증이 모두 통과했다. PDF 네 개의 모든 캡처 텍스트를 확인했으며 각 언어 현재 21쪽, 누적 41쪽이다. 변경 파일의 `git diff --check`도 통과했다.
