# 와이드→롱 오류 번역·지시값 보존 — 2026-09-17

## 수정

변환할 행 없음, 반복측정 변수 미선택, 원본 열 미선택, 집단 수×시점 수 불일치, 저장 창 사용 불가, 변환 그룹 없음, 설정한 원본 열 없음, 지시변수 없음의 8개 오류를 8언어로 연결했다. 미리보기/그룹 설정과 저장 실패 알림에 적용했다. 알려진 앱 오류 문장과 정확히 일치할 때만 번역하며 외부 오류 상세·경로는 원문을 유지한다.

검사 중 지시값의 소문자 `r`·`n`이 구분자로 처리되는 오류도 발견했다. 와이드→롱과 병합의 지시값 파서는 쉼표 및 실제 CR/LF만 구분하도록 수정했다. 와이드→롱 수동 시점 입력의 줄바꿈 처리도 수정했다. `Normality`, `morning`, `return`, 한글, `<&> %s`, 역슬래시 포함 값의 원문을 보존한다.

## 검증

- `validate_wide_long_errors_i18n.R`: 실제 오류 8종 × 8언어, 외부 상세 보존, 실제 서버의 미리보기/그룹 설정 알림 언어를 확인했다. 저장 창 부재는 함수 환경을 격리해 검사했으며 OS 대화상자를 연 것은 아니다.
- `validate_data_editor_indicator_text.R`: 쉼표/LF/CRLF/CR 구분, 수동 시점 매핑, 실제 병합·변환의 지시값과 수치 보존 확인.
- `validate_data_editor_wide_long.R`: 기존 변환·CSV 저장 연결 검사 통과.
- `validate_merge_errors_i18n.R`, `validate_multilingual_coverage.R` 회귀 검사 통과.

로그: `tmp/wide-long-errors-i18n.log`, `tmp/data-editor-indicator-text.log`, `tmp/wide-long-regression.log`, `tmp/merge-errors-i18n.log`, `tmp/wide-long-errors-coverage.log`.

## 남은 범위

와이드→롱의 실제 브라우저 언어 왕복, 입력 상태·동적 완료 문구·표 컨트롤 및 저장 창의 최종 점검은 남아 있다. 이번 변경은 데이터 편집과 오류 안내이며 분석 출력/내보내기 경로 변경은 없다. 설치본 생성 및 설치 앱 변경 없음.
