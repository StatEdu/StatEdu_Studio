# HTMT 부트스트랩 상태 안내 다국어

2026-09-17

HTMT 상세 보조표의 백그라운드 계산, 사용자 중단, 계산 실패, 일부 재표집 실패, BCa 불가, Caution, Unreliable 안내 7개 문단에 추가 6개 언어를 적용했다. 영어·한국어 문구와 상태 판정 조건 및 수치 계산은 변경하지 않았다.

`scripts/validate_htmt_status_i18n.R`는 실제 lavaan 적합의 HTMT 결과와 상태별 부트스트랩 표시 fixture를 사용한다. 진행 중·중단·실패·주의·신뢰 불가·BCa 불가·정상 완료의 7개 상황을 8개 언어에서 검사했다. 번역된 상태 문구가 올바른 분기에서 나타나고, 안내가 필요 없는 정상 완료에는 추가되지 않음을 확인했다. HTMT 본표 HTML은 언어별로 동일한 영어 출력임을 확인했다. 부트스트랩 계산을 실제로 실패시키는 검사는 아니다.

공통 다국어 커버리지 검사 통과. 기록은 `tmp/htmt-status-validation.log`, `tmp/htmt-status-coverage.log`, `tmp/htmt-status-exports.log`, 산출물은 `tmp/htmt-status-i18n/`에 저장한다.

일본어 현재 결과 11개 표 및 누적 결과 12개 표의 HTML/PDF/Word/HWPX/Excel 내보내기, PDF 텍스트, HTML/Word 표 순서와 Excel 시트 수를 확인했다. 첫 실행에서 현재 결과 HWPX 변환 후 한컴 자동화의 null 객체 오류가 발생했다. 동일 스냅샷 재실행에서는 현재·누적 HWPX 모두 내용 검사까지 통과했다(`tmp/htmt-status-exports-retry.log`). 변환기의 간헐적 오류 자체를 수정한 것은 아니다.

이번 범위는 상태 안내 문단이다. 상세 보조표의 동적 제목, 일반 해설 및 셀의 번역 상태는 추가 점검 대상으로 남는다. 설치본은 생성하지 않았다.
