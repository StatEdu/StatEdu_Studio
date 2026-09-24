# 생존분석 자료 포함·결측 제외 진단

2026-09-17

- 원자료 대상자·분석 대상자·제외 사유 열 제목의 추가 6개 언어 번역을 보완했다.
- 제외 사유 열에서 알려진 결측 코드 4개(missing_time/event/group/covariate)를 한국어와 추가 6개 언어로 표시한다. 코드 전체가 일치하는 경우만 번역하며 미등록 사유는 원문 유지한다. 계산·제외 로직은 변경하지 않았다.
- `validate_survival_inclusion_i18n.R`: 생존 fixture에 시간·사건·공변량 결측을 넣고 실제 Cox 분석의 포함/제외 표를 생성했다. 집단 결측과 사용자 사유는 별도 표시 fixture로 검증했다. 8개 언어에서 건수 및 언어 간 표시 정밀도, 미등록 사유·한글·<&>·%s 원문과 영어 본표를 확인했다.
- 기존 Cox Efron/Breslow/Exact 동률 검증 통과.
- 일본어 현재 4표·누적 6표의 HTML/PDF/Word/HWPX/Excel 내용, PDF 표지, 표 순서, Excel 시트 수 검증 통과. 저장 과정에서 재분석하지 않는다.
- 산출물: `tmp/survival-inclusion-i18n/`, `tmp/survival-inclusion-test.log`, `tmp/survival-inclusion-exports.log`, `tmp/survival-inclusion-regression.log`.
- 설치본은 생성하지 않았다. 시간 순서 오류·구간 중복 등 나머지 제외 코드와 영향력 진단은 후속 점검 대상이다.
