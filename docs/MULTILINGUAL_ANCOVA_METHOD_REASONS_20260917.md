# ANCOVA 모형 선택 명칭 및 사유

2026-09-17

- 상호작용·순위·강건 ANCOVA 명칭 3개와 모형 선택 사유 5개의 추가 6개 언어 번역을 보완했다. 기존 보조표 번역 경로에서 적용되며 분석 선택/계산 및 본표 코드는 변경하지 않았다.
- `validate_ancova_method_reasons_i18n.R`: 알려진 가정 p값 조합을 실제 `ancova_choose_method`와 `ancova_method_reason`에 전달해 표준, 경고 모드 유지, 상호작용, 순위, 강건의 다섯 경우를 생성했다. 실제 모형 개요 렌더러로 8개 언어를 검증했다. 다섯 경우 모두 개별 분석 적합을 수행한 검증은 아니다.
- 별도로 실제 적합한 기본 ANCOVA의 전체 본표 셀이 8개 언어에서 동일함을 확인했다. 모형명·선택 사유 자체를 사용자 이름으로 사용한 경우 및 한글·<&>·%s도 원문 유지됨을 확인했다.
- 일본어 모형 개요 스냅샷의 현재 5표·누적 6표를 HTML/PDF/Word/HWPX/Excel로 저장했다. 문구, PDF 표지, 표 순서 및 Excel 시트 수 검증 통과. 저장 시 분석을 다시 실행하지 않는다.
- 산출물: `tmp/ancova-method-reasons-i18n/`, `tmp/ancova-method-reasons-test.log`, `tmp/ancova-method-reasons-exports.log`.
- 설치본은 생성하지 않았다. 전체 다국어 감사 완료를 뜻하지 않는다.
