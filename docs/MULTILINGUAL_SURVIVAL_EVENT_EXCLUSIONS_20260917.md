# 생존분석 사건 코드 제외 사유

보조표의 `excluded_event_code`, `competing_event_requires_competing_risk`,
`unsupported_other_state`를 한국어 및 일본어·중국어·스페인어·프랑스어·독일어·베트남어로
표시하도록 추가했다. 영어는 기존 코드 표시를 유지한다. 알려진 코드에만 적용하며
임의의 사용자 문구는 번역하지 않는다.

## 검증 결과

- `validate_survival_event_exclusions_i18n.R`: 실제 사건 매핑과 사전검사로 세 제외 사유를
  생성했다. 일반 분석과 경쟁위험 분석에서 경쟁사건 제외 여부가 다른 것을 확인했다.
  오류 자료의 모형을 적합하지 않고 실제 사전검사와 제외 집계 함수를 검증했다.
- 세 표 × 8개 언어의 제외 건수, 표시 정밀도 및 사용자 문구 보존 통과.
- `validate_survival_inclusion_i18n.R` 재검증 통과: 기존 결측 사유 및 실제 Cox 본 표 영어 유지.
- 일본어 현재 결과 3개 표 / 누적 결과 4개 표의 HTML/PDF/Word/HWPX/Excel 검증 통과.
  PDF 내용과 표지, 표 순서와 Excel 시트 수를 확인했다. 저장 시 분석을 재실행하지 않았다.

설치본은 생성하지 않았다. 생존분석의 기타 진단 보조표 및 전체 다국어 완료를 뜻하지 않는다.
