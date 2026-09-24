# PLS 품질표의 형성형 근거와 f² 실패 안내

## 변경

- 형성형 근거 행의 제목, 영역/지표 포함 근거/내용타당도 절차·출처/중복성 근거의 기록 상태, 완비·미비 안내를 UI 언어로 표시한다.
- f² 계산의 알려진 실패 사유 8종과 축소 모형 추정 오류 접두사를 번역한다. 분석 엔진의 원본 오류 내용은 보존한다.
- 표시용 함수가 원시 행의 근거 자료와 실패 사유 속성을 사용한다. 판정·계산 및 원시 영문 결과는 바꾸지 않는다.
- 번역된 복합 셀은 재번역에서 제외하여 사용자 구성개념 이름을 보존한다. 검토 필요 항목 표에도 동일한 처리를 적용한다.

## 검증

- `validate_structural_pls_dynamic_quality_i18n.R`: 8개 언어에서 형성형 문서 완비/미비, 알려진 f² 실패 사유와 엔진 오류 원문, 사용자 이름 `Review` 및 `Normality 사용자 <&>` 보존을 확인했다.
- 형성형 메타데이터와 실패 사유는 실제 적합 객체에 덧붙인 표시 검증 fixture이다. 형성형 추정이나 8종의 수치적 실패 자체를 재현한 검사는 아니다.
- 기존 실제 PLS 18행 상세 검사, PLS 영어 본표 불변 검사, CFA 본표·사용자 라벨·품질 요약 검사를 함께 통과했다.
- 로그: `tmp/structural-pls-dynamic-quality-validation.log`.
- 번역 사전 검사 및 일본어 현재·누적 HTML/PDF/Word/HWPX/Excel 내용 검사를 통과했다. 현재 10개·누적 13개 표의 HTML/Word 순서와 Excel 시트 수가 일치하며, PDF 본문과 표지도 확인했다. 저장 시 재적합하지 않는다.
- 저장 fixture: `tmp/structural-pls-dynamic-quality-i18n`; 로그: `tmp/structural-pls-dynamic-quality-exports.log`, `tmp/structural-pls-dynamic-quality-coverage.log`.

## 범위 제한

이번 대상은 품질 체크리스트 및 검토 필요 항목 표다. 별도의 측정 진단/형성형 내용타당도 표, 보고 맥락·구성개념 명세 표는 추가 점검 대상이다. 설치본은 만들지 않았다.
