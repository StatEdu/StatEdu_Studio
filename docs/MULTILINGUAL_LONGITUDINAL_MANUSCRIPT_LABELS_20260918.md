# 종단분석 원고·소프트웨어 표제 번역

## 변경

- `SuggestedText`, `Methods`, `Sensitivity`, `Software` 4개 표제를 8개 언어 사전에 반영했다.
- 한국어 기존 화면 표현인 제안 문장·방법·민감도 분석·소프트웨어를 유지했다.
- 프랑스어 Section/Version, 독일어 Software/Version, 스페인어 Software는 원문과 철자가 같은 정상 번역으로 구분했다. 새 표제는 실제 출력이 사전값과 일치하는지도 검사한다.
- 분석·표시·저장 알고리즘은 수정하지 않았다.

## 검증 결과

- `scripts/validate_longitudinal_manuscript_labels_i18n.R`: 고정 난수 자료 200행/40명으로 실제 LMM을 적합하고 원고 제안 표(5행)와 소프트웨어 버전 표를 생성했다. 수정 전 비영어 표제 누락을 재현했으며, 수정 후 8개 언어에서 표제·구분값 및 생성된 원고 문장의 번역 검사를 통과했다. 패키지명·버전 값과 원본 표가 유지됨을 확인했다.
- `scripts/validate_i18n_contract.R`: 통과.
- `scripts/validate_longitudinal_result_table_contract.R`: 통과.
- `scripts/validate_longitudinal_count_exports.R`: 한국어·일본어 각각 현재·누적 결과의 HTML/PDF/DOCX/HWPX/XLSX 저장 통과. 캡처된 표를 재사용해 저장하며 저장 시 분석을 다시 실행하지 않는다. 8개 언어에서 영어 본표 보존도 확인했다.
- `scripts/validate_longitudinal_error_pdf.py`: PDF 전체 캡처 텍스트 보존 통과. 한국어·일본어 모두 현재 2쪽, 누적 3쪽.
- 산출물: `tmp/longitudinal-manuscript-labels-i18n/exports`.

## 범위

이번 검사는 LMM에서 생성된 원고·소프트웨어 표와 공통 표제에 한정한다. 모든 모형·원고 생성 분기를 검증한 것은 아니며 남은 번역 후보는 계속 확인해야 한다. 문서 앱별 수동 시각 검토와 설치파일 빌드는 수행하지 않았다.
