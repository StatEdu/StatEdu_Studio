# 상관분석 저장 HTML 재사용 경로 검증

2026-09-14. 보관된 결과를 다른 형식으로 내보낼 때 분석·그림을 다시 생성하는지 확인했다. **결과 목록의 내보내기는 이미 보관 HTML을 재사용하므로 새 캐시를 추가하지 않았다.** 운영 코드는 변경하지 않았다.

## 확인한 경로

개별 상관분석 화면의 HTML/PDF/Excel 저장 이벤트는 `R/server_correlation.R`에서 각각 `write_correlation_results_html`, `write_correlation_results_pdf`, `save_correlation_excel_file`로 연결된다. 이 세 함수는 `saved_correlation_results_html`을 호출하여 저장용 HTML을 만든다. PDF는 report_mode 옵션을 사용한다. 따라서 이 세 직접 저장 경로에는 저장용 그림 생성 작업이 남아 있다.

반면 ‘결과 추가’는 `register_add_result_snapshot`에서 브라우저 화면 캡처를 요청하고 반환된 HTML을 `append_result_snapshot`에 전달한다. 결과 목록의 HTML/PDF/Word/HWPX/Excel writer는 entries에 포함된 HTML을 입력으로 받는다. 두 경로를 구분해야 한다.

## 재생성 차단 시험

앞선 상관분석 프로파일에서 검증한 2,000행 × 8변수 Pearson 결과의 HTML을 입력으로 사용했다. HTML에는 산점도 행렬 및 히트맵 PNG가 이미 포함돼 있다.

다음 7개 함수를 호출 기록 후 즉시 오류를 내는 테스트 함수로 바꾼 별도 R 프로세스에서 내보내기를 실행했다.

- `prepare_correlation_results`
- `draw_correlation_scatter_plot`
- `draw_correlation_heatmap`
- `saved_correlation_results_html`
- `plot_data_uri`
- `prepare_km_analysis_result`
- `saved_survival_results_html`

단일 보관 항목과 같은 항목을 한 번 더 추가한 두 항목 모음에서 HTML/PDF/Word/HWPX/Excel을 각각 생성했다. **10개 모두 성공했고 차단 함수 호출 기록은 0개였다.** 매 변환 후 원래 entries의 직렬화 바이트도 유지됐다. 테스트 함수 교체는 검증용 프로세스 안에서만 수행했으며 패키지 namespace나 운영 파일은 수정하지 않았다.

## 산출물 검증

| 항목 | 단일 스냅샷 | 두 스냅샷 |
|---|---:|---:|
| HTML의 비어 있지 않은 표 셀 | 90 | 180 |
| Word/HWPX에서 일치한 셀 내용·개수 | 90 | 180 |
| HTML/Word/HWPX 그림 수 | 2 | 4 |
| PDF 페이지 수 | 3 | 6 |
| PDF 그림 수 | 2 | 4 |

Word/HWPX 표 내용은 공백 정규화 후 셀의 다중집합으로 비교했다. 단일 PDF 2쪽의 산점도 행렬과 3쪽의 히트맵을 렌더링하여 그림·축·범례·변수 설명을 확인했다. Excel은 파일 생성 성공을 확인했으며 이번에 모든 셀을 별도 대조한 것은 아니다.

## 판단 및 범위

보관 결과의 형식 변환 경로는 이미 생성된 내용과 그림을 재사용한다. 여기에 추가 분석·그림 캐시를 만드는 것은 이번 검증에서 확인한 재생성 비용을 줄이지 못한다. 다음 후보는 **개별 상관분석 화면의 직접 HTML/PDF/Excel 저장 경로**다. 이 경로를 바꾸려면 언어·표시 설정·결과 변경 시의 갱신과 report_mode 차이를 보존해야 한다.

이번 시험은 이미 만들어진 HTML을 입력으로 하는 writer 경로를 검증했다. 실제 브라우저 캡처 및 버튼 클릭부터의 전체 UI 흐름은 실행하지 않았다. 파일명의 `current`는 단일 보관 스냅샷을 뜻하며 개별 분석 화면의 직접 저장 handler를 뜻하지 않는다. 이번 결과로 변환 속도 개선이나 첫 분석 시간 단축을 주장하지 않는다. 설치 파일도 재빌드하지 않았다.

## 증거

- `output/correlation-snapshot-reuse-20260914/exports.R`: 재생성 차단 및 entries 불변 검증
- `reuse-validation.txt`, `export-status.csv`, `exports.log`
- `inspect_exports.py`, `artifact-validation.json`: 표 내용·그림 수 및 PDF 확인
- `current.*`, `accumulated.*`: 각 5개 형식의 단일/두 스냅샷 결과
- 입력: `output/correlation-plot-profile-20260914/plain-1.rds`
- 경로 확인 소스: `R/server_correlation.R`, `R/result_export.R`, `R/result_saved_ui.R`, `R/result_export_files.R`
