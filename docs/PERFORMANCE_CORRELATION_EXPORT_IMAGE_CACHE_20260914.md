# 개별 상관분석 저장의 PNG 재사용

2026-09-14. 개별 상관분석 화면에서 HTML/PDF/Excel을 연속 저장할 때 두 PNG를 세션 안에서 재사용하도록 적용했다. 표와 문서 HTML은 매번 다시 생성한다.

## 동작 및 범위

- `R/result_correlation_ui.R`: `correlation_export_image_cache` 추가. 결과, 그림 함수, 크기, 해상도 및 그래픽 컨텍스트가 `identical(..., num.eq=FALSE)`로 같을 때만 PNG data URI를 재사용한다.
- 컨텍스트에는 출력 dpi, R options, locale, 작업 디렉터리, Windows 그래픽 옵션 및 폰트 매핑이 포함된다. CSS와 report_mode는 이미지 밖의 HTML을 매번 생성하여 반영한다.
- 경고·메시지가 발생하거나 RNG 상태가 변한 렌더링은 보관하지 않는다. 렌더 오류를 재시도하거나 정상 값으로 대체하지 않는다.
- 최대 2개 항목을 FIFO로 보관하며, entries의 `object.size` 추정 합계가 16 MiB를 넘으면 오래된 항목을 제거한다. 단일 항목이 한도를 넘으면 저장하지 않는다.
- `R/server_correlation.R`: 세션별 캐시를 생성한다. 새 분석 성공, 분석 선택 초기화 및 세션 종료 시 비운다. 개별 HTML/PDF/Excel 저장 handler에만 캐시 renderer를 전달한다.
- `R/result_saved_ui.R`, `R/result_export.R`: 선택적인 `plot_renderer` 인수를 추가했다. 기존 인수 및 기본 비캐시 호출 방식은 유지한다. 보관된 결과 목록의 기존 HTML 재사용 경로는 변경하지 않는다.

## 성능

번들 R 4.5.3/JIT 3, 모듈 표현식 캐시, 패키지 설치 검사 생략. seed 714의 2,000행 × 8개 연속변수 Pearson 결과에 산점도 행렬과 히트맵을 포함했다. 두 PNG는 각각 600 dpi, 5625×5625픽셀이다. 분석 준비는 측정 밖이다.

실험 후보 검토 후, 실제 적용 함수의 캐시 renderer와 기본 비캐시 renderer를 각각 새 R 프로세스 3개에서 순차 실행했다. 두 번째 세트는 순서를 뒤집었다. 각 프로세스에서 같은 결과를 두 번 저장했다.

| 적용 코드의 3회 중앙값 | 비캐시(초) | 캐시(초) |
|---|---:|---:|
| 첫 저장용 HTML 생성 | **2.45** | **2.45** |
| 반복 저장용 HTML 생성 | **1.86** | **0.14** |

반복 HTML 생성은 **1.72초, 약 92.5%** 단축됐다. 첫 저장은 기존처럼 PNG를 생성하므로 속도 개선을 주장하지 않는다. 이 비율은 PDF 변환이나 Excel 파일 작성까지 포함한 전체 저장 시간의 개선율이 아니다.

이번 입력의 두 캐시 항목은 `object.size` 기준 **2,681,400바이트(약 2.68MB)**였다. 이것은 보관 객체의 추정 크기이며 프로세스 최대 메모리나 실제 RSS 증가량 측정이 아니다. 16 MiB 제한도 보관 entries의 추정 크기에 대한 제한으로, PNG 생성 중 임시 메모리까지 제한하지 않는다.

## 보존 및 무효화 검증

- 실제 적용 코드의 3쌍에서 전체 HTML(두 PNG 포함), 경고·메시지, 표준 출력, RNG 상태가 비캐시 출력과 정확히 같았다. 각 프로세스의 첫/반복 HTML도 같았으며 원래 분석 결과의 직렬화 바이트가 유지됐다.
- 기본/반복/report_mode/CSS/OutDec/라벨/300 dpi/600 dpi의 8개 경우에서 캐시 출력과 비캐시 출력이 같았다. 한국어·영어 정규성 부록이 실제로 바뀌는 별도 2개 경우도 같은 방식으로 검증했다.
- 경계 시험: 같은 키의 적중, 결과·함수·크기·res·컨텍스트 변경 시 재생성, FIFO 2개 한도, 명시적 clear, 캐시 인스턴스 격리, 크기 초과 시 우회, 경고·메시지·RNG·오류 시 비보관 및 무재시도를 확인했다.
- 실제 `register_correlation_handlers`를 MockShinySession에서 실행했다. HTML 반복 저장 후 PDF 입력 HTML·Excel 저장까지 PNG 렌더 횟수는 2회였고, 새 분석 후 저장에서는 4회가 됐다. 초기화와 세션 종료 후 entries는 비었다.
- 위 Mock 시험에서는 PDF 변환기만 대체하여 입력 HTML 전달을 확인했다. 별도 실제 직접 저장 시험에서 `write_correlation_results_html`, `write_correlation_results_pdf`, `save_correlation_excel_file`로 파일 3개를 생성했고 PNG 렌더 횟수는 역시 총 2회였다.
- 현재/누적 HTML은 바이트 단위로, Excel은 시트 값 및 압축 내부 파일 단위로 비캐시 결과와 같았다. Excel 생성·수정 시각만 정규화했다.

## 전체 형식 산출물

캐시로 만든 현재 결과와 이를 한 번 더 추가한 누적 결과에서 HTML/PDF/Word/HWPX/Excel 10개를 생성했다. 위 실제 직접 HTML/PDF/Excel 3개도 별도로 생성했다.

| 항목 | 현재 | 누적 |
|---|---:|---:|
| HTML 비어 있지 않은 표 셀 | 90 | 180 |
| Word/HWPX에서 일치한 셀 내용·개수 | 90 | 180 |
| HTML/Word/HWPX 그림 수 | 2 | 4 |
| PDF 페이지 수 | 3 | 6 |
| PDF 그림 수 | 2 | 4 |

Word/HWPX 셀 내용은 공백 정규화 후 다중집합으로 비교했다. 실제 개별 저장 PDF는 3쪽이며 그림 2개를 포함했다. 2쪽 산점도와 3쪽 히트맵을 렌더링하여 패널·축·범례·변수 설명을 확인했다. 모든 문서의 바이너리 동일성을 주장하지 않는다. 설치 파일은 재빌드하지 않았다.

## 재현 자료

- `scripts/validate_correlation_export_image_cache.R`: 캐시 경계 검증
- `output/correlation-image-cache-review-20260914/integration/measure.R`, `integration/compare.R`, `integration/times.csv`, `integration/*-retained.csv`
- `presentation.R`, `presentation-checks.csv`, `language.R`: 설정·내용 보존
- `server.R`, `server.log`: 실제 서버 handler의 적중 및 생명주기
- `exports.R`, `direct_exports.R`, `export-status.csv`, `artifact-validation.json`: 파일 변환 검증
- 최종 SHA256:
  - `R/result_correlation_ui.R`: `A4DFE5B395285BFA110A216A9CDF09E5F0D0EEDF8F5D58CA1B3E8C24E75A724E`
  - `R/result_saved_ui.R`: `AA6E2744ADD6C5874459E546959AEF245C29D789E4166CD38AA9E00D6F21D004`
  - `R/result_export.R`: `FBDA582D2E32E2B646604AA322A1937378EF7789D032BA1EDAE3DDC53176A470`
  - `R/server_correlation.R`: `6D11B13A95F6B18054A50E5FE271F8C2AA5A07A447B534ECDFCC815EA1843C2F`
