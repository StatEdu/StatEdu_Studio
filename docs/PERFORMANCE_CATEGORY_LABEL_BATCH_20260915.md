# 범주 라벨 저장값 일괄 적용

2026-09-15. `R/data_category_labels.R`의 `category_label_display_data()`에서 일반 문자형 라벨을 셀별 대입 대신 열별 대입으로 적용했다. 저장 이름의 기존 match 결과를 재사용하므로 첫 번째 중복 이름을 선택하는 동작과 결측 이름 매칭을 유지한다.

일반 data.frame이며 적용 대상의 모든 원본/저장 열이 속성 없는 character 벡터일 때만 일괄 처리한다. factor, list, matrix, 속성이 있는 열 및 특수 데이터프레임은 기존 행 우선 반복문을 사용하여 변환과 경고 순서를 유지한다. 분석 수식과 데이터 범위, 선택 및 출력 열 순서는 변경하지 않았다.

## 검증

- 변경 직전 함수: `scripts/fixtures/category_label_batch_reference.R`.
- `scripts/validate_category_label_batch.R`: 180개 조건의 반환값 전체(속성 포함), 경고/메시지/오류, stdout, RNG를 `identical(..., num.eq=FALSE)`로 비교하여 통과. 0/1/12/100행, 라벨 쌍 1/3/11개, 중복·결측·불일치 이름, 빈 저장 표, factor/numeric 이름, named/속성/factor/list/matrix 열, 전체 라벨의 한글·특수문자·NA·빈 문자열 포함.
- 한글/영문 고정 ID DT HTML 2개 비교 통과.
- 기존 `validate_category_label_lookup.R`: 84개 및 HTML 2개 통과.
- `validate_data_io.R` 통과. 테스트 데이터 생성 중 기존 haven `write_sas()` 사용 중단 예정 경고가 출력됨.
- 최종 주석/블록 정리 후 180개와 HTML 2개 재검증 통과.

## 측정

`scripts/benchmark_category_label_batch.R`로 독립 R 프로세스 3회, 조건별 3회 측정 중앙값. 양쪽 함수를 동일하게 컴파일하고 준비 호출 후 측정 순서를 교대했다. `Sys.time()`으로 전체 함수와 진단 수집 시간을 측정하며 각 측정 반환값도 기준과 정확히 비교했다. 입력 변수의 3/4가 범주형이므로 2,000변수 입력의 반환 표는 1,500행이다.

| 입력 변수 / 저장 열 | 실행 | 기존 초 | 변경 초 |
|---|---:|---:|---:|
| 2,000 / 전체 25열 | 1 | 0.417195 | 0.004336 |
| 2,000 / 전체 25열 | 2 | 0.410779 | 0.004317 |
| 2,000 / 전체 25열 | 3 | 0.411215 | 0.004413 |
| 2,000 / reference만 | 1 | 0.018318 | 0.003717 |
| 2,000 / reference만 | 2 | 0.017634 | 0.003655 |
| 2,000 / reference만 | 3 | 0.017988 | 0.003714 |

전체 열 조건은 약 99%, reference만 있는 조건은 약 79~80% 단축. 500변수 조건도 각 CSV에 기록했다. 이 측정은 표 데이터 준비 구간이며 DT 직렬화, 브라우저 표시, 편집 후 서버 저장, 전체 시작 시간, 분석 계산 또는 내보내기 성능을 뜻하지 않는다. 이번 변경에서 실제 브라우저 저장 및 PDF/Word/HWPX 검사를 수행하지 않았다.

자료: `output/category-label-batch-20260915/times-1.csv`, `times-2.csv`, `times-3.csv`.

최종 제품 SHA256: `0DD4D9DF41541630FC6C752A5F4A3D0658CAF2DB7042606F7FF9D4767FB9AAC5`.
