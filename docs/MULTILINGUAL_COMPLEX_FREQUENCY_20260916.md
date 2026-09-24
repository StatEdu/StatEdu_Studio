# 복합표본 빈도·기술통계 다국어 검증

## 수정

- 조사설계 행 수/최종 N, Taylor 선형화, 단일 PSU 처리 설명에 언어별 문장 템플릿을 연결했다. 영어 본표용 설계 주석은 영어를 유지한다.
- 비결측값이 없는 변수 안내는 번역 후 변수 이름을 삽입한다. 사용자 이름에 있는 한글·공백·%를 유지한다.
- 결측 N 설명, Survey design, Skipped analyses, Details 사전을 보완했다. 번역 원본은 `scripts/fill_complex_frequency_i18n.py`다.

## 검증

- `scripts/validate_complex_frequency_i18n.R`: 80행, 4개 층, 20개 PSU, 가중치, 연속형·범주형 변수, 일부 결측 및 전부 결측인 변수를 포함한 실제 분석.
- 8개 언어에서 본표 셀 동일, 보조표 언어 및 주요 번역문 확인. 표본 수 80과 사용자 제외 변수 이름 `사용자 제외 50%` 보존.
- `scripts/validate_complex_sample_analysis.R` 통과: 설정 입력, 빈도 제외 변수, 교차표/집단 비교의 독립 조합, 상관/부모집단 기본값, 회귀·로지스틱의 독립 결과.
- 일본어 결과 스냅샷으로 현재·누적 HTML/PDF/Word/HWPX/Excel 생성 및 내용 대조 통과. PDF 실제 본문·표지 검사 통과.
- 근거: `tmp/complex-frequency-i18n`, `tmp/complex-frequency-regression.log`, `tmp/complex-frequency-exports.log`.

## 남은 범위

- 복제 가중치, FPC, 부모집단 제외, 유효하지 않은 설계 행 등의 상세 설명과 옵션/오류는 아직 전부 다국어 검증하지 않았다.
- 이번 대표 결과는 빈도·기술통계 화면이다. 다른 복합표본 화면의 전용 다국어 검증을 대체하지 않는다. 실제 브라우저 왕복도 별도다.
- 대표 결과 근거 22개, 부분 근거 7개, 전용 결과 검증 공백 8개. 모든 분기 완료 판정은 0개이며 전체 완료율을 의미하지 않는다.
- 설치본을 만들지 않았다.
