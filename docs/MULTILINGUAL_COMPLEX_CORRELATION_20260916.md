# 복합표본 상관분석 다국어 검증

## 변경

- 분석 개요의 복합표본 상관분석, Spearman 순위상관, 표시 변수 쌍 수, Holm–Bonferroni 보정, 행렬 표시 상태, 원자료 N, 조사설계 N의 번역을 보완했다.
- 자료: `scripts/fill_complex_correlation_i18n.py`. 영어 본표 및 주석의 문구는 변경하지 않았다.

## 검증

- `scripts/validate_complex_correlation_i18n.R`: 실제 120행, 3개 층, 30개 PSU와 가중치. Pearson 및 Spearman, Holm 보정, 상관행렬 표시를 포함한다.
- 결측 2건, 순서형 변수와 상수 변수를 포함했다. 8개 언어에서 본표 영역 전체(셀·제목·주석) 일치, 사용자 라벨 보존, 보조표 언어 및 주요 번역 내용 대조 통과.
- 기존 `scripts/validate_complex_sample_analysis.R` 통과.
- 결과 근거: `tmp/complex-correlation-i18n`, `tmp/complex-correlation-regression.log`.
- 일본어 실제 결과 스냅샷으로 현재·누적 HTML/PDF/Word/HWPX/Excel 저장 및 내용 비교 통과. PDF 실제 본문·표지 검사 통과. 로그: `tmp/complex-correlation-exports.log`.

## 제한

- 행렬 미표시 문구도 사전에 추가했으나 이번 실제 결과는 행렬 표시 설정이다. 다른 p 보정, 큰 상관행렬, 설계 옵션, 전체 오류, 브라우저 언어 왕복/분할은 별도 검증이 필요하다.
- 상수 변수 등의 본표 Note 열은 영문 본표의 일부로 영어를 유지한다.
- 설치본을 만들지 않았다.
