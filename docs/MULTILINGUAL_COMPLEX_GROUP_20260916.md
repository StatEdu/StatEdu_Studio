# 복합표본 집단 비교 다국어 검증

## 수정

- 종속/집단 변수의 결측 행 제외, 완전사례 없음, 유효 집단 2개 미만 안내를 문장 템플릿으로 번역한 뒤 사용자 변수 이름과 건수를 삽입한다.
- 완성한 안내를 공통 번역기로 다시 바꾸지 않도록 진단 Details를 보존한다.
- 전체 분석이 제외될 때 결과 제목으로 쓰는 사용자 변수명을 그대로 표시한다. 변수명이 `Survey design`과 같은 시스템 문구여도 번역하지 않는다.
- 본표, 사후비교, 통계 계산은 변경하지 않았다. 번역 자료: `scripts/fill_complex_group_i18n.py`.

## 실제 검증

- `scripts/validate_complex_group_i18n.R`: 120행, 3개 층, 30개 PSU, 가중치. 두 집단 t 검정, 세 집단 ANOVA와 유의한 Holm 사후비교를 생성했다.
- 결측 종속값 2건, 단일 집단, 전부 결측인 종속변수를 포함했다. 정상/일부 제외/전체 제외 결과를 8개 언어에서 확인했다.
- 영문 본표·사후비교 셀·주석 일치. 결측 건수, 사용자 이름의 한글과 %, 시스템 문구와 동일한 사용자 제목 보존. 주요 번역 안내의 실제 내용 대조 통과.
- 기존 `validate_complex_sample_analysis.R` 및 `validate_complex_crosstab_i18n.R` 통과.
- 근거: `tmp/complex-group-i18n`, `tmp/complex-group-regression.log`.
- 일본어 실제 결과 스냅샷으로 현재·누적 HTML/PDF/Word/HWPX/Excel 저장과 내용 대조 통과. 실제 PDF 본문·표지 검사 통과. 저장 로그: `tmp/complex-group-exports.log`.

## 남은 범위

- 다른 사후보정, 추세검정, 평균/분산 표기 옵션, 복제 가중치/FPC/부모집단, 외부 계산 오류, 브라우저 언어 왕복·분할은 모두 검증한 것이 아니다.
- 설치본을 생성하지 않았다.
