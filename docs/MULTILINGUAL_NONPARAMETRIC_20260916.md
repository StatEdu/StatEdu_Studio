# 비모수 독립·대응표본 대표 결과 다국어 점검

## 변경

- 보조표의 사후분석 포함, 쌍별 Wilcoxon 순위합 검정의 Holm–Bonferroni/Bonferroni 설명, 반복측정 검정의 측정수준 설명, 짧은 Wilcoxon/McNemar/Friedman 표기에 번역을 연결했다.
- 여러 측정수준을 포함한 반복측정 설명은 측정수준별 번역과 문장 템플릿을 조합한다. 본표와 모델 계산을 변경하지 않는다.
- 번역 생성 자료: `scripts/fill_nonparametric_i18n.py`. 영어는 원문을 유지하며 한국어·일본어·중국어·스페인어·프랑스어·독일어·베트남어 사전을 보완했다.

## 실제 검증

- `scripts/validate_nonparametric_i18n.R`: 72행 자료의 Mann–Whitney, Kruskal–Wallis, Wilcoxon, McNemar, Friedman, Cochran Q 및 Holm 사후비교를 포함한 독립·대응 결과를 생성했다.
- 8개 언어에서 본표 셀·제목·주석 일치. 사용자 변수/집단/시점 이름을 제외한 본표에 한글이 남지 않는지 확인했다.
- 보조표의 언어 속성뿐 아니라 추가한 주요 문구의 실제 번역 결과를 검사했다. 혼합 측정수준 설명도 확인했다.
- `validate_compact_nonparametric.R` 통과: 중앙값 기본값, 결합 셀, 추가 M ± SD 표와 계산값 보존.
- 일본어 화면 HTML을 원본으로 현재·누적 HTML/PDF/Word/HWPX/Excel 생성 및 내용 대조 통과. 실제 PDF의 본문·표지 검사 통과.
- 결과 폴더: `tmp/nonparametric-i18n`. 로그: `tmp/nonparametric-i18n-final.log`, `tmp/nonparametric-i18n-exports.log`.

## 남은 범위

- Bonferroni 설명은 사전을 추가했으나 이번 실제 분석은 Holm 설정이다. 순서 추세검정, 결측·상수·제외 경고, 다른 옵션, 실제 브라우저 언어 왕복/분할을 모두 검증한 것은 아니다.
- 대표 결과 검증 근거가 있는 화면은 21개, 부분 근거 7개, 전용 결과 검증 공백 9개다. 이 수치는 전체 완료율이 아니다. 모든 분기 완료로 판정한 화면은 여전히 0개다.
- 설치본을 생성하지 않았다.
