# t 검정/분산분석 자동 비모수 및 경고 다국어 보완

## 변경

최소 집단·사례 수, 상수 종속변수, 유효 결과 없음, 집단별 관측값 부족, 집단별 표준편차 0 안내의 다섯 문구/템플릿을 8개 언어에 등록했다. 집단명이 들어가는 안내는 원문에서 집단명을 추출하여 언어별 문장에 그대로 넣는다.

검사 중 `Normality met 50%`라는 집단명이 공통 진단표에서 다시 번역되는 현상을 재현했다. `analysis_diagnostics_section`에 기본값이 FALSE인 `messages_localized`를 추가하고, t/ANOVA가 이미 번역한 메시지를 전달할 때만 TRUE로 지정한다. 이 경로에서는 메시지 열을 재번역하지 않는다. 다른 분석의 기본 처리 방식은 유지한다.

통계 계산과 본표 구성은 변경하지 않았다.

## 검증

`scripts/validate_ttest_guards_i18n.R`:

- 실제 자료로 단일 집단, 상수 종속변수, 관측값 1개인 집단, 표준편차 0인 집단의 네 경고/제외 경로를 재현했다.
- 극단적으로 비대칭인 세 집단 자료에서 자동 Kruskal–Wallis 선택 및 정규성 미충족 안내를 확인했다.
- 8개 언어의 정확한 안내 문장, 집단명 `Normality met 50%` 보존, 유효 결과 없음 안내, 본표 영어 내용 동일성 검사 통과.
- 기존 `validate_ttest_paths_i18n.R`, `validate_ttest_anova.R`, `validate_i18n_contract.R` 통과.

일본어 다섯 경로 전체를 한 항목으로 묶은 `tmp/ttest-guards-i18n/entries.rds`로 현재·누적 HTML/Word/HWPX/Excel 내용 비교와 PDF 생성을 통과했다. `validate_multilingual_pdf.py`의 실제 PDF 텍스트 검사도 현재·누적 모두 통과했다. 로그: `tmp/ttest-guards-exports.log`.

## 한계

집단별 정규성 검사, 모든 사후검정, Welch 등 계산 실패의 나머지 원문과 외부 라이브러리 예외는 추가 확인 대상이다. 모든 분석 화면의 완료를 의미하지 않는다. 전용 결과 검증 공백은 17개를 유지한다. 설치본은 만들지 않았다.
