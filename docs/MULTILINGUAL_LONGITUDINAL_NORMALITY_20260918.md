# 종단분석 잔차 정규성 설명·상태 번역

2026-09-18

## 확인 및 변경

정적 누락 후보를 실제 `longitudinal_check_normality()` 출력으로 확인했다. 비정규 분포, 잔차 3개 미만, 상수 잔차로 인한 Shapiro-Wilk 계산 실패, 정규성 위반, 정규성 위반 근거 없음의 다섯 경로에서 설명·권고 8개와 상태 2개가 추가 6개 언어에서 영어로 남았다.

`scripts/fill_longitudinal_normality_i18n.py`로 10개 원문에 대응하는 `analysis.ui.*` 사전 항목을 8개 언어에 추가했다. 기존 한국어 화면 경로와 영어 본표 원칙을 유지한다. 분석 계산이나 결과 표 구성 코드는 변경하지 않았다.

## 검증

- `validate_longitudinal_normality_i18n.R`: 수정 전 실제 설명 미번역을 재현했다. 최종 검사에서는 다섯 경로 × 8개 언어의 Check/Result/Interpretation/Recommendation 번역, Statistic/p/Issue 보존, 직렬화한 원본 결과 불변, 동일 문구를 가진 사용자 변수명 보존을 확인했다. 모두 통과했다.
- `validate_longitudinal_count_exports.R tmp/longitudinal-normality-i18n/tables.rds tmp/longitudinal-normality-i18n/exports`: 이미 계산한 다섯 진단 표를 재사용했다. 한국어/일본어 × 현재/누적 결과를 HTML, PDF, Word, HWPX, Excel로 생성했다. HTML/Word/HWPX/Excel의 모든 예상 표 문구가 보존됐고 8언어의 영어 본표가 동일했다.
- `validate_longitudinal_error_pdf.py tmp/longitudinal-normality-i18n/exports`: 네 PDF의 모든 예상 표 문구가 추출됐다. 현재 결과는 표지 포함 7쪽, 두 항목을 누적한 결과는 13쪽이다.
- `validate_i18n_contract.R`: Windows UTF-8 로캘(`LANG=LC_ALL=Korean_Korea.utf8`)에서 통과했다. 기본 실행의 로캘 오류는 UTF-8 로캘 지정으로 해결했으며 제품 결함으로 분류하지 않는다.
- 수정한 언어 사전의 `git diff --check` 통과.

출력 자료: `tmp/longitudinal-normality-i18n/exports`. PDF 생성은 실행 환경의 Chrome 프로세스 제한 때문에 최초 실패했으나 허용된 실행으로 재검증하여 통과했다. 초기 저장 검사 도중 상태 문구 2개의 누락을 추가 확인하여 검사를 중지하고 보완한 뒤 전체 저장 검사를 다시 통과했다.

이번 범위는 정규성 진단 문구와 저장 내용의 검증이다. 새 원어민 검수나 Word/Hancom의 시각적 페이지 검사는 아니다. 다른 종단 진단(과산포·이분산·확률효과 분포 등)의 미번역 후보와 전체 앱 점검은 남아 있다. 설치본을 생성하거나 설치 앱을 변경하지 않았다.
