# 생존분석 Cox 부록 제목 번역

## 변경

실제 Cox 결과 패널에서 비영어 UI에 영어로 남는 다음 제목 5개를 재현하고 8개 언어 사전에 반영했다.

- Categorical reference levels and contrast coding
- Stratum event counts
- Robust-variance cluster summary
- Supplementary statistics and diagnostics
- Marginal adjusted survival

한국어 기존 표현과 영어 본표 규칙을 유지했다. 계산·표 생성·저장 코드는 변경하지 않았다.

## 검증

- `validate_survival_cox_section_titles_i18n.R`: 기존 생존분석 자료로 층화·군집 강건 Cox와 보정 생존곡선을 포함한 Cox를 실제 적합했다. sex는 범주형, 사용자 층 이름은 Review/Normality로 설정했다. 두 전체 결과 패널에서 5개 제목의 실제 표시를 확인하고 8개 언어의 영어 본표 동일성과 원본 결과 보존을 검증했다.
- 보정 생존은 부트스트랩 0회로 실행했다. 이 검사는 제목 표시 경로 확인이며 부트스트랩 신뢰구간 검증은 아니다.
- `validate_i18n_contract.R`: 통과.
- `validate_longitudinal_count_exports.R`에 생존분석 제목·대응 표의 캡처 HTML을 전달해 한국어·일본어 현재·누적 HTML/PDF/DOCX/HWPX/XLSX 내용 보존 검사 통과. 저장 중 분석을 다시 실행하지 않았다.
- `validate_longitudinal_error_pdf.py`: 한국어·일본어 현재 각 4쪽, 누적 각 6쪽에서 모든 캡처 텍스트 보존 확인.
- 산출물: `tmp/survival-cox-section-titles-i18n/exports`.

## 범위

저장 검사는 수정한 제목 5개와 대응 표에 한정하며 전체 결과의 그림·주변 주석까지 이번에 다시 검증한 것은 아니다. 다른 생존분석 안내와 동적 오류는 계속 확인해야 한다. 문서 앱별 수동 시각 검토와 설치본 빌드는 수행하지 않았다. 기존 매개·조절효과 패널 제외 원칙은 유지한다.
