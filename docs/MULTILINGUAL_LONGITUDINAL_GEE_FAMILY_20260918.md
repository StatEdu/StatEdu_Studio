# GEE 분포·연결함수 안내 번역

## 변경 범위

- 기존 `fill_longitudinal_family_guidance_i18n.py`에 GEE 경로의 설명 6개를 추가해 8개 언어 사전에 반영했다.
- Gaussian, binomial, Poisson, Gamma 및 계수형 안내와 음이항 경로의 주변 음이항 GLM 안내를 번역했다.
- 음이항 경로의 기존 명칭인 대상자 군집 강건 표준오차를 사용하는 주변 음이항 GLM을 유지했다. 이를 geepack의 음이항 GEE라고 바꾸지 않았다.
- 모형 적합·저장 코드는 변경하지 않았다. 소스 확인 결과 분포·연결함수 가정 검사는 GEE/GLMM 경로에서 사용되므로 LMM·패널 회귀의 사용되지 않는 조합은 추가하지 않았다.

## 검증 결과

- `scripts/validate_longitudinal_gee_family_i18n.R`: 고정 난수 자료 250행/50명을 사용해 Gaussian·binomial·Poisson·Gamma의 GEE와 음이항 GLM을 실제 적합하고, 가정 검사에서 생성된 분포 설명을 검사했다. 미해결 count 표기는 별도의 안내 생성 함수 분기로 확인했으며 별도 모형을 적합한 것으로 집계하지 않았다.
- 수정 전 미번역 문구 6개를 재현했고 수정 후 8개 언어 검증을 통과했다. 수치·논리형 값, 사용자 변수명과 원본 결과 보존을 확인했다.
- 함께 실행한 기존 GLMM·지수화 안내 표 10종의 8개 언어 검사도 통과했다.
- `scripts/validate_i18n_contract.R`, `scripts/validate_longitudinal_result_table_contract.R`: 통과.
- `scripts/validate_longitudinal_count_exports.R`: 한국어·일본어 현재·누적 결과의 HTML, PDF, DOCX, HWPX, XLSX 내용 보존 검사 통과. 저장된 표를 재사용해 저장 중 분석을 다시 실행하지 않았으며 8개 언어에서 영어 본표 보존을 확인했다.
- `scripts/validate_longitudinal_error_pdf.py`: 한국어·일본어 PDF의 전체 캡처 텍스트 보존 검사 통과. 각각 현재 8쪽, 누적 15쪽.
- 산출물: `tmp/longitudinal-gee-family-i18n/exports`.

## 한계와 후속 범위

이번 변경은 GEE 경로의 분포 안내에 한정한다. 전체 다국어 번역 완료를 의미하지 않는다. 문서 앱별 수동 시각 검토와 설치파일 빌드는 수행하지 않았다.
