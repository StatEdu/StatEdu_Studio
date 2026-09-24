# 종단분석 GLMM 분포·연결함수 안내 번역

## 변경 범위

- 영어·한국어·일본어·중국어·스페인어·프랑스어·독일어·베트남어 사전에 8개 문구를 추가했다.
- GLMM의 Gaussian, binomial, count, Poisson, negative binomial, Gamma 분포·연결함수 설명 6개를 번역했다.
- 결과변수 척도와 선택 분포의 일치 여부, Poisson 산포 임계값 선별 및 보조 AIC/BIC 진단 안내를 번역했다.
- 지수화 계수의 OR·발생률비·평균비 해석 안내를 번역했다.
- 모형 식별자와 통계 약어는 보존했다. 추정 알고리즘과 결과 저장 구현은 변경하지 않았다.

## 검증

- `scripts/validate_longitudinal_family_guidance_i18n.R`: 실제 안내 생성 함수에서 만든 표 10종(분포 확인 6종, 지수화 안내 4종)을 8개 언어로 검증했다. 비영어 출력의 미번역 문구가 수정 전 재현되었고 수정 후 검증을 통과했다.
- 수치·논리형 열, 사용자 변수명과 원본 표의 직렬화 내용이 유지되는지 확인했다. 이 검사는 안내 생성·표시 경로 검사이며 새로운 모형 추정 정확도 검사는 아니다.
- `scripts/validate_i18n_contract.R`: 통과.
- `scripts/validate_longitudinal_result_table_contract.R`: 통과.
- `scripts/validate_longitudinal_count_exports.R`: 저장된 표를 사용해 한국어·일본어의 현재·누적 결과 각각을 HTML, PDF, DOCX, HWPX, XLSX로 저장하고 내용 보존을 확인했다. 저장 중 분석을 다시 실행하지 않았다. 8개 언어에서 영어 본표 보존도 확인했다.
- `scripts/validate_longitudinal_error_pdf.py`: 한국어·일본어 PDF의 모든 캡처 텍스트 확인 통과. 현재 결과는 각각 8쪽, 누적 결과는 각각 15쪽이다.
- 산출물: `tmp/longitudinal-family-guidance-i18n/exports`.

## 범위와 후속 작업

이번 변경은 GLMM의 분포·연결함수 설명과 두 공통 안내를 대상으로 한다. 다른 모형의 조합 문구, 남은 UI 번역 후보 및 원고·소프트웨어 안내는 추가로 실행 경로를 확인해야 한다. 전체 다국어 번역 완료를 의미하지 않는다. 문서 앱별 수동 시각 검토와 설치파일 빌드는 수행하지 않았다.
