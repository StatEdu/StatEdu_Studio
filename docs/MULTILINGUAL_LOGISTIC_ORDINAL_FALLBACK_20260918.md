# 비례오즈 미충족·다항 모형 전환 안내 번역

## 변경

순서형 결과를 분석할 때 비례오즈 가정이 충족되지 않아 다항 로지스틱 회귀로 전환되는 결과의 세 문구를 8개 언어 사전에 반영했다.

- Proportional odds not met
- The proportional odds assumption was not met in the nominal-effects likelihood-ratio test; multinomial logistic regression was fitted instead.
- Accuracy (apparent)

통계 계산·판정 기준·모형 전환 코드는 변경하지 않았다.

## 검증

`validate_logistic_ordinal_fallback_i18n.R`는 연속형 예측변수에 따라 중간 결과 범주의 확률이 달라지는 600명·3범주 순서형 자료를 생성한다. 실제 비례오즈 검정 p < .05, ordinal_fallback=TRUE 및 다항 로지스틱 적합을 확인한다. 결과 속성을 인위적으로 바꿔 전환을 모사하지 않았다.

- 보완 전 6개 외국어 번역 누락 재현, 보완 후 8개 언어 실제 판정·사유·성능 지표 표시 검사 통과.
- 영어 본표의 언어 간 동일성, 전체 결과 객체와 안내문과 같은 사용자 변수명 보존 확인.
- 기존 순서형·다항 분석의 `validate_logistic_odds_i18n.R`, `validate_i18n_contract.R`, 변경 공백 검사 통과.
- 한국어·일본어 현재/누적 HTML/PDF/DOCX/HWPX/XLSX 내용 및 PDF 텍스트 보존 검사 통과. 두 언어 모두 현재 4쪽·누적 7쪽이다.
- 저장 검증은 `validate_longitudinal_count_exports.R`와 `validate_longitudinal_error_pdf.py`를 재사용했다. 공용 표지 제목은 Longitudinal이고 본문은 실제 로지스틱 모형 전환 결과 캡처다. 저장 시 분석을 재실행하지 않았다. 산출물: `tmp/logistic-ordinal-fallback-i18n/exports`.

기존 매개·조절효과 화면은 제외했다. 설치본 빌드 및 문서 앱별 수동 시각 검토는 수행하지 않았다.
