# 비례오즈 평가 불가 안내 번역

## 변경

`Proportional odds not assessable` 및 오류 상세를 포함한 명목효과 검정 불가 설명을 8개 언어 사전에 반영했다. `logistic_appendix_text`에서 전체 생성 문장 형식을 인식하고 상세 사유만 원문 그대로 삽입하도록 기존 패턴 처리에 추가했다. 통계 계산 및 실패 시 모형 유지 규칙은 변경하지 않았다.

## 검증

`validate_logistic_unavailable_odds_i18n.R`는 180명·3범주 순서형 자료에서 명목효과 검정 함수만 오류를 발생시키도록 일시 대체한다. 분석 호출 후 원래 함수를 복구한다. 누적 로짓 모형은 실제 적합하고 비례오즈 p=NA, available=FALSE, 순서형 모형 유지 및 원문 안내 생성을 확인한다.

이는 특정 실제 데이터나 라이브러리 장애를 재현한 것이 아니라 **통제된 오류 주입 검사**다.

- 보완 전 6개 외국어 번역 누락 재현, 보완 후 8개 언어 요약·상세 설명 표시 검사 통과.
- 오류 상세 `검증 (Review) 50% <&>`가 번역·HTML 표시 후에도 그대로 유지되는지 확인.
- 영어 본표의 언어 간 동일성, 전체 결과 객체, 안내문과 같은 사용자 변수명 보존 확인.
- 기존 순서형·다항 로지스틱의 비례오즈·성능·참조범주·VIF 회귀 검사 및 `validate_i18n_contract.R`, 변경 공백 검사 통과.
- 한국어·일본어 현재/누적 HTML/PDF/DOCX/HWPX/XLSX 내용과 PDF 텍스트 보존 검사 통과. 두 언어 모두 현재 4쪽·누적 7쪽이다.
- 저장 검증은 `validate_longitudinal_count_exports.R`와 `validate_longitudinal_error_pdf.py`를 재사용했다. 공용 표지 제목은 Longitudinal이고 본문은 이번 오류 주입 검증의 로지스틱 결과 캡처다. 저장 시 분석을 재실행하지 않았다. 산출물: `tmp/logistic-unavailable-odds-i18n/exports`.

기존 매개·조절효과 화면은 제외했다. 설치본 빌드 및 문서 앱별 수동 시각 검토는 수행하지 않았다.
