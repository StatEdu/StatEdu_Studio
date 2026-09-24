# 계수형 GLMM의 ITT 사용 안내 번역

## 변경

혼합 반복측정 ITT 결과의 다음 두 안내를 8개 언어 사전과 한국어 전용 변환에 반영했다.

- Use the fitted count GLMM as the ITT mixed-model result.
- Use the fitted count GLMM as the ITT result.

전자는 완전 사례 ANOVA가 함께 산출되는 경우, 후자는 완전 사례가 없는 경우에 사용된다. 한국어 안내 안에 남던 count 표현도 계수형으로 번역했다. 통계 계산 및 모형 선택은 변경하지 않았다.

## 검증

`validate_mixed_rm_count_itt_i18n.R`는 60명·2집단·3시점 계수 자료로 두 경우를 검사한다. 완전 자료와 각 대상자의 한 시점 값이 결측인 자료를 각각 ITT 분석 진입점에 전달하여 count GLMM을 실제 적합했다.

- 두 결과 모두 혼합모형 계수 및 exp(B) 열이 생성되는지 확인했다.
- 첫 결과에는 완전 사례 ANOVA가 있고 두 번째에는 없는지 확인했다.
- 보완 전 한국어 및 6개 외국어 사전 검사 실패를 재현했고, 보완 후 2개 결과 × 8개 언어의 실제 안내 표시 검사 통과.
- 영어 본표의 언어 간 동일성, 전체 결과 객체, 안내문과 같은 철자의 사용자 변수명 보존 확인.
- 기존 순서형 ITT 안내 회귀 검사, `validate_i18n_contract.R`, 변경 공백 검사 통과.
- 한국어·일본어 현재/누적 HTML/PDF/DOCX/HWPX/XLSX 내용 및 PDF 텍스트 보존 검사 통과. 두 언어 모두 현재 PDF 11쪽·누적 20쪽이다.
- 저장 검증은 `validate_longitudinal_count_exports.R`와 `validate_longitudinal_error_pdf.py`를 재사용했다. 공용 표지 제목은 Longitudinal이고 본문은 이번 계수형 GLMM 결과 캡처다. 저장 단계에서 분석을 재실행하지 않았다. 산출물: `tmp/mixed-rm-count-itt-i18n/exports`.

기존 매개·조절효과 화면은 제외했다. Gamma GLMM 등 다른 모형 안내 전체의 완료를 의미하지 않는다. 설치본 빌드 및 문서 앱별 수동 시각 검토는 수행하지 않았다.
