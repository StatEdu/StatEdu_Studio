# 혼합 반복측정 정규성 별칭·예외 안내 번역

## 변경

이전 잔여 후보 점검에서 남긴 4개 문구를 8개 언어 사전에 반영했다. 영어·기존 한국어 표기를 유지했고 제품 계산·렌더링 코드는 변경하지 않았다.

- Shapiro-Wilk by group
- Lilliefors (K-S)
- Kolmogorov-Smirnov (Lilliefors)
- No matching time interaction was returned by the model.

## 검증 자료의 성격

`validate_mixed_rm_compatibility_i18n.R`는 실제 표준 분석을 한 번 수행한 후 정규성 방법 속성을 세 별칭으로 바꾼 **합성 검증 객체**를 만든다. 별도 객체에서는 ANOVA의 Group 행만 남기고 실제 권고 생성 함수에 전달하여 상호작용을 찾지 못하는 예외 안내를 생성한다. 네 객체를 RDS로 저장하고 다시 읽어 표시를 검사한다.

이 자료는 실제 과거 사용자 저장 파일이 아니다. Lilliefors 계산, 정규성 검정 선택 기능, 앱 이력 복원 전체 흐름 또는 정상 분석에서의 상호작용 누락을 검증했다는 의미가 아니다. 합성 산출물은 사용자 분석 보고서로 사용하면 안 된다.

## 검증

- 수정 전 6개 외국어의 사전 누락 재현.
- 수정 후 4개 합성 결과 × 8개 언어에서 방법명 및 예외 안내 표시 확인.
- 각 합성 객체의 영어 본표가 언어 전환으로 달라지지 않는지 및 전체 객체 보존 확인.
- 기존 실제 Levene 위반 패널과 검정 불가 진단 함수의 8개 언어 회귀 검사 통과.
- `validate_i18n_contract.R`와 변경 공백 검사 통과.
- 한국어·일본어 현재/누적 HTML/PDF/DOCX/HWPX/XLSX 내용 및 PDF 텍스트 보존 검사 통과. 두 언어 모두 현재 PDF 11쪽·누적 21쪽이다.
- 저장 검증은 `validate_longitudinal_count_exports.R`와 `validate_longitudinal_error_pdf.py`를 재사용했다. 공용 표지 제목은 Longitudinal이고 본문은 위 합성 검증 캡처다. 저장 단계에서 분석을 재실행하지 않았다. 산출물: `tmp/mixed-rm-compatibility-i18n/exports`.
- 정적 재검사: 전체 333개 파일·4,236개 출현·고유 누락 후보 524개. 이 파일에 남은 2개 후보는 이전 실제/함수 검사로 정상 번역을 확인한 Levene 문구다. 원시 후보 수는 미번역 수나 전체 완료율이 아니다.

기존 매개·조절효과 화면은 제외했다. 설치본 빌드 및 문서 앱별 수동 시각 검토는 수행하지 않았다.
