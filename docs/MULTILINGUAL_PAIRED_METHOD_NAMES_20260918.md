# 대응표본 t·정확 McNemar 검정명 번역

## 변경

실제 분석 개요에서 `paired t`, `Exact McNemar`가 비한국어 언어에 영어로 남는 것을 확인했다. 긴 표기 `Paired t-test`, `Exact McNemar test`를 포함한 4개 문구를 8개 언어 사전에 반영했다. 기존 한국어·영어 표현을 유지했다. 계산·렌더링·저장 코드는 변경하지 않았다.

## 검증

- `validate_paired_method_names_i18n.R`: 연속형 자료와 이분형 자료를 실제 분석하여 결과가 Paired t-test와 Exact McNemar test인지 확인했다. 8개 언어의 전체 결과 패널에서 부록 분석 개요의 짧은 이름, 사용자 이름 Review/Normality를 검사했다.
- 긴 이름 2개는 부록 표 번역 함수로 별도 검사했다. 실제 분석 개요는 짧은 이름을 사용하므로 두 검사 범위를 구분한다.
- 영어 본표 동일성과 원본 결과 객체 보존 검사 통과.
- `validate_paired_guards.R`, `validate_i18n_contract.R`: 통과.
- 한국어·일본어 현재·누적 HTML/PDF/DOCX/HWPX/XLSX 저장 내용 및 PDF 캡처 텍스트 보존 검사 통과.
- 산출물: `tmp/paired-method-names-i18n/exports`. 실제 패널 캡처를 저장하며 저장 단계에서 분석을 재실행하지 않는다. 긴 이름만 넣은 별도 검사 표는 저장 캡처에 포함하지 않았다.

## 범위

이번 수정은 위 두 분석법의 표기 4개에 한정한다. 다른 범주형 검정명 및 반복측정 분석명 후보는 후속 점검 대상으로 남긴다. 문서 앱별 수동 시각 검토와 설치본 빌드는 수행하지 않았다. 기존 매개·조절효과 화면은 계속 제외한다.
