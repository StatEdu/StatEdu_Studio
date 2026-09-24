# 범주형 대응검정 분석명 번역

## 변경

McNemar·Stuart–Maxwell·Bowker 결과의 부록에서 사용되는 다음 표기 4개를 8개 언어 사전에 반영했다.

- McNemar test
- Stuart-Maxwell
- Stuart-Maxwell test
- Bowker

기존 한국어·영어 표현을 유지했다. McNemar 짧은 표기와 Bowker symmetry test 긴 표기는 기존 번역을 유지하고 함께 검증했다. 계산·렌더링·저장 코드는 변경하지 않았다.

## 검증

- `validate_paired_categorical_names_i18n.R`: 불일치 쌍 40개인 이분형 자료로 근사 McNemar 검정을 실행했다. 3범주 자료로 Stuart–Maxwell과 Bowker 검정을 각각 실행하고, 실제 선택된 분석법이 예상과 일치하는지 확인했다.
- 8개 언어에서 실제 전체 결과 패널의 부록 검정명과 사용자 변수명, 긴 검정명의 부록 번역 함수를 검사했다.
- Review/Normality 및 한글·특수문자·`%s`가 포함된 사용자 범주명 보존, 영어 본표 동일성, 원본 결과 객체 보존 검사 통과.
- `validate_paired_guards.R`, `validate_i18n_contract.R`: 통과.
- 한국어·일본어 현재·누적 HTML/PDF/DOCX/HWPX/XLSX 저장 내용 및 PDF 캡처 텍스트 보존 검사 통과.
- 산출물: `tmp/paired-categorical-names-i18n/exports`. 캡처한 패널을 저장하며 분석을 재실행하지 않는다. 긴 이름만 넣은 별도 함수 검사 표는 저장 캡처에 포함하지 않았다.

## 범위

이번 작업은 범주형 대응검정의 분석명 표기에 한정한다. 반복측정 분석명 등 다른 후보는 후속 점검 대상으로 남긴다. 문서 앱별 수동 시각 검토와 설치본 빌드는 수행하지 않았다. 기존 매개·조절효과 화면 제외 원칙은 유지한다.
