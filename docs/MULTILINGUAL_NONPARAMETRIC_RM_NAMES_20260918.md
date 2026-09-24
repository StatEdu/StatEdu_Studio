# 비모수 반복측정 검정명 점검과 Cochran Q 번역

## 변경

실제 비모수 반복측정 결과에서 Friedman 검정명은 기존 번역이 정상임을 확인했다. `Cochran Q`, `Cochran's Q test`는 6개 비한국어 언어에서 영어로 남아, 해당 2개 표기를 8개 언어 사전에 반영했다. 기존 영어·한국어 표현을 유지했다. 계산·렌더링·저장 코드는 변경하지 않았다.

## 검증

- `validate_nonparametric_rm_names_i18n.R`: 연속형 자료로 Friedman, 이분형 자료로 Cochran Q를 실제 실행하고 선택된 검정명이 예상과 일치하는지 확인했다.
- 8개 언어의 전체 결과 패널에서 부록 검정명, 별도 부록 표 함수에서 긴 검정명을 검사했다.
- 사용자 범주값 no/yes, 영어 본표 동일성, 원본 결과 객체 보존 검사 통과.
- `validate_paired_guards.R`, `validate_i18n_contract.R`: 통과.
- 한국어·일본어 현재·누적 HTML/PDF/DOCX/HWPX/XLSX 저장 내용 및 PDF 캡처 텍스트 보존 검사 통과.
- 산출물: `tmp/nonparametric-rm-names-i18n/exports`. 캡처한 결과를 저장하며 저장 단계에서 분석을 재실행하지 않는다. 긴 검정명만 넣은 별도 함수 검사 표는 저장 캡처에 포함하지 않았다.

## 범위

이번 검증은 Friedman·Cochran Q 검정명과 해당 결과 패널에 한정한다. 비모수 방법 설명의 모든 측정수준 조합이나 저장된 Greenhouse–Geisser 결과까지 이번에 검증한 것은 아니다. 문서 앱별 수동 시각 검토와 설치본 빌드는 수행하지 않았다. 기존 매개·조절효과 화면은 계속 제외한다.
