# 민감도 비교 결과표 — 2026-09-17

민감도 비교표의 Comparison 열은 앱이 생성한 비교명을 담지만 공통 사용자 데이터 보호 규칙으로 번역되지 않았다. 해당 표에만 표시 메타데이터를 붙여 비교명을 번역하도록 수정했다. 일반 표의 사용자 Comparison 값은 계속 보호한다. 계산과 원본 셀 값은 변경하지 않는다.

분석 유형 4개, 적합/계산 상태 3개, 확률절편/기울기 비교 2개, Hausman 비교명, RE 독립 가정과 Driscoll-Kraay 권고 등 12개 문구를 8언어 사전에 연결했다. 패널 모형명이 포함된 Driscoll-Kraay 비교명은 모형명 부분을 번역하며 통용 표기는 유지한다.

`validate_longitudinal_sensitivity_table_i18n.R`에서 합성 자료를 이용해 실제 GEE 상관구조, LMM 확률효과, 패널 FE/RE·공분산·Hausman 비교를 실행했다. 3개 결과 묶음 × 8언어 24개 검사에서 분석명·비교명·상태·설명 번역, 수치 문자열 및 원본 표 보존을 확인했다. 별도 일반 표의 사용자 비교명도 보존했다. 실패 경로와 Metric 열 전체 번역 검사는 이번 범위가 아니다.

기존 실제 GEE/LMM/패널 고정효과 본 표 회귀 검사는 `validate_longitudinal_structural_i18n.R`로 수행한다. 저장 입력은 `tmp/longitudinal-sensitivity-table/tables.rds`, 출력은 `tmp/longitudinal-sensitivity-exports`이며 저장 시 분석을 재실행하지 않는다. 한국어/일본어 현재·누적 HTML·PDF·Word·직접 생성 HWPX·Excel 및 PDF 캡처 문구를 검사한다.

GLMM 등 다른 비교 조합·실패 안내·지표명과 브라우저·페이지별 시각 검토는 남아 있다. 메타분석과 설치본은 변경하지 않았다.

위 24개 검사와 기존 실제 GEE/LMM/패널 고정효과의 8언어 검사가 모두 통과했다. 본 표 영어 유지·진단 번역·원래 라벨 보존을 확인했다. 기존 합성 LMM의 특이 적합 경고는 유지된다. 한국어/일본어 현재·누적 5종 저장 및 PDF 문구 검사도 통과했다(두 언어 모두 현재 5쪽/누적 9쪽). 변경 파일의 `git diff --check`가 통과했다.
