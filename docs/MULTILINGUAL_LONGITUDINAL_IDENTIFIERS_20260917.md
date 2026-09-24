# 종단 분포·GEE 상관구조 식별자 표시 — 2026-09-17

자동·카운트·Poisson·음이항 분포와 교환가능·AR(1)·독립·비구조화·실험적 SPSS 호환 GEE 상관구조의 9개 식별자에 8언어 표시 명칭을 연결했다. `Working correlation: ...` 안내에서도 알려진 상관구조 값은 표시 명칭으로 변환한다. 설정 값·계산 값 자체를 바꾸지 않는다. 임의 문자열이나 사용자 이름을 부분 치환하지 않는다.

`validate_longitudinal_identifiers_i18n.R`에서 9개 보조표 셀 × 8언어 표시, 상관구조 명칭이 안내 문구에 포함되는지, 원본 표와 선택 항목의 내부 값 보존을 확인했다. 선택 메뉴의 모든 문구·브라우저 동작을 검증한 것은 아니다. 기존 실제 GEE/LMM/패널 고정효과 검사는 `validate_longitudinal_structural_i18n.R`로 실행한다.

현재·누적 저장은 `validate_longitudinal_count_exports.R tmp/longitudinal-identifiers/tables.rds tmp/longitudinal-identifier-exports`로 수행한다. 저장 시 분석을 다시 실행하지 않는다. 한국어·일본어 HTML/PDF/Word/직접 생성 HWPX/Excel과 PDF 캡처 문구를 검사한다.

상관구조가 들어가는 다른 설명 문장, 모든 모형·옵션 조합 및 브라우저·페이지별 시각 검토는 남아 있다. 메타분석과 설치본은 변경하지 않았다.

위 표시 검사와 기존 실제 GEE/LMM/패널 고정효과의 8언어 검사가 모두 통과했다. 본 표 영어 유지·진단 번역·원본 라벨 보존을 확인했으며 합성 LMM의 특이 적합 경고는 유지된다. 한국어·일본어 현재·누적 5종 저장과 PDF 4개 문구 검사도 통과했다(두 언어 모두 현재 2쪽/누적 3쪽). 변경 파일의 `git diff --check`가 통과했다.
