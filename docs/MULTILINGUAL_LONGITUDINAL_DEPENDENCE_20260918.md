# 종단분석 자기상관·횡단면 의존성 권고문 번역

2026-09-18

`longitudinal_check_serial_correlation`와 `longitudinal_check_cross_section_dependence`의 실제 출력에서 미번역 권고문 7개를 확인하고 `scripts/fill_longitudinal_dependence_i18n.py`로 8언어 사전에 연결했다. 반복측정 부족, 추가 자기상관 보정 불필요, 공통 충격에 대한 시점 고정효과·패널 강건 추론·Driscoll-Kraay 표준오차, 횡단면 의존성 보정 불필요 안내를 포함한다. 분석 계산과 판정 기준은 변경하지 않았다.

## 실제 진단 검사

`scripts/validate_longitudinal_dependence_i18n.R`는 고정 난수 시드 725의 합성 30명 × 20시점 자료를 사용한다.

- 자기상관 5경로: 잔차쌍 부족, 일반 시차 잔차의 상관 검출/비검출, 패널 Breusch-Godfrey 검출/비검출.
- 횡단면 의존성 5경로: 패널 이외 모형, 선택 패키지 없음, 검사 실패, Pesaran CD 검출/비검출.

패널 검사는 실제 `plm::pbgtest`와 `plm::pcdtest`를 사용한다. 자기상관 비검출 문구를 위한 패널 fixture는 pooled 모형이며 검출은 within 모형이다. 초기 within 독립오차 fixture에서는 검사가 유의했으므로 독립오차라는 생성 조건을 비검출 보장으로 취급하지 않았다. pooled 결과를 검증 함수의 패널 분기로 전달하여 비검출 권고문을 검사한다. 모든 실제 고정효과 분석에서 비검출됨을 주장하는 시험은 아니다. CD 검출/비검출에는 실제 within 모형을 사용한다.

패키지 없음은 복사한 진단 함수의 격리 환경에서 `plm` 탐지만 false로 바꿔 확인하며 제품 함수나 설치 패키지를 변경하지 않는다. CD 계산 실패는 잘못된 모형을 전달해 실제 오류 처리를 거친다. 일반 시차상관의 `cor.test` 자체 실패 경로는 이번 10개 fixture에 포함되지 않았다.

수정 전 미번역을 재현했고 수정 후 10경로 × 8언어 검사가 통과했다. Check/Result/Interpretation/Recommendation 번역, Statistic/p/Issue 보존, 직렬화된 원본 결과 불변과 사용자 변수명 원문 보존을 확인했다. 원본 진단 표는 `tmp/longitudinal-dependence-i18n/tables.rds`에 저장했다.

## 저장·공통 검증

`validate_longitudinal_count_exports.R tmp/longitudinal-dependence-i18n/tables.rds tmp/longitudinal-dependence-i18n/exports`는 이미 계산한 표를 재사용하여 한국어/일본어의 현재/누적 결과를 HTML, PDF, Word, HWPX, Excel로 저장했다. HTML/Word/HWPX/Excel에서 모든 예상 표 문구가 보존됐고 영어 본표는 8개 언어에서 동일했다. 종료 코드 0.

`validate_longitudinal_error_pdf.py tmp/longitudinal-dependence-i18n/exports`로 네 PDF의 모든 예상 표 문구를 확인했다. 표지 포함 현재 결과 12쪽, 두 항목을 누적한 결과 23쪽이다. Word/Hancom의 별도 시각적 페이지 검수는 수행하지 않았다.

`validate_i18n_contract.R`는 Windows UTF-8 로캘에서 통과했고 수정 사전과 검사 스크립트의 diff 공백 검사도 통과했다.

## 남은 범위

이 검사는 두 진단 함수의 대표 경로와 번역을 대상으로 한다. 전체 종단 모형·옵션·오류 조합이나 전체 앱 다국어 완료를 뜻하지 않는다. 설치본을 생성하거나 설치 앱을 변경하지 않았다.
