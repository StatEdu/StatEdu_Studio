# 종단분석 종합 검증의 Excel 셀 위치 검사 복구

2026-09-15. 이전 수치 최적화 보고서에서 남겨 둔 `scripts/validate_longitudinal.R`의 Excel 검사 실패를 재현하고 검사 코드를 수정했다. 제품의 계산 및 저장 코드는 변경하지 않았다.

## 원인과 수정

기존 검사는 모든 표가 Excel 3행에서 시작한다고 가정했다. 현재 저장은 화면에서 캡처한 제목·설명을 먼저 기록한다. 실제 첫 시트에는 `Longitudinal / Panel Models`, 설명, `Model overview` 세 줄과 빈 줄이 있고 표는 5행부터 시작한다. 두 번째 표는 앞선 제목이 한 줄이므로 3행부터 시작한다. 실제 저장 파일을 읽어 확인했으며 셀 내용의 누락이 아니었다.

검사에서 캡처한 앞선 문구 수에 따라 위치를 계산하도록 변경했다. 모든 앞선 문구의 내용과 순서, 표가 들어갈 행·열 수, 각 비어 있지 않은 원본 셀의 문자열을 정확히 검사한다. 표의 순서와 시트 수 검사도 유지했다. 숫자의 반올림이나 허용오차를 추가하지 않았다.

## 실행 결과 및 범위

번들 R 4.5.3으로 직접 실행하면 번들에서 예제 자료 `geepack::ohio`를 찾지 못해 먼저 중단됐다. 연구용 `run.R`은 그 자료를 읽는 단 하나의 호출에만 설치 라이브러리 `C:/Users/drlee/AppData/Local/R/win-library/4.5`의 `lib.loc`을 지정한다. 계산용 라이브러리 경로는 번들로 유지했다. 패키지 설치나 번들 수정은 하지 않았다.

이 자료 보완 조건에서 수정 전에는 기록된 Excel assertion이 재현됐고, 수정 후에는 종합 스크립트 끝의 `Longitudinal / panel validation passed.`까지 통과했다. 이전에 중단 지점 뒤에 있던 선택적 패널 가정 검사도 실행됐다. 특이 적합 메시지와 ordered factor 연산 경고는 숨기지 않았다. 보완 없이 번들만으로 직접 실행하는 경로의 예제 자료 부재는 해결하지 않았다.

이는 검사 기준의 수정이다. 수치 성능 개선이나 전체 내부 모형 객체의 새 동일성 검증은 아니다. 표시·저장 내용이 바뀌지 않았으므로 PDF/Word/HWPX 전체 변환 및 설치 파일 빌드는 수행하지 않았다.

증거: `output/longitudinal-excel-validation-20260915/`의 `run.R`, `run-before.log`(예제 자료 부재), `run-data-fallback.log`(Excel 실패), `run-after.log`(통과), `current.xlsx`, `screen-tables.rds`, `inspect.R`, `inspect.log`. Excel과 화면 자료 RDS는 수정 전 실패 실행에서 보존한 자료다.
