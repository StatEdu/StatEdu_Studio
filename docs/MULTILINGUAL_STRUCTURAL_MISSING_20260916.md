# 구조방정식 결측자료·이상치 보조표 다국어 점검

## 수정 범위

- CFA/SEM의 결측 사례 수, 목록 삭제·쌍별 처리·FIML 설명, 민감도 기록, 결측 패턴, Mahalanobis 후보 안내와 진단 불가 사유를 UI 언어로 표시한다.
- PLS의 평균대체 사례/셀 수, 지표별 대체 평균, 분석·부트스트랩 처리 설명, 경고와 진단 불가 안내를 UI 언어로 표시한다.
- 민감도 기록의 `Assumptions, results, and conclusion` 열을 사용자 데이터로 명시했다. `Normality`처럼 사전에 있는 문구와 일치하는 사용자 입력도 번역하지 않는다.
- 결측 패턴의 생성 문구만 번역하고 그 뒤의 변수명은 보존한다. 처리 옵션의 내부 값과 분석 계산은 변경하지 않았다.
- 본표는 영어로 유지한다. 정상 영어 렌더링은 변경 전과 동일하다.

## 검증

- `scripts/validate_structural_missing_i18n.R`: 실제 240행 자료의 목록 삭제/FIML/순서형 WLSMV 쌍별 처리 CFA를 적합하고, 3개 방식 × 8개 언어를 검사했다. 본표 HTML·보조표 수치 유지와 사용자 변수·라벨·민감도 입력 보존을 확인했다.
- 부족한 완전 사례·특이 공분산·비숫자 지표의 실제 진단 실패 자료, 이상치 없음·결측 없음·처리 옵션 미지정, 민감도 미평가와 사용자 입력 특수문자 분기를 확인했다. 실패 자료는 정상 적합 객체에 대해 진단 렌더링만 검사했으며 실패 자료 자체를 재적합한 것은 아니다.
- PLS는 실제 결측 진단 데이터로 8개 언어에서 대체 평균/사례/셀 수와 경고·진단 불가·결측 없음 분기를 확인했다. 이 8개 언어 검사는 PLS 전체 모형 실행 검사가 아니다.
- 별도의 기존 `scripts/validate_pls_missing_policy.R`는 실제 seminr 모형과 재표집 데이터의 평균대체 계산 검사를 통과했다.
- 전체 번역 사전 검사 통과. 이번 렌더러의 고정 `tr()` 문구에 대해 6개 추가 언어 키 누락도 점검했다.

## 저장

일본어 현재·누적 스냅샷으로 `STATEDU_I18N_EXPORT_FIXTURE=tmp/structural-missing-i18n`을 지정하여 `scripts/validate_structural_normality_exports.R`을 실행했다. 저장은 캡처를 사용하며 재적합하지 않는다.

- 현재·누적 HTML/Word/Excel의 제목·셀·문단 내용 보존 통과.
- `scripts/validate_multilingual_pdf.py tmp/structural-missing-i18n`: 실제 PDF의 위 내용과 표지 통과.
- `scripts/validate_structural_missing_export_counts.R`: 현재 15개/누적 20개 표의 HTML·Word 순서와 Excel 시트 수 통과.
- HWPX는 최초 현재·누적 변환에서 시간 초과가 발생했으나, 이후 `scripts/validate_pending_diagnostic_hwpx.R`에서 두 결과 모두 캡처와 기존 Word 내용 대조를 통과했다(`tmp/pending-diagnostic-hwpx.log`). 이 대표 fixture의 5형식 저장 검증은 통과했다.

로그: `tmp/structural-missing-validation.log`, `tmp/structural-missing-pls-policy.log`, `tmp/structural-missing-coverage.log`, `tmp/structural-missing-exports.log`, `tmp/structural-missing-export-counts.log`.

전체 다국어 완료가 아니다. 모형 요약·추정 품질 등 다른 구조방정식 보조표, 실제 브라우저 언어 왕복 및 전체 SEM/PLS 모형별 검증이 남아 있다. 설치본은 만들지 않았다.
