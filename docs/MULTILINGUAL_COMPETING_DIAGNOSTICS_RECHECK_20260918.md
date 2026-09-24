# 경쟁위험·Fine–Gray 동적 진단 재점검

최근 KM 사건 수 문구 수정 이후 경쟁위험·Fine–Gray 결과의 기존 다국어 검사 12개와 새 접두 문구 검사를 실행했다. 모두 통과했으며 이번 범위에서 추가 번역 누락은 발견되지 않았다. 제품 코드·사전은 변경하지 않았다.

## 기존 검사 12개

다음 `scripts/validate_<이름>_i18n.R` 검사를 8개 언어로 실행했다.

| 이름 | 범위 |
| --- | --- |
| competing_named_warnings | 잔차 시간패턴·검열 층·CIF 무결성·Gray 검정 경고에서 선택된 사용자 이름 보존 |
| competing_sparse | 실제 소수 사건 자료의 관심·경쟁사건 수, 사건 없는 셀 및 희소 셀 안내 |
| competing_event_counts | 집단·원인별 사건 수와 검토 상태, 빈 결과 및 사용자 문구 |
| competing_epv | 원인별 Cox·Fine–Gray의 공변량당 사건 수 경계와 수치 |
| fine_gray_residual_warnings | 고유 사건시점 수, 잔차 시간패턴 및 검열 층 경고 |
| fine_gray_residual_table | 실제 잔차 표의 상태·이름·수치 정밀도 |
| fine_gray_optimizer | score·허용오차·정보행렬 계수·조건수·공분산·표준오차 경고 분기 |
| fine_gray_fit_failures | 적합 상태 및 경고 심각도 |
| fine_gray_test_failures | 모형 검정·범주형 전체 검정 실패 및 변수명 |
| fine_gray_collinearity | VIF·조건수 경계와 정밀도 |
| competing_overview | 분석 개요와 사용자가 설정한 값 |
| competing_collinearity_notes | 실제 적합 결과의 공선성 주석·수치 |

해당 스크립트의 영어 본표 보존 검사도 통과했다. 일부 검사는 실제 적합 결과에 진단값·상태를 주입해 경계와 실패 분기를 검사한다. 모든 실패 신호를 원자료에서 자연 발생시킨 검증은 아니다.

## 추가한 동적 접두 문구 검사

`validate_survival_evidence_prefix_catalog.R`는 제품 함수의 접두 문구 목록을 직접 읽어 36개 코드 × 8개 언어 × 사용자 이름/수치 2종 = 576개 사례를 확인한다.

- 비영어 언어의 접두 문구 번역 및 영어 원문 유지.
- Review, Normality, None, 한글, HTML 특수문자, `%s`, `: events = 99`가 포함된 사용자 문자열 보존.
- 지수 표기, 소수점 쉼표, 분수 형태 수치 문자열 보존.
- 최종 HTML 표의 근거 셀과 번역 헬퍼 결과 동일성.

이는 목록 기반 렌더링 검사이며 모든 진단 신호를 실제 분석으로 발생시킨 검사는 아니다. 직전 수정한 KM 사건 수 전용 분기는 별도 `validate_survival_sparse_km_i18n.R`에서 검증했다.

## 근거와 범위

- 기존 검사 로그: `tmp/<위 검사 이름>-recheck.log`.
- 접두 문구 검사 결과: `tmp/survival-evidence-prefix-catalog/evidence.csv`.
- 출력 변경이 없어 이번에는 저장 파일을 재생성하지 않았다. 직전 수정의 현재·누적 5개 형식 저장 검증은 `MULTILINGUAL_SURVIVAL_STRATUM_EVENTS_20260918.md`에 기록되어 있다.
- 전체 프로그램 번역 완료 판정은 아니다. 기존 매개·조절효과 패널 제외 원칙을 유지했고 설치본 빌드는 수행하지 않았다.
