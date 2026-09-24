# 생존분석 안내·오류·부록 제목 재점검

기존 번역 작업과 최근 보완 사항을 합친 상태에서 아래 검사를 다시 실행했다. 이번 검증 범위에서는 새로운 번역 누락이 발견되지 않아 제품 코드와 번역 사전은 변경하지 않았다.

## 통과한 검사

| 검사 | 확인 범위 |
| --- | --- |
| `validate_survival_internal_errors_i18n.R` | 실제 오류 분기 4개, 분석 소스의 정적 stop 문구 50개, 외부 오류 상세 보존, 8개 언어 |
| `validate_survival_named_warnings_i18n.R` | 실제 KM·원인별 Cox 결과에 교차·PH·범주형 검정 진단 신호를 주입하여 경고 선택과 사용자 이름 보존, 8개 언어 |
| `validate_survival_followup_warnings_i18n.R` | 실제 KM 결과에 추적관찰 진단값을 주입하여 말단 위험집단·검열률 경계값, 결측, 신호 없음 분기와 영어 본표 보존, 8개 언어 |
| `validate_survival_reporting_notes_i18n.R` | 실제 분석 결과에서 제목·보고 주석 번역과 표 내용 보존, 8개 언어 |
| `validate_survival_interpretation_i18n.R` | 실제 결과 패널의 해석 안내, 통계 기호 및 영어 본표 보존, 8개 언어 |
| `validate_survival_checklist_i18n.R` | 보고 체크리스트 번역 및 사용자 근거 문구 보존, 8개 언어 |
| `validate_survival_appendix_title_catalog.R` | 기존 함수의 제목 목록을 직접 추출하여 부록 제목 31개 × 8개 언어 = 248개 매핑 확인 |

제목 매핑 검사는 함수의 반환값 검사이며, 31개 제목의 모든 조건부 화면을 실제로 실행했다는 의미는 아니다. 신호를 주입한 경고 검사는 경고 렌더링 검증이며 모든 통계적 신호를 자료에서 자연 발생시킨 검증은 아니다.

## 산출물과 범위

- 기존 6개 검사 로그: `tmp/survival-*-recheck.log` 중 위 검사 이름에 대응하는 파일.
- 제목 목록: `tmp/survival-appendix-title-catalog/titles.csv`.
- 제품 출력 변경이 없으므로 이번에는 5개 형식 저장을 다시 생성하지 않았다. 직전 KM 안내 수정의 저장 검증은 `MULTILINGUAL_SURVIVAL_SPARSE_KM_20260918.md`에 기록되어 있다.
- 전체 프로그램 번역 완료 판정이나 전체 생존분석의 모든 상호작용 완료 판정은 아니다. 기존 매개·조절효과 패널 제외 원칙을 유지했다. 설치본 빌드는 수행하지 않았다.
