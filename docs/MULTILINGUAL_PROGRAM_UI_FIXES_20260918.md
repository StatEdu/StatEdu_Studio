# 전체 점검에서 확인된 활성 화면 문구 보완

## 변경

전체 점검에서 확인한 5개 문구를 영어·한국어·일본어·중국어·스페인어·프랑스어·독일어·베트남어 사전에 반영했다.

- ID 집계: Create data.
- 변수 변환: ID conditional statistic, Number of missing values, Complete case flag.
- 복합표본 사용자 모형: 공통 캔버스와 설계변수 자동 적용 안내.

기존 매개·조절효과 패널은 사용자 지시에 따라 제외했다. 변경 대상은 사전과 검증 스크립트이며 계산 알고리즘·입력 코드·결과 출력·저장 코드는 변경하지 않았다.

## 검증 결과

- `validate_program_audit_ui_i18n.R`: 실제 ID 집계 버튼, 변수 변환 옵션, 복합표본 캔버스 안내가 사전값과 일치하는지 8개 언어로 검증했다. 선택 코드 id_stat/N_miss/F_miss와 사용자 변수가 들어간 수식은 동일하게 유지된다.
- `validate_id_aggregate_ui_i18n.R`: 실제 집계값, 서버 동작, 언어 변경 시 자료 보존 검사 통과.
- `validate_complex_custom_i18n.R`: 복합표본 사용자 모형의 실제 분석 및 8개 언어 결과 검사 통과. 영어 본표와 사용자 모형 구문 보존 확인. 이 검사는 제외한 기존 매개·조절 패널이 아니라 계속 사용하는 복합표본 사용자 모형 대상이다.
- `validate_i18n_contract.R`: 통과.
- `audit_multilingual_all_panels.R`: 73개 등록 패널 × 8개 언어, 584개 화면 생성 검사 통과. 비한국어 표제 노드의 한국어 후보 0개.
- `summarize_multilingual_program_audit.py`: 앞서 확인했던 5개 영어 문구의 잔존 조합이 30개에서 0개로 감소했다.

## 범위

0개는 이번에 추적한 5개 문구가 남지 않았다는 뜻이며 전체 앱의 미번역 0개를 의미하지 않는다. 일반 원문 반환 후보는 47개 조합이 남으며 정상 번역·약어 등을 구분해야 한다. 다른 결과 분기와 정적 후보의 실제 경로 검토도 남아 있다.

UI 문구만 변경해 5개 형식 저장 파일은 이번에 재생성하지 않았다. 기존 출력·저장 검증은 전체 점검 보고서의 근거를 유지한다. 설치파일도 생성하지 않았다.

실행 산출물은 `tmp/multilingual-program-audit/all-panels.csv`, `confirmed-english-findings.csv`, `summary.json`에 갱신했다. 3일 작업 근거 목록은 최초 전체 점검 시점의 스냅샷을 유지한다.
