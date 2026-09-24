# 추천 설계의 설정 파일 저장·복원

설정 파일의 `survival.design`에 연구 목적·자료 형태·사건 구조·추정량·시간의존 설정, 시간 원점과 단위, 변수 역할·공변량, 사건 코드/역할/사용자 라벨/확인 여부를 저장한다. 기존 KM·Cox·경쟁위험 설정은 유지하며, 예전 파일에 design이 없으면 기본 설계로 복원한다. 계산된 추천이나 분석 결과는 설정으로 저장하지 않는다.

자료의 SHA-256 해시를 저장하여 동일 자료와 코드 목록에만 저장된 사건 매핑·확인을 적용한다. 데이터 자체를 이 항목에 중복 저장하지 않는다. 다른 자료이면 사건 매핑은 초기화하고 재확인을 요구한다. 추천 설계는 같은 세션의 복원에도 화면을 갱신하며, 언어 전환 시 사용자 원문을 유지한다. 새 자료 로드/설정 초기화 경로에서는 설계도 기본값으로 초기화된다.

검증 결과:

- `validate_survival_design_persistence.R`: 실제 `.studio` 입출력, 8언어 서버의 최초/재복원, 복원 후 수정, 다른 자료의 저장된 확인 거부 통과.
- `validate_survival_design_persistence_browser.cjs`: 제품의 실제 설정 UI·핸들러를 조립한 격리 앱에서 8언어 입력 복원·재복원·언어 전환 및 다른 자료의 확인 거부 통과. 숨겨진 Selectize 원본 입력을 일반 입력처럼 기다리거나 선택한 초기 테스트 오류는 제품 오류가 아니다.
- 기존 KM·Cox·경쟁위험 설정 입출력·복원 검사와 전체 개발 앱의 설계 언어 왕복 검사가 통과했다.
- 전체 개발 앱의 자료 교체 후 초기화·8언어 왕복·재확인 후 추천 회귀 검사도 통과했다. 테스트 브라우저와 서버를 종료했다.

관련 로그: `tmp/survival-design-persistence.log`, `tmp/survival-design-persistence-browser.log`, `tmp/survival-design-persistence-regression.log`, `tmp/survival-design-save-language-regression.log`, `tmp/survival-design-save-dataset-regression.log`.

전체 설치 앱의 네이티브 파일 대화상자를 통한 저장/열기 왕복을 완료한 것은 아니다. 브라우저 복원 테스트는 격리된 테스트 버튼으로 실제 복원 요청을 전달하며 파일 대화상자를 대체 검증하지 않는다. 대규모 자료 해시 성능은 이번에 측정하지 않았다. 통계 출력·내보내기 코드는 변경하지 않았고 설치본을 만들지 않았다.
