# SEM 본표와 부록 정리

- 구조경로 본표 3과 특정 간접효과 본표 6에는 통계량을 남기고, 추론/CI 산출 근거·유효 반복 수·부트스트랩 상태·BH 검정군은 보조 결과로 분리한다. BH 보정 p 자체는 통계량이므로 유지한다.
- 매개효과가 있는 일반 SEM의 본표 7–10: 직접/간접/총효과별 B·p, β·p, B·95% CI, β·95% CI. β p와 CI는 표준화 추론 결과를 사용하며 B의 값을 복사하지 않는다. 정의되지 않은 효과 또는 사용할 수 없는 추론은 공백으로 두고 설명한다.
- 부트스트랩 z는 반올림 전 B/Boot SE의 Wald 비율이다. p는 경험적 양측 부트스트랩 검정값이므로 z로부터 계산한 정규이론 p와 구분하는 주석을 둔다. 유효 반복 부족/고정 효과의 추론 억제 규칙은 유지한다.
- 수정지수의 화면 패널과 저장 방향을 가로로 맞춘다. PDF는 구조방정식 전용 표 래퍼의 중복 페이지 나눔을 제거하여 제목과 표를 같은 페이지에 유지한다.
- 화면 결과를 현재/누적 HTML·Word·HWPX·PDF·Excel로 저장해 모든 셀/제목/주석을 검증한다. 관련 검증: `validate_sem_publication_tables.R --exports`, `validate_sem_publication_pdf.py`, `validate_sem_structural_reporting_tables.R`, `validate_sem_bootstrap_diagram_consistency.R`.

## 성능 측정

현재 R 4.5.2 / lavaan 0.6-21, 16 물리 코어 장치의 3요인·9지표·N=240 완전자료 예제:

- 5,000회, 12 workers: 총 94.25초, 재표집 92.40초, 유효 적합 4,998회.
- 500회: 4 workers 15.40초, 12 workers 9.91초. 12 workers에서 배치 250→500은 9.89초로 실질적인 개선이 없었다.
- 15 workers도 10.53초로 기본 12개보다 느렸다. 따라서 작업 수나 배치 크기를 임의로 변경하지 않았다.
- 실행 환경의 0.6-21에는 기존 0.7-2 전용 가속 경로가 적용되지 않는다. 검증된 추정/허용성 판정/표준화/CI 계산을 바꾸거나 버전 가드를 무조건 해제하지 않는다.
- 사용자 실제 분석의 소요 시간은 아직 전달받지 못했다. 위 수치는 합성 예제의 진단 결과이며, 사용자 분석의 속도 개선을 입증한 결과가 아니다.

Tables 9 and 10 use portrait orientation, consistent with Tables 7 and 8. This applies to current and accumulated exports; modification indices remain landscape.
