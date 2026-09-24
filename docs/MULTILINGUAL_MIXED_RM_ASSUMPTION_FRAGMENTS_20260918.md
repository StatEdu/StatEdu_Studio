# 혼합 반복측정 가정 안내의 부분 번역 수정

## 발견 및 변경

일본어 실제 결과를 재점검하던 중 결합된 가정 안내에 `Levene: satisfied`와 `球面性 assumed`가 남아 있는 것을 발견했다. 한국어도 결합 문장 안의 assumed가 남았다.

- `Levene: satisfied`를 8개 언어 사전에 반영했다.
- 기존 번역이 있는 `Sphericity assumed`를 더 짧은 Sphericity보다 먼저 처리하도록 했다.
- 외국어 결합 문장 처리에 위 두 완전한 문구를 추가하고, 한국어 치환에도 구형성 가정을 추가했다.

통계 계산 및 판정 기준은 변경하지 않았다. 이번 수정은 등분산성 충족과 구형성 가정 표기에 한정한다.

## 검증

`validate_mixed_rm_assumption_fragments_i18n.R`는 기존 3종 자료의 실제 완전 사례 분석 및 대체 모형 권고 검사를 재사용한다. 해당 안내가 원본 결과에 실제 생성되는지 확인하고, 8개 언어 결과에서 문구 전체의 번역과 영어 잔여 문구 제거를 검사한다.

- 보완 전 한국어 assumed 잔여 및 6개 외국어의 사전 누락·영어 잔여를 재현했다.
- 보완 후 3개 결과 × 8개 언어 검사 통과.
- 영어 본표 동일성, 전체 결과 객체 및 진단 문구와 철자가 같은 사용자 변수명 보존 확인.
- 공통 16개 부록 변환 함수·8개 결과 렌더러의 8개 언어 회귀 검사, `validate_i18n_contract.R`, 변경 공백 검사 통과.
- 한국어·일본어 현재/누적 HTML/PDF/DOCX/HWPX/XLSX 내용 및 PDF 텍스트 보존 검사 통과. 한국어 현재 PDF 9쪽·누적 16쪽, 일본어 현재 9쪽·누적 17쪽이다.
- 저장 검증은 `validate_longitudinal_count_exports.R`와 `validate_longitudinal_error_pdf.py`를 재사용했다. 공용 표지 제목은 Longitudinal이고 본문은 이번 혼합 반복측정 결과 캡처다. 저장 단계에서 분석을 재실행하지 않았다. 산출물: `tmp/mixed-rm-assumption-fragments-i18n/exports`.

기존 매개·조절효과 화면은 제외했다. 설치본 빌드 및 문서 앱별 수동 시각 검토는 수행하지 않았다.
