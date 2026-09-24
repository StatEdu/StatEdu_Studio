# 혼합 반복측정 자료 유형 안내 번역

## 변경

실제 혼합 반복측정 결과에서 다음 안내 3개가 일본어·중국어·스페인어·프랑스어·독일어·베트남어에서 영어로 남는 것을 재현했다.

- At least one repeated-measures variable is ordinal.
- The stacked repeated outcome is non-negative integer-like count data.
- The stacked repeated outcome is positive and strongly right-skewed.

8개 언어 사전에 반영하고 기존 영어·한국어 표기는 유지했다. 사전만 보완한 중간 상태에서는 정규성 설명과 합쳐진 Reason 셀에 영어가 남았다. `mixed_rm_appendix_table`의 기존 문장 조각 처리 목록에 세 문구를 추가했다. 계산 및 분석 방법 선택은 변경하지 않았다.

## 검증

`validate_mixed_rm_family_reasons_i18n.R`는 순서형, 계수형, 양수·우측 편향 자료 각각에 40명·2집단·3시점의 실제 완전 사례 혼합 반복측정 분석을 수행한다. 각 결과에서 해당 안내가 단독 Reason과 정규성 설명에 결합된 Reason에 모두 생성되는지 검사한다. 권고된 대체 순서형 모형·GLMM을 적합하거나 그 통계적 정확성을 검증한 것은 아니다.

- 수정 전: 6개 언어 × 3종류 사전 검사 실패.
- 사전만 수정한 중간 상태: 6개 언어 × 3종류의 결합 설명 및 렌더링 검사 실패.
- 표시 처리 수정 후: 3종류 × 8개 언어의 단독·결합 안내, 실제 패널 표시 검사 통과.
- 영어 본표의 언어 간 동일성, 분석 결과 객체 보존, 안내문과 철자가 같은 사용자 변수명 및 Review·Normality·Unknown 집단명 보존 검사 통과.
- 이전 `validate_mixed_rm_normality_guidance_i18n.R` 회귀 검사 통과.
- `validate_multilingual_table_roles.R`: 16개 부록 변환 함수와 8개 결과 렌더러의 8개 언어 회귀 검사 통과. `validate_i18n_contract.R`는 초기 실행의 시스템 로캘 오류를 UTF-8 로캘 설정으로 해결한 후 통과했다.
- 한국어·일본어 현재 및 누적 HTML/PDF/DOCX/HWPX/XLSX 내용 검사 및 PDF 캡처 텍스트 보존 검사 통과. 한국어 PDF 현재 9쪽·누적 16쪽, 일본어 현재 9쪽·누적 17쪽.
- `validate_longitudinal_count_exports.R`와 `validate_longitudinal_error_pdf.py`를 재사용했다. 공용 표지 제목은 Longitudinal이고 본문은 이번 혼합 반복측정 캡처다. 저장 시 분석을 재실행하지 않았다. 산출물: `tmp/mixed-rm-family-reasons-i18n/exports`.

기존 매개·조절효과 화면은 제외했다. 설치본 빌드와 문서 앱별 수동 시각 검토는 수행하지 않았다.
