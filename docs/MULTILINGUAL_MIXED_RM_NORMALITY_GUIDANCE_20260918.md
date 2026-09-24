# 혼합 반복측정 정규성·민감도 분석 안내 번역

## 변경 및 재현

혼합 반복측정 결과의 다음 3개 문구가 일본어·중국어·스페인어·프랑스어·독일어·베트남어에서 영어로 남는 것을 실제 분석 결과로 재현했다.

- At least one Shapiro-Wilk p < .05; treat this as a sensitivity-analysis cue.
- Normality flagged; review LMM/robust sensitivity if distributional mismatch is meaningful.
- Consider LMM with robust/bootstrap inference as a sensitivity analysis.

8개 언어 사전에 반영했고 영어·기존 한국어 의미를 유지했다. 사전만 보완한 중간 상태에서도 첫 문구가 다른 설명과 합쳐진 Reason 셀에서는 영어로 남았다. `mixed_rm_appendix_table`의 기존 문장 조각 처리 목록에 첫 문구를 추가하여 결합 안내도 번역되도록 했다. 통계 계산은 변경하지 않았다.

## 검증

`validate_mixed_rm_normality_guidance_i18n.R`는 40명·2집단·3시점의 검사용 자료에 실제 혼합 반복측정 분석을 수행한다. 정규성 검토에서 문제가 표시되는 자료로 세 안내가 실제 생성되는지 확인한다. 사전 보완 전 6개 언어 catalog 검사 실패, 사전만 보완한 뒤 6개 언어 render 검사 실패, 결합 문장 처리 보완 후 8개 언어 검사 통과를 확인했다.

- 실제 출력의 3개 안내와 결합된 Reason 번역 확인.
- 영어 본표의 언어 간 동일성 및 분석 결과 객체 보존 확인.
- 번역 문구와 철자가 같은 변수명, Review·Normality·Unknown 집단명 보존 확인.
- `validate_multilingual_table_roles.R`: 16개 부록 변환 함수와 8개 결과 렌더러의 8개 언어 회귀 검사 통과. `validate_i18n_contract.R` 통과.
- 한국어·일본어 현재 및 누적 HTML/PDF/DOCX/HWPX/XLSX 저장 내용 검사 통과. PDF 캡처 텍스트 보존 검사 통과(한국어 현재 4쪽·누적 7쪽, 일본어 현재 5쪽·누적 8쪽).
- 저장 검증은 `validate_longitudinal_count_exports.R`와 `validate_longitudinal_error_pdf.py`를 재사용했다. 공용 표지 제목은 Longitudinal이고 실제 본문은 이번 혼합 반복측정 캡처다. 저장 시 분석을 재실행하지 않았다. 산출물은 `tmp/mixed-rm-normality-guidance-i18n/exports`에 있다.
- 전체 프로그램 정적 후보 검사 재실행: 333개 파일, 4,222개 출현, 누락 후보 고유 문구 543개(이번 3개 보완 전). 영어 본표·대체 경로·별도 키를 포함하므로 실제 미번역 수나 완료율로 해석하지 않는다.

이번 대상은 혼합 반복측정 결과이며 사용하지 않기로 한 기존 매개·조절효과 화면은 제외했다. 설치본 빌드 및 문서 앱별 수동 시각 검토는 수행하지 않았다.
