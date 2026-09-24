# 순서형 ITT 미적합 안내 번역

## 변경

혼합 반복측정에서 순서형 ITT 분석을 선택하면 표시되는 `ordinal mixed model`과 `Use an ordinal mixed model path for ITT; automatic fitting was not available.`를 8개 언어에 반영했다. 한국어 전용 변환에도 추가하여 영어 모형 이름이 한국어 문장에 남지 않도록 했다.

현재 모듈은 순서형 혼합모형 자동 적합을 지원하지 않는다. 이번 작업은 그 상태를 설명하는 이름·안내 번역이며 자동 적합 기능을 추가하거나 결과를 생성한 것이 아니다.

## 검증

`validate_mixed_rm_ordinal_itt_i18n.R`는 40명·2집단·3시점 순서형 자료로 두 경우를 검사한다. 첫 번째는 완전 사례 ANOVA가 산출되는 자료이고, 두 번째는 각 대상자의 한 시점이 결측이라 완전 사례가 없는 자료다. 둘 모두 ITT 분석 진입점을 실제 호출한다.

- 수정 전 한국어 및 6개 외국어 사전 검사 실패를 재현했다.
- 수정 후 2개 결과 × 8개 언어의 모형 이름·미적합 안내·미적합 상태 검사 통과.
- 순서형 혼합모형 계수가 생성되지 않는지 확인했다.
- 완전 사례가 있는 결과의 영어 본표 동일성 및 완전 사례가 없는 결과의 본표 부재를 확인했다.
- 전체 결과 객체와 안내문과 같은 철자의 사용자 변수명 보존 검사 통과.
- 기존 ITT LMM·PP 참고 결과 안내 회귀 검사, `validate_i18n_contract.R`, 변경 공백 검사 통과.
- 한국어·일본어 현재/누적 HTML/PDF/DOCX/HWPX/XLSX 내용 및 PDF 텍스트 보존 검사 통과. 두 언어 모두 현재 PDF 7쪽·누적 12쪽이다.
- 저장 검증은 `validate_longitudinal_count_exports.R`와 `validate_longitudinal_error_pdf.py`를 재사용했다. 공용 표지 제목은 Longitudinal이며 본문은 이번 순서형 ITT 결과 캡처다. 저장 단계에서 분석을 재실행하지 않았다. 산출물: `tmp/mixed-rm-ordinal-itt-i18n/exports`.

기존 매개·조절효과 화면은 제외했다. 설치본 빌드 및 문서 앱별 수동 시각 검토는 수행하지 않았다.
