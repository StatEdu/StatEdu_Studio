# GLM 분포·옵션 실제 출력 점검

2026-09-16. 설치본은 만들지 않았다.

## 변경

- 감마 역수 링크, 이분형 HC3, 노출량 오프셋을 포함한 과산포 카운트 모형을 실제 실행했다.
- 출력에서 확인한 Exposure / offset 역할명과 음이항 전환/비수렴, Poisson MI 고정 안내 등 4개 항목을 한국어와 6개 다국어에 추가했다.
- 한국어 역수 링크를 `inverse` 대신 `역수`로 표시하도록 수정했다.
- 계산 로직은 변경하지 않았다.

## 검증

- 새 `scripts/validate_glm_options_multilingual.R`: 8개 언어 통과. 실제 카운트 적합이 음이항으로 전환되는 것, 감마 inverse 및 이분형 HC3 적용을 확인했다.
- 본표 셀·주석·영어 언어 속성, 오프셋 사용자 라벨 `Model-based`가 유지되는지 검사했다. 신규 문구가 보조표에서 영어로 남지 않는지 확인했다.
- `scripts/validate_i18n_contract.R`: 통과.
- 일본어 실제 음이항 결과와 오류 분기 문구 fixture를 현재/누적 저장 검증에 사용했다.
- 현재/누적 HTML·Word·HWPX·Excel 내용 비교 및 PDF 생성 통과. `scripts/validate_multilingual_pdf.py`의 두 PDF 실제 텍스트 비교도 통과했다.

## 범위

음이항 비수렴 문구는 fixture로 검사했으며 실제 비수렴을 유발한 계산 시험은 아니다. 카운트 MI와 다른 분석 화면 등은 추가 점검 대상이다.
