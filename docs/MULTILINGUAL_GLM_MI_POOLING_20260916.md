# GLM MI 통합 및 분석 사례 수 설명

2026-09-16. 설치본은 생성하지 않았다.

## 변경

- 6개 언어에 고정 문구 3개와 동적 형식 6개를 추가했다. MI 통합(Rubin 총분산, Barnard–Rubin 자유도, t 기반 신뢰구간), 종속변수 결측 제외/포함, 대표 완성 자료의 진단 사용, 분포·링크 고정, 카운트 분포 선택, MI/IPW/완전사례 분석 행 수를 포함한다.
- MI 통합 설명의 종속변수 처리 뒷문장은 별도로 번역한다. 분포·링크는 해당 필드에서만 번역하고 수치는 원문대로 삽입한다.
- 본표, 추정 알고리즘, 사용자 변수명과 라벨을 변경하지 않는다.

## 검증

- `scripts/validate_glm_multilingual.R`: 8개 언어 통과. MI 뒷문장 3종을 각각 조합하여 끝부분 번역 및 숫자 보존을 검증했다. 같은 설명을 사용자 Variable 열에 둔 경우 원문 보존을 확인했다.
- Gaussian/binomial/count 실제 분석의 본표 영어 및 옵션 값 유지 검사. 합성 count 데이터의 기존 음이항 추정 반복 한도 경고가 발생했다.
- `scripts/validate_i18n_contract.R`: 통과.
- 일본어 fixture의 현재/누적 HTML·Word·HWPX·Excel 내용 비교 및 PDF 생성 통과. `scripts/validate_multilingual_pdf.py`의 두 PDF 실제 텍스트 비교도 통과했다.

## 범위

이번 변경은 출력 번역이며 MI를 새로 적합하는 계산 검증은 아니다. 원고의 Methods/Results 복합 문장과 다른 미번역 진단 안내는 추가 점검이 필요하다.
