# 복합표본 사용자 모형 다국어 검증

## 변경

- 분석 개요의 매개·조절효과 분석명, 분석 N, 설계 자유도, 방정식 수와 분석 구문의 종류/열 제목 번역을 보완했다.
- 구문 표의 Syntax 열과 실제 회귀 방정식의 사용자 변수명 셀을 보존한다. 변수명이 `Normality`와 같은 시스템 문구여도 번역하지 않는다.
- 본표의 경로계수·직접/간접/조건부 효과와 모델 계산은 변경하지 않았다. 번역 자료: `scripts/fill_complex_custom_i18n.py`.

## 검증

- `scripts/validate_complex_custom_i18n.R`: 120행, 3개 층, 30개 PSU, 가중치. X→M→Y 매개경로와 직접경로, M→Y 경로에 W 조절, 공변량 C를 포함한다.
- 실제 survey 회귀 및 복제 가중치 공분산으로 간접효과를 계산하고 조절변수의 세 조건에서 유한한 효과 추정치를 확인했다.
- 8개 언어에서 본표 영역 전체(제목·셀·주석) 일치. 보조표 언어/실제 번역 문구 확인 및 원래 구문·사용자 방정식 이름 일치.
- 기존 `validate_complex_sample_custom_model.R` 통과: 매개·조건부 효과, 가중치 없는 모형, 순환 경로 거부, 설계 변수 누락 검사.
- 근거: `tmp/complex-custom-i18n`, `tmp/complex-custom-regression.log`.
- 동일한 일본어 결과 스냅샷으로 현재·누적 HTML/Word/Excel 제목·셀 보존 검사와 실제 PDF 텍스트/표지 검사를 통과했다. 저장 시 모델을 다시 계산하지 않았다.
- HWPX는 `convert_result_hwpx.ps1`의 Hancom 변환 90초 제한에서 두 차례 실패했다. 최초/재시도 로그: `tmp/complex-custom-exports.log`, `tmp/complex-custom-exports-retry.log`. 두 실행 모두 현재 결과 HWPX 단계에서 중단되어 누적 HWPX는 검증하지 못했다.
- 나머지 형식은 `tmp/validate_complex_custom_available_exports.R`로 현재·누적 모두 확인했다(`tmp/complex-custom-available-exports.log`). 이 임시 검증은 실패한 HWPX 단계만 제외하며 제품 저장 코드는 변경하지 않는다. PDF 검사: `scripts/validate_multilingual_pdf.py tmp/complex-custom-i18n`.

## 제한

- 이번 검증은 실제 모델의 결과표 렌더링이다. 브라우저 캔버스 조작/그림 저장, 모든 모형/설계 옵션, 오류 문구의 전체 다국어 처리는 별도 검증이 필요하다.
- 설치본을 만들지 않았다.
- 이후 `scripts/validate_pending_diagnostic_hwpx.R` 재검증에서 현재·누적 HWPX 모두 기존 캡처와 Word 내용의 대조를 통과했다(`tmp/pending-diagnostic-hwpx.log`). 이 대표 결과의 5형식 저장 검증은 충족했다. 한글 파일 접근 정책이나 변환 제한 시간은 변경하지 않았다. 모든 모형/화면 분기의 완료를 뜻하지 않는다.
