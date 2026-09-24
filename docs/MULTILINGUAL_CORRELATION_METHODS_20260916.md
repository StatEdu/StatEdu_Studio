# 추가 상관 방법·잠재상관 다국어 검사

## 변경

Kendall, 점이연, phi, Cramér's V, eta, polyserial, polychoric, tetrachoric의 약어 설명 8개와 잠재상관의 고정 임계값·2단계 점근 표준오차·Fisher-z Wald 추론 주석을 8개 언어에 등록했다. 계산 엔진은 변경하지 않았다.

현재 `latent_correlations` 옵션은 기본 상관 방법을 잠재상관으로 대체하며 별도 잠재상관 표를 추가하지 않는다. 이전 결과 구조의 `result$latent`를 렌더링하는 별도 제목 경로는 이번 실제 실행 검증 범위에 포함하지 않았다.

## 검증

- `scripts/validate_correlation_methods_i18n.R`: 연속·순서·이분·명목 변수가 포함된 실제 120행 자료에서 관측 상관과 잠재상관을 실행했다. 추가 8개 방법 발생을 확인했다.
- 8개 언어에서 본표 셀·제목·주석 영어 동일성, 보조표 언어, 방법 약어 설명, 잠재상관 추론 주석, 번호로 축약된 행렬 축 및 원래 변수 라벨 안내 보존 검사를 통과했다.
- 기존 Pearson/Spearman 다국어 검사 및 `validate_i18n_contract.R` 통과.
- 일본어 실제 두 결과를 하나의 항목으로 묶은 저장 fixture: `tmp/correlation-methods-i18n/entries.rds`. 내보내기 로그: `tmp/correlation-methods-exports.log`.
- 현재·누적 HTML/Word/HWPX/Excel 내용 비교 및 PDF 생성 통과. 실제 PDF 텍스트 검사도 두 경로 모두 통과.

## 남은 범위

상관분석 설정·오류, 추론 불가 안내, 이전 결과 구조의 별도 잠재상관 제목, 도표와 브라우저 언어 왕복을 추가 확인해야 한다. 전용 결과 검증 공백은 16개이며 전체 완료를 의미하지 않는다. 설치본은 만들지 않았다.
