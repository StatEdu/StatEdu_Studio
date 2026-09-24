# KM 집단별 소수 사건 안내 번역

## 변경

집단별 사건 수가 5건 미만일 때 나오는 안내 `Emphasize uncertainty in stratum curves and comparison tests.`가 일본어·중국어·스페인어·프랑스어·독일어·베트남어 화면에서 영어로 남는 것을 실제 결과 패널에서 재현했다. 해당 문구를 8개 언어 사전에 반영하고 기존 한국어 표현을 유지했다.

한국어: 집단별 곡선과 비교검정의 불확실성을 강조하세요.

계산 및 저장 코드는 변경하지 않았다. `Supplementary table: group comparison test`는 호출부가 영어 본표로 명시한 제목이므로 유지했다.

## 검증

- `validate_survival_sparse_km_i18n.R`: 20개 관측치, 두 집단 각각 사건 3건의 자료로 KM 분석을 실제 실행했다. 두 `few_events_in_stratum` 안내 행을 확인하고 8개 언어의 전체 결과 패널 및 안정성 표에서 번역 표시를 검증했다.
- 사용자 집단명 Review/Normality가 근거 문구에 유지되는지 확인했다. 영어 본표 동일성과 원본 결과 객체 보존도 통과했다.
- `validate_i18n_contract.R`: 통과.
- 한국어·일본어 안정성 표의 현재·누적 HTML/PDF/DOCX/HWPX/XLSX 저장 내용 검증 통과. 저장은 캡처 HTML을 사용하며 분석을 재실행하지 않는다.
- PDF 캡처 텍스트 보존 검사 통과. 산출물: `tmp/survival-sparse-km-i18n/exports`.

## 범위

저장 검사는 안정성 표와 제목에 한정한다. 전체 결과의 그림과 다른 설명까지 재검증한 것은 아니다. 문서 앱별 수동 시각 검토와 설치본 빌드는 수행하지 않았다. 기존 매개·조절효과 패널은 제외하며, 생존분석의 다른 안내와 동적 오류는 후속 점검 대상으로 남긴다.
