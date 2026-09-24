# 종단분석 민감도 상태·실험적 GEE 안내 번역

## 변경

- `Not needed`, `Strategy`, `R package warnings`와 실험적 SPSS 호환 GEE 설명 2개, 총 5개 문구를 8개 언어 사전에 반영했다.
- SPSS 호환 모드의 자체 추정량 사용·비기본 분석 안내, ADJUSTCORR=YES·N-p·강건 샌드위치 표준오차 및 관측 가중치 미지원 설명을 번역했다.
- 추정 알고리즘이나 기본 분석 설정은 변경하지 않았다.

## 검증

- `scripts/validate_longitudinal_status_guidance_i18n.R`: 결측이 없는 200행/40명 자료로 MI 불필요 분기를 실행하고 실험적 비구조화 GEE를 실제 적합했다. 수정 전 미번역을 재현하고 수정 후 상태·방법·설명과 경고 표제의 8개 언어 검증을 통과했다.
- 원본 표, 수치·계수 열과 외부 경고 예문의 내용 보존을 확인했다. 외부 경고는 합성 예문을 렌더링한 검사이며 실제 패키지 경고 발생을 유도한 검사는 아니다.
- `scripts/validate_i18n_contract.R`: 통과.
- `scripts/validate_longitudinal_sensitivity_failures_i18n.R`: 오류 표 검사 48건, 지표 표 검사 8건 통과.
- `scripts/validate_longitudinal_count_exports.R`: 캡처한 화면 HTML을 사용해 한국어·일본어 현재·누적 결과의 HTML/PDF/DOCX/HWPX/XLSX 저장을 검증했다. 저장 중 분석을 재실행하지 않았다. 8개 언어에서 영어 본표 내용이 동일함을 확인했다.
- `scripts/validate_longitudinal_error_pdf.py`: 한국어·일본어 현재 결과 각 4쪽, 누적 결과 각 7쪽의 모든 캡처 텍스트 보존 검사 통과.
- 산출물: `tmp/longitudinal-status-guidance-i18n/exports`.

## 범위

검증 범위는 MI 불필요 상태, 실험적 GEE의 두 안내 및 패키지 경고 표제다. 외부 패키지 경고 자체는 번역하지 않는다. 전체 다국어 작업 완료를 의미하지 않으며, 남은 모형별 조합 안내 등은 계속 확인해야 한다. 문서 앱별 수동 시각 검토와 설치파일 빌드는 수행하지 않았다.
