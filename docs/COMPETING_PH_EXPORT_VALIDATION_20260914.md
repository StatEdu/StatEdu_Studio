# 20공변량 원인별 Cox·Fine–Gray 및 저장 검증 — 2026-09-14

## 결과

원인별 Cox와 Fine–Gray를 실제로 활성화한 경쟁위험 분석에서도 최근 PH 그래프 저장 수정이 정상 동작했다. 운영 코드를 추가 변경하지 않았으며, 재발 방지 검사와 과거 검증 범위 기록을 보완했다.

600행, 정규 공변량 20개, 세 집단, 사건 코드 0/1/2의 합성 자료를 시드 44로 만들었다. `prepare_competing_risk_result(..., regression="both")`를 명시했다. 원인별 Cox의 PH 잔차 열 수와 Fine–Gray 계수 수가 실제로 20개인지 검사했다.

## 이전 검증 범위 정정

과거 Cox 성능 보고서 8건의 `integration.R`은 공변량을 전달했지만 `regression`을 지정하지 않았다. 기본값은 `"none"`이므로 당시 경쟁위험 비교는 CIF/Gray 경로만 검사했다. 이를 Fine–Gray 전체 결과 검증이라고 기술한 것은 잘못이므로 해당 보고서 상단에 정정을 추가했다. 기존 직접 함수 검사와 일반 Cox 검사 결과는 이 정정과 별개다.

이번에는 `output/cox-finite-rows-20260914/baseline.R`의 초기 Cox 최적화 전 모듈과 현재 `R/analysis_survival.R`의 모든 함수를 각각 교체해 두 회귀를 활성화한 전체 분석을 비교했다. 원인별 Cox 적합의 formula/terms 환경 참조만 정규화한 뒤 전체 결과·경고/메시지·난수 상태가 `identical(..., num.eq=FALSE)`로 일치했다. 수치에는 허용오차를 적용하지 않았다. 이번 한 자료의 일치가 모든 입력에 대한 증명은 아니다.

## 화면·그림 검증

- PH 높이는 20항/두 열/10행에 대해 2400px이다. 수정 전후 화면 HTML은 해당 높이를 제외하고 동일했다.
- 저장 및 그림 생성 전후 분석 객체의 직렬화 바이트가 동일했다.
- CIF 및 위험집단 표, 원인별 Cox PH, Martingale 함수형태, Fine–Gray 잔차의 개별 PNG 네 개가 생성됐다. 개별 파일 경로는 110dpi로 검사했으며 PH 그림의 실제 크기는 792×2400px이었다.
- 실제 저장 HTML·Excel 생성이 성공했다.

## 현재·누적 결과의 다섯 형식

현재 결과 1개와 누적 결과 2개에 대해 HTML·Word·Excel·PDF·HWPX 총 10개 파일 생성이 모두 성공했다. HTML·Word·HWPX의 네 그림 데이터는 원본 저장 HTML의 그림 SHA-256과 일치했다. 그림 참조 수는 현재 4개, 누적 8개였다.

현재 PDF는 23쪽이며 PH 그림은 9–10쪽에, 누적 PDF는 46쪽이며 PH 그림은 9–10쪽 및 32–33쪽에 포함됐다. PH 원본 이미지 해상도는 5455×13091px이었다. 현재 PDF의 해당 두 페이지를 렌더링해 20개 패널이 빠짐없이 표시되는 것을 확인했다. Word/HWPX의 앱 화면 렌더링까지 수행하지는 않았다. Excel은 생성 성공을 확인했으며 이번에 모든 셀을 외부 통계 프로그램과 대조한 것은 아니다.

PDF·HWPX는 설치된 Chrome과 한글을 실행할 수 있는 검증 환경에서 생성했다. 검증 R 프로세스에 상속된 PATH/Path 중복만 정리했으며 사용자 환경과 운영 변환 스크립트는 변경하지 않았다.

## 재발 방지와 자료

`scripts/validate_survival_ph_height.R`의 원인별 Cox 검사도 가상의 PH 객체 대신 실제 `regression="both"` 분석을 실행하도록 보완했다. 20개 PH 항과 Fine–Gray 계수, 실제 그림 생성 및 분석 객체 보존을 검사하며 통과했다.

`output/competing-ph-height-20260914/`에 `verify.R`, `verify_models.R`, `exports.R`, `inspect_exports.py`, 전체 결과 RDS, 개별 그림, 다섯 형식 파일, `export-status.csv`, `artifact-validation.json`과 PDF 페이지 렌더링을 보관했다. 네 운영 소스 파일의 해시가 검사 전후 동일함을 확인했다.

이번에는 실행 시간 개선을 측정하거나 주장하지 않았다. 설치 파일은 재빌드하지 않았다.
