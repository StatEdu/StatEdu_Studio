# 새 설치 패키지의 실제 GUI 실행 검증

2026-09-15. 재빌드한 1.3.0의 `dist/electron/win-unpacked/StatEdu Studio.exe`를 별도 임시 프로필로 실행했다. 기존 설치와 사용자 설정을 변경하지 않았다.

## 확인 결과

- 새 임시 프로필의 첫 화면 준비: **3,638ms**.
- 같은 프로필 재실행 및 화면 준비: **2,875ms**.
- 80행 합성 자료를 화면에서 불러와 릿지·라소·엘라스틱넷 분석 실행, 요약 표·검증 결과·이미지 로딩 확인.
- 세 결과를 누적 저장한 뒤 순서 변경·전체 항목 삭제·되돌리기·원래 순서 복구를 수행했다. 각 단계의 결과 HTML이 원본과 정확히 같았다.
- 창을 닫고 같은 프로필로 다시 실행해 결과 3개가 자동 복원되고, ID·순서·HTML 내용이 저장본과 정확히 일치함을 확인했다.
- 두 실행 모두 창 닫기 후 Electron 종료와 R 종료 로그를 확인했다. Electron PID 123796/152616도 종료 후 남아 있지 않았다. Windows의 기존 `taskkill /t /f` 종료 경로에 따라 R 로그는 code=1이며, 정상적인 R 반환 코드 0을 검증했다는 의미는 아니다.
- 첫 데이터 화면과 세 분석의 요약 표 PNG를 시각 검토했다. 검토한 화면/표에서 글자 누락·겹침은 보이지 않았다. 전체 화면 및 모든 분석의 시각 검사를 수행한 것은 아니다.

로딩 시간은 Electron 자동화 실행 시작부터 Shiny 연결·데이터 입력 UI 준비·초기 화면 캡처까지의 단일 관측이다. OS 캐시를 비운 콜드 스타트나 통계적 성능 비교가 아니다. 분석 실행 관측은 릿지 2.754초, 라소 12.745초, 엘라스틱넷 32.817초로, 스모크 테스트 설정에 한정한다. 후선택 추론 등을 포함하는 서로 다른 설정이며 분석 간 속도 우열이나 기존 대비 개선율로 해석하지 않는다.

## 재현과 증거

`scripts/smoke_packaged_isolated.cjs --manage`, 이후 `--restore=D:/Program/Studio/tmp/packaged-smoke-IitOHs`를 실행했다. 번들 Playwright 경로를 `STATEDU_PLAYWRIGHT_MODULE`로 지정했다.

증거 디렉터리: `tmp/packaged-smoke-IitOHs/`.

- `result.json`, `restore-result.json`: 실행·복원·종료 결과.
- `startup.png`, `ridge.png`, `lasso.png`, `elastic_net.png`: 시각 확인 자료.
- 각 분석 HTML, 저장된 HTML, `management-store.json`, `saved-results.json`: 내용/순서 비교 근거.
- `profile/logs/startup.log`: 로딩·R 종료 기록.

설치 파일 SHA256은 이전 검증과 동일한 `19F507607BED125B25F771042A8C0825ABDD35B9C287A5E658C2E6280D3DB182`다. 이번에는 제품 코드 수정이나 설치 파일 재생성이 없었다. 설치 마법사·기존 설치 업그레이드·실제 사용자 프로필 이관·파일 형식별 내보내기·전체 GUI QA·외부 배포는 별도 범위다.
