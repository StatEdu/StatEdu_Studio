# 보관된 Electron 1.3.0 패키지 시작 시간

## 결과

보관된 `dist/electron/win-unpacked/StatEdu Studio.exe`의 실제 실행을 4회 측정했다. 첫 성공 실행에서 파일 선택 버튼 사용 가능 확인까지 8.789초, 이후 3회는 6.536 / 6.618 / 6.177초로 중앙값 6.536초였다. 제품 소스나 설치 패키지는 수정하지 않았다.

|구간|첫 성공 실행(초)|후속 3회 중앙값(초)|
|---|---:|---:|
|실행 요청 → createWindow 로그|0.280|0.199|
|startShiny → 포트 준비|5.329|3.463|
|포트 준비 → 문서 로드 완료|2.164|1.606|
|문서 로드 완료 → 사용 가능 확인|0.940|1.179|
|실행 요청 → 사용 가능 확인 전체|8.789|6.536|

구간 중앙값의 합이 전체 중앙값과 반드시 같지는 않는다. createWindow부터 startShiny까지의 짧은 구간도 전체 시간에 포함된다. 후속 실행에서는 R 서버 시작이 가장 큰 구간이며 문서 로드와 초기 UI 준비도 남아 있다. 포트 준비 확인에는 기존 150ms 재시도 간격이 포함된다.

## 버전과 측정 범위

이 결과는 **현재 작업 소스의 측정값이 아니다**. `dist/electron/packaged-source-verification.json`은 2026-09-13 빌드 검증 기록이다. 패키지의 `analysis_crosstabs.R` SHA256은 `3F169D550D458AD5B2BAA55644138F12A74A94559D576364AAE711A7B00D38B5`로 현재 파일과 다르다. 현재 개발용 `packaging/electron/node_modules/electron/dist/electron.exe`는 없다. 새 패키지 빌드를 수행하지 않았다. 이전 최신 소스의 Chrome 시작 측정과 이번 값을 직접 비교해 회귀나 개선율로 해석해서는 안 된다.

측정 대상 SHA256:

- 실행 파일: `675B5CE76D5DDFC51C65B7ADF2DF432363B7A6056421543F95A4AAF5BEDD3EAA`
- `resources/app.asar`: `41F44AB8E94EFCA6E81226CE78870096DB9ECFB671AA69C427116A60E5161EFE`

전용 `output/electron-packaged-startup-20260915/profile` 사용자 설정과 `module-cache`를 사용했다. 첫 성공 실행 전 모듈 캐시 파일은 없었고, 종료 후 결합 소스와 manifest 파일이 생성돼 있었다. 다만 앞선 실패 시도에서 Chromium 프로필 일부가 생성됐으며 OS 디스크 캐시는 비우지 않았다. 따라서 첫 성공 실행을 완전한 cold start로 부르지 않는다. 이후 실행은 동일 프로필과 모듈 캐시를 재사용했다.

PowerShell `Start-Process -WindowStyle Hidden` 호출 직전부터 시간을 잰다. CDP로 실제 Electron 페이지에 연결한 뒤 Shiny 소켓 연결, 파일 input 존재, 파일 버튼의 Playwright trial 클릭 가능 상태, shiny-busy 해제를 기다린다. 실제 파일 선택은 하지 않는다. 숨김 시작 요청과 자동화 연결을 사용한 조건의 관측 시각이며, 사용자에게 보이는 첫 페인트 시각이나 최초 상호작용 가능 시점의 정밀 이벤트 계측은 아니다. Node/Playwright의 연결·관측 비용도 포함한다. 연결 이후 수집한 페이지 오류와 확인 시점의 표시 오류는 네 실행 모두 없었다. 연결 전 오류 전부를 검증한 것은 아니다.

## 실행 환경 문제와 정리

초기 제한 실행 환경에서는 60초 안에 CDP 연결에 성공하지 못했고 시작 로그가 비어 있었다. 이 실패는 시간 비교에서 제외했다. 측정용 메인 프로세스와 그 자식 관계를 확인해 정리한 뒤 일반 실행 환경에서 같은 측정을 재시도하자 네 실행 모두 완료됐다. 원인이 확정되지 않았으므로 제품의 60초 지연으로 보고하지 않는다. 실패 로그는 `run.log`, 성공 로그는 `retry.log`로 분리했다.

성공 실행은 각자 페이지를 닫고 프로세스 종료를 확인했다. 종료 후 StatEdu Studio 프로세스가 남지 않았음을 확인했다. 다른 R 프로세스는 종료하지 않았다.

증거: 해당 output 폴더의 `run.ps1`, `observe.cjs`, `summarize.cjs`, `launch-*.json`, `result-*.json`, `startup-through-*.log`, `stages.json`. 연구 산출물 외 제품 코드 변경은 없다. 현재 `packaging/electron/main.js` SHA256도 시작 시 값 `8D110B9AF4DD79D9F1B27401BCACDFE1BC1FC28D9975013CCBDCBBEF45353032`와 같다.

다음 단계는 현재 소스를 실행하는 Electron 환경을 마련한 뒤 같은 구간을 측정하는 것이다. 이 보관 패키지 수치만으로 최신 코드의 R 로딩이나 렌더러 변경을 결정하지 않는다.
