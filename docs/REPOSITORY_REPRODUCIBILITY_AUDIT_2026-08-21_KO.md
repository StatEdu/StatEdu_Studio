# 저장소 재현성 감사 기록

검토일: 2026-08-21

기준 GitHub 커밋: `2e69ac8`

## 발견사항

1. `scripts/validate_pls_external_comparator.R`와 외부 PLS handoff가
   `sample/HolzingerSwineford1939.csv`를 요구했지만, `sample/` 전체 ignore 규칙으로 인해
   깨끗한 GitHub 클론에는 해당 자료가 없었다. 로컬 작업 폴더에서는 통과하지만 새 클론의
   `scripts/validate_stabilization.ps1 -Full`은 실패하는 저장소 재현성 결함이었다.
2. `scripts/validate_longitudinal.R`의 `geepack::ohio` 참조 검증은 별도 CSV가 있을 때만 실행되어,
   깨끗한 클론에서는 생략 사실을 표시하지 않고 통과할 수 있었다.

## 보완

- `sample/HolzingerSwineford1939.csv`만 선택적으로 Git 추적 대상으로 허용하고 다른 개인 sample
  파일은 계속 제외했다.
- CSV 체크아웃을 UTF-8/LF로 고정하고 SHA-256
  `140519C3E46920B38191D4CD9415FA33DDC40633294E6D3E30AF82242F7B6204`를 계약으로 설정했다.
- 자료 출처를 `lavaan::HolzingerSwineford1939`로, 생성 절차를
  `utils::write.csv(lavaan::HolzingerSwineford1939, row.names = FALSE)` 후 LF 정규화로 기록했다.
- benchmark manifest에 `lavaan` 버전, 자료 출처, 생성 명령과 실제 자료 해시를 추가했다.
- `scripts/validate_repository_fixtures.R`를 전체 안정화 검증에 등록했다. 필수 fixture의 존재,
  Git 추적 여부, SHA-256, 열 순서, 사례 수와 x1-x9 원값을 검사한다.
- 종단 Ohio 검증은 별도 파일 조건을 제거하고 설치가 확인된 `geepack::ohio`를 직접 불러 반드시
  실행하도록 변경했다.

## 독립 재검증 결과

- 원 작업 폴더에서 `scripts/validate_stabilization.ps1 -Full`: 통과
- 수정 파일만 임시 커밋한 저장소를 다시 새로 클론한 후 동일 `-Full`: 통과
- PLS/PLSc 외부 비교기, 종단·패널, SEM/CFA, 생존, 매개·조절, 선형·위계 및 로지스틱 회귀,
  66개 분석 기준 비교: 통과
- `cSEM` package-level 비교: 시스템·배포 런타임과 분리된 임시 라이브러리에 CRAN `cSEM 0.6.1`을
  설치해 다시 실행했으며 SRMR, d_G, d_ULS가 허용오차 `1e-12`에서 일치했다.

## 해석 경계

이 검증은 Git에 기록된 코드와 fixture만으로 자동 검증을 재현할 수 있음을 뜻한다. 실제
SmartPLS/ADANCO 버전별 외부 실행, 최종 설치본 수동 QA, 설치본 해시와 공개 승인을 대신하지 않는다.
