# 1.3.1 공개판 소스 준비

2026-09-24 사용자 확정: 현재 개발 소스의 다른 변경도 검증 후 포함한다.
메타분석과 동일개체 내 처치 반복측정 분산분석은 공개 메뉴에서 제외한다.
2026-09-24 후속 사용자 요청: CFA 등 최신 수정과 최소 버전 정책을 포함하여 Windows 1.3.1 공개 설치본을 제작한다. 업로드·게시와 서버 최소 버전 정책 변경은 별도다.

## 기준과 버전

- 공개 기준: `v1.3.0` (`84fbbda`, 실제 공개 설치본에서 복원·검증된 소스).
- 기존 공개 설치본 SHA-256: `DEFE844B8A46CCD416553322818FE724441A81681C26FAA9C4EB02A2092C5E10`.
- 준비 소스 `VERSION`: `1.3.1`. 개발판 `VERSION_DEV`: `1.3.1-dev` 유지.
- 이미 배포된 버전과 인용 정보는 1.3.0으로 유지한다. README의 다음 공개 버전과 8개 언어 변경 이력에서 준비 상태를 표시한다.
- 현재 작업 폴더의 개발 변경을 포함한다. 따로 만든 `public-1-3-1` 작업 폴더는 공개 1.3.0 비교 기준이며 이번 배포 소스가 아니다.

## 포함 범위

1. 시작 시 최소 지원 버전 정책 확인, 무료판·Pro판 정책 분리, 오프라인 캐시.
2. SEM 점수·문항묶음 워크플로와 관련 캔버스·다국어 개선.
3. 복합표본 다항·서열 로지스틱 분석과 진단·표시·설명 개선.
4. 빈도표 숫자 줄바꿈·열 너비와 IQR 표시 개선.
5. 누적 결과 화면·저장 내용의 추가 시각 표시 제거. 이력 내부 시각은 보존.
6. 여러 종속변수 t 검정 요약, 로지스틱 신뢰구간 머리글, 실제 데이터 경로를 우선하는 기본 저장 폴더 개선.
7. CFA·SEM 모형 적합도 세로 출력, 보충 적합도 약어 주석과 다국어 GFI 열 제목. 화면의 동일 내용을 현재·누적 HTML·PDF·Word·HWPX·Excel에 보존한다.

공개판에서 제외하는 항목은 `meta`, `one_group_rm_anova`다. 혼합 반복측정 분석, 기존 대응표본 비교 등 다른 공개 메뉴는 유지한다. 개발판의 실험 기능은 제거하지 않는다.

## 시작 속도와 운영

최소 버전 조회 실측은 정상 연결 0.08~0.30초(5회), 응답 지연 시 캐시 유무에 따라 8.00~8.13초였다. 현재 구현은 동기 조회이며 백그라운드 방식으로 변경하지 않았다. 배포 전 시작 지연의 수용 여부 또는 개선을 결정해야 한다.

서버의 최소 버전 정책은 변경하지 않았다. 기존 기능 없는 1.3.0 설치본에는 소급 적용되지 않는다. 이 기능은 라이선스 구매 확인 기능이 아니다.

## 소스 검증 결과 (2026-09-24)

- `validate_version_metadata.R`: 준비 버전 1.3.1과 실제 공개 1.3.0 인용 정보 구분 통과.
- `validate_public_131_scope.R`: 8개 언어에서 두 공개 메뉴 제외, 나머지 반복측정 메뉴 및 개발판 메뉴 유지 통과.
- `validate_minimum_version_policy.R`: 버전 경계, 정책 분리, 오프라인·캐시·철회 및 분석 서버 차단 통과.
- `validate_canvas_scores.R --exports`: 점수·문항묶음 계산과 현재/누적 HTML·PDF·Word·HWPX·Excel 저장 통과.
- `validate_complex_categorical_logistic.R`, `validate_complex_logistic_case_flow.R`, `validate_complex_logistic_collinearity.R`, `validate_complex_logistic_sparse_screen.R`, `validate_complex_logistic_reporting.R`, `validate_complex_logistic_table_roles.R`: 수치 비교·진단·다국어·표 역할 검증 통과.
- `validate_complex_categorical_exports.R`: 현재/누적 다섯 형식 생성, 셀·제목·주석과 스냅샷 보존 통과. `validate_complex_categorical_pdf.py`는 PDF 12/17쪽의 텍스트 보존 통과.
- `validate_frequency_numeric_widths.R` 및 `.cjs`: 현재/누적 다섯 형식과 브라우저 숫자 너비·IQR 두 줄 표시 검증 통과.
- `validate_result_no_entry_time.R`: 현재/누적 다섯 형식에서 추가 시각 표시 제거와 내부 이력 보존 통과.
- `validate_loaded_language_and_paths.R`: 다국어 문자 인코딩, 실제 데이터 폴더 우선 및 6개 저장 대화상자 기본 경로 검증 통과.
- `validate_public_131_result_exports.R`: 다중 종속변수 t 검정과 로지스틱 신뢰구간 표의 현재/누적 다섯 형식, 셀·주석·원본 스냅샷 보존 통과. `validate_public_131_pdf.py`에서 현재 PDF 4/5쪽 및 누적 PDF 8쪽의 셀·주석 텍스트 보존 통과.

## 설치본 단계에서 남은 일

- 최종 소스 확정 후 Windows 설치본 빌드, 패키지 소스 일치 검사와 실제 실행 점검.
- macOS는 독립 패키징에서 별도 빌드·실행 점검. Windows 런처를 복사하지 않는다.
- 설치본 생성과 게시, 웹사이트·업데이트 manifest 변경은 이후 사용자 요청에 따라 진행한다.
- 출력 용지 표준화와 CFA·SEM 모형 저장 호환 계획은 이번 준비 작업에 포함하지 않는다.
