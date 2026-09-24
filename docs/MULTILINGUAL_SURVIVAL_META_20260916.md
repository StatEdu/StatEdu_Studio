# 생존분석·메타분석 다국어 보완 (2026-09-16)

## 이번 변경

- 일본어·중국어·스페인어·프랑스어·독일어·베트남어에 206개 원문 항목의 번역을 추가 또는 보완했다.
- 생존분석 공통 사전: 시간·사건·검열·경쟁사건, Kaplan–Meier/Cox 실행, RMST, 생존함수·누적위험·도표 설정.
- 생존분석 선택 도우미: 연구 목적, 자료 형태, 사건 구조, 시간 정의, 변수 선택. 영어/한국어 분기만 있던 두 설정 함수를 공통 번역 조회 함수에 연결했다.
- 메타분석: 기본 설정, 의존 효과, RVE, 출판편향, 조절변수 입력 안내, 가져오기·검증 알림의 공통 사전을 보완했다.
- 메타분석 효과량 입력: 세 가지 효과 척도와 14가지 보고 형식, 표본 수·평균·SD·SE·CI·사건 빈도 등의 입력 항목을 연결했다.
- 메타분석 파일 선택 버튼·빈 파일 안내를 현지화했다. 긴 다운로드 버튼 문구가 겹치지 않도록 줄바꿈을 적용했다.

표시할 때 외부 번역 서비스를 호출하지 않는다. `statedu_localized_text()`가 `i18n/{언어}.json`의 번역값을 조회한다. 기존 한국어와 영어 문구, 내부 선택값, 입력 범위, 사용자 변수명과 자료값은 유지한다.

## 검증

- `scripts/validate_survival_meta_i18n.R`: 영어·한국어와 6개 추가 언어 통과. 실제 함수의 사전 항목 및 연결한 리터럴을 검사하고, 14개 입력 형식과 생존자료 4개 형태의 control ID/value/min/max/step/selected/multiple 속성이 영어 화면과 동일함을 확인했다.
- `scripts/validate_survival_meta_i18n.cjs`: 6개 언어로 생성한 설정 화면을 Chrome에서 검사했다. 제목·입력 라벨·파일 선택 안내·선택값·사용자 변수명 보존 및 다운로드 버튼 가로 넘침 검사를 통과했다. 정적 설정 화면 검사이며, 전체 분석을 각 언어에서 실행한 검사는 아니다.
- `scripts/validate_multilingual_coverage.R`, `scripts/validate_i18n_contract.R`, `scripts/validate_multilingual_rendering.R` 통과.
- `scripts/validate_survival_multilingual_exports.R`: 실제 Kaplan–Meier 분석과 4종 도표를 사용했다. 일본어 도표 제목을 포함한 화면 내용을 현재/누적 HTML·PDF·Word·HWPX·Excel로 저장하고 표 제목·셀·수치 및 이미지 포함을 검사했다.
- `scripts/validate_multilingual_exports.R`: 기존 회귀·매개/조절·반복측정 진단 fixture의 현재/누적 5종 내보내기 통과.
- `scripts/validate_multilingual_pdf.py`: 기존 fixture 및 생존분석 fixture의 실제 PDF 텍스트와 표지 검사 통과.
- 생산 소스와 번역 JSON의 `git diff --check` 통과.

검증 산출물: `tmp/multilingual/`, `tmp/survival-multilingual/`.

## 남아 있는 범위

전체 다국어 번역이 완료된 상태는 아니다. 생존분석의 사건코드 매핑·추천 사유·사전검사 메시지·상세 진단, 메타분석 결과의 상세 진단·해석 안내, 종단/일반화/로지스틱/구조방정식 등의 상세 문구는 추가 작업이 필요하다. 사전 키가 모두 채워졌다는 검사만으로 소스 전체의 번역 완성을 의미하지 않는다.

기존 결과 표시 규칙에 따른 영문 출판용 주표·주그림과 사용자 데이터 라벨은 이번 번역 대상으로 바꾸지 않았다.

설치본 생성, 패키징, 설치된 앱 변경은 수행하지 않았다.
