# 표시 수정 설치본 재빌드

StatEdu Studio 1.3.0 설치본 재빌드 완료. 포함 변경: 회귀 β/Δ 표시, 연속형 기술통계 세로 배치와 열 너비, RM ANOVA F(df1,df2) 문자열 보존. 기존 이력 저장/모델 이관 보완도 유지된다.

전체 설치 회귀 검사, 번들 준비 및 최종 패키지 각각 19개 PLS/PLSc 검사, 모듈 로딩 통과. R/web/HWPX helper 337파일 해시 일치. `scripts/validate_packaged_display_fixes.R`를 최종 패키지의 R로 실행하여 세 표시 수정 반영도 확인했다.

세 변경의 현재·누적 HTML/PDF/Word/HWPX/Excel 검증은 `REGRESSION_GREEK_SYMBOLS_20260915_KO.md`, `DESCRIPTIVE_PORTRAIT_20260915_KO.md`, `RM_ANOVA_DF_DISPLAY_20260915_KO.md` 참조. 해당 문서의 설치본 미반영 상태는 이 빌드로 해소됐다.

설치 파일: `dist/electron/StatEdu_Studio_Setup_1.3.0.exe`, 324,298,570 bytes.

SHA256: `71E2F79319F2A36B11EEBEBCFE39A6ABF4ABDBA9C6FC30DB88F7ABFBD9A8933B`.

로그: `output/display-fixes-installer-20260915/`. 이번 요청은 설치본 생성이며, 현재 PC에 다시 설치하거나 외부에 게시하지 않았다.
