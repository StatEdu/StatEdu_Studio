# Centralized Korean label dictionary for StatEdu Studio.
# Korean text stored as UTF-8 strings (sourced with encoding = "UTF-8").
# Use statedu_ko("key") for standalone Korean-only strings.
# For bilingual strings with paired English, continue using statedu_text().

statedu_ko <- function(key) {
  labels <- c(
    # About / document panel labels
    doc_overview_title         = "개요",
    doc_overview_subtitle      = "프로젝트 범위, 현재 버전, 검증, 인용 정보를 제공합니다.",
    doc_user_guide_title       = "사용자 가이드",
    doc_user_guide_subtitle    = "데이터 불러오기, 변수 선택, 분석 실행, 결과 저장 절차를 안내합니다.",
    doc_analyses_title         = "분석",
    doc_analyses_subtitle      = "StatEdu Studio 1.0의 분석 메뉴, 통계 출력, 표, 내보내기 범위를 정리합니다.",
    doc_method_notes_title     = "방법론 노트",
    doc_method_notes_subtitle  = "분석 방법 선택, 가정, 경고, 결과 해석에 대한 노트를 제공합니다.",
    doc_validation_title       = "검증",
    doc_validation_subtitle    = "공개 1.0 계산과 자동 판단 경로의 기준 비교를 제공합니다.",
    doc_version_history_title  = "버전 기록",
    doc_version_history_subtitle = "릴리스 노트와 버전 기록을 제공합니다.",

    # Language selector
    lang_korean = "한국어",

    # Analysis group menu labels (used in app_static_language_labels_script)
    group_descriptives   = "기술통계 / 표",
    group_comparisons    = "집단 비교",
    group_nonparametric  = "비모수 검정",
    group_association    = "연관 / 측정",
    group_regression     = "회귀 / 모형",
    group_longitudinal   = "종단 / 패널",
    group_study_design   = "연구 설계 / 정밀도",

    # Calculator menu labels
    calc_metabolic_syndrome = "대사증후군",
    calc_framingham_risk    = "Framingham 위험도",
    calc_metabolic_severity = "대사증후군 중증도"
  )
  value <- labels[[key]]
  if (is.null(value)) key else value
}
