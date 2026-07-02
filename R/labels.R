# Centralized Korean label dictionary for StatEdu Studio.
# Korean text stored as UTF-8 strings (sourced with encoding = "UTF-8").
# Use statedu_ko("key") for standalone Korean-only strings.
# For bilingual strings with paired English, continue using statedu_text().

statedu_ko <- function(key) {
  labels <- c(
    # About / document panel labels
    doc_overview_title         = "\uAC1C\uC694",
    doc_overview_subtitle      = "\uD504\uB85C\uC81D\uD2B8 \uBC94\uC704, \uD604\uC7AC \uBC84\uC804, \uAC80\uC99D, \uC778\uC6A9 \uC815\uBCF4\uB97C \uC81C\uACF5\uD569\uB2C8\uB2E4.",
    doc_user_guide_title       = "\uC0AC\uC6A9\uC790 \uAC00\uC774\uB4DC",
    doc_user_guide_subtitle    = "\uB370\uC774\uD130 \uBD88\uB7EC\uC624\uAE30, \uBCC0\uC218 \uC120\uD0DD, \uBD84\uC11D \uC2E4\uD589, \uACB0\uACFC \uC800\uC7A5 \uC808\uCC28\uB97C \uC548\uB0B4\uD569\uB2C8\uB2E4.",
    doc_analyses_title         = "\uBD84\uC11D",
    doc_analyses_subtitle      = "StatEdu Studio 1.0\uC758 \uBD84\uC11D \uBA54\uB274, \uD1B5\uACC4 \uCD9C\uB825, \uD45C, \uB0B4\uBCF4\uB0B4\uAE30 \uBC94\uC704\uB97C \uC815\uB9AC\uD569\uB2C8\uB2E4.",
    doc_method_notes_title     = "\uBC29\uBC95\uB860 \uB178\uD2B8",
    doc_method_notes_subtitle  = "\uBD84\uC11D \uBC29\uBC95 \uC120\uD0DD, \uAC00\uC815, \uACBD\uACE0, \uACB0\uACFC \uD574\uC11D\uC5D0 \uB300\uD55C \uB178\uD2B8\uB97C \uC81C\uACF5\uD569\uB2C8\uB2E4.",
    doc_validation_title       = "\uAC80\uC99D",
    doc_validation_subtitle    = "\uACF5\uAC1C 1.0 \uACC4\uC0B0\uACFC \uC790\uB3D9 \uD310\uB2E8 \uACBD\uB85C\uC758 \uAE30\uC900 \uBE44\uAD50\uB97C \uC81C\uACF5\uD569\uB2C8\uB2E4.",
    doc_version_history_title  = "\uBC84\uC804 \uAE30\uB85D",
    doc_version_history_subtitle = "\uB9B4\uB9AC\uC2A4 \uB178\uD2B8\uC640 \uBC84\uC804 \uAE30\uB85D\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4.",

    # Language selector
    lang_korean = "\uD55C\uAD6D\uC5B4",

    # Analysis group menu labels (used in app_static_language_labels_script)
    group_descriptives   = "\uAE30\uC220\uD1B5\uACC4 / \uD45C",
    group_comparisons    = "\uC9D1\uB2E8 \uBE44\uAD50",
    group_nonparametric  = "\uBE44\uBAA8\uC218 \uAC80\uC815",
    group_association    = "\uC5F0\uAD00 / \uCE21\uC815",
    group_regression     = "\uD68C\uADC0 / \uBAA8\uD615",
    group_longitudinal   = "\uC885\uB2E8 / \uD328\uB110",
    group_study_design   = "\uC5F0\uAD6C \uC124\uACC4 / \uC815\uBC00\uB3C4",

    # Calculator menu labels
    calc_metabolic_syndrome = "\uB300\uC0AC\uC99D\uD6C4\uAD70",
    calc_framingham_risk    = "Framingham \uC704\uD5D8\uB3C4",
    calc_metabolic_severity = "\uB300\uC0AC\uC99D\uD6C4\uAD70 \uC911\uC99D\uB3C4"
  )
  value <- labels[[key]]
  if (is.null(value)) key else value
}
