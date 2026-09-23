audit <- paste(readLines(file.path("docs", "SEM_SCI_GAP_AUDIT_KO.md"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

stopifnot(
  grepl("## 핵심 영역별 판정", audit, fixed = TRUE),
  grepl("지표 영역·포함 근거·내용타당도 절차의 구성개념별 기록", audit, fixed = TRUE),
  grepl("양측 permutation PLS-MGA", audit, fixed = TRUE),
  grepl("대안 처리·MNAR 이탈 민감도 방법과 결론 기록", audit, fixed = TRUE),
  grepl("복합표본·군집·종단", audit, fixed = TRUE),
  grepl("SCI 투고 전 수동 확인", audit, fixed = TRUE)
)

message("SEM SCI gap-audit validation passed.")
