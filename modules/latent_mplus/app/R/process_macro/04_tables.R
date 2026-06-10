T0_TABLES <- Sys.time()

log_step_start("TABLES", "04_tables.R")
log_info("Reloading process model outputs ...")

`%||%` <- function(x, y) if (is.null(x)) y else x

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Package 'openxlsx' is required for process_macro tables.", call. = FALSE)
}

PROCESS_SETTINGS <- load_step_rds("PROCESS_SETTINGS", dir_rds = DIR_RDS, required = TRUE)
PROCESS_MODEL_RESULTS <- load_step_rds("PROCESS_MODEL_RESULTS", dir_rds = DIR_RDS, default = data.frame())
PROCESS_MODEL_SUMMARY <- load_step_rds("PROCESS_MODEL_SUMMARY", dir_rds = DIR_RDS, default = data.frame())
PROCESS_INDIRECT_RESULTS <- load_step_rds("PROCESS_INDIRECT_RESULTS", dir_rds = DIR_RDS, default = data.frame())
PROCESS_CONDITIONAL_EFFECTS <- load_step_rds("PROCESS_CONDITIONAL_EFFECTS", dir_rds = DIR_RDS, default = data.frame())
MODEL_RUN_SUMMARY <- load_step_rds("MODEL_RUN_SUMMARY", dir_rds = DIR_RDS, default = list())

fmt2_pm <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- ifelse(is.na(x), "", sprintf("%.2f", x))
  out <- sub("^0\\.", ".", out)
  out <- sub("^-0\\.", "-.", out)
  out
}

fmt3_pm <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- ifelse(is.na(x), "", sprintf("%.3f", x))
  out <- sub("^0\\.", ".", out)
  out <- sub("^-0\\.", "-.", out)
  out
}

fmt_p_pm <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- ifelse(is.na(x), "", ifelse(x < .001, "<.001", sprintf("%.3f", x)))
  out <- sub("^0\\.", ".", out)
  out <- sub("^-0\\.", "-.", out)
  out
}

sig_mark_pm <- function(p) {
  p <- suppressWarnings(as.numeric(p))
  out <- rep("", length(p))
  out[!is.na(p) & p < .001] <- "***"
  out[!is.na(p) & p >= .001 & p < .01] <- "**"
  out[!is.na(p) & p >= .01 & p < .05] <- "*"
  out
}

lookup_label_pm <- function(x) {
  x <- as.character(x)[1]
  if (!nzchar(x) || is.na(x)) return("")
  if (exists("lookup_label", mode = "function")) {
    return(lookup_label(x))
  }

  hit_res <- PROCESS_MODEL_RESULTS[
    as.character(PROCESS_MODEL_RESULTS$mediator %||% "") == x |
      as.character(PROCESS_MODEL_RESULTS$x_var %||% "") == x |
      as.character(PROCESS_MODEL_RESULTS$moderator %||% "") == x |
      as.character(PROCESS_MODEL_RESULTS$target_outcome %||% "") == x |
      as.character(PROCESS_MODEL_RESULTS$outcome %||% "") == x,
    ,
    drop = FALSE
  ]

  label_candidates <- c(
    hit_res$mediator_label %||% character(0),
    hit_res$x_label %||% character(0),
    hit_res$moderator_label %||% character(0),
    hit_res$target_outcome_label %||% character(0),
    hit_res$outcome_label %||% character(0)
  )
  label_candidates <- unique(label_candidates[nzchar(label_candidates)])
  if (length(label_candidates) > 0) label_candidates[1] else x
}

blank_repeat <- function(x) {
  x <- as.character(x)
  if (length(x) <= 1) return(x)
  x[duplicated(x)] <- ""
  x
}

compose_model_summary_text <- function(ss) {
  bits <- character(0)
  if (!is.na(ss$r2[1])) bits <- c(bits, paste0("R2 = ", fmt3_pm(ss$r2[1])))
  if (!is.na(ss$f_value[1])) {
    f_txt <- paste0("F = ", fmt3_pm(ss$f_value[1]))
    if (!is.na(ss$p[1])) {
      p_txt <- fmt_p_pm(ss$p[1])
      if (nzchar(p_txt)) {
        p_label <- if (startsWith(p_txt, "<")) paste0("p", p_txt) else paste0("p=", p_txt)
        f_txt <- paste0(f_txt, " (", p_label, ")")
      }
    }
    bits <- c(bits, f_txt)
  }
  paste(bits, collapse = ", ")
}

build_T1 <- function(df, summary_df = data.frame()) {
  # T1 is intended as a manuscript-ready regression table.
  # PROCESS-style path shorthand columns such as a / b / c / d are
  # intentionally not printed here; coefficients are shown directly.
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(data.frame(
      Effect = character(),
      Variable = character(),
      Category = character(),
      B = character(),
      SE = character(),
      t = character(),
      LLCI = character(),
      ULCI = character(),
      p = character(),
      sig = character(),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!"variable_label" %in% names(df)) df$variable_label <- df$term_label %||% ""
  if (!"category_label" %in% names(df)) df$category_label <- ""
  if (!"reference_label" %in% names(df)) df$reference_label <- ""
  if (!"effect_type" %in% names(df)) df$effect_type <- df$effect %||% ""
  if (!"model_component" %in% names(df)) df$model_component <- df$effect %||% ""

  build_coef_block <- function(block_df) {
    out_rows <- list()
    idx <- 1L
    i <- 1L
    while (i <= nrow(block_df)) {
      row_i <- block_df[i, , drop = FALSE]
      is_cat_cov <- identical(as.character(row_i$effect_type[1]), "Categorical covariate") &&
        nzchar(as.character(row_i$reference_label[1] %||% ""))

      if (is_cat_cov) {
        var_i <- as.character(row_i$variable_label[1] %||% row_i$term_label[1] %||% "")
        ref_i <- as.character(row_i$reference_label[1] %||% "")
        out_rows[[idx]] <- data.frame(
          Effect = as.character(row_i$effect[1] %||% ""),
          Variable = var_i,
          Category = ref_i,
          B = "reference",
          SE = "",
          t = "",
          LLCI = "",
          ULCI = "",
          p = "",
          sig = "",
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        idx <- idx + 1L

        while (i <= nrow(block_df)) {
          row_j <- block_df[i, , drop = FALSE]
          same_group <- identical(as.character(row_j$effect_type[1]), "Categorical covariate") &&
            identical(as.character(row_j$variable_label[1] %||% ""), var_i) &&
            identical(as.character(row_j$reference_label[1] %||% ""), ref_i)
          if (!same_group) break

          out_rows[[idx]] <- data.frame(
            Effect = as.character(row_j$effect[1] %||% ""),
            Variable = as.character(row_j$variable_label[1] %||% row_j$term_label[1] %||% ""),
            Category = as.character(row_j$category_label[1] %||% ""),
            B = fmt2_pm(row_j$estimate[1]),
            SE = fmt2_pm(row_j$se[1]),
            t = fmt2_pm(row_j$t_value[1]),
            LLCI = fmt2_pm(row_j$llci[1]),
            ULCI = fmt2_pm(row_j$ulci[1]),
            p = fmt_p_pm(row_j$p[1]),
            sig = as.character(row_j$sig[1] %||% sig_mark_pm(row_j$p[1])),
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
          idx <- idx + 1L
          i <- i + 1L
        }
        next
      }

      out_rows[[idx]] <- data.frame(
        Effect = as.character(row_i$effect[1] %||% ""),
        Variable = as.character(row_i$variable_label[1] %||% row_i$term_label[1] %||% ""),
        Category = as.character(row_i$category_label[1] %||% ""),
        B = fmt2_pm(row_i$estimate[1]),
        SE = fmt2_pm(row_i$se[1]),
        t = fmt2_pm(row_i$t_value[1]),
        LLCI = fmt2_pm(row_i$llci[1]),
        ULCI = fmt2_pm(row_i$ulci[1]),
        p = fmt_p_pm(row_i$p[1]),
        sig = as.character(row_i$sig[1] %||% sig_mark_pm(row_i$p[1])),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      idx <- idx + 1L
      i <- i + 1L
    }

    out <- do.call(rbind, out_rows)
    out$Effect <- blank_repeat(out$Effect)
    out$Variable <- blank_repeat(out$Variable)
    out
  }

  out <- build_coef_block(df)

  if (is.data.frame(summary_df) && nrow(summary_df) > 0) {
    summary_df$summary_text <- vapply(seq_len(nrow(summary_df)), function(i) compose_model_summary_text(summary_df[i, , drop = FALSE]), character(1))
    if (PROCESS_SETTINGS$process_model != 4L) {
      summary_df <- summary_df[nzchar(summary_df$summary_text), , drop = FALSE]
    }
  }

  if ((isTRUE(PROCESS_SETTINGS$custom_model_enabled) || PROCESS_SETTINGS$process_model %in% c(4L, 5L, 6L, 7L, 8L, 14L, 15L, 58L, 59L)) && is.data.frame(summary_df) && nrow(summary_df) > 0) {
    if ("analysis_key" %in% names(df) && any(nzchar(as.character(df$analysis_key)))) {
      df$combo_key <- as.character(df$analysis_key)
    } else if ("table_block_key" %in% names(df) && any(nzchar(as.character(df$table_block_key)))) {
      df$combo_key <- as.character(df$table_block_key)
    } else {
      df$combo_key <- paste(
        df$target_outcome_label %||% df$outcome_label %||% "",
        df$x_label %||% df$x_var %||% "",
        df$mediator_label %||% df$mediator %||% "",
        sep = " | "
      )
    }
    if ("analysis_key" %in% names(summary_df) && any(nzchar(as.character(summary_df$analysis_key)))) {
      summary_df$combo_key <- as.character(summary_df$analysis_key)
    } else if ("table_block_key" %in% names(summary_df) && any(nzchar(as.character(summary_df$table_block_key)))) {
      summary_df$combo_key <- as.character(summary_df$table_block_key)
    } else {
      summary_df$combo_key <- paste(
        summary_df$target_outcome_label %||% summary_df$outcome_label %||% "",
        summary_df$x_label %||% summary_df$x_var %||% "",
        summary_df$mediator_label %||% summary_df$mediator %||% "",
        sep = " | "
      )
    }

    combo_order <- unique(as.character(df$combo_key))
    component_order_default <- c("Mediator model", "Outcome model")
    out_rows <- list()
    idx <- 1L
    empty_row <- data.frame(
      Effect = "",
      Variable = "",
      Category = "",
      B = "",
      SE = "",
      t = "",
      LLCI = "",
      ULCI = "",
      p = "",
      sig = "",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    for (cc in seq_along(combo_order)) {
      combo_k <- combo_order[cc]
      df_combo <- df[as.character(df$combo_key) == combo_k, , drop = FALSE]
      if (!nrow(df_combo)) next

      component_order <- unique(c(component_order_default, as.character(df_combo$model_component)))
      component_order <- component_order[component_order %in% unique(as.character(df_combo$model_component))]
      mediator_block_orders <- unique(suppressWarnings(as.integer(df_combo$block_order[as.character(df_combo$model_component) == "Mediator model"])))
      mediator_block_orders <- mediator_block_orders[!is.na(mediator_block_orders)]
      n_mediator_blocks <- length(mediator_block_orders)

      for (k in seq_along(component_order)) {
        comp_k <- component_order[k]
        block_df <- df_combo[as.character(df_combo$model_component) == comp_k, , drop = FALSE]
        if (!nrow(block_df)) next
        is_model6 <- isTRUE(suppressWarnings(as.numeric(PROCESS_SETTINGS$process_model)) == 6)
        is_model4 <- isTRUE(suppressWarnings(as.numeric(PROCESS_SETTINGS$process_model)) == 4)
        is_model5 <- isTRUE(suppressWarnings(as.numeric(PROCESS_SETTINGS$process_model)) == 5)
        is_model7 <- isTRUE(suppressWarnings(as.numeric(PROCESS_SETTINGS$process_model)) == 7)
        is_model8 <- isTRUE(suppressWarnings(as.numeric(PROCESS_SETTINGS$process_model)) == 8)
        is_model14 <- isTRUE(suppressWarnings(as.numeric(PROCESS_SETTINGS$process_model)) == 14)
        is_model15 <- isTRUE(suppressWarnings(as.numeric(PROCESS_SETTINGS$process_model)) == 15)
        is_model58 <- isTRUE(suppressWarnings(as.numeric(PROCESS_SETTINGS$process_model)) == 58)
        is_model59 <- isTRUE(suppressWarnings(as.numeric(PROCESS_SETTINGS$process_model)) == 59)
        is_custom_model <- isTRUE(PROCESS_SETTINGS$custom_model_enabled)
        block_keys <- if (identical(comp_k, "Mediator model") && n_mediator_blocks > 1L) {
          as.character(sort(unique(suppressWarnings(as.integer(block_df$block_order)))))
        } else {
          "ALL"
        }

        for (bk in seq_along(block_keys)) {
          block_key_i <- block_keys[bk]
          block_df_i <- if (!identical(block_key_i, "ALL")) {
            block_df[suppressWarnings(as.integer(block_df$block_order)) == as.integer(block_key_i), , drop = FALSE]
          } else {
            block_df
          }
          if (!nrow(block_df_i)) next

          block_order_i <- suppressWarnings(as.integer(block_df_i$block_order[1] %||% NA_integer_))
          mediator_label_i <- as.character(block_df_i$mediator_label[1] %||% block_df_i$mediator[1] %||% "")
          outcome_label_i <- as.character(block_df_i$target_outcome_label[1] %||% block_df_i$outcome_label[1] %||% "")

          block_label <- if (identical(comp_k, "Outcome model")) {
            if (is_custom_model) {
              step_outcome <- max(df_combo$block_order, na.rm = TRUE)
              if (nzchar(outcome_label_i)) paste0("Step ", step_outcome, " (", outcome_label_i, ")") else paste0("Step ", step_outcome)
            } else if (is_model6) {
              step_outcome <- max(df_combo$block_order, na.rm = TRUE)
              if (nzchar(outcome_label_i)) paste0("Step ", step_outcome, " (", outcome_label_i, ")") else paste0("Step ", step_outcome)
            } else if (is_model4) {
              if (nzchar(outcome_label_i)) paste0("Step 2 (", outcome_label_i, ")") else "Step 2"
            } else if ((is_model7 || is_model8 || is_model14 || is_model15 || is_model58 || is_model59) && n_mediator_blocks > 1L && nzchar(outcome_label_i)) {
              paste0("Step 2\n(", outcome_label_i, ")")
            } else if ((is_model7 || is_model8 || is_model14 || is_model15 || is_model58 || is_model59) && n_mediator_blocks > 1L) {
              "Step 2"
            } else if ((is_model7 || is_model8 || is_model14 || is_model15 || is_model58 || is_model59) && nzchar(outcome_label_i)) {
              paste0("Step 2\n(", outcome_label_i, ")")
            } else if (is_model7 || is_model8 || is_model14 || is_model15 || is_model58 || is_model59) {
              "Step 2"
            } else if (is_model5 && n_mediator_blocks > 1L && nzchar(outcome_label_i)) {
              paste0("Step 2\n(", outcome_label_i, ")")
            } else if (is_model5 && n_mediator_blocks > 1L) {
              "Step 2"
            } else if (is_model5 && nzchar(outcome_label_i)) {
              paste0("Step 2\n(", outcome_label_i, ")")
            } else if (is_model5) {
              "Step 2"
            } else {
              comp_k
            }
          } else if (!is.na(block_order_i) && block_order_i >= 1L) {
            if (is_custom_model) {
              if (nzchar(mediator_label_i)) {
                paste0("Step ", block_order_i, " (", mediator_label_i, ")")
              } else {
                paste0("Step ", block_order_i)
              }
            } else if (is_model6) {
              if (nzchar(mediator_label_i)) {
                paste0("Step ", block_order_i, " (", mediator_label_i, ")")
              } else {
                paste0("Step ", block_order_i)
              }
            } else if (is_model4 && nzchar(mediator_label_i)) {
              paste0("Step 1-", block_order_i, " (", mediator_label_i, ")")
            } else if (is_model4) {
              paste0("Step 1-", block_order_i)
            } else if ((is_model7 || is_model8 || is_model14 || is_model15 || is_model58 || is_model59) && n_mediator_blocks > 1L && nzchar(mediator_label_i)) {
              paste0("Step 1-", block_order_i, "\n(", mediator_label_i, ")")
            } else if ((is_model7 || is_model8 || is_model14 || is_model15 || is_model58 || is_model59) && n_mediator_blocks > 1L) {
              paste0("Step 1-", block_order_i)
            } else if ((is_model7 || is_model8 || is_model14 || is_model15 || is_model58 || is_model59) && nzchar(mediator_label_i)) {
              paste0("Step 1\n(", mediator_label_i, ")")
            } else if (is_model7 || is_model8 || is_model14 || is_model15 || is_model58 || is_model59) {
              "Step 1"
            } else if (is_model5 && n_mediator_blocks > 1L && nzchar(mediator_label_i)) {
              paste0("Step 1-", block_order_i, "\n(", mediator_label_i, ")")
            } else if (is_model5 && n_mediator_blocks > 1L) {
              paste0("Step 1-", block_order_i)
            } else if (is_model5 && nzchar(mediator_label_i)) {
              paste0("Step 1\n(", mediator_label_i, ")")
            } else if (is_model5) {
              "Step 1"
            } else {
              comp_k
            }
          } else {
            if ((is_model7 || is_model8 || is_model14 || is_model15 || is_model58 || is_model59) && identical(comp_k, "Mediator model") && nzchar(mediator_label_i)) {
              paste0("Step 1\n(", mediator_label_i, ")")
            } else if ((is_model7 || is_model8 || is_model14 || is_model15 || is_model58 || is_model59) && identical(comp_k, "Mediator model")) {
              "Step 1"
            } else if (is_model5 && identical(comp_k, "Mediator model") && nzchar(mediator_label_i)) {
              paste0("Step 1\n(", mediator_label_i, ")")
            } else if (is_model5 && identical(comp_k, "Mediator model")) {
              "Step 1"
            } else {
              comp_k
            }
          }

          block_df_i$effect <- block_label
          block_out <- build_coef_block(block_df_i)
          out_rows[[idx]] <- block_out
          idx <- idx + 1L

          ss <- summary_df[
            as.character(summary_df$combo_key) == combo_k &
              as.character(summary_df$model_component) == comp_k &
              if (!identical(block_key_i, "ALL")) suppressWarnings(as.integer(summary_df$block_order)) == as.integer(block_key_i) else TRUE,
            ,
            drop = FALSE
          ]
          if (nrow(ss) > 0) {
            out_rows[[idx]] <- data.frame(
              Effect = "Model summary",
              Variable = "",
              Category = "",
              B = ss$summary_text[1] %||% "",
              SE = "",
              t = "",
              LLCI = "",
              ULCI = "",
              p = "",
              sig = "",
              stringsAsFactors = FALSE,
              check.names = FALSE
            )
            idx <- idx + 1L
          }

          is_last_subblock <- bk == length(block_keys)
          if (!is_last_subblock || k < length(component_order)) {
            out_rows[[idx]] <- empty_row
            idx <- idx + 1L
            out_rows[[idx]] <- empty_row
            idx <- idx + 1L
          }
        }
      }

      if (cc < length(combo_order)) {
        out_rows[[idx]] <- empty_row
        idx <- idx + 1L
        out_rows[[idx]] <- empty_row
        idx <- idx + 1L
      }
    }

    out <- do.call(rbind, out_rows)
  } else if (is.data.frame(summary_df) && nrow(summary_df) > 0) {
    note_rows <- do.call(
      rbind,
      lapply(seq_len(nrow(summary_df)), function(i) {
        ss <- summary_df[i, , drop = FALSE]
        data.frame(
          Effect = if (i == 1) "Model summary" else "",
          Variable = "",
          Category = "",
          B = ss$summary_text[1],
          SE = "",
          t = "",
          LLCI = "",
          ULCI = "",
          p = "",
          sig = "",
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      })
    )

    out <- rbind(out, note_rows)
  }
  out
}

build_T2 <- function(df) {
  is_custom_model <- isTRUE(PROCESS_SETTINGS$custom_model_enabled)
  is_custom_moderated <- is_custom_model && nzchar(as.character(PROCESS_SETTINGS$w_var %||% "")[1])
  if (!is.data.frame(df) || nrow(df) == 0) {
    if (is_custom_moderated || identical(PROCESS_SETTINGS$process_model, 5L) || identical(PROCESS_SETTINGS$process_model, 7L) || identical(PROCESS_SETTINGS$process_model, 8L) || identical(PROCESS_SETTINGS$process_model, 14L) || identical(PROCESS_SETTINGS$process_model, 15L) || identical(PROCESS_SETTINGS$process_model, 58L) || identical(PROCESS_SETTINGS$process_model, 59L)) {
      return(data.frame(
        Outcome = character(),
        Independent_variable = character(),
        Mediator = character(),
        Moderator = character(),
        Model = character(),
        n = character(),
        R2 = character(),
        Adj_R2 = character(),
        F = character(),
        df1 = character(),
        df2 = character(),
        p = character(),
        sig = character(),
        stringsAsFactors = FALSE,
        check.names = FALSE
      ))
    }
    return(data.frame(
      Outcome = character(),
      Independent_variable = character(),
      Mediator = character(),
      Model = character(),
      n = character(),
      R2 = character(),
      Adj_R2 = character(),
      F = character(),
      df1 = character(),
      df2 = character(),
      p = character(),
      sig = character(),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  if (is_custom_moderated || identical(PROCESS_SETTINGS$process_model, 5L) || identical(PROCESS_SETTINGS$process_model, 7L) || identical(PROCESS_SETTINGS$process_model, 8L) || identical(PROCESS_SETTINGS$process_model, 14L) || identical(PROCESS_SETTINGS$process_model, 15L) || identical(PROCESS_SETTINGS$process_model, 58L) || identical(PROCESS_SETTINGS$process_model, 59L)) {
    data.frame(
      Outcome = df$target_outcome_label %||% df$outcome_label,
      Independent_variable = df$x_label %||% "",
      Mediator = df$mediator_label %||% "",
      Moderator = df$moderator_label %||% "",
      Model = df$model_component %||% "",
      n = ifelse(is.na(df$n), "", sprintf("%.0f", df$n)),
      R2 = fmt3_pm(df$r2),
      Adj_R2 = fmt3_pm(df$adj_r2),
      F = fmt3_pm(df$f_value),
      df1 = ifelse(is.na(df$df1), "", sprintf("%.0f", df$df1)),
      df2 = ifelse(is.na(df$df2), "", sprintf("%.0f", df$df2)),
      p = fmt_p_pm(df$p),
      sig = df$sig %||% sig_mark_pm(df$p),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  } else if (PROCESS_SETTINGS$process_model %in% c(4L, 6L)) {
    data.frame(
      Outcome = df$target_outcome_label %||% df$outcome_label,
      Independent_variable = df$x_label %||% "",
      Mediator = df$mediator_label %||% "",
      Model = df$model_component %||% "",
      n = ifelse(is.na(df$n), "", sprintf("%.0f", df$n)),
      R2 = fmt3_pm(df$r2),
      Adj_R2 = fmt3_pm(df$adj_r2),
      F = fmt3_pm(df$f_value),
      df1 = ifelse(is.na(df$df1), "", sprintf("%.0f", df$df1)),
      df2 = ifelse(is.na(df$df2), "", sprintf("%.0f", df$df2)),
      p = fmt_p_pm(df$p),
      sig = df$sig %||% sig_mark_pm(df$p),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  } else {
    data.frame(
      Outcome = df$target_outcome_label %||% df$outcome_label,
      Independent_variable = df$x_label %||% "",
      Moderator = df$moderator_label %||% "",
      Model = df$model_component %||% "",
      n = ifelse(is.na(df$n), "", sprintf("%.0f", df$n)),
      R2 = fmt3_pm(df$r2),
      Adj_R2 = fmt3_pm(df$adj_r2),
      F = fmt3_pm(df$f_value),
      df1 = ifelse(is.na(df$df1), "", sprintf("%.0f", df$df1)),
      df2 = ifelse(is.na(df$df2), "", sprintf("%.0f", df$df2)),
      p = fmt_p_pm(df$p),
      sig = df$sig %||% sig_mark_pm(df$p),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
}

build_T3 <- function(df) {
  # T3 is kept separate for Model 4 indirect-effect decomposition.
  # Path components a and b remain here because they belong to the
  # mediation breakdown rather than the main T1 regression table.
  is_custom_model <- isTRUE(PROCESS_SETTINGS$custom_model_enabled)
  is_conditional_indirect_model <- is_custom_model && nzchar(as.character(PROCESS_SETTINGS$w_var %||% "")[1])
  if (!is_conditional_indirect_model) {
    is_conditional_indirect_model <- isTRUE(suppressWarnings(as.numeric(PROCESS_SETTINGS$process_model)) %in% c(7, 8, 14, 15, 58, 59))
  }
  if (!is.data.frame(df) || nrow(df) == 0) {
    if (is_conditional_indirect_model) {
      return(data.frame(
        Path = character(),
        Variables = character(),
        Moderator_level = character(),
        Value = character(),
        a = character(),
        b = character(),
        c = character(),
        Indirect = character(),
        SE = character(),
        z = character(),
        LLCI = character(),
        ULCI = character(),
        p = character(),
        sig = character(),
        stringsAsFactors = FALSE,
        check.names = FALSE
      ))
    }
    return(data.frame(
      Path = character(),
      Variables = character(),
      Contrast = character(),
      a = character(),
      b = character(),
      c = character(),
      Indirect = character(),
      SE = character(),
      z = character(),
      LLCI = character(),
      ULCI = character(),
      p = character(),
      sig = character(),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  mediator_vars <- PROCESS_SETTINGS$m_vars %||% character(0)
  if (!"c" %in% names(df)) df$c <- NA_real_
  mediator_labels <- vapply(mediator_vars, lookup_label_pm, character(1))
  mediator_labels[!nzchar(mediator_labels)] <- mediator_vars[!nzchar(mediator_labels)]
  mediator_symbols <- paste0("M", seq_along(mediator_vars))

  label_to_symbol <- stats::setNames(mediator_symbols, mediator_labels)
  var_to_symbol <- stats::setNames(mediator_symbols, mediator_vars)

  trim_parts_pm <- function(x, sep) {
    vals <- unlist(strsplit(as.character(x)[1] %||% "", sep, fixed = TRUE), use.names = FALSE)
    vals <- trimws(vals)
    vals[nzchar(vals)]
  }

  mediator_chain_to_cells <- function(parts, x_name, y_name) {
    parts <- trimws(as.character(parts))
    parts <- parts[nzchar(parts)]
    if (!length(parts)) {
      return(list(
        path = "X --> Y",
        variables = paste(c(x_name, y_name), collapse = " --> ")
      ))
    }

    symbols <- vapply(parts, function(p) {
      hit <- label_to_symbol[[p]]
      if (!is.null(hit) && nzchar(hit)) return(hit)
      hit2 <- var_to_symbol[[p]]
      if (!is.null(hit2) && nzchar(hit2)) return(hit2)
      "M?"
    }, character(1))

    list(
      path = paste(c("X", symbols, "Y"), collapse = " --> "),
      variables = paste(c(x_name, parts, y_name), collapse = " --> ")
    )
  }

  build_path_cells_pm <- function(x_label, outcome_label, mediator_label, contrast_label = "") {
    x_name <- as.character(x_label %||% "")[1]
    y_name <- as.character(outcome_label %||% "")[1]
    med_name <- as.character(mediator_label %||% "")[1]
    contrast_name <- as.character(contrast_label %||% "")[1]

    if (grepl("difference", contrast_name, ignore.case = TRUE) && grepl(" - ", med_name, fixed = TRUE)) {
      diff_parts <- trim_parts_pm(med_name, " - ")
      if (length(diff_parts) >= 2) {
        left_cells <- mediator_chain_to_cells(trim_parts_pm(diff_parts[1], " -> "), x_name, y_name)
        right_cells <- mediator_chain_to_cells(trim_parts_pm(diff_parts[2], " -> "), x_name, y_name)
        return(list(
          path = paste(left_cells$path, right_cells$path, sep = " - "),
          variables = paste(left_cells$variables, right_cells$variables, sep = " - ")
      ))
    }
  }

    mediator_parts <- trim_parts_pm(med_name, " -> ")
    mediator_chain_to_cells(mediator_parts, x_name, y_name)
  }

  ind_fmt <- if (is_conditional_indirect_model) fmt3_pm else fmt2_pm

  parse_moderated_indirect_label_pm <- function(x) {
    x <- as.character(x)[1] %||% ""
    if (!nzchar(x)) {
      return(list(level = "", value = ""))
    }
    if (identical(trimws(x), "Index of moderated mediation")) {
      return(list(level = "Index of moderated mediation", value = ""))
    }
    m <- regexec("^Conditional indirect effect at .*?=\\s*(.*?)\\s*\\((.*?)\\)\\s*$", x, perl = TRUE)
    hit <- regmatches(x, m)[[1]]
    if (length(hit) == 3) {
      level_txt <- trimws(hit[2])
      level_txt <- gsub("\\s+", " ", level_txt)
      level_txt <- gsub("^M\\s*-\\s*1\\s*SD$", "M-SD", level_txt, ignore.case = TRUE)
      level_txt <- gsub("^M\\s*\\+\\s*1\\s*SD$", "M+SD", level_txt, ignore.case = TRUE)
      level_txt <- gsub("^M$", "Mean", level_txt, ignore.case = TRUE)
      return(list(level = level_txt, value = trimws(hit[3])))
    }
    m2 <- regexec("^(.*?)\\s*=\\s*(.*?)\\s*\\((.*?)\\)\\s*$", x, perl = TRUE)
    hit2 <- regmatches(x, m2)[[1]]
    if (length(hit2) == 4) {
      level_txt <- trimws(hit2[3])
      level_txt <- gsub("\\s+", " ", level_txt)
      level_txt <- gsub("^M\\s*-\\s*1\\s*SD$", "M-SD", level_txt, ignore.case = TRUE)
      level_txt <- gsub("^M\\s*\\+\\s*1\\s*SD$", "M+SD", level_txt, ignore.case = TRUE)
      level_txt <- gsub("^M$", "Mean", level_txt, ignore.case = TRUE)
      return(list(level = level_txt, value = trimws(hit2[4])))
    }
    list(level = x, value = "")
  }

  path_cells <- lapply(seq_len(nrow(df)), function(i) {
    build_path_cells_pm(
      x_label = df$x_label[i] %||% df$x_var[i] %||% "",
      outcome_label = df$outcome_label[i] %||% df$outcome[i] %||% "",
      mediator_label = df$mediator_label[i] %||% df$mediator[i] %||% "",
      contrast_label = df$class_contrast[i] %||% ""
    )
  })

  if (is_conditional_indirect_model) {
    cond_cells <- lapply(seq_len(nrow(df)), function(i) parse_moderated_indirect_label_pm(df$class_contrast[i] %||% ""))
    out <- data.frame(
      Path = vapply(path_cells, `[[`, character(1), "path"),
      Variables = vapply(path_cells, `[[`, character(1), "variables"),
      Moderator_level = vapply(cond_cells, `[[`, character(1), "level"),
      Value = vapply(cond_cells, `[[`, character(1), "value"),
      a = fmt2_pm(df$a),
      b = fmt2_pm(df$b),
      c = fmt2_pm(df$c),
      Indirect = ind_fmt(df$indirect),
      SE = ind_fmt(df$se),
      z = fmt2_pm(df$z_value),
      LLCI = ind_fmt(df$llci),
      ULCI = ind_fmt(df$ulci),
      p = fmt_p_pm(df$p),
      sig = df$sig %||% sig_mark_pm(df$p),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  } else {
    out <- data.frame(
      Path = vapply(path_cells, `[[`, character(1), "path"),
      Variables = vapply(path_cells, `[[`, character(1), "variables"),
      Contrast = df$class_contrast %||% "Indirect effect",
      a = fmt2_pm(df$a),
      b = fmt2_pm(df$b),
      c = fmt2_pm(df$c),
      Indirect = ind_fmt(df$indirect),
      SE = ind_fmt(df$se),
      z = fmt2_pm(df$z_value),
      LLCI = ind_fmt(df$llci),
      ULCI = ind_fmt(df$ulci),
      p = fmt_p_pm(df$p),
      sig = df$sig %||% sig_mark_pm(df$p),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  orig_path <- out$Path
  orig_vars <- out$Variables
  group_key <- paste(out$Path, out$Variables, sep = " | ")
  out$Path <- ave(out$Path, group_key, FUN = blank_repeat)
  out$Variables <- ave(out$Variables, group_key, FUN = blank_repeat)
  if (is_conditional_indirect_model) {
    idx_mm <- grepl("^Index of moderated mediation$", as.character(out$Moderator_level))
    out$Path[idx_mm] <- orig_path[idx_mm]
    out$Variables[idx_mm] <- orig_vars[idx_mm]
  }
  out
}

build_S1 <- function() {
  data.frame(
    Item = c(
      "Source analysis",
      "PROCESS model",
      "Moderator source",
      "Independent variable count",
      "Model count",
      "Coefficient rows",
      "Indirect rows"
    ),
    Value = c(
      PROCESS_SETTINGS$source_analysis_id %||% "",
      PROCESS_SETTINGS$process_model %||% "",
      PROCESS_SETTINGS$moderator_source %||% "",
      length(PROCESS_SETTINGS$x_vars %||% character(0)),
      MODEL_RUN_SUMMARY$n_models %||% 0,
      MODEL_RUN_SUMMARY$n_rows %||% 0,
      MODEL_RUN_SUMMARY$n_indirect_rows %||% 0
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

build_S2 <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(data.frame(
      Outcome = character(),
      Predictor = character(),
      Moderator = character(),
      Probe_Level = character(),
      Moderator_Value = character(),
      Effect = character(),
      SE = character(),
      t = character(),
      LLCI = character(),
      ULCI = character(),
      p = character(),
      sig = character(),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  parse_probe_label <- function(x, moderator_label = "") {
    x <- as.character(x)[1]
    mod_name <- moderator_label
    level_txt <- x
    value_txt <- ""

    m <- regexec("^\\s*(.*?)\\s*=\\s*(.*?)\\s*\\((.*?)\\)\\s*$", x, perl = TRUE)
    hit <- regmatches(x, m)[[1]]
    if (length(hit) == 4) {
      if (nzchar(trimws(hit[2]))) mod_name <- trimws(hit[2])
      level_txt <- trimws(hit[3])
      value_txt <- trimws(hit[4])
    }

    level_txt <- gsub("\\s+", " ", level_txt)
    level_txt <- gsub("^M\\s*-\\s*1\\s*SD$", "M-SD", level_txt, ignore.case = TRUE)
    level_txt <- gsub("^M\\s*\\+\\s*1\\s*SD$", "M+SD", level_txt, ignore.case = TRUE)
    level_txt <- gsub("^M$", "Mean", level_txt, ignore.case = TRUE)

    list(
      moderator = mod_name,
      probe_level = level_txt,
      moderator_value = value_txt
    )
  }

  parsed_labels <- lapply(seq_len(nrow(df)), function(i) {
    parse_probe_label(
      x = df$class_contrast[i] %||% "",
      moderator_label = df$moderator_label[i] %||% df$moderator[i] %||% "Moderator"
    )
  })

  out <- data.frame(
    Outcome = df$outcome_label %||% df$outcome,
    Predictor = df$x_label %||% df$x_var,
    Moderator = vapply(parsed_labels, `[[`, character(1), "moderator"),
    Probe_Level = vapply(parsed_labels, `[[`, character(1), "probe_level"),
    Moderator_Value = vapply(parsed_labels, `[[`, character(1), "moderator_value"),
    Effect = fmt2_pm(df$conditional_effect),
    SE = fmt2_pm(df$se),
    t = fmt2_pm(df$t_value),
    LLCI = fmt2_pm(df$llci),
    ULCI = fmt2_pm(df$ulci),
    p = fmt_p_pm(df$p),
    sig = df$sig %||% sig_mark_pm(df$p),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  probe_order <- c("M-SD", "Mean", "M+SD")
  out$..group_id <- paste(out$Outcome, out$Predictor, out$Moderator, sep = " | ")
  group_levels <- unique(out$..group_id)
  out$..group_id <- factor(out$..group_id, levels = group_levels)
  out$..probe_ord <- match(out$Probe_Level, probe_order)
  out$..probe_ord[is.na(out$..probe_ord)] <- length(probe_order) + seq_len(sum(is.na(out$..probe_ord)))
  out <- out[order(out$..group_id, out$..probe_ord), , drop = FALSE]
  rownames(out) <- NULL

  grp <- paste(out$Outcome, out$Predictor, sep = " | ")
  out$Outcome <- ave(out$Outcome, grp, FUN = blank_repeat)
  out$Predictor <- ave(out$Predictor, grp, FUN = blank_repeat)
  out$..group_id <- NULL
  out$..probe_ord <- NULL
  out
}

top_bottom_styles <- function() {
  list(
    title = openxlsx::createStyle(textDecoration = "bold", fontSize = 12, halign = "left"),
    header = openxlsx::createStyle(textDecoration = "bold", halign = "center", valign = "center", border = "bottom", borderStyle = "thick"),
    body = openxlsx::createStyle(valign = "center"),
    left = openxlsx::createStyle(halign = "left", valign = "center", wrapText = TRUE),
    center = openxlsx::createStyle(halign = "center", valign = "center"),
    note = openxlsx::createStyle(halign = "left", valign = "top", wrapText = TRUE),
    top = openxlsx::createStyle(border = "top", borderStyle = "thick"),
    bottom = openxlsx::createStyle(border = "bottom", borderStyle = "thick")
  )
}

write_simple_sheet <- function(wb, sheet_name, dat, title_text) {
  dat <- as.data.frame(dat, stringsAsFactors = FALSE, check.names = FALSE)
  sty <- top_bottom_styles()

  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, data.frame(title_text), startRow = 1, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name, sty$title, rows = 1, cols = 1, gridExpand = TRUE, stack = TRUE)
  openxlsx::writeData(wb, sheet_name, dat, startRow = 3, startCol = 1, withFilter = FALSE)

  if (ncol(dat) > 0) {
    openxlsx::addStyle(wb, sheet_name, sty$top, rows = 3, cols = seq_len(ncol(dat)), gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, sheet_name, sty$header, rows = 3, cols = seq_len(ncol(dat)), gridExpand = TRUE, stack = TRUE)
  }
  if (nrow(dat) > 0 && ncol(dat) > 0) {
    body_rows <- 4:(nrow(dat) + 3)
    openxlsx::addStyle(wb, sheet_name, sty$body, rows = body_rows, cols = seq_len(ncol(dat)), gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, sheet_name, sty$left, rows = body_rows, cols = seq_len(min(4, ncol(dat))), gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, sheet_name, sty$bottom, rows = nrow(dat) + 3, cols = seq_len(ncol(dat)), gridExpand = TRUE, stack = TRUE)
  }
      if (identical(sheet_name, "T2")) {
    t2_widths <- c(20, 20, 38, 18, 16, 10, 8, 8, 8, 8, 8, 8, 6)
    openxlsx::setColWidths(
      wb,
      sheet_name,
      cols = seq_len(min(length(t2_widths), ncol(dat))),
      widths = t2_widths[seq_len(min(length(t2_widths), ncol(dat)))]
    )
  } else if (identical(sheet_name, "S2")) {
    s2_widths <- c(22, 22, 18, 12, 10, 10, 8, 8, 8, 8, 8, 6)
    openxlsx::setColWidths(
      wb,
      sheet_name,
      cols = seq_len(min(length(s2_widths), ncol(dat))),
      widths = s2_widths[seq_len(min(length(s2_widths), ncol(dat)))]
    )
  } else if (identical(sheet_name, "T3")) {
    t3_widths <- if (isTRUE(suppressWarnings(as.numeric(PROCESS_SETTINGS$process_model)) %in% c(7, 14, 15, 58))) {
      c(18, 42, 20, 10, 8, 8, 8, 10, 10, 8, 10, 10, 8, 6)
    } else {
      c(18, 42, 38, 8, 8, 8, 10, 10, 8, 10, 10, 8, 6)
    }
    openxlsx::setColWidths(
      wb,
      sheet_name,
      cols = seq_len(min(length(t3_widths), ncol(dat))),
      widths = t3_widths[seq_len(min(length(t3_widths), ncol(dat)))]
    )
  } else {
    openxlsx::setColWidths(wb, sheet_name, cols = seq_len(max(1, ncol(dat))), widths = "auto")
  }
}

write_t1_sheet <- function(wb, sheet_name, dat, title_text, note_text = NULL) {
  dat <- as.data.frame(dat, stringsAsFactors = FALSE, check.names = FALSE)
  sty <- top_bottom_styles()

  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, data.frame(title_text), startRow = 1, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name, sty$title, rows = 1, cols = 1, gridExpand = TRUE, stack = TRUE)

  data_start_row <- 3

  openxlsx::writeData(wb, sheet_name, dat, startRow = data_start_row, startCol = 1, withFilter = FALSE)

  if (ncol(dat) > 0) {
    openxlsx::addStyle(wb, sheet_name, sty$top, rows = data_start_row, cols = seq_len(ncol(dat)), gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, sheet_name, sty$header, rows = data_start_row, cols = seq_len(ncol(dat)), gridExpand = TRUE, stack = TRUE)
  }
  if (nrow(dat) > 0 && ncol(dat) > 0) {
    body_rows <- (data_start_row + 1):(nrow(dat) + data_start_row)
    openxlsx::addStyle(wb, sheet_name, sty$body, rows = body_rows, cols = seq_len(ncol(dat)), gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, sheet_name, sty$left, rows = body_rows, cols = seq_len(min(3, ncol(dat))), gridExpand = TRUE, stack = TRUE)
    block_idx <- which(grepl("^(Step\\s|Mediator model|Outcome model)", as.character(dat$Effect)))
    block_idx <- block_idx[block_idx > 1]
    if (length(block_idx) > 0) {
      openxlsx::addStyle(
        wb, sheet_name, sty$top,
        rows = data_start_row + block_idx,
        cols = seq_len(ncol(dat)),
        gridExpand = TRUE,
        stack = TRUE
      )
    }
    label_idx <- which(grepl("\n", as.character(dat$Effect), fixed = TRUE))
    if (length(label_idx) > 0) {
      openxlsx::setRowHeights(wb, sheet_name, rows = data_start_row + label_idx, heights = 28)
    }
    summary_idx <- which(dat$Effect == "Model summary")
    if (length(summary_idx) > 0) {
      for (si in summary_idx) {
        summary_row <- data_start_row + si
        if (ncol(dat) >= 10) {
          openxlsx::mergeCells(wb, sheet_name, cols = 4:10, rows = summary_row)
          openxlsx::addStyle(wb, sheet_name, sty$left, rows = summary_row, cols = 4, gridExpand = TRUE, stack = TRUE)
        }
        if (si > 1) {
          openxlsx::addStyle(wb, sheet_name, sty$bottom, rows = data_start_row + si - 1, cols = seq_len(ncol(dat)), gridExpand = TRUE, stack = TRUE)
        }
        openxlsx::addStyle(wb, sheet_name, sty$bottom, rows = summary_row, cols = seq_len(ncol(dat)), gridExpand = TRUE, stack = TRUE)
      }
    } else {
      openxlsx::addStyle(wb, sheet_name, sty$bottom, rows = nrow(dat) + data_start_row, cols = seq_len(ncol(dat)), gridExpand = TRUE, stack = TRUE)
    }
  }

  if (!is.null(note_text) && nzchar(note_text) && ncol(dat) > 0) {
    note_row <- data_start_row + nrow(dat) + 2
    openxlsx::writeData(wb, sheet_name, data.frame(note_text), startRow = note_row, startCol = 1, colNames = FALSE)
    openxlsx::mergeCells(wb, sheet_name, cols = 1:ncol(dat), rows = note_row)
    openxlsx::addStyle(wb, sheet_name, sty$note, rows = note_row, cols = 1, gridExpand = TRUE, stack = TRUE)
    openxlsx::setRowHeights(wb, sheet_name, rows = note_row, heights = 34)
  }

  widths <- c(24, 28, 24, 10, 8, 8, 8, 8, 8, 6)
  openxlsx::setColWidths(wb, sheet_name, cols = seq_len(min(length(widths), ncol(dat))), widths = widths[seq_len(min(length(widths), ncol(dat)))])
}

T1 <- build_T1(PROCESS_MODEL_RESULTS, PROCESS_MODEL_SUMMARY)
T2 <- build_T2(PROCESS_MODEL_SUMMARY)
T3 <- build_T3(PROCESS_INDIRECT_RESULTS)
S1 <- build_S1()
S2 <- build_S2(PROCESS_CONDITIONAL_EFFECTS)

is_custom_model_pm <- isTRUE(PROCESS_SETTINGS$custom_model_enabled)
is_custom_moderated_pm <- is_custom_model_pm && nzchar(as.character(PROCESS_SETTINGS$w_var %||% "")[1])

TABLE_REGISTRY <- if (is_custom_model_pm) {
  list(T1 = T1, T2 = T2, T3 = T3, S1 = S1)
} else if (PROCESS_SETTINGS$process_model %in% c(4L, 6L)) {
  list(T1 = T1, T2 = T2, T3 = T3, S1 = S1)
} else if (PROCESS_SETTINGS$process_model %in% c(5L, 7L, 8L, 14L, 15L, 58L, 59L)) {
  list(T1 = T1, T2 = T2, T3 = T3, S1 = S1, S2 = S2)
} else {
  list(T1 = T1, T2 = T2, S1 = S1, S2 = S2)
}

model_title_num <- if (is_custom_model_pm) {
  as.character(PROCESS_SETTINGS$custom_model_label %||% "Custom")
} else {
  PROCESS_SETTINGS$process_model %||% ""
}
is_mediation_model <- is_custom_model_pm || PROCESS_SETTINGS$process_model %in% c(4L, 5L, 6L, 7L, 8L, 14L, 15L, 58L, 59L)
table1_title <- if (is_custom_model_pm) {
  paste0("Table 1. ", model_title_num, " PROCESS-style regression coefficients")
} else {
  paste0("Table 1. PROCESS Model ", model_title_num, "-style regression coefficients")
}
table2_title <- if (is_custom_model_pm) {
  paste0("Table 2. ", model_title_num, " process model summary")
} else {
  paste0("Table 2. PROCESS Model ", model_title_num, " model summary")
}
table3_title <- if (is_custom_model_pm && is_custom_moderated_pm) {
  paste0("Table 3. ", model_title_num, " custom conditional indirect effects")
} else if (is_custom_model_pm) {
  paste0("Table 3. ", model_title_num, " custom indirect effects")
} else if (identical(PROCESS_SETTINGS$process_model, 6L)) {
  "Table 3. PROCESS Model 6 indirect effects"
} else if (identical(PROCESS_SETTINGS$process_model, 7L)) {
  "Table 3. PROCESS Model 7 conditional indirect effects"
} else if (identical(PROCESS_SETTINGS$process_model, 8L)) {
  "Table 3. PROCESS Model 8 conditional indirect effects"
} else if (identical(PROCESS_SETTINGS$process_model, 14L)) {
  "Table 3. PROCESS Model 14 conditional indirect effects"
} else if (identical(PROCESS_SETTINGS$process_model, 15L)) {
  "Table 3. PROCESS Model 15 conditional indirect effects"
} else if (identical(PROCESS_SETTINGS$process_model, 58L)) {
  "Table 3. PROCESS Model 58 conditional indirect effects"
} else if (identical(PROCESS_SETTINGS$process_model, 59L)) {
  "Table 3. PROCESS Model 59 conditional indirect effects"
} else if (identical(PROCESS_SETTINGS$process_model, 5L)) {
  "Table 3. PROCESS Model 5 indirect effects"
} else {
  "Table 3. PROCESS Model 4 indirect effects"
}

TABLE_META <- data.frame(
  table_name = names(TABLE_REGISTRY),
  title = if (is_custom_model_pm) c(
    table1_title,
    table2_title,
    table3_title,
    "Supplementary Table 1. Custom PROCESS analysis overview"
  ) else if (identical(PROCESS_SETTINGS$process_model, 7L)) c(
    table1_title,
    table2_title,
    table3_title,
    "Supplementary Table 1. PROCESS macro analysis overview",
    "Supplementary Table 2. Conditional effects of the focal predictor on the mediator by moderator level"
  ) else if (identical(PROCESS_SETTINGS$process_model, 8L)) c(
    table1_title,
    table2_title,
    table3_title,
    "Supplementary Table 1. PROCESS macro analysis overview",
    "Supplementary Table 2. Conditional effects of the focal predictor on the mediator and outcome by moderator level"
  ) else if (identical(PROCESS_SETTINGS$process_model, 14L)) c(
    table1_title,
    table2_title,
    table3_title,
    "Supplementary Table 1. PROCESS macro analysis overview",
    "Supplementary Table 2. Conditional effects of the mediator on the outcome by moderator level"
  ) else if (identical(PROCESS_SETTINGS$process_model, 15L)) c(
    table1_title,
    table2_title,
    table3_title,
    "Supplementary Table 1. PROCESS macro analysis overview",
    "Supplementary Table 2. Conditional direct and second-stage effects by moderator level"
  ) else if (identical(PROCESS_SETTINGS$process_model, 58L)) c(
    table1_title,
    table2_title,
    table3_title,
    "Supplementary Table 1. PROCESS macro analysis overview",
    "Supplementary Table 2. Conditional effects by moderator level"
  ) else if (identical(PROCESS_SETTINGS$process_model, 59L)) c(
    table1_title,
    table2_title,
    table3_title,
    "Supplementary Table 1. PROCESS macro analysis overview",
    "Supplementary Table 2. Conditional first-stage, second-stage, and direct effects by moderator level"
  ) else if (identical(PROCESS_SETTINGS$process_model, 5L)) c(
    table1_title,
    table2_title,
    table3_title,
    "Supplementary Table 1. PROCESS macro analysis overview",
    "Supplementary Table 2. Conditional direct effects of the focal predictor by moderator level"
  ) else if (is_mediation_model) c(
      table1_title,
      table2_title,
      table3_title,
      "Supplementary Table 1. PROCESS macro analysis overview"
  ) else c(
    "Table 1. PROCESS Model 1-style regression coefficients",
    "Table 2. PROCESS Model 1 model summary",
    "Supplementary Table 1. PROCESS macro analysis overview",
    "Supplementary Table 2. Conditional effects of the focal predictor by moderator level"
  ),
  stringsAsFactors = FALSE
)

for (nm in names(TABLE_REGISTRY)) {
  write_csv_safe(TABLE_REGISTRY[[nm]], file.path(DIR_TABLES, paste0(nm, ".csv")))
  save_step_rds(TABLE_REGISTRY[[nm]], nm, dir_rds = DIR_RDS)
}

TABLE_INDEX <- data.frame(
  table_name = names(TABLE_REGISTRY),
  file_path = file.path(DIR_TABLES, paste0(names(TABLE_REGISTRY), ".csv")),
  nrow = vapply(TABLE_REGISTRY, nrow, integer(1)),
  ncol = vapply(TABLE_REGISTRY, ncol, integer(1)),
  stringsAsFactors = FALSE
)

write_csv_safe(TABLE_INDEX, file.path(DIR_TABLES, "TABLE_INDEX.csv"))

wb <- openxlsx::createWorkbook()
note_t1 <- paste0(
  if (is_custom_model_pm) {
    paste0("Note. ", model_title_num, " custom PROCESS model")
  } else {
    paste0("Note. PROCESS Model ", PROCESS_SETTINGS$process_model)
  },
  ". Dependent variable: ", paste(unique(PROCESS_MODEL_RESULTS$target_outcome_label %||% PROCESS_MODEL_RESULTS$outcome_label), collapse = ", "),
  ". Independent variable: ", paste(unique(PROCESS_MODEL_RESULTS$x_label %||% character(0)), collapse = ", "),
  if (is_mediation_model) {
    mediator_labels <- unique(vapply(PROCESS_SETTINGS$m_vars %||% character(0), lookup_label_pm, character(1)))
    if (identical(PROCESS_SETTINGS$process_model, 6L)) {
      paste0(". Mediators in serial order: ", paste(mediator_labels, collapse = " -> "), ".")
    } else if (identical(PROCESS_SETTINGS$process_model, 5L) || identical(PROCESS_SETTINGS$process_model, 7L) || identical(PROCESS_SETTINGS$process_model, 8L) || identical(PROCESS_SETTINGS$process_model, 14L) || identical(PROCESS_SETTINGS$process_model, 15L) || identical(PROCESS_SETTINGS$process_model, 58L) || identical(PROCESS_SETTINGS$process_model, 59L)) {
      paste0(
        ". Mediator: ", paste(mediator_labels, collapse = ", "),
        ". Moderator: ", paste(unique(PROCESS_MODEL_RESULTS$moderator_label %||% character(0)), collapse = ", "),
        "."
      )
    } else {
      paste0(". Mediator: ", paste(mediator_labels, collapse = ", "), ".")
    }
  } else {
    paste0(". Moderator: ", paste(unique(PROCESS_MODEL_RESULTS$moderator_label %||% character(0)), collapse = ", "), ".")
  }
)
write_t1_sheet(
  wb, "T1", T1,
  if (is_mediation_model) table1_title else "Table 1. PROCESS Model 1-style regression coefficients",
  note_text = note_t1
)
write_simple_sheet(
  wb, "T2", T2,
  if (is_mediation_model) table2_title else "Table 2. PROCESS Model 1 model summary"
)
if (is_mediation_model) {
  write_simple_sheet(wb, "T3", T3, table3_title)
}
write_simple_sheet(wb, "S1", S1, if (is_custom_model_pm) "Supplementary Table 1. Custom PROCESS analysis overview" else "Supplementary Table 1. PROCESS macro analysis overview")
if (!is_custom_model_pm && PROCESS_SETTINGS$process_model %in% c(1L, 5L, 7L, 8L, 14L, 15L, 58L, 59L)) {
  write_simple_sheet(
    wb, "S2", S2,
    if (identical(PROCESS_SETTINGS$process_model, 7L)) {
      "Supplementary Table 2. Conditional effects of the focal predictor on the mediator by moderator level"
    } else if (identical(PROCESS_SETTINGS$process_model, 8L)) {
      "Supplementary Table 2. Conditional effects of the focal predictor on the mediator and outcome by moderator level"
    } else if (identical(PROCESS_SETTINGS$process_model, 14L)) {
      "Supplementary Table 2. Conditional effects of the mediator on the outcome by moderator level"
    } else if (identical(PROCESS_SETTINGS$process_model, 15L)) {
      "Supplementary Table 2. Conditional direct and second-stage effects by moderator level"
    } else if (identical(PROCESS_SETTINGS$process_model, 58L)) {
      "Supplementary Table 2. Conditional effects by moderator level"
    } else if (identical(PROCESS_SETTINGS$process_model, 59L)) {
      "Supplementary Table 2. Conditional first-stage, second-stage, and direct effects by moderator level"
    } else {
      "Supplementary Table 2. Conditional effects of the focal predictor by moderator level"
    }
  )
}
openxlsx::saveWorkbook(wb, PATH_FINAL_EXCEL, overwrite = TRUE)

TABLE_MANIFEST <- data.frame(
  table_name = c(names(TABLE_REGISTRY), "TABLE_INDEX"),
  file_path = c(file.path(DIR_TABLES, paste0(names(TABLE_REGISTRY), ".csv")), file.path(DIR_TABLES, "TABLE_INDEX.csv")),
  stringsAsFactors = FALSE
)

TABLE_SUMMARY <- list(
  table_dir = DIR_TABLES,
  n_tables = length(TABLE_REGISTRY),
  excel_file = PATH_FINAL_EXCEL,
  created_at = Sys.time()
)

TABLE_VALIDATION <- data.frame(
  table_name = names(TABLE_REGISTRY),
  ok = vapply(TABLE_REGISTRY, function(x) is.data.frame(x), logical(1)),
  note = "",
  stringsAsFactors = FALSE
)

save_named_rds_list(
  list(
    TABLE_REGISTRY = TABLE_REGISTRY,
    TABLE_META = TABLE_META,
    TABLE_INDEX = TABLE_INDEX,
    TABLE_MANIFEST = TABLE_MANIFEST,
    TABLE_SUMMARY = TABLE_SUMMARY,
    TABLE_VALIDATION = TABLE_VALIDATION
  ),
  dir_rds = DIR_RDS
)

elapsed_sec <- round(as.numeric(difftime(Sys.time(), T0_TABLES, units = "secs")), 2)
log_info("04_tables.R completed.")
log_info("n(T1)             = ", nrow(T1))
log_info("n(T2)             = ", nrow(T2))
if (is_mediation_model) log_info("n(T3)             = ", nrow(T3))
log_info("Excel file        = ", PATH_FINAL_EXCEL)
log_info("elapsed           = ", elapsed_sec, " sec")
log_step_end("tables", elapsed_sec, ok = TRUE)
