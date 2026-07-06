# ============================================================
# 10_bch_core.R
# Common BCH core helpers
# ============================================================

`%||%` <- function(x, y) if (is.null(x)) y else x

ensure_df <- function(x) {
  if (is.null(x)) return(data.frame())
  if (is.data.frame(x)) return(x)
  as.data.frame(x, stringsAsFactors = FALSE)
}

safe_num_local <- function(x) suppressWarnings(as.numeric(as.character(x)))

fmt_p_tbl <- function(p, digits = 3) {
  p       <- safe_num_local(p)
  out     <- rep(NA_character_, length(p))
  ok      <- !is.na(p)
  out[ok] <- ifelse(p[ok] < .001, "<.001", formatC(p[ok], format = "f", digits = digits))
  out
}

p_to_sig_tbl <- function(p) {
  p   <- safe_num_local(p)
  out <- rep("", length(p))
  out[!is.na(p) & p < .001] <- "***"
  out[!is.na(p) & p >= .001 & p < .01] <- "**"
  out[!is.na(p) & p >= .01 & p < .05] <- "*"
  out
}

resolve_out_file_from_inp <- function(inp_file) sub("\\.inp$", ".out", inp_file, ignore.case = TRUE)

norm_space_collect <- function(x) {
  x <- as.character(x)
  x <- gsub("[\\r\\n\\t]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

extract_nums_collect <- function(x) {
  x   <- norm_space_collect(x)
  m   <- gregexpr("[-+]?[0-9]*\\.?[0-9]+(?:[EeDd][-+]?[0-9]+)?", x, perl = TRUE)
  out <- regmatches(x, m)[[1]]
  gsub("D", "E", out, fixed = TRUE)
}

mplus_wrap_statement <- function(keyword, vars, indent = "  ", width = 78) {
  vars        <- unique(as.character(vars))
  vars        <- vars[!is.na(vars) & nzchar(vars)]
  if (length(vars) == 0) return(character(0))
  line_prefix <- paste0(indent, keyword, " = ")
  cont_prefix <- paste0(indent, "  ")
  out         <- character(0)
  current     <- line_prefix
  for (v in vars) {
    candidate <- if (identical(current, line_prefix)) paste0(current, v) else paste(current, v)
    if (nchar(candidate, type = "width") > width) {
      out     <- c(out, current)
      current <- paste0(cont_prefix, v)
    } else {
      current <- candidate
    }
  }
  c(out, paste0(current, ";"))
}

make_mplus_type_line <- function(weight_var = NULL, strata_var = NULL, cluster_var = NULL) {
  has_complex        <- !is.null(weight_var) || !is.null(strata_var) || !is.null(cluster_var)
  if (has_complex) "  TYPE = MIXTURE COMPLEX;" else "  TYPE = MIXTURE;"
}

build_survey_variable_lines <- function(weight_var = NULL, strata_var = NULL, cluster_var = NULL) {
  out <- character(0)
  if (!is.null(weight_var) && nzchar(weight_var)) out   <- c(out, paste0("  WEIGHT = ", weight_var, ";"))
  if (!is.null(strata_var) && nzchar(strata_var)) out   <- c(out, paste0("  STRATIFICATION = ", strata_var, ";"))
  if (!is.null(cluster_var) && nzchar(cluster_var)) out <- c(out, paste0("  CLUSTER = ", cluster_var, ";"))
  out
}

make_output_lines_mplus <- function(CFG = NULL) {
  tech1    <- isTRUE(CFG$mplus$output$tech1 %||% FALSE)
  tech4    <- isTRUE(CFG$mplus$output$tech4 %||% FALSE)
  tech8    <- isTRUE(CFG$mplus$output$tech8 %||% FALSE)
  tech11   <- isTRUE(CFG$mplus$output$tech11 %||% FALSE)
  sampstat <- isTRUE(CFG$mplus$output$sampstat %||% FALSE)
  out <- c("OUTPUT:")
  if (sampstat) out <- c(out, "  SAMPSTAT;")
  if (tech1) out    <- c(out, "  TECH1;")
  if (tech4) out    <- c(out, "  TECH4;")
  if (tech11) out   <- c(out, "  TECH11;")
  if (tech8) out    <- c(out, "  TECH8;")
  if (length(out) == 1L) return(character(0))
  out
}

empty_bch_long <- function() {
  data.frame(
    analysis         = character(0),
    model_type       = character(0),
    method           = character(0),
    best_k           = integer(0),
    best_tag         = character(0),
    model_structure  = character(0),

    outcome          = character(0),
    var_name         = character(0),
    var_label        = character(0),
    outcome_type     = character(0),

    result_type      = character(0),
    class            = character(0),
    class_num        = integer(0),
    contrast         = character(0),

    estimate         = numeric(0),
    se               = numeric(0),
    stat             = numeric(0),
    df               = numeric(0),
    p                = numeric(0),
    p_fmt            = character(0),
    sig              = character(0),
    omnibus_p        = numeric(0),
    omnibus_p_fmt    = character(0),
    omnibus_sig      = character(0),

    inp_file         = character(0),
    out_file         = character(0),
    stringsAsFactors = FALSE
  )
}

empty_bch_omnibus_basic <- function() {
  data.frame(var_name=character(0), var_label=character(0), chi_sq=numeric(0), df=numeric(0), p=numeric(0), p_fmt=character(0), sig=character(0), stringsAsFactors=FALSE)
}

build_bch_outcome_spec <- function(data, outcomes) {
  rows                         <- list(); idx <- 1L
  for (v in outcomes) {
    if (!v %in% names(data)) next
    x                          <- data[[v]]
    out_type                   <- if (is.factor(x) || is.character(x) || is.logical(x)) "categorical" else {
      ux                       <- unique(stats::na.omit(x))
      if (is.numeric(x) && length(ux) <= 5 && all(abs(ux - round(ux)) < 1e-8)) "categorical" else "continuous"
    }
    if (out_type == "continuous") {
      x2                       <- suppressWarnings(as.numeric(x))
    } else {
      if (is.factor(x)) x      <- as.character(x)
      if (is.logical(x)) x     <- as.integer(x)
      if (is.character(x)) {
        suppressWarnings(x_num <- as.numeric(x))
        if (sum(!is.na(x_num)) > 0 && sum(is.na(x_num)) < length(x_num)) x2 <- x_num else x2 <- as.integer(factor(x))
      } else x2 <- suppressWarnings(as.numeric(x))
    }
    data[[v]] <- x2
    rows[[idx]] <- data.frame(outcome=v, var_name=v, var_label=if (exists("resolve_var_label_local", mode="function")) resolve_var_label_local(v) else v, outcome_type=out_type, stringsAsFactors=FALSE)
    idx <- idx + 1L
  }
  spec_df <- if (length(rows) == 0) data.frame() else do.call(rbind, rows)
  if (is.data.frame(spec_df) && nrow(spec_df) > 0) rownames(spec_df) <- NULL
  list(data=data, spec=spec_df)
}

build_bch_input <- function(outcome_name, outcome_type, model_tag) {
  inp_file       <- file.path(DIR_MPLUS_BCH_INP, paste0(model_tag, ".inp"))
  out_file       <- resolve_out_file_from_inp(inp_file)
  usevars        <- c(INDICATORS, outcome_name, WEIGHT_VAR, STRATA_VAR, CLUSTER_VAR, ID_VAR)
  usevars        <- unique(usevars[!is.na(usevars) & nzchar(usevars)])
  variable_lines <- c("VARIABLE:", mplus_wrap_statement("NAMES", names(BCH_DATA)), mplus_wrap_statement("USEVARIABLES", usevars))
  cat_use        <- intersect(INDICATORS_CATEGORICAL %||% character(0), usevars)
  if (tolower(as.character(outcome_type)[1]) %in% c("categorical", "binary", "ordinal")) cat_use <- unique(c(cat_use, outcome_name))
  if (length(cat_use) > 0) variable_lines                <- c(variable_lines, mplus_wrap_statement("CATEGORICAL", cat_use))
  survey_lines                                           <- build_survey_variable_lines(WEIGHT_VAR, STRATA_VAR, CLUSTER_VAR)
  if (length(survey_lines) > 0) variable_lines           <- c(variable_lines, survey_lines)
  if (!is.null(ID_VAR) && nzchar(ID_VAR)) variable_lines <- c(variable_lines, paste0("  IDVARIABLE = ", ID_VAR, ";"))
  variable_lines                                         <- c(variable_lines, paste0("  CLASSES = c(", best_k, ");"), paste0("  MISSING = ALL(", MISSING_CODE, ");"), paste0("  AUXILIARY = ", outcome_name, " (BCH);"))
  analysis_lines                                         <- c("ANALYSIS:", make_mplus_type_line(WEIGHT_VAR, STRATA_VAR, CLUSTER_VAR))
  input_lines                                            <- c(paste0("TITLE: BCH for ", outcome_name, ";"), "", "DATA:", paste0("  FILE = ", normalizePath(BCH_DATA_FILE, winslash = "/", mustWork = FALSE), ";"), "", variable_lines, "", analysis_lines, "", make_output_lines_mplus(CFG))
  writeLines(input_lines, inp_file)
  list(inp_file=inp_file, out_file=out_file, model_tag=model_tag)
}

parse_bch_model_results <- function(lines,
                                    outcome_name,
                                    outcome_meta = NULL,
                                    best_k = NA_integer_,
                                    best_tag = NA_character_,
                                    model_structure = NA_character_) {

  # ----------------------------------------------------------
  # 0. empty guard
  # ----------------------------------------------------------
  if (length(lines) == 0) {
    return(empty_bch_long()[0, , drop = FALSE])
  }

  # ----------------------------------------------------------
  # 1. helpers
  # ----------------------------------------------------------
  trim2 <- function(x) trimws(gsub("[\t\r\n]+", " ", x))

  norm_space_collect <- function(x) {
    x <- gsub("\u00A0", " ", x, fixed = TRUE)
    x <- gsub("[[:space:]]+", " ", x)
    trimws(x)
  }

  safe_num <- function(x) suppressWarnings(as.numeric(x))

  add_row_df <- function(df, row) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      return(as.data.frame(row, stringsAsFactors = FALSE))
    }
    out <- rbind(df, as.data.frame(row, stringsAsFactors = FALSE))
    rownames(out) <- NULL
    out
  }

  get_label1 <- function(meta, default = NA_character_) {
    if (is.null(meta) || !is.data.frame(meta) || nrow(meta) == 0) return(default)
    cand <- c("label_en", "label_ko", "var_label", "label", "var_name")
    cand <- cand[cand %in% names(meta)]
    if (length(cand) == 0) return(default)
    for (cc in cand) {
      val <- as.character(meta[[cc]][1])
      if (!is.na(val) && nzchar(val)) return(val)
    }
    default
  }

  outcome_label <- get_label1(outcome_meta, default = outcome_name)
  outcome_type <- if (!is.null(outcome_meta) &&
                      is.data.frame(outcome_meta) &&
                      nrow(outcome_meta) > 0 &&
                      "outcome_type" %in% names(outcome_meta)) {
    as.character(outcome_meta$outcome_type[1])
  } else {
    NA_character_
  }

  # ----------------------------------------------------------
  # 2. normalize lines
  # ----------------------------------------------------------
  x <- vapply(lines, norm_space_collect, character(1))
  x <- x[!is.na(x) & nzchar(x)]

  if (length(x) == 0) {
    return(empty_bch_long()[0, , drop = FALSE])
  }

  # ----------------------------------------------------------
  # 3. overall test parser
  # ----------------------------------------------------------
  overall_df <- data.frame(stringsAsFactors = FALSE)
  class_df   <- data.frame(stringsAsFactors = FALSE)
  posthoc_df <- data.frame(stringsAsFactors = FALSE)

  for (i in seq_along(x)) {
    ln <- x[i]

    # 예: Wald Chi-Square = 12.345  df = 3  P-Value = 0.006
    if (grepl("Wald", ln, ignore.case = TRUE) &&
        grepl("Chi", ln, ignore.case = TRUE) &&
        grepl("P-?Value|p-?value|p\\s*=", ln, ignore.case = TRUE)) {

      stat_val <- NA_real_
      df_val   <- NA_real_
      p_val    <- NA_real_

      m1 <- regexec("Chi[- ]?Square\\s*=\\s*([-]?[0-9]*\\.?[0-9]+)", ln, ignore.case = TRUE)
      r1 <- regmatches(ln, m1)[[1]]
      if (length(r1) >= 2) stat_val <- safe_num(r1[2])

      m2 <- regexec("\\bdf\\s*=\\s*([-]?[0-9]*\\.?[0-9]+)", ln, ignore.case = TRUE)
      r2 <- regmatches(ln, m2)[[1]]
      if (length(r2) >= 2) df_val <- safe_num(r2[2])

      m3 <- regexec("(P-?Value|p-?value|\\bp\\b)\\s*=\\s*([.0-9]+)", ln, ignore.case = TRUE)
      r3 <- regmatches(ln, m3)[[1]]
      if (length(r3) >= 3) p_val <- safe_num(r3[3])

      overall_df <- add_row_df(overall_df, list(
        outcome            = outcome_name,
        var_name           = outcome_name,
        var_label          = outcome_label,
        outcome_type       = outcome_type,
        result_type        = "overall",
        class              = NA_character_,
        class_num          = NA_integer_,
        contrast           = NA_character_,
        estimate           = NA_real_,
        se                 = NA_real_,
        stat               = stat_val,
        df                 = df_val,
        p                  = p_val,
        best_k             = best_k,
        best_tag           = best_tag,
        model_structure    = model_structure
      ))
    }
  }

  # ----------------------------------------------------------
  # 3B. Mplus BCH equality-test block parser
  # ----------------------------------------------------------
  eq_start <- grep("EQUALITY TESTS OF MEANS ACROSS CLASSES USING THE BCH PROCEDURE", x, ignore.case = TRUE)

  if (length(eq_start) > 0) {
    i0 <- eq_start[1]
    df_overall <- NA_real_

    if ((i0 + 1) <= length(x)) {
      m_df <- regexec("WITH\\s+([0-9]+)\\s+DEGREE\\(S\\) OF FREEDOM", x[i0 + 1], ignore.case = TRUE)
      r_df <- regmatches(x[i0 + 1], m_df)[[1]]
      if (length(r_df) >= 2) df_overall <- safe_num(r_df[2])
    }

    # class means/se block
    mean_header_idx <- grep("^Mean\\s+S\\.E\\.", x[(i0 + 1):length(x)], ignore.case = TRUE)
    if (length(mean_header_idx) > 0) {
      mean_i <- i0 + mean_header_idx[1]
      j <- mean_i + 1L
      while (j <= length(x) &&
             nzchar(x[j]) &&
             !grepl("^Chi-Square\\s+P-Value", x[j], ignore.case = TRUE) &&
             !grepl("^TECHNICAL\\s+[0-9]+\\s+OUTPUT", x[j], ignore.case = TRUE)) {
        ln <- x[j]
        mm <- gregexpr(
          "Class\\s+([0-9]+)\\s+([-]?[0-9]*\\.?[0-9]+)\\s+([-]?[0-9]*\\.?[0-9]+)",
          ln,
          perl = TRUE,
          ignore.case = TRUE
        )
        rr <- regmatches(ln, mm)[[1]]
        if (length(rr) > 0) {
          for (hit in rr) {
            one <- regexec(
              "Class\\s+([0-9]+)\\s+([-]?[0-9]*\\.?[0-9]+)\\s+([-]?[0-9]*\\.?[0-9]+)",
              hit,
              perl = TRUE,
              ignore.case = TRUE
            )
            tok <- regmatches(hit, one)[[1]]
            if (length(tok) >= 4) {
              cls_num <- as.integer(tok[2])
              est_val <- safe_num(tok[3])
              se_val  <- safe_num(tok[4])
              class_df <- add_row_df(class_df, list(
                outcome            = outcome_name,
                var_name           = outcome_name,
                var_label          = outcome_label,
                outcome_type       = outcome_type,
                result_type        = "class_estimate",
                class              = paste0("Class ", cls_num),
                class_num          = cls_num,
                contrast           = NA_character_,
                estimate           = est_val,
                se                 = se_val,
                stat               = NA_real_,
                df                 = NA_real_,
                p                  = NA_real_,
                best_k             = best_k,
                best_tag           = best_tag,
                model_structure    = model_structure
              ))
            }
          }
        }
        j <- j + 1L
      }
    }

    # overall + pairwise chi-square/p block
    test_header_idx <- grep("^Chi-Square\\s+P-Value", x[(i0 + 1):length(x)], ignore.case = TRUE)
    if (length(test_header_idx) > 0) {
      test_i <- i0 + test_header_idx[1]
      j <- test_i + 1L
      while (j <= length(x) &&
             nzchar(x[j]) &&
             !grepl("^TECHNICAL\\s+[0-9]+\\s+OUTPUT", x[j], ignore.case = TRUE)) {
        ln <- x[j]

        overall_hit <- regexec(
          "Overall test\\s+([-]?[0-9]*\\.?[0-9]+)\\s+([.0-9]+)",
          ln,
          ignore.case = TRUE
        )
        overall_tok <- regmatches(ln, overall_hit)[[1]]
        if (length(overall_tok) >= 3) {
          stat_val <- safe_num(overall_tok[2])
          p_val    <- safe_num(overall_tok[3])
          overall_df <- add_row_df(overall_df, list(
            outcome            = outcome_name,
            var_name           = outcome_name,
            var_label          = outcome_label,
            outcome_type       = outcome_type,
            result_type        = "overall",
            class              = NA_character_,
            class_num          = NA_integer_,
            contrast           = "Overall test",
            estimate           = NA_real_,
            se                 = NA_real_,
            stat               = stat_val,
            df                 = df_overall,
            p                  = p_val,
            best_k             = best_k,
            best_tag           = best_tag,
            model_structure    = model_structure
          ))
        }

        pair_hits <- gregexpr(
          "Class\\s+([0-9]+)\\s+vs\\.\\s+([0-9]+)\\s+([-]?[0-9]*\\.?[0-9]+)\\s+([.0-9]+)",
          ln,
          perl = TRUE,
          ignore.case = TRUE
        )
        pair_str <- regmatches(ln, pair_hits)[[1]]
        if (length(pair_str) > 0) {
          for (hit in pair_str) {
            one <- regexec(
              "Class\\s+([0-9]+)\\s+vs\\.\\s+([0-9]+)\\s+([-]?[0-9]*\\.?[0-9]+)\\s+([.0-9]+)",
              hit,
              perl = TRUE,
              ignore.case = TRUE
            )
            tok <- regmatches(hit, one)[[1]]
            if (length(tok) >= 5) {
              c1 <- as.integer(tok[2])
              c2 <- as.integer(tok[3])
              stat_val <- safe_num(tok[4])
              p_val    <- safe_num(tok[5])

              posthoc_df <- add_row_df(posthoc_df, list(
                outcome            = outcome_name,
                var_name           = outcome_name,
                var_label          = outcome_label,
                outcome_type       = outcome_type,
                result_type        = "posthoc",
                class              = NA_character_,
                class_num          = NA_integer_,
                contrast           = paste0("Class ", c1, " vs Class ", c2),
                estimate           = NA_real_,
                se                 = NA_real_,
                stat               = stat_val,
                df                 = NA_real_,
                p                  = p_val,
                best_k             = best_k,
                best_tag           = best_tag,
                model_structure    = model_structure,
                class1             = c1,
                class2             = c2
              ))
            }
          }
        }
        j <- j + 1L
      }
    }
  }

  # ----------------------------------------------------------
  # 4. class-specific estimates parser
  # ----------------------------------------------------------
  # 핵심: 각 class 결과를 절대 1행으로 축소하지 않음
  # ----------------------------------------------------------
  for (ln in x) {
    tokens <- unlist(strsplit(ln, "\\s+"))
    if (length(tokens) < 2) next

    if (length(tokens) >= 4 &&
        tolower(tokens[1]) == "class" &&
        !is.na(safe_num(tokens[2])) &&
        !is.na(safe_num(tokens[3]))) {

      cls_num <- as.integer(safe_num(tokens[2]))
      est_val <- safe_num(tokens[3])
      se_val  <- if (length(tokens) >= 4) safe_num(tokens[4]) else NA_real_

      if (!is.na(cls_num) && !is.na(est_val)) {
        class_df <- add_row_df(class_df, list(
          outcome            = outcome_name,
          var_name           = outcome_name,
          var_label          = outcome_label,
          outcome_type       = outcome_type,
          result_type        = "class_estimate",
          class              = paste0("Class ", cls_num),
          class_num          = cls_num,
          contrast           = NA_character_,
          estimate           = est_val,
          se                 = se_val,
          stat               = NA_real_,
          df                 = NA_real_,
          p                  = NA_real_,
          best_k             = best_k,
          best_tag           = best_tag,
          model_structure    = model_structure
        ))
        next
      }
    }

    # 패턴 2: "C#1 3.276 0.189"
    if (length(tokens) >= 3 &&
        grepl("^C#?[0-9]+$", toupper(tokens[1]))) {

      cls_num <- as.integer(gsub("[^0-9]", "", tokens[1]))
      est_val <- safe_num(tokens[2])
      se_val  <- if (length(tokens) >= 3) safe_num(tokens[3]) else NA_real_

      if (!is.na(cls_num) && !is.na(est_val)) {
        class_df <- add_row_df(class_df, list(
          outcome            = outcome_name,
          var_name           = outcome_name,
          var_label          = outcome_label,
          outcome_type       = outcome_type,
          result_type        = "class_estimate",
          class              = paste0("Class ", cls_num),
          class_num          = cls_num,
          contrast           = NA_character_,
          estimate           = est_val,
          se                 = se_val,
          stat               = NA_real_,
          df                 = NA_real_,
          p                  = NA_real_,
          best_k             = best_k,
          best_tag           = best_tag,
          model_structure    = model_structure
        ))
        next
      }
    }

    # 패턴 3: "1 3.276 0.189" 같은 숫자 행
    if (length(tokens) >= 3 &&
        !is.na(safe_num(tokens[1])) &&
        !is.na(safe_num(tokens[2]))) {

      cls_num <- as.integer(safe_num(tokens[1]))
      est_val <- safe_num(tokens[2])
      se_val  <- safe_num(tokens[3])

      if (!is.na(cls_num) &&
          cls_num >= 1 &&
          !is.na(best_k) &&
          cls_num <= best_k &&
          !is.na(est_val)) {
        class_df <- add_row_df(class_df, list(
          outcome            = outcome_name,
          var_name           = outcome_name,
          var_label          = outcome_label,
          outcome_type       = outcome_type,
          result_type        = "class_estimate",
          class              = paste0("Class ", cls_num),
          class_num          = cls_num,
          contrast           = NA_character_,
          estimate           = est_val,
          se                 = se_val,
          stat               = NA_real_,
          df                 = NA_real_,
          p                  = NA_real_,
          best_k             = best_k,
          best_tag           = best_tag,
          model_structure    = model_structure
        ))
      }
    }
  }

  # 중복 제거: class_num 기준으로 첫 행 유지
  if (is.data.frame(class_df) && nrow(class_df) > 0) {
    class_df <- class_df[order(class_df$class_num), , drop = FALSE]
    class_df <- class_df[!duplicated(class_df$class_num), , drop = FALSE]
    rownames(class_df) <- NULL
  }

  # ----------------------------------------------------------
  # 5. pairwise / posthoc parser
  # ----------------------------------------------------------
  for (ln in x) {
    # 가장 흔한 형태 먼저
    m <- regexec(
      "([0-9]+)\\s*(vs|VS|Vs|-)\\s*([0-9]+)\\s+([-]?[0-9]*\\.?[0-9]+)\\s+([-]?[0-9]*\\.?[0-9]+)\\s+([-]?[0-9]*\\.?[0-9]+)\\s+([.0-9]+)",
      ln
    )
    rr <- regmatches(ln, m)[[1]]

    if (length(rr) >= 8) {
      c1 <- as.integer(rr[2])
      c2 <- as.integer(rr[4])
      est_val  <- safe_num(rr[5])
      se_val   <- safe_num(rr[6])
      stat_val <- safe_num(rr[7])
      p_val    <- safe_num(rr[8])

      posthoc_df <- add_row_df(posthoc_df, list(
        outcome            = outcome_name,
        var_name           = outcome_name,
        var_label          = outcome_label,
        outcome_type       = outcome_type,
        result_type        = "posthoc",
        class              = NA_character_,
        class_num          = NA_integer_,
        contrast           = paste0("Class ", c1, " vs Class ", c2),
        estimate           = est_val,
        se                 = se_val,
        stat               = stat_val,
        df                 = NA_real_,
        p                  = p_val,
        best_k             = best_k,
        best_tag           = best_tag,
        model_structure    = model_structure,
        class1             = c1,
        class2             = c2
      ))
    }
  }

  # ----------------------------------------------------------
  # FIX: align columns before rbind
  # ----------------------------------------------------------
  align_df <- function(df, cols) {
    if (!is.data.frame(df) || nrow(df) == 0) {
      return(data.frame())
    }

    for (cc in setdiff(cols, names(df))) {
      df[[cc]] <- NA
    }

    df[, cols, drop = FALSE]
  }

  all_cols <- unique(c(
    names(overall_df),
    names(class_df),
    names(posthoc_df)
  ))

  overall_df <- align_df(overall_df, all_cols)
  class_df   <- align_df(class_df, all_cols)
  posthoc_df <- align_df(posthoc_df, all_cols)

  # ----------------------------------------------------------
  # 🔥 FIX: force consistent structure BEFORE rbind
  # ----------------------------------------------------------

  # 1. 기준 컬럼 정의 (empty_bch_long 기준)
  base_cols <- names(empty_bch_long())

  force_df <- function(df) {
    if (!is.data.frame(df) || nrow(df) == 0) {
      return(data.frame(matrix(ncol = length(base_cols), nrow = 0, dimnames = list(NULL, base_cols))))
    }

    for (cc in base_cols) {
      if (!cc %in% names(df)) {
        df[[cc]] <- NA
      }
    }

    df <- df[, base_cols, drop = FALSE]
    rownames(df) <- NULL
    df
  }

  overall_df <- force_df(overall_df)
  class_df   <- force_df(class_df)
  posthoc_df <- force_df(posthoc_df)

    # ----------------------------------------------------------
  # 6. bind all
  # ----------------------------------------------------------
  out <- rbind(overall_df, class_df, posthoc_df)

  out <- as.data.frame(out, stringsAsFactors = FALSE)

  if (is.null(out) || !is.data.frame(out) || nrow(out) == 0) {
    return(empty_bch_long()[0, , drop = FALSE])
  }

  # ----------------------------------------------------------
  # 7. final ordering
  # ----------------------------------------------------------
  need_cols <- names(empty_bch_long())
  miss_cols <- setdiff(need_cols, names(out))
  if (length(miss_cols) > 0) {
    for (cc in miss_cols) out[[cc]] <- NA
  }
  out <- out[, need_cols, drop = FALSE]
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  rownames(out) <- NULL

  overall_rows <- out[out$result_type == "overall", , drop = FALSE]
  if (nrow(overall_rows) > 0) {
    stat1 <- overall_rows$stat[which(!is.na(overall_rows$stat))[1]]
    df1   <- overall_rows$df[which(!is.na(overall_rows$df))[1]]
    p1    <- overall_rows$p[which(!is.na(overall_rows$p))[1]]
    p1_fmt <- if (!is.na(p1)) fmt_p_tbl(p1) else NA_character_
    p1_sig <- if (!is.na(p1)) p_to_sig_tbl(p1) else ""

    hit_class <- out$result_type == "class_estimate"
    if (any(hit_class)) {
      if (!is.na(stat1)) out$stat[hit_class] <- stat1
      if (!is.na(df1)) out$df[hit_class] <- df1
      if (!is.na(p1)) out$omnibus_p[hit_class] <- p1
      if (!is.na(p1_fmt)) out$omnibus_p_fmt[hit_class] <- p1_fmt
      if (!is.na(p1_sig)) out$omnibus_sig[hit_class] <- p1_sig
    }
  }

  n_out <- nrow(out)

  # 항상 nrow(out) 길이로 강제 생성
  ord_type <- rep(999L, n_out)
  if ("result_type" %in% names(out)) {
    tmp <- match(as.character(out$result_type), c("overall", "class_estimate", "posthoc"))
    ord_type[!is.na(tmp)] <- tmp[!is.na(tmp)]
  }

  ord_class <- rep(999L, n_out)
  if ("class_num" %in% names(out)) {
    tmp <- suppressWarnings(as.integer(out$class_num))
    ord_class[!is.na(tmp)] <- tmp[!is.na(tmp)]
  }

  ord_contrast <- rep("", n_out)
  if ("contrast" %in% names(out)) {
    tmp <- as.character(out$contrast)
    tmp[is.na(tmp)] <- ""
    ord_contrast <- tmp
  }

  out <- out[order(ord_type, ord_class, ord_contrast), , drop = FALSE]
  rownames(out) <- NULL

  if (nrow(posthoc_df) > 0) {
    rownames(posthoc_df) <- NULL
    attr(out, "posthoc") <- posthoc_df
  }

  return(out)
}

run_bch_basic_subset    <- function(DATA_SUB, DICT, CFG, OUTCOMES, best_k, best_tag, best_model_structure, DATASET_ID, RUN_MPLUS, MPLUS_EXE, INDICATORS, INDICATORS_CATEGORICAL, INDICATORS_CONTINUOUS, WEIGHT_VAR, STRATA_VAR, CLUSTER_VAR, ID_VAR, MISSING_CODE, MIXTURE_TYPE, DIR_MPLUS_BCH_INP, BCH_DATA_FILE, BCH_DATA, build_bch_input_fun, parse_bch_model_results_fun, make_bch_outcome_spec_fun = build_bch_outcome_spec, read_mplus_out_lines_fun, run_mplus_model_fun, write_mplus_data_fun) {
  BCH_UNIVARIABLE       <- list(); BCH_POSTHOC <- list();
  BCH_RUN_REGISTRY      <- list()
  if (!is.data.frame(DATA_SUB) || nrow(DATA_SUB) == 0) return(list(BCH_RESULTS_FULL=empty_bch_long()[0,,drop=FALSE],
                                                                   BCH_POSTHOC=data.frame(),
                                                                   BCH_OMNIBUS_BASIC=empty_bch_omnibus_basic(),
                                                                   BCH_RUN_REGISTRY=list()))
  bch_obj_sub           <- make_bch_outcome_spec_fun(DATA_SUB, OUTCOMES)
  BCH_DATA_SUB          <- bch_obj_sub$data; BCH_OUTCOME_SPEC_SUB <- bch_obj_sub$spec
  if (!is.data.frame(BCH_OUTCOME_SPEC_SUB) || nrow(BCH_OUTCOME_SPEC_SUB) == 0) return(list(BCH_RESULTS_FULL=empty_bch_long()[0,,drop=FALSE], BCH_POSTHOC=data.frame(), BCH_OMNIBUS_BASIC=empty_bch_omnibus_basic(), BCH_RUN_REGISTRY=list()))
  for (nm in names(BCH_DATA_SUB)) {
    x                                       <- BCH_DATA_SUB[[nm]]
    if (is.factor(x)) x                     <- as.character(x)
    if (is.logical(x)) x                    <- as.integer(x)
    if (is.character(x)) suppressWarnings(x <- as.numeric(x))
    if (!is.numeric(x)) suppressWarnings(x  <- as.numeric(x))
    x[is.na(x)]                             <- MISSING_CODE
    BCH_DATA_SUB[[nm]]                      <- x
  }
  write_mplus_data_fun(BCH_DATA_SUB, BCH_DATA_FILE, missing_code = MISSING_CODE)
  local_env                          <- new.env(parent = environment(build_bch_input_fun))
  local_env$BCH_DATA                 <- BCH_DATA_SUB;
  local_env$BCH_DATA_FILE            <- BCH_DATA_FILE;
  local_env$DIR_MPLUS_BCH_INP        <- DIR_MPLUS_BCH_INP;
  local_env$INDICATORS               <- INDICATORS;
  local_env$INDICATORS_CATEGORICAL   <- INDICATORS_CATEGORICAL;
  local_env$INDICATORS_CONTINUOUS    <- INDICATORS_CONTINUOUS;
  local_env$WEIGHT_VAR               <- WEIGHT_VAR;
  local_env$STRATA_VAR               <- STRATA_VAR;
  local_env$CLUSTER_VAR              <- CLUSTER_VAR;
  local_env$ID_VAR                   <- ID_VAR;
  local_env$MISSING_CODE             <- MISSING_CODE;
  local_env$MIXTURE_TYPE             <- MIXTURE_TYPE;
  local_env$best_k                   <- best_k; local_env$CFG <- CFG
  build_bch_input_local              <- function(outcome_name, outcome_type, model_tag) {
    environment(build_bch_input_fun) <- local_env; build_bch_input_fun(outcome_name, outcome_type, model_tag) }
  for (i in seq_len(nrow(BCH_OUTCOME_SPEC_SUB))) {
    spec_i                                 <- BCH_OUTCOME_SPEC_SUB[i,,drop=FALSE];
    outcome_i                              <- spec_i$outcome[1]; outcome_type_i <- spec_i$outcome_type[1]
    model_tag_i                            <- paste0(tolower(DATASET_ID), "_bch_", best_model_structure, "_k", best_k, "_", make_clean_names(outcome_i))
    io_i                                   <- build_bch_input_local(outcome_i, outcome_type_i, model_tag_i)
    if (isTRUE(RUN_MPLUS)) {
      exec_res                             <- run_mplus_model_fun(inp_file=io_i$inp_file, mplus_exe=MPLUS_EXE, workdir=dirname(io_i$inp_file), wait=TRUE, intern=FALSE, quiet=TRUE)
    } else exec_res                        <- list(ok=TRUE,status=NA_integer_,output=NULL,error=NULL)
    out_path_i                             <- io_i$out_file
    if (!file.exists(out_path_i)) next
    lines_i                                <- read_mplus_out_lines_fun(out_path_i)
    parsed_i                               <- parse_bch_model_results_fun(lines_i, outcome_i, spec_i, best_k, best_tag, best_model_structure)
    posthoc_i                              <- attr(parsed_i, "posthoc")

    if (is.data.frame(posthoc_i) && nrow(posthoc_i) > 0) BCH_POSTHOC[[length(BCH_POSTHOC)+1L]] <- posthoc_i

    if (is.data.frame(parsed_i) && nrow(parsed_i) > 0) {
      parsed_i$inp_file   <- io_i$inp_file;
      parsed_i$out_file   <- out_path_i;
      BCH_UNIVARIABLE[[length(BCH_UNIVARIABLE)+1L]] <- parsed_i }

    BCH_RUN_REGISTRY[[i]] <- data.frame(
      outcome          = outcome_i,
      outcome_type     = outcome_type_i,
      best_k           = best_k,
      best_tag         = best_tag,
      model_structure  = best_model_structure,
      inp_file         = io_i$inp_file,
      out_file         = out_path_i,
      exec_ok          = isTRUE(exec_res$ok),
      exec_status      = as.character(exec_res$status %||% ""),
      exec_error       = as.character(exec_res$error %||% ""),
      stringsAsFactors = FALSE
    )
  }
  BCH_UNIVARIABLE_DF           <- if (length(BCH_UNIVARIABLE)==0) empty_bch_long() else do.call(rbind, BCH_UNIVARIABLE)
  rownames(BCH_UNIVARIABLE_DF) <- NULL
  BCH_RESULTS_FULL             <- ensure_df(BCH_UNIVARIABLE_DF)
  BCH_POSTHOC_DF               <- if (length(BCH_POSTHOC)==0) data.frame() else do.call(rbind, BCH_POSTHOC)
  rownames(BCH_POSTHOC_DF)     <- NULL

  if (nrow(BCH_RESULTS_FULL) > 0) {
    BCH_RESULTS_FULL           <- BCH_RESULTS_FULL[order(match(BCH_RESULTS_FULL$var_name, OUTCOMES), BCH_RESULTS_FULL$class_num), , drop=FALSE]
    rownames(BCH_RESULTS_FULL) <- NULL
  }
  BCH_OMNIBUS_BASIC  <- if (nrow(BCH_RESULTS_FULL) == 0) empty_bch_omnibus_basic() else {
    om               <- unique(BCH_RESULTS_FULL[, c("var_name", "var_label", "stat", "df", "omnibus_p", "omnibus_p_fmt", "omnibus_sig"), drop = FALSE])
    names(om)        <- c("var_name", "var_label", "chi_sq", "df", "p", "p_fmt", "sig")
    om$display_order <- match(om$var_name, OUTCOMES)
    om               <- om[order(om$display_order, om$var_name), , drop = FALSE]
    om$display_order <- NULL
    rownames(om)     <- NULL
    om
  }

  if (length(BCH_RUN_REGISTRY) == 0) {
    BCH_RUN_REGISTRY_DF <- data.frame()
  } else {
    BCH_RUN_REGISTRY_DF <- do.call(rbind, BCH_RUN_REGISTRY)
    rownames(BCH_RUN_REGISTRY_DF) <- NULL
  }

  list(
    BCH_RESULTS_FULL  = BCH_RESULTS_FULL,
    BCH_POSTHOC       = BCH_POSTHOC_DF,
    BCH_OMNIBUS_BASIC = BCH_OMNIBUS_BASIC,
    BCH_RUN_REGISTRY  = BCH_RUN_REGISTRY_DF
  )
}
