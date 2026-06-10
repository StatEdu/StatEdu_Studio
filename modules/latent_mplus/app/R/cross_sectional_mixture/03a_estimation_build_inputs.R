# ============================================================
# 03a_estimation_build_inputs.R
# Build Mplus input files for mixture models
# ------------------------------------------------------------
# 역할
# 1) prep/settings 산출물 재로딩
# 2) mixture model용 Mplus .inp 생성
# 3) run_plan 및 estimation build summary 저장
#
# 핵심 수정
# - Mplus OUTPUT block을 CFG 기반으로 제어
# - 기본값은 조용하게(false)
# - model_structure_mode 지원: single / compare
# - model_structure / model_structures 지원
# - model_tag에 model_structure 포함
# - run_plan / ESTIMATION_REGISTRY에 model_structure 저장
# ============================================================

T0_03A <- Sys.time()

# ------------------------------------------------------------
# 0. helpers
# ------------------------------------------------------------
.now_txt <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

log_txt <- function(level = "INFO", ...) {
  cat(sprintf("[%s] [%s] %s\n", .now_txt(), level, paste0(...)))
}
log_info <- function(...) log_txt("INFO", ...)
log_warn <- function(...) log_txt("WARN", ...)
log_err  <- function(...) log_txt("ERROR", ...)

`%||%` <- function(x, y) if (is.null(x)) y else x

collapse_nonempty <- function(x, sep = " ") {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x) == 0) return("")
  paste(x, collapse = sep)
}

wrap_mplus_statement <- function(keyword, vars, indent = "  ", width = 78) {
  vars <- unique(as.character(vars))
  vars <- vars[!is.na(vars) & nzchar(vars)]

  if (length(vars) == 0) return(character(0))

  line_prefix <- paste0(indent, keyword, " = ")
  cont_prefix <- paste0(indent, "  ")

  out <- character(0)
  current <- line_prefix

  for (v in vars) {
    candidate <- if (identical(current, line_prefix)) {
      paste0(current, v)
    } else {
      paste(current, v)
    }

    if (nchar(candidate, type = "width") > width) {
      out <- c(out, current)
      current <- paste0(cont_prefix, v)
    } else {
      current <- candidate
    }
  }

  c(out, paste0(current, ";"))
}

make_output_lines_main <- function(CFG = NULL) {
  mp <- CFG$mplus %||% list()
  out_cfg <- mp$output %||% list()

  sampstat     <- isTRUE(out_cfg$sampstat %||% FALSE)
  tech1        <- isTRUE(out_cfg$tech1 %||% FALSE)
  tech4        <- isTRUE(out_cfg$tech4 %||% FALSE)
  tech8        <- isTRUE(out_cfg$tech8 %||% FALSE)
  tech11       <- isTRUE(out_cfg$tech11 %||% FALSE)
  tech14       <- isTRUE(out_cfg$tech14 %||% FALSE)
  standardized <- isTRUE(out_cfg$standardized %||% FALSE)

  out <- character(0)

  if (sampstat)     out <- c(out, "SAMPSTAT;")
  if (tech1)        out <- c(out, "TECH1;")
  if (tech4)        out <- c(out, "TECH4;")
  if (tech11)       out <- c(out, "TECH11;")
  if (tech14)       out <- c(out, "TECH14;")
  if (standardized) out <- c(out, "STANDARDIZED;")
  if (tech8)        out <- c(out, "TECH8;")

  out
}

resolve_model_structures <- function(cfg) {
  mode_i <- tolower(
    cfg$estimation$model_structure_mode %||%
      cfg$model_structure_mode %||%
      "single"
  )

  one_i <- tolower(
    cfg$estimation$model_structure %||%
      cfg$model_structure %||%
      "model2"
  )

  many_i <- cfg$estimation$model_structures %||%
    cfg$model_structures %||%
    NULL

  valid_models <- c("model1", "model2", "model3", "model4")

  if (identical(mode_i, "compare")) {
    if (is.null(many_i) || length(many_i) == 0) {
      many_i <- valid_models
    }
    many_i <- unique(tolower(as.character(many_i)))
    many_i <- many_i[many_i %in% valid_models]
    if (length(many_i) == 0) many_i <- "model2"
    return(many_i)
  }

  if (!one_i %in% valid_models) one_i <- "model2"
  one_i
}

make_lpa_model_lines <- function(model_structure, indicators_continuous) {
  inds <- unique(as.character(indicators_continuous))
  inds <- inds[!is.na(inds) & nzchar(inds)]

  if (length(inds) == 0) {
    return(c("  %OVERALL%"))
  }

  # ----------------------------------------------------------
  # Practical mapping for LPA model structures
  # ----------------------------------------------------------
  # model1: equal variances, zero covariances
  # model2: varying variances, zero covariances
  # model3: equal variances, equal covariances
  # model4: varying variances, varying covariances
  #
  # 주의:
  # 아래는 현재 파이프라인 확장용 "구조 분기 뼈대"이다.
  # 실제 연구 설계에서 원하는 Mplus 제약식에 맞게
  # 추후 더 정밀하게 조정 가능.
  # ----------------------------------------------------------

  if (model_structure == "model1") {
    return(c(
      "  %OVERALL%",
      paste0("  ", paste(inds, collapse = " "), ";"),
      paste0("  [", paste(inds, collapse = " "), "];")
    ))
  }

  if (model_structure == "model2") {
    return(c(
      "  %OVERALL%",
      paste0("  [", paste(inds, collapse = " "), "];")
    ))
  }

  if (model_structure == "model3") {
    pair_terms <- character(0)
    if (length(inds) >= 2) {
      pair_terms <- combn(
        inds,
        2,
        FUN = function(z) paste0(z[1], " WITH ", z[2]),
        simplify = TRUE
      )
    }

    out <- c(
      "  %OVERALL%",
      paste0("  ", paste(inds, collapse = " "), ";"),
      paste0("  [", paste(inds, collapse = " "), "];")
    )

    if (length(pair_terms) > 0) {
      out <- c(out, paste0("  ", pair_terms, ";"))
    }
    return(out)
  }

  if (model_structure == "model4") {
    return(c(
      "  %OVERALL%",
      paste0("  [", paste(inds, collapse = " "), "];")
    ))
  }

  c(
    "  %OVERALL%",
    paste0("  [", paste(inds, collapse = " "), "];")
  )
}

make_lca_model_lines <- function(model_structure, indicators_categorical) {
  inds <- unique(as.character(indicators_categorical))
  inds <- inds[!is.na(inds) & nzchar(inds)]

  if (length(inds) == 0) {
    return(c("  %OVERALL%"))
  }

  # 현재는 안전하게 기존형 유지
  c("  %OVERALL%")
}

make_mixed_model_lines <- function(model_structure, indicators_continuous, indicators_categorical) {
  cont <- unique(as.character(indicators_continuous))
  cont <- cont[!is.na(cont) & nzchar(cont)]
  cat <- unique(as.character(indicators_categorical))
  cat <- cat[!is.na(cat) & nzchar(cat)]

  if (length(cont) == 0 && length(cat) == 0) {
    return(c("  %OVERALL%"))
  }

  cont_lines <- make_lpa_model_lines(
    model_structure = model_structure,
    indicators_continuous = cont
  )

  if (length(cont_lines) == 0) cont_lines <- "  %OVERALL%"
  unique(cont_lines)
}

# ------------------------------------------------------------
# 1. reload prep outputs
# ------------------------------------------------------------
log_info("Reloading prep outputs ...")

CFG <- load_step_rds("CFG", dir_rds = DIR_RDS, required = TRUE)
SETTINGS_SUMMARY <- load_step_rds("SETTINGS_SUMMARY", dir_rds = DIR_RDS, required = TRUE)
PREP_SUMMARY <- load_step_rds("PREP_SUMMARY", dir_rds = DIR_RDS, required = TRUE)
MPLUS_DATA <- load_step_rds("MPLUS_DATA", dir_rds = DIR_RDS, required = TRUE)
SURVEY_BUNDLE <- load_step_rds("SURVEY_BUNDLE", dir_rds = DIR_RDS, default = list())

if (!is.data.frame(MPLUS_DATA) || nrow(MPLUS_DATA) == 0) {
  stop("MPLUS_DATA is missing or empty.", call. = FALSE)
}

dir.create(DIR_MPLUS_INP, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_MPLUS_OUT, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_MPLUS_SAVEDATA, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 2. resolve core settings
# ------------------------------------------------------------
MIXTURE_TYPE <- tolower(
  SETTINGS_SUMMARY$mixture_type %||%
    SETTINGS_SUMMARY$MIXTURE_TYPE %||%
    "lpa"
)
if (!is.na(MIXTURE_TYPE) && MIXTURE_TYPE %in% c("auto", "mixed_model", "mixed-model")) {
  MIXTURE_TYPE <- NA_character_
}

INDICATORS <- SETTINGS_SUMMARY$indicators %||% SETTINGS_SUMMARY$INDICATORS %||% character(0)
INDICATORS_CONTINUOUS <- SETTINGS_SUMMARY$indicators_continuous %||% SETTINGS_SUMMARY$INDICATORS_CONTINUOUS %||% character(0)
INDICATORS_CATEGORICAL <- SETTINGS_SUMMARY$indicators_categorical %||% SETTINGS_SUMMARY$INDICATORS_CATEGORICAL %||% character(0)
COVARIATES <- SETTINGS_SUMMARY$covariates %||% SETTINGS_SUMMARY$COVARIATES %||% character(0)
OUTCOMES <- SETTINGS_SUMMARY$outcomes %||% SETTINGS_SUMMARY$OUTCOMES %||% character(0)

WEIGHT_VAR  <- SURVEY_BUNDLE$weight_var %||% SETTINGS_SUMMARY$weight_var %||% SETTINGS_SUMMARY$WEIGHT_VAR %||% NULL
STRATA_VAR  <- SURVEY_BUNDLE$strata_var %||% SETTINGS_SUMMARY$strata_var %||% SETTINGS_SUMMARY$STRATA_VAR %||% NULL
CLUSTER_VAR <- SURVEY_BUNDLE$cluster_var %||% SETTINGS_SUMMARY$cluster_var %||% SETTINGS_SUMMARY$CLUSTER_VAR %||% NULL
ID_VAR      <- SURVEY_BUNDLE$id_var %||% SETTINGS_SUMMARY$id_var %||% SETTINGS_SUMMARY$ID_VAR %||% NULL

INDICATORS <- intersect(unique(INDICATORS), names(MPLUS_DATA))
INDICATORS_CONTINUOUS <- intersect(unique(INDICATORS_CONTINUOUS), names(MPLUS_DATA))
INDICATORS_CATEGORICAL <- intersect(unique(INDICATORS_CATEGORICAL), names(MPLUS_DATA))
COVARIATES <- intersect(unique(COVARIATES), names(MPLUS_DATA))
OUTCOMES <- intersect(unique(OUTCOMES), names(MPLUS_DATA))

if (is.na(MIXTURE_TYPE) || !nzchar(MIXTURE_TYPE)) {
  if (length(INDICATORS_CATEGORICAL) > 0 && length(INDICATORS_CONTINUOUS) == 0) {
    MIXTURE_TYPE <- "lca"
  } else if (length(INDICATORS_CONTINUOUS) > 0 && length(INDICATORS_CATEGORICAL) == 0) {
    MIXTURE_TYPE <- "lpa"
  } else if (length(INDICATORS_CONTINUOUS) > 0 && length(INDICATORS_CATEGORICAL) > 0) {
    MIXTURE_TYPE <- "mixed"
  } else {
    MIXTURE_TYPE <- "lpa"
  }
}

if (!MIXTURE_TYPE %in% c("lca", "lpa", "mixed")) {
  stop("Invalid MIXTURE_TYPE: ", MIXTURE_TYPE, call. = FALSE)
}

if (!is.null(WEIGHT_VAR)  && !WEIGHT_VAR  %in% names(MPLUS_DATA)) WEIGHT_VAR  <- NULL
if (!is.null(STRATA_VAR)  && !STRATA_VAR  %in% names(MPLUS_DATA)) STRATA_VAR  <- NULL
if (!is.null(CLUSTER_VAR) && !CLUSTER_VAR %in% names(MPLUS_DATA)) CLUSTER_VAR <- NULL
if (!is.null(ID_VAR)      && !ID_VAR      %in% names(MPLUS_DATA)) ID_VAR      <- NULL

if (length(INDICATORS) == 0) {
  stop("No indicators found for mixture modeling.", call. = FALSE)
}

K_VALUES <- CFG$estimation$k_values %||% CFG$k_values %||% NULL
if (is.null(K_VALUES) || length(K_VALUES) == 0) {
  k_min    <- suppressWarnings(as.integer(CFG$estimation$k_min %||% 2L))
  k_max    <- suppressWarnings(as.integer(CFG$estimation$k_max %||% 5L))
  K_VALUES <- seq.int(k_min, k_max)
}
K_VALUES <- suppressWarnings(as.integer(K_VALUES))
K_VALUES <- K_VALUES[!is.na(K_VALUES)]

if (length(K_VALUES) == 0) {
  stop("K_VALUES is empty.", call. = FALSE)
}

MODEL_STRUCTURES <- resolve_model_structures(CFG)

MISSING_CODE <- CFG$mplus_missing_code %||% CFG$missing_code %||% -9999

STARTS       <- CFG$estimation$starts %||% CFG$mplus$starts %||% "500 100"
STITERATIONS <- as.integer(CFG$estimation$stiterations %||% CFG$mplus$stiterations %||% 20L)
PROCESSORS   <- as.integer(CFG$estimation$processors %||% CFG$mplus$processors %||% 4L)
ESTIMATOR    <- toupper(CFG$estimation$estimator %||% CFG$mplus$estimator %||% "MLR")
LRT_STARTS   <- CFG$estimation$lrtstarts %||% CFG$estimation$lrt_starts %||% "0 0 200 40"
SEED_VALUE   <- CFG$seed %||% NA

SURVEY_CASE  <- SURVEY_BUNDLE$survey_case %||% "none"

ALL_NAMES <- unique(c(
  INDICATORS,
  COVARIATES,
  OUTCOMES,
  WEIGHT_VAR,
  STRATA_VAR,
  CLUSTER_VAR,
  ID_VAR
))
ALL_NAMES <- ALL_NAMES[!is.na(ALL_NAMES) & nzchar(ALL_NAMES)]

dataset_id_lc  <- tolower(DATASET_ID)
analysis_id_lc <- tolower(ANALYSIS_ID)

log_info("MIXTURE_TYPE     = ", MIXTURE_TYPE)
log_info("SURVEY_CASE      = ", SURVEY_CASE)
log_info("K_VALUES         = ", paste(K_VALUES, collapse = ", "))
log_info("MODEL_STRUCTURES = ", paste(MODEL_STRUCTURES, collapse = ", "))
log_info("ESTIMATOR        = ", ESTIMATOR)

# ------------------------------------------------------------
# 3. write Mplus data file
# ------------------------------------------------------------
MPLUS_EXPORT_DATA <- MPLUS_DATA

for (nm in names(MPLUS_EXPORT_DATA)) {
  x <- MPLUS_EXPORT_DATA[[nm]]

  if (is.factor(x)) x <- as.character(x)
  if (is.logical(x)) x <- as.integer(x)
  if (is.character(x)) suppressWarnings(x <- as.numeric(x))
  if (!is.numeric(x)) suppressWarnings(x <- as.numeric(x))

  x[is.na(x)] <- MISSING_CODE
  MPLUS_EXPORT_DATA[[nm]] <- x
}

MPLUS_DATA_FILE <- file.path(
  DIR_MPLUS_DATA,
  "d.dat"
)

unlink(list.files(DIR_MPLUS_INP, pattern = "\\.(inp|out|log)$", full.names = TRUE), force = TRUE)
unlink(list.files(DIR_MPLUS_SAVEDATA, pattern = "\\.dat$", full.names = TRUE), force = TRUE)

write_mplus_data(
  data         = MPLUS_EXPORT_DATA,
  path         = MPLUS_DATA_FILE,
  missing_code = MISSING_CODE,
  sep          = "\t"
)

# ------------------------------------------------------------
# 4. initialize run plan
# ------------------------------------------------------------
run_plan <- data.frame(
  k                = integer(),
  model_structure  = character(),
  model_tag        = character(),
  inp_file         = character(),
  out_file         = character(),
  cprob_file       = character(),
  stringsAsFactors = FALSE
)

ESTIMATION_REGISTRY <- list()

# ------------------------------------------------------------
# 5. build .inp files
# ------------------------------------------------------------
for (model_structure_i in MODEL_STRUCTURES) {
  for (k in K_VALUES) {

    model_tag <- paste0(
      dataset_id_lc, "_",
      analysis_id_lc, "_",
      model_structure_i, "_",
      "k", k, "_",
      MIXTURE_TYPE
    )

    inp_file         <- file.path(DIR_MPLUS_INP, paste0(model_tag, ".inp"))
    out_file         <- file.path(DIR_MPLUS_OUT, paste0(model_tag, ".out"))
    cprob_file_short <- paste0(model_structure_i, "_cprob_k", k, ".dat")
    cprob_file       <- file.path(DIR_MPLUS_SAVEDATA, cprob_file_short)

    variable_lines <- c(
      wrap_mplus_statement("NAMES", ALL_NAMES),
      wrap_mplus_statement("USEVARIABLES", INDICATORS),
      paste0("  CLASSES = c(", k, ");"),
      paste0("  MISSING = ALL(", MISSING_CODE, ");")
    )

    if (length(INDICATORS_CATEGORICAL) > 0) {
      variable_lines <- c(
        variable_lines,
        wrap_mplus_statement("CATEGORICAL", INDICATORS_CATEGORICAL)
      )
    }

    if (!is.null(ID_VAR) && nzchar(ID_VAR)) {
      variable_lines <- c(variable_lines, paste0("  IDVARIABLE = ", ID_VAR, ";"))
    }
    if (!is.null(WEIGHT_VAR) && nzchar(WEIGHT_VAR)) {
      variable_lines <- c(variable_lines, paste0("  WEIGHT = ", WEIGHT_VAR, ";"))
    }
    if (!is.null(STRATA_VAR) && nzchar(STRATA_VAR)) {
      variable_lines <- c(variable_lines, paste0("  STRATIFICATION = ", STRATA_VAR, ";"))
    }
    if (!is.null(CLUSTER_VAR) && nzchar(CLUSTER_VAR)) {
      variable_lines <- c(variable_lines, paste0("  CLUSTER = ", CLUSTER_VAR, ";"))
    }

    type_terms <- c("MIXTURE")
    if (SURVEY_CASE %in% c(
      "weight_strata",
      "weight_cluster",
      "weight_strata_cluster",
      "strata_only",
      "cluster_only",
      "strata_cluster"
    )) {
      type_terms <- c(type_terms, "COMPLEX")
    }

    analysis_lines <- c(
      paste0("  TYPE = ", paste(type_terms, collapse = " "), ";"),
      paste0("  ESTIMATOR = ", ESTIMATOR, ";"),
      paste0("  STARTS = ", STARTS, ";"),
      paste0("  STITERATIONS = ", STITERATIONS, ";"),
      paste0("  PROCESSORS = ", PROCESSORS, ";"),
      paste0("  LRTSTARTS = ", LRT_STARTS, ";")
    )

    if (tolower(MIXTURE_TYPE) == "lpa") {
      model_lines <- make_lpa_model_lines(
        model_structure = model_structure_i,
        indicators_continuous = INDICATORS_CONTINUOUS
      )
    } else if (tolower(MIXTURE_TYPE) == "mixed") {
      model_lines <- make_mixed_model_lines(
        model_structure = model_structure_i,
        indicators_continuous = INDICATORS_CONTINUOUS,
        indicators_categorical = INDICATORS_CATEGORICAL
      )
    } else {
      model_lines <- make_lca_model_lines(
        model_structure = model_structure_i,
        indicators_categorical = INDICATORS_CATEGORICAL
      )
    }

    output_lines <- make_output_lines_main(CFG)
    mplus_data_file_inp <- "../data/d.dat"
    cprob_file_inp <- file.path("../savedata", cprob_file_short)

    savedata_lines <- c(
      paste0("  FILE = ", gsub("\\\\", "/", cprob_file_inp), ";"),
      "  SAVE = CPROBABILITIES;"
    )

    inp_text <- c(
      paste0("TITLE: ", model_tag, ";"),
      "",
      "DATA:",
      paste0("  FILE = ", gsub("\\\\", "/", mplus_data_file_inp), ";"),
      "",
      "VARIABLE:",
      variable_lines,
      "",
      "ANALYSIS:",
      analysis_lines,
      "",
      "MODEL:",
      model_lines
    )

    if (length(output_lines) > 0) {
      inp_text <- c(
        inp_text,
        "",
        "OUTPUT:",
        paste0("  ", output_lines)
      )
    }

    inp_text <- c(
      inp_text,
      "",
      "PLOT:",
      "  TYPE = PLOT3;"
    )

    inp_text <- c(
      inp_text,
      "",
      "SAVEDATA:",
      savedata_lines,
      ""
    )

    writeLines(inp_text, con = inp_file, useBytes = TRUE)

    run_plan <- rbind(
      run_plan,
      data.frame(
        k                = as.integer(k),
        model_structure  = model_structure_i,
        model_tag        = model_tag,
        inp_file         = inp_file,
        out_file         = out_file,
        cprob_file       = cprob_file,
        stringsAsFactors = FALSE
      )
    )

    reg_i <- make_estimation_registry_row(
      k                  = k,
      model_tag          = model_tag,
      mixture_type       = MIXTURE_TYPE,
      inp_file           = inp_file,
      out_file           = out_file,
      savedata_file      = cprob_file,
      data_file          = MPLUS_DATA_FILE,
      data_header        = NA_character_,
      id_var             = ID_VAR,
      usevariables       = INDICATORS,
      all_names          = ALL_NAMES,
      categorical        = INDICATORS_CATEGORICAL,
      continuous         = INDICATORS_CONTINUOUS,
      survey_case        = SURVEY_CASE,
      weight_var         = WEIGHT_VAR,
      strata_var         = STRATA_VAR,
      cluster_var        = CLUSTER_VAR,
      missing_code       = MISSING_CODE,
      starts             = NA_integer_,
      stiterations       = STITERATIONS,
      processors         = PROCESSORS,
      estimator          = ESTIMATOR,
      lratio_starts      = LRT_STARTS
    )
    reg_i$model_structure <- model_structure_i

    ESTIMATION_REGISTRY[[length(ESTIMATION_REGISTRY) + 1L]] <- reg_i

    log_info("Prepared Mplus input: model = ", model_structure_i, ", k = ", k)
  }
}

# ------------------------------------------------------------
# 6. summary
# ------------------------------------------------------------
ESTIMATION_BUILD_SUMMARY <- list(
  mixture_type = MIXTURE_TYPE,
  survey_case = SURVEY_CASE,
  k_values = K_VALUES,
  model_structures = MODEL_STRUCTURES,
  n_models = nrow(run_plan),
  missing_code = MISSING_CODE,
  mplus_data_file = MPLUS_DATA_FILE,
  created_at = Sys.time()
)

ESTIMATION_BUILD_SUMMARY$MIXTURE_TYPE <- MIXTURE_TYPE
ESTIMATION_BUILD_SUMMARY$SURVEY_CASE <- SURVEY_CASE
ESTIMATION_BUILD_SUMMARY$K_VALUES <- K_VALUES
ESTIMATION_BUILD_SUMMARY$MODEL_STRUCTURES <- MODEL_STRUCTURES

# ------------------------------------------------------------
# 7. save
# ------------------------------------------------------------
log_info("Saving estimation-build outputs ...")

saveRDS(
  list(
    run_plan = run_plan,
    ESTIMATION_BUILD_SUMMARY = ESTIMATION_BUILD_SUMMARY
  ),
  file = file.path(DIR_RDS, "estimation_build.rds")
)

save_named_rds_list(
  list(
    MPLUS_EXPORT_DATA = MPLUS_EXPORT_DATA,
    ESTIMATION_BUILD_SUMMARY = ESTIMATION_BUILD_SUMMARY,
    ESTIMATION_REGISTRY = ESTIMATION_REGISTRY
  ),
  dir_rds = DIR_RDS
)

elapsed_sec <- round(as.numeric(difftime(Sys.time(), T0_03A, units = "secs")), 2)

log_info("03a_estimation_build_inputs.R completed.")
log_info("n(models)         = ", nrow(run_plan))
log_info("k_values          = ", paste(K_VALUES, collapse = ", "))
log_info("model_structures  = ", paste(MODEL_STRUCTURES, collapse = ", "))
log_info("data file         = ", MPLUS_DATA_FILE)
log_info("elapsed           = ", elapsed_sec, " sec")
