`%||%` <- function(x, y) if (is.null(x)) y else x

normalize_chr_vec <- function(x) {
  x <- as.character(unlist(x, use.names = FALSE))
  x <- trimws(x)
  unique(x[!is.na(x) & nzchar(x)])
}

resolve_lta_spec <- function(cfg, raw_data = NULL, longitudinal_spec = NULL) {
  lt <- cfg$latent_transition %||% list()
  wave_order <- normalize_chr_vec(lt$wave_order %||% longitudinal_spec$wave_order %||% character(0))
  id_var <- as.character(lt$id_var %||% longitudinal_spec$id_var %||% cfg$survey_design$id_var %||% "id")[1]
  n_classes <- suppressWarnings(as.integer(lt$n_classes %||% lt$k %||% 3L))
  measurement_mode <- tolower(as.character(lt$measurement_mode %||% "lca")[1])
  invariance <- tolower(as.character(lt$invariance %||% if (measurement_mode == "lca") "thresholds" else "means")[1])
  indicators_by_wave <- lt$indicators_by_wave %||% list()
  indicators_by_wave <- indicators_by_wave[intersect(wave_order, names(indicators_by_wave))]
  indicators_by_wave <- lapply(indicators_by_wave, normalize_chr_vec)
  all_vars <- unique(unlist(indicators_by_wave, use.names = FALSE))
  all_vars <- all_vars[!is.na(all_vars) & nzchar(all_vars)]
  missing_vars <- if (is.data.frame(raw_data) && nrow(raw_data) > 0) setdiff(c(id_var, all_vars), names(raw_data)) else character(0)
  threshold_counts <- setNames(integer(0), character(0))
  category_counts <- setNames(integer(0), character(0))
  if (measurement_mode == "lca" && is.data.frame(raw_data) && nrow(raw_data) > 0) {
    category_counts <- stats::setNames(
      vapply(all_vars, function(v) {
        length(sort(unique(stats::na.omit(raw_data[[v]]))))
      }, integer(1)),
      all_vars
    )
    threshold_counts <- stats::setNames(
      vapply(all_vars, function(v) {
        x <- raw_data[[v]]
        u <- sort(unique(stats::na.omit(x)))
        max(length(u) - 1L, 1L)
      }, integer(1)),
      all_vars
    )
  }
  list(
    enabled = isTRUE(lt$enabled %||% FALSE),
    id_var = id_var,
    wave_order = wave_order,
    n_classes = ifelse(is.na(n_classes) || n_classes < 2L, 3L, n_classes),
    measurement_mode = if (measurement_mode %in% c("lca", "lpa")) measurement_mode else "lca",
    invariance = invariance,
    indicators_by_wave = indicators_by_wave,
    all_indicator_vars = all_vars,
    category_counts = category_counts,
    threshold_counts = threshold_counts,
    missing_vars = missing_vars
  )
}

build_lta_dataset <- function(raw_data, lta_spec) {
  if (!is.data.frame(raw_data) || nrow(raw_data) == 0) {
    stop("RAW_DATA is missing or empty.", call. = FALSE)
  }
  cols <- unique(c(lta_spec$id_var, lta_spec$all_indicator_vars))
  cols <- cols[cols %in% names(raw_data)]
  dat <- raw_data[, cols, drop = FALSE]
  for (nm in names(dat)) {
    x <- dat[[nm]]
    if (is.factor(x)) x <- as.character(x)
    if (is.logical(x)) x <- as.integer(x)
    if (!identical(nm, lta_spec$id_var)) suppressWarnings(x <- as.numeric(x))
    dat[[nm]] <- x
  }
  dat
}

cat("\n============================================================\n")
cat("12_lta.R loaded\n")
cat("------------------------------------------------------------\n")
cat("Available functions:\n")
cat(" - resolve_lta_spec()\n")
cat(" - build_lta_dataset()\n")
cat("============================================================\n\n")
