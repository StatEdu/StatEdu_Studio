`%||%` <- function(x, y) if (is.null(x)) y else x

.lg_nonempty_chr <- function(x) {
  x <- as.character(x %||% character(0))
  x <- trimws(x)
  x[!is.na(x) & nzchar(x)]
}

.lg_named_chr_map <- function(x) {
  if (is.null(x)) return(character(0))

  if (is.list(x) && !is.null(names(x))) {
    vals <- vapply(x, function(v) as.character(v %||% NA_character_)[1], character(1))
    vals <- trimws(vals)
    vals <- vals[!is.na(vals) & nzchar(vals)]
    return(vals)
  }

  x <- as.character(x)
  x <- trimws(x)
  x <- x[!is.na(x) & nzchar(x)]
  x
}

.lg_read_wave_file <- function(path, required = TRUE) {
  if (exists("read_data_file")) {
    return(read_data_file(path = path, required = required))
  }
  stop("read_data_file() is required before loading longitudinal helpers.", call. = FALSE)
}

resolve_longitudinal_spec <- function(cfg = NULL, raw_data = NULL, dir_data = NULL) {
  lg <- cfg$longitudinal %||% list()

  data_layout <- tolower(as.character(lg$data_layout %||% "wide")[1])
  if (!data_layout %in% c("long", "wide", "multi_file")) {
    data_layout <- "wide"
  }

  id_var <- as.character(
    lg$id_var %||%
      cfg$survey_design$id_var %||%
      cfg$id_var %||%
      "id"
  )[1]

  time_var <- as.character(lg$time_var %||% lg$wave_var %||% "wave")[1]
  state_var <- as.character(lg$state_var %||% "state")[1]
  state_value_var <- as.character(lg$state_value_var %||% NA_character_)[1]

  wave_order <- .lg_nonempty_chr(lg$wave_order %||% names(lg$wave_map) %||% character(0))
  if (length(wave_order) == 0 && is.data.frame(raw_data) && data_layout == "long" && time_var %in% names(raw_data)) {
    wave_order <- unique(as.character(raw_data[[time_var]]))
    wave_order <- wave_order[!is.na(wave_order) & nzchar(wave_order)]
  }

  state_wide_map <- .lg_named_chr_map(lg$state_wide_map %||% lg$measure_map$state %||% NULL)
  wave_files <- .lg_named_chr_map(lg$wave_files %||% NULL)
  state_file_vars <- .lg_named_chr_map(lg$state_file_vars %||% NULL)

  if (!is.null(dir_data) && length(wave_files) > 0) {
    wave_files <- stats::setNames(
      vapply(
        wave_files,
        function(fp) {
          fp <- as.character(fp)[1]
          if (grepl("^[A-Za-z]:[/\\\\]|^/", fp)) return(fp)
          file.path(dir_data, fp)
        },
        character(1)
      ),
      names(wave_files)
    )
  }

  if (length(wave_order) == 0) {
    wave_order <- unique(c(names(state_wide_map), names(wave_files)))
  }

  invariant_vars <- .lg_nonempty_chr(lg$invariant_vars %||% lg$time_invariant_vars %||% character(0))

  list(
    data_layout = data_layout,
    id_var = id_var,
    time_var = time_var,
    state_var = state_var,
    state_value_var = state_value_var,
    wave_order = wave_order,
    state_wide_map = state_wide_map,
    wave_files = wave_files,
    state_file_vars = state_file_vars,
    invariant_vars = invariant_vars
  )
}

build_panel_from_long <- function(raw_data, spec) {
  if (!is.data.frame(raw_data) || nrow(raw_data) == 0) {
    stop("Longitudinal long-form data is missing.", call. = FALSE)
  }

  id_var <- spec$id_var
  time_var <- spec$time_var
  state_var <- spec$state_var

  missing_vars <- setdiff(c(id_var, time_var), names(raw_data))
  if (length(missing_vars) > 0) {
    stop("Missing required long-form variables: ", paste(missing_vars, collapse = ", "), call. = FALSE)
  }

  if (!state_var %in% names(raw_data)) {
    if (!is.na(spec$state_value_var) && nzchar(spec$state_value_var) && spec$state_value_var %in% names(raw_data)) {
      state_var <- spec$state_value_var
    } else {
      stop("State variable not found in long-form data: ", spec$state_var, call. = FALSE)
    }
  }

  panel_long <- raw_data
  panel_long$panel_id <- as.character(panel_long[[id_var]])
  panel_long$panel_wave <- as.character(panel_long[[time_var]])
  panel_long$state <- suppressWarnings(as.integer(as.numeric(panel_long[[state_var]])))

  if (length(spec$wave_order) > 0) {
    panel_long$panel_wave <- factor(panel_long$panel_wave, levels = spec$wave_order, ordered = TRUE)
    panel_long$panel_wave_index <- as.integer(panel_long$panel_wave)
    panel_long$panel_wave <- as.character(panel_long$panel_wave)
  } else {
    panel_long$panel_wave_index <- match(panel_long$panel_wave, unique(panel_long$panel_wave))
  }

  panel_long <- panel_long[!is.na(panel_long$panel_id) & nzchar(panel_long$panel_id), , drop = FALSE]
  panel_long <- panel_long[!is.na(panel_long$panel_wave) & nzchar(panel_long$panel_wave), , drop = FALSE]
  panel_long <- panel_long[order(panel_long$panel_id, panel_long$panel_wave_index), , drop = FALSE]
  rownames(panel_long) <- NULL

  panel_wide <- reshape(
    panel_long[, c("panel_id", "panel_wave", "state"), drop = FALSE],
    idvar = "panel_id",
    timevar = "panel_wave",
    direction = "wide"
  )

  list(
    PANEL_LONG = panel_long,
    PANEL_WIDE = panel_wide
  )
}

build_panel_from_wide <- function(raw_data, spec) {
  if (!is.data.frame(raw_data) || nrow(raw_data) == 0) {
    stop("Wide-form panel data is missing.", call. = FALSE)
  }

  id_var <- spec$id_var
  if (!id_var %in% names(raw_data)) {
    stop("ID variable not found in wide-form data: ", id_var, call. = FALSE)
  }

  state_map <- spec$state_wide_map
  if (length(state_map) == 0) {
    stop("state_wide_map must be defined for wide-form panel data.", call. = FALSE)
  }

  missing_state_cols <- setdiff(unname(state_map), names(raw_data))
  if (length(missing_state_cols) > 0) {
    stop("Missing state columns in wide-form data: ", paste(missing_state_cols, collapse = ", "), call. = FALSE)
  }

  long_list <- lapply(seq_along(state_map), function(i) {
    wave_i <- names(state_map)[i]
    col_i <- unname(state_map)[i]
    out <- data.frame(
      panel_id = as.character(raw_data[[id_var]]),
      panel_wave = as.character(wave_i),
      state = suppressWarnings(as.integer(as.numeric(raw_data[[col_i]]))),
      stringsAsFactors = FALSE
    )

    if (length(spec$invariant_vars) > 0) {
      keep_inv <- intersect(spec$invariant_vars, names(raw_data))
      if (length(keep_inv) > 0) {
        out <- cbind(out, raw_data[, keep_inv, drop = FALSE], stringsAsFactors = FALSE)
      }
    }

    out
  })

  panel_long <- do.call(rbind, long_list)
  panel_long$panel_wave <- factor(panel_long$panel_wave, levels = spec$wave_order %||% names(state_map), ordered = TRUE)
  panel_long$panel_wave_index <- as.integer(panel_long$panel_wave)
  panel_long$panel_wave <- as.character(panel_long$panel_wave)
  panel_long <- panel_long[order(panel_long$panel_id, panel_long$panel_wave_index), , drop = FALSE]
  rownames(panel_long) <- NULL

  panel_wide <- raw_data
  panel_wide$panel_id <- as.character(panel_wide[[id_var]])

  list(
    PANEL_LONG = panel_long,
    PANEL_WIDE = panel_wide
  )
}

build_panel_from_multi_file <- function(spec) {
  wave_files <- spec$wave_files
  if (length(wave_files) == 0) {
    stop("wave_files must be defined for multi_file panel input.", call. = FALSE)
  }

  long_list <- vector("list", length(wave_files))

  for (i in seq_along(wave_files)) {
    wave_i <- names(wave_files)[i]
    file_i <- wave_files[i]
    dat_i <- .lg_read_wave_file(file_i, required = TRUE)

    id_var <- spec$id_var
    state_var_i <- spec$state_file_vars[[wave_i]] %||% spec$state_var

    missing_i <- setdiff(c(id_var, state_var_i), names(dat_i))
    if (length(missing_i) > 0) {
      stop(
        "Missing required variables in wave file ", basename(file_i), ": ",
        paste(missing_i, collapse = ", "),
        call. = FALSE
      )
    }

    out_i <- data.frame(
      panel_id = as.character(dat_i[[id_var]]),
      panel_wave = as.character(wave_i),
      state = suppressWarnings(as.integer(as.numeric(dat_i[[state_var_i]]))),
      stringsAsFactors = FALSE
    )

    keep_inv <- intersect(spec$invariant_vars, names(dat_i))
    if (length(keep_inv) > 0) {
      out_i <- cbind(out_i, dat_i[, keep_inv, drop = FALSE], stringsAsFactors = FALSE)
    }

    long_list[[i]] <- out_i
  }

  panel_long <- do.call(rbind, long_list)
  panel_long$panel_wave <- factor(panel_long$panel_wave, levels = spec$wave_order %||% names(wave_files), ordered = TRUE)
  panel_long$panel_wave_index <- as.integer(panel_long$panel_wave)
  panel_long$panel_wave <- as.character(panel_long$panel_wave)
  panel_long <- panel_long[order(panel_long$panel_id, panel_long$panel_wave_index), , drop = FALSE]
  rownames(panel_long) <- NULL

  panel_wide <- reshape(
    panel_long[, c("panel_id", "panel_wave", "state"), drop = FALSE],
    idvar = "panel_id",
    timevar = "panel_wave",
    direction = "wide"
  )

  list(
    PANEL_LONG = panel_long,
    PANEL_WIDE = panel_wide
  )
}

build_panel_bundle <- function(cfg = NULL, raw_data = NULL, dir_data = NULL) {
  spec <- resolve_longitudinal_spec(cfg = cfg, raw_data = raw_data, dir_data = dir_data)

  panel <- switch(
    spec$data_layout,
    long = build_panel_from_long(raw_data = raw_data, spec = spec),
    wide = build_panel_from_wide(raw_data = raw_data, spec = spec),
    multi_file = build_panel_from_multi_file(spec = spec),
    stop("Unsupported panel data layout: ", spec$data_layout, call. = FALSE)
  )

  panel_long <- panel$PANEL_LONG
  panel_wide <- panel$PANEL_WIDE

  wave_summary <- aggregate(
    list(n_state_nonmissing = !is.na(panel_long$state)),
    by = list(panel_wave = panel_long$panel_wave),
    FUN = sum
  )

  panel_meta <- list(
    spec = spec,
    n_rows_long = nrow(panel_long),
    n_rows_wide = nrow(panel_wide),
    n_id = length(unique(stats::na.omit(panel_long$panel_id))),
    wave_order = spec$wave_order,
    wave_summary = wave_summary
  )

  list(
    PANEL_LONG = panel_long,
    PANEL_WIDE = panel_wide,
    PANEL_META = panel_meta
  )
}

build_transition_pairs <- function(panel_long,
                                   id_col = "panel_id",
                                   wave_col = "panel_wave",
                                   wave_index_col = "panel_wave_index",
                                   state_col = "state") {
  if (!is.data.frame(panel_long) || nrow(panel_long) == 0) {
    return(data.frame())
  }

  req <- c(id_col, wave_col, wave_index_col, state_col)
  miss <- setdiff(req, names(panel_long))
  if (length(miss) > 0) {
    stop("Transition pair build failed; missing columns: ", paste(miss, collapse = ", "), call. = FALSE)
  }

  panel_long <- panel_long[order(panel_long[[id_col]], panel_long[[wave_index_col]]), , drop = FALSE]

  split_dat <- split(panel_long, panel_long[[id_col]])
  out <- lapply(split_dat, function(df_i) {
    if (nrow(df_i) < 2) return(NULL)
    from <- df_i[-nrow(df_i), , drop = FALSE]
    to <- df_i[-1, , drop = FALSE]
    data.frame(
      panel_id = as.character(from[[id_col]]),
      from_wave = as.character(from[[wave_col]]),
      to_wave = as.character(to[[wave_col]]),
      from_wave_index = as.integer(from[[wave_index_col]]),
      to_wave_index = as.integer(to[[wave_index_col]]),
      from_state = suppressWarnings(as.integer(from[[state_col]])),
      to_state = suppressWarnings(as.integer(to[[state_col]])),
      stringsAsFactors = FALSE
    )
  })

  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) == 0) return(data.frame())

  out <- do.call(rbind, out)
  out <- out[!is.na(out$from_state) & !is.na(out$to_state), , drop = FALSE]
  out$interval <- paste0(out$from_wave, "_to_", out$to_wave)
  rownames(out) <- NULL
  out
}

compute_transition_matrix <- function(transition_data) {
  if (!is.data.frame(transition_data) || nrow(transition_data) == 0) {
    return(data.frame())
  }

  tab <- stats::xtabs(~ interval + from_state + to_state, data = transition_data)
  df <- as.data.frame(tab, stringsAsFactors = FALSE)
  names(df) <- c("interval", "from_state", "to_state", "n")
  df <- df[df$n > 0, , drop = FALSE]

  totals <- aggregate(n ~ interval + from_state, data = df, FUN = sum)
  names(totals)[names(totals) == "n"] <- "row_total"
  out <- merge(df, totals, by = c("interval", "from_state"), all.x = TRUE, sort = FALSE)
  out$prob <- ifelse(out$row_total > 0, out$n / out$row_total, NA_real_)
  out[order(out$interval, out$from_state, out$to_state), , drop = FALSE]
}

cat("\n============================================================\n")
cat("11_longitudinal.R loaded\n")
cat("------------------------------------------------------------\n")
cat("Available functions:\n")
cat(" - resolve_longitudinal_spec()\n")
cat(" - build_panel_bundle()\n")
cat(" - build_transition_pairs()\n")
cat(" - compute_transition_matrix()\n")
cat("============================================================\n\n")
