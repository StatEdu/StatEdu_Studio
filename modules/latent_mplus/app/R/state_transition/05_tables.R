T0_TABLES <- Sys.time()

log_step_start("TABLES", "05_tables.R")
log_info("Building SCI-style state-transition tables ...")

`%||%` <- function(x, y) if (is.null(x)) y else x

safe_df <- function(x) {
  if (is.null(x)) return(data.frame())
  if (is.data.frame(x)) return(as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE))
  out <- tryCatch(as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
  if (is.null(out)) out <- data.frame()
  out
}

fmt_n <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "", sprintf("%.0f", x))
}

fmt_pct1 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "", sprintf("%.1f", 100 * x))
}

fmt_est2 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "", sprintf("%.2f", x))
}

fmt_stat3 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- ifelse(is.na(x), "", sprintf("%.3f", x))
  out <- ifelse(grepl("^0\\.", out), sub("^0", "", out), out)
  out <- ifelse(grepl("^-0\\.", out), sub("^-0\\.", "-.", out), out)
  out
}

fmt_pval <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- ifelse(is.na(x), "", ifelse(x < 0.001, "<.001", sprintf("%.3f", x)))
  out <- ifelse(grepl("^0\\.", out), sub("^0", "", out), out)
  out
}

fmt_sig <- function(p) {
  p <- suppressWarnings(as.numeric(p))
  ifelse(
    is.na(p), "",
    ifelse(p < 0.001, "***",
      ifelse(p < 0.01, "**",
        ifelse(p <= 0.05, "*", "")
      )
    )
  )
}

compute_col_widths <- function(dat, min_first = 12, min_other = 14, max_width = 28) {
  dat <- safe_df(dat)
  if (ncol(dat) == 0) return(numeric(0))
  widths <- vapply(
    seq_len(ncol(dat)),
    function(i) {
      vals <- c(names(dat)[i], as.character(dat[[i]]))
      vals[is.na(vals)] <- ""
      max(nchar(vals, type = "width"), na.rm = TRUE) + 2
    },
    numeric(1)
  )
  mins <- c(min_first, rep(min_other, max(0, ncol(dat) - 1)))
  pmin(max_width, pmax(mins, widths))
}

apply_table_rule_lines <- function(wb, sheet_name, top_row, header_rows = integer(0), bottom_row, n_cols) {
  if (is.null(n_cols) || length(n_cols) == 0 || is.na(n_cols) || n_cols <= 0) return(invisible(NULL))
  cols <- seq_len(n_cols)
  top_style <- openxlsx::createStyle(border = "top", borderStyle = "thick")
  header_style <- openxlsx::createStyle(border = "bottom", borderStyle = "thick")
  bottom_style <- openxlsx::createStyle(border = "bottom", borderStyle = "thick")
  if (!is.null(top_row) && length(top_row) == 1 && !is.na(top_row)) {
    openxlsx::addStyle(wb, sheet_name, top_style, rows = top_row, cols = cols, gridExpand = TRUE, stack = TRUE)
  }
  header_rows <- as.integer(header_rows)
  header_rows <- header_rows[!is.na(header_rows)]
  if (length(header_rows) > 0) {
    openxlsx::addStyle(wb, sheet_name, header_style, rows = header_rows, cols = cols, gridExpand = TRUE, stack = TRUE)
  }
  if (!is.null(bottom_row) && length(bottom_row) == 1 && !is.na(bottom_row) && bottom_row >= top_row) {
    openxlsx::addStyle(wb, sheet_name, bottom_style, rows = bottom_row, cols = cols, gridExpand = TRUE, stack = TRUE)
  }
  invisible(NULL)
}

make_state_label_map <- function(dict = NULL, cfg = NULL, state_var_names = character(0)) {
  dict_levels <- if (is.list(dict) && is.data.frame(dict$levels)) dict$levels else data.frame()
  state_var_names <- unique(as.character(state_var_names))
  state_var_names <- state_var_names[!is.na(state_var_names) & nzchar(state_var_names)]
  if (is.data.frame(dict_levels) && nrow(dict_levels) > 0 &&
      all(c("var_name", "value", "value_label") %in% names(dict_levels))) {
    lv <- dict_levels[dict_levels$var_name %in% state_var_names, c("value", "value_label"), drop = FALSE]
    lv <- lv[!is.na(lv$value) & nzchar(as.character(lv$value)), , drop = FALSE]
    lv <- lv[!duplicated(as.character(lv$value)), , drop = FALSE]
    if (nrow(lv) > 0) {
      vals <- as.character(lv$value_label)
      nms <- as.character(lv$value)
      keep <- !is.na(vals) & nzchar(vals) & !is.na(nms) & nzchar(nms)
      if (any(keep)) return(stats::setNames(vals[keep], nms[keep]))
    }
  }
  x <- cfg$longitudinal$state_labels %||% cfg$state_labels %||% NULL
  if (is.null(x)) return(setNames(character(0), character(0)))
  if (is.list(x)) {
    vals <- vapply(x, function(z) as.character(z %||% NA_character_)[1], character(1))
    nms <- names(vals) %||% character(0)
    keep <- !is.na(vals) & nzchar(vals) & !is.na(nms) & nzchar(nms)
    return(stats::setNames(vals[keep], as.character(nms[keep])))
  }
  x <- as.character(x)
  stats::setNames(x, names(x))
}

state_label <- function(x, label_map) {
  key <- as.character(x)
  out <- unname(label_map[key])
  out[is.na(out) | !nzchar(out)] <- paste0("state", key[is.na(out) | !nzchar(out)])
  out
}

state_header_key <- function(x, label_map) {
  lab <- state_label(x, label_map)
  make.names(lab, unique = FALSE)
}

pretty_interval_label_tbl <- function(x) {
  x <- as.character(x)
  gsub("^Y([0-9]+)_to_Y([0-9]+)$", "W\\1 to W\\2", x, perl = TRUE)
}

build_interval_matrix <- function(df, value_col = "prob", prefix = "State", label_map = setNames(character(0), character(0))) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(data.frame())

  state_order <- sort(unique(c(suppressWarnings(as.numeric(df$from_state)), suppressWarnings(as.numeric(df$to_state)))))
  from_states <- state_order[state_order %in% suppressWarnings(as.numeric(df$from_state))]
  to_states <- state_order[state_order %in% suppressWarnings(as.numeric(df$to_state))]

  out <- data.frame(
    From_State = state_label(from_states, label_map),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  for (st in to_states) {
    vals <- vapply(
      from_states,
      function(fs) {
        hit <- df[df$from_state == fs & df$to_state == st, value_col, drop = TRUE]
        if (length(hit) == 0) return(NA_real_)
        suppressWarnings(as.numeric(hit[1]))
      },
      numeric(1)
    )
    out[[state_label(st, label_map)]] <- vals
  }

  out
}

build_interval_matrix_chr <- function(df, value_col = "cell", label_map = setNames(character(0), character(0))) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(data.frame())
  state_order <- sort(unique(c(suppressWarnings(as.numeric(df$from_state)), suppressWarnings(as.numeric(df$to_state)))))
  from_states <- state_order[state_order %in% suppressWarnings(as.numeric(df$from_state))]
  to_states <- state_order[state_order %in% suppressWarnings(as.numeric(df$to_state))]
  out <- data.frame(
    From_State = state_label(from_states, label_map),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  for (st in to_states) {
    vals <- vapply(
      from_states,
      function(fs) {
        hit <- df[df$from_state == fs & df$to_state == st, value_col, drop = TRUE]
        if (length(hit) == 0) return("")
        as.character(hit[1])
      },
      character(1)
    )
    out[[state_label(st, label_map)]] <- vals
  }
  out
}

build_interval_matrix_split <- function(df, label_map = setNames(character(0), character(0))) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(data.frame())
  state_order <- sort(unique(c(suppressWarnings(as.numeric(df$from_state)), suppressWarnings(as.numeric(df$to_state)))))
  from_states <- state_order[state_order %in% suppressWarnings(as.numeric(df$from_state))]
  to_states <- state_order[state_order %in% suppressWarnings(as.numeric(df$to_state))]
  out <- data.frame(
    From_State = state_label(from_states, label_map),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  for (st in to_states) {
    n_vals <- vapply(
      from_states,
      function(fs) {
        hit <- df[df$from_state == fs & df$to_state == st, "n", drop = TRUE]
        if (length(hit) == 0) return("")
        fmt_n(hit[1])
      },
      character(1)
    )
    p_vals <- vapply(
      from_states,
      function(fs) {
        hit <- df[df$from_state == fs & df$to_state == st, "row_prob", drop = TRUE]
        if (length(hit) == 0) return("")
        fmt_pct1(hit[1])
      },
      character(1)
    )
    key <- state_label(st, label_map)
    out[[paste0(key, "_n")]] <- n_vals
    out[[paste0(key, "_pct")]] <- p_vals
  }
  out
}

build_wave_state_wide <- function(df, wave_order = NULL, prefix = "state", label_map = setNames(character(0), character(0))) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(data.frame())

  states <- sort(unique(df$State))
  waves <- wave_order %||% unique(as.character(df$Wave))

  out <- data.frame(Wave = waves, stringsAsFactors = FALSE, check.names = FALSE)

  for (st in states) {
    df_st <- df[df$State == st, , drop = FALSE]
    n_vals <- vapply(
      waves,
      function(wv) {
        hit <- df_st[df_st$Wave == wv, "n_num", drop = TRUE]
        if (length(hit) == 0) return(NA_real_)
        hit[1]
      },
      numeric(1)
    )
    p_vals <- vapply(
      waves,
      function(wv) {
        hit <- df_st[df_st$Wave == wv, "Percent_num", drop = TRUE]
        if (length(hit) == 0) return(NA_real_)
        hit[1]
      },
      numeric(1)
    )

    key <- state_header_key(st, label_map)
    out[[paste0(key, "_n")]] <- fmt_n(n_vals)
    out[[paste0(key, "_pct")]] <- fmt_pct1(p_vals)
  }

  out
}

build_wave_state_chr <- function(df, wave_order = NULL, label_map = setNames(character(0), character(0))) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(data.frame())

  states <- sort(unique(df$State))
  waves <- wave_order %||% unique(as.character(df$Wave))
  out <- data.frame(Wave = waves, stringsAsFactors = FALSE, check.names = FALSE)

  for (st in states) {
    df_st <- df[df$State == st, , drop = FALSE]
    cell_vals <- vapply(
      waves,
      function(wv) {
        hit_n <- df_st[df_st$Wave == wv, "n_num", drop = TRUE]
        hit_p <- df_st[df_st$Wave == wv, "Percent_num", drop = TRUE]
        if (length(hit_n) == 0 || length(hit_p) == 0) return("")
        if (is.na(hit_n[1]) || is.na(hit_p[1])) return("")
        paste0(fmt_n(hit_n[1]), " (", fmt_pct1(hit_p[1]), ")")
      },
      character(1)
    )
    out[[state_label(st, label_map)]] <- cell_vals
  }

  out
}

write_review_sheet <- function(wb, sheet_name, dat, title_text) {
  dat <- safe_df(dat)
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, data.frame(title_text), startRow = 1, startCol = 1, colNames = FALSE)
  openxlsx::mergeCells(wb, sheet_name, cols = 1:max(2, ncol(dat)), rows = 1)
  openxlsx::addStyle(
    wb, sheet_name,
    openxlsx::createStyle(textDecoration = "bold", fontSize = 12, halign = "left"),
    rows = 1, cols = 1, gridExpand = TRUE, stack = TRUE
  )
  openxlsx::writeData(wb, sheet_name, dat, startRow = 3, startCol = 1, withFilter = FALSE)
  if (ncol(dat) > 0) {
    openxlsx::addStyle(
      wb, sheet_name,
      openxlsx::createStyle(textDecoration = "bold", border = "bottom", borderStyle = "thick", halign = "center"),
      rows = 3, cols = seq_len(ncol(dat)), gridExpand = TRUE, stack = TRUE
    )
    if (nrow(dat) > 0) {
      openxlsx::addStyle(
        wb, sheet_name,
        openxlsx::createStyle(valign = "center"),
        rows = 4:(nrow(dat) + 3), cols = seq_len(ncol(dat)), gridExpand = TRUE, stack = TRUE
      )
    }
    widths <- compute_col_widths(dat, min_first = 12, min_other = 14)
    openxlsx::setColWidths(wb, sheet_name, cols = seq_len(ncol(dat)), widths = widths)
    apply_table_rule_lines(wb, sheet_name, top_row = 3, header_rows = 3, bottom_row = 3 + nrow(dat), n_cols = ncol(dat))
  }
  openxlsx::freezePane(wb, sheet_name, firstActiveRow = 4, firstActiveCol = 2)
}

write_t2_twoline_sheet <- function(wb, sheet_name, dat, title_text) {
  dat <- safe_df(dat)
  openxlsx::addWorksheet(wb, sheet_name)
  ncol_dat <- max(1, ncol(dat))

  title_style <- openxlsx::createStyle(textDecoration = "bold", fontSize = 12, halign = "left")
  header_style <- openxlsx::createStyle(textDecoration = "bold", halign = "center", valign = "center", border = "bottom", borderStyle = "thick")
  group_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#EAF2F8", halign = "center", valign = "center", border = "bottom", borderStyle = "thick")
  body_style <- openxlsx::createStyle(valign = "center", halign = "center")
  left_style <- openxlsx::createStyle(valign = "center", halign = "left")

  openxlsx::writeData(wb, sheet_name, data.frame(title_text), startRow = 1, startCol = 1, colNames = FALSE)
  openxlsx::mergeCells(wb, sheet_name, cols = 1:max(2, ncol_dat), rows = 1)
  openxlsx::addStyle(wb, sheet_name, title_style, rows = 1, cols = 1, gridExpand = TRUE, stack = TRUE)

  states <- sub("_(n|pct)$", "", names(dat)[grepl("_(n|pct)$", names(dat))])
  states <- unique(states)
  states <- states[!is.na(states) & nzchar(states)]

  openxlsx::writeData(wb, sheet_name, "Wave", startRow = 3, startCol = 1, colNames = FALSE)
  openxlsx::mergeCells(wb, sheet_name, cols = 1, rows = 3:4)

  col_ptr <- 2
  for (st in states) {
    openxlsx::writeData(wb, sheet_name, st, startRow = 3, startCol = col_ptr, colNames = FALSE)
    openxlsx::mergeCells(wb, sheet_name, cols = col_ptr:(col_ptr + 1), rows = 3)
    openxlsx::writeData(wb, sheet_name, "n", startRow = 4, startCol = col_ptr, colNames = FALSE)
    openxlsx::writeData(wb, sheet_name, "%", startRow = 4, startCol = col_ptr + 1, colNames = FALSE)
    col_ptr <- col_ptr + 2
  }

  body <- dat
  body <- body[, c("Wave", as.vector(rbind(paste0(states, "_n"), paste0(states, "_pct")))), drop = FALSE]
  openxlsx::writeData(wb, sheet_name, body, startRow = 5, startCol = 1, colNames = FALSE, withFilter = FALSE)

  openxlsx::addStyle(wb, sheet_name, group_style, rows = 3, cols = seq_len(ncol(body)), gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, sheet_name, header_style, rows = 4, cols = seq_len(ncol(body)), gridExpand = TRUE, stack = TRUE)
  if (nrow(body) > 0) {
    openxlsx::addStyle(wb, sheet_name, body_style, rows = 5:(nrow(body) + 4), cols = seq_len(ncol(body)), gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, sheet_name, left_style, rows = 5:(nrow(body) + 4), cols = 1, gridExpand = TRUE, stack = TRUE)
  }
  widths <- compute_col_widths(body, min_first = 10, min_other = 12, max_width = 18)
  openxlsx::setColWidths(wb, sheet_name, cols = seq_len(ncol(body)), widths = widths)
  apply_table_rule_lines(wb, sheet_name, top_row = 3, header_rows = 4, bottom_row = 4 + nrow(body), n_cols = ncol(body))
  openxlsx::freezePane(wb, sheet_name, firstActiveRow = 5, firstActiveCol = 2)
}

write_t3_matrix_sheet <- function(wb, sheet_name, dat, title_text) {
  dat <- safe_df(dat)
  openxlsx::addWorksheet(wb, sheet_name)
  title_style <- openxlsx::createStyle(textDecoration = "bold", fontSize = 12, halign = "left")
  header_style <- openxlsx::createStyle(textDecoration = "bold", halign = "center", valign = "center", border = "bottom", borderStyle = "thick")
  section_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#EAF2F8", halign = "left")
  body_style <- openxlsx::createStyle(valign = "center", halign = "center")
  left_style <- openxlsx::createStyle(valign = "center", halign = "left")

  ncol_dat <- max(2, ncol(dat))
  openxlsx::writeData(wb, sheet_name, data.frame(title_text), startRow = 1, startCol = 1, colNames = FALSE)
  openxlsx::mergeCells(wb, sheet_name, cols = 1:ncol_dat, rows = 1)
  openxlsx::addStyle(wb, sheet_name, title_style, rows = 1, cols = 1, gridExpand = TRUE, stack = TRUE)

  if (nrow(dat) == 0) {
    openxlsx::writeData(wb, sheet_name, data.frame("No data"), startRow = 3, startCol = 1, colNames = FALSE)
    return(invisible(NULL))
  }

  intervals <- unique(as.character(dat$Interval))
  cur_row <- 3
  for (iv in intervals) {
    block <- dat[dat$Interval == iv, , drop = FALSE]
    block <- block[, c("From_State", names(block)[!(names(block) %in% c("Interval", "From_State"))]), drop = FALSE]
    names(block)[1] <- "From / To"

    openxlsx::writeData(wb, sheet_name, data.frame(iv), startRow = cur_row, startCol = 1, colNames = FALSE)
    openxlsx::mergeCells(wb, sheet_name, cols = 1:ncol(block), rows = cur_row)
    openxlsx::addStyle(wb, sheet_name, section_style, rows = cur_row, cols = 1, gridExpand = TRUE, stack = TRUE)

    openxlsx::writeData(wb, sheet_name, block, startRow = cur_row + 1, startCol = 1, colNames = TRUE, withFilter = FALSE)
    openxlsx::addStyle(wb, sheet_name, header_style, rows = cur_row + 1, cols = seq_len(ncol(block)), gridExpand = TRUE, stack = TRUE)
    if (nrow(block) > 0) {
      openxlsx::addStyle(wb, sheet_name, body_style, rows = (cur_row + 2):(cur_row + 1 + nrow(block)), cols = seq_len(ncol(block)), gridExpand = TRUE, stack = TRUE)
      openxlsx::addStyle(wb, sheet_name, left_style, rows = (cur_row + 2):(cur_row + 1 + nrow(block)), cols = 1, gridExpand = TRUE, stack = TRUE)
    }
    apply_table_rule_lines(wb, sheet_name, top_row = cur_row, header_rows = cur_row + 1, bottom_row = cur_row + 1 + nrow(block), n_cols = ncol(block))
    cur_row <- cur_row + nrow(block) + 4
  }

  block_widths <- compute_col_widths(dat[, c("From_State", names(dat)[!(names(dat) %in% c("Interval", "From_State"))]), drop = FALSE], min_first = 12, min_other = 14)
  if (length(block_widths) > 0) openxlsx::setColWidths(wb, sheet_name, cols = seq_along(block_widths), widths = block_widths)
  openxlsx::freezePane(wb, sheet_name, firstActiveRow = 4, firstActiveCol = 2)
}

write_t3_twoline_sheet <- function(wb, sheet_name, dat, title_text) {
  dat <- safe_df(dat)
  openxlsx::addWorksheet(wb, sheet_name)
  title_style <- openxlsx::createStyle(textDecoration = "bold", fontSize = 12, halign = "left")
  header_style <- openxlsx::createStyle(textDecoration = "bold", halign = "center", valign = "center", border = "bottom", borderStyle = "thick")
  section_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#EAF2F8", halign = "left")
  body_style <- openxlsx::createStyle(valign = "center", halign = "center")
  left_style <- openxlsx::createStyle(valign = "center", halign = "left")

  openxlsx::writeData(wb, sheet_name, data.frame(title_text), startRow = 1, startCol = 1, colNames = FALSE)
  openxlsx::mergeCells(wb, sheet_name, cols = 1:max(4, ncol(dat)), rows = 1)
  openxlsx::addStyle(wb, sheet_name, title_style, rows = 1, cols = 1, gridExpand = TRUE, stack = TRUE)

  if (nrow(dat) == 0) {
    openxlsx::writeData(wb, sheet_name, data.frame("No data"), startRow = 3, startCol = 1, colNames = FALSE)
    return(invisible(NULL))
  }

  intervals <- unique(as.character(dat$Interval))
  cur_row <- 3
  for (iv in intervals) {
    block <- dat[dat$Interval == iv, , drop = FALSE]
    state_keys <- sub("_(n|pct)$", "", names(block)[grepl("_(n|pct)$", names(block))])
    state_keys <- unique(state_keys)

    openxlsx::writeData(wb, sheet_name, data.frame(iv), startRow = cur_row, startCol = 1, colNames = FALSE)
    openxlsx::mergeCells(wb, sheet_name, cols = 1:max(2, 1 + 2 * length(state_keys)), rows = cur_row)
    openxlsx::addStyle(wb, sheet_name, section_style, rows = cur_row, cols = 1, gridExpand = TRUE, stack = TRUE)

    openxlsx::writeData(wb, sheet_name, "From / To", startRow = cur_row + 1, startCol = 1, colNames = FALSE)
    openxlsx::mergeCells(wb, sheet_name, cols = 1, rows = (cur_row + 1):(cur_row + 2))

    col_ptr <- 2
    for (st in state_keys) {
      openxlsx::writeData(wb, sheet_name, st, startRow = cur_row + 1, startCol = col_ptr, colNames = FALSE)
      openxlsx::mergeCells(wb, sheet_name, cols = col_ptr:(col_ptr + 1), rows = cur_row + 1)
      openxlsx::writeData(wb, sheet_name, "n", startRow = cur_row + 2, startCol = col_ptr, colNames = FALSE)
      openxlsx::writeData(wb, sheet_name, "%", startRow = cur_row + 2, startCol = col_ptr + 1, colNames = FALSE)
      col_ptr <- col_ptr + 2
    }

    body <- block[, c("From_State", as.vector(rbind(paste0(state_keys, "_n"), paste0(state_keys, "_pct")))), drop = FALSE]
    openxlsx::writeData(wb, sheet_name, body, startRow = cur_row + 3, startCol = 1, colNames = FALSE, withFilter = FALSE)
    openxlsx::addStyle(wb, sheet_name, header_style, rows = (cur_row + 1):(cur_row + 2), cols = seq_len(ncol(body)), gridExpand = TRUE, stack = TRUE)
    if (nrow(body) > 0) {
      openxlsx::addStyle(wb, sheet_name, body_style, rows = (cur_row + 3):(cur_row + 2 + nrow(body)), cols = seq_len(ncol(body)), gridExpand = TRUE, stack = TRUE)
      openxlsx::addStyle(wb, sheet_name, left_style, rows = (cur_row + 3):(cur_row + 2 + nrow(body)), cols = 1, gridExpand = TRUE, stack = TRUE)
    }
    apply_table_rule_lines(wb, sheet_name, top_row = cur_row, header_rows = cur_row + 2, bottom_row = cur_row + 2 + nrow(body), n_cols = ncol(body))
    cur_row <- cur_row + nrow(body) + 6
  }
  body_widths <- compute_col_widths(dat[, c("From_State", names(dat)[grepl("_(n|pct)$", names(dat))]), drop = FALSE], min_first = 12, min_other = 14)
  if (length(body_widths) > 0) openxlsx::setColWidths(wb, sheet_name, cols = seq_along(body_widths), widths = body_widths)
  openxlsx::freezePane(wb, sheet_name, firstActiveRow = 6, firstActiveCol = 2)
}

write_t5_twoline_sheet <- function(wb, sheet_name, dat, title_text) {
  dat <- safe_df(dat)
  openxlsx::addWorksheet(wb, sheet_name)
  title_style <- openxlsx::createStyle(textDecoration = "bold", fontSize = 12, halign = "left")
  header_style <- openxlsx::createStyle(textDecoration = "bold", halign = "center", valign = "center", border = "bottom", borderStyle = "thick")
  body_style <- openxlsx::createStyle(valign = "center", halign = "center")
  left_style <- openxlsx::createStyle(valign = "center", halign = "left")

  openxlsx::writeData(wb, sheet_name, data.frame(title_text), startRow = 1, startCol = 1, colNames = FALSE)
  openxlsx::mergeCells(wb, sheet_name, cols = 1:max(5, ncol(dat)), rows = 1)
  openxlsx::addStyle(wb, sheet_name, title_style, rows = 1, cols = 1, gridExpand = TRUE, stack = TRUE)

  openxlsx::writeData(wb, sheet_name, "Interval", startRow = 3, startCol = 1, colNames = FALSE)
  openxlsx::mergeCells(wb, sheet_name, cols = 1, rows = 3:4)
  openxlsx::writeData(wb, sheet_name, "Stable", startRow = 3, startCol = 2, colNames = FALSE)
  openxlsx::mergeCells(wb, sheet_name, cols = 2:3, rows = 3)
  openxlsx::writeData(wb, sheet_name, "Change", startRow = 3, startCol = 4, colNames = FALSE)
  openxlsx::mergeCells(wb, sheet_name, cols = 4:5, rows = 3)
  openxlsx::writeData(wb, sheet_name, "Prob", startRow = 4, startCol = 2, colNames = FALSE)
  openxlsx::writeData(wb, sheet_name, "%", startRow = 4, startCol = 3, colNames = FALSE)
  openxlsx::writeData(wb, sheet_name, "Prob", startRow = 4, startCol = 4, colNames = FALSE)
  openxlsx::writeData(wb, sheet_name, "%", startRow = 4, startCol = 5, colNames = FALSE)

  body <- dat[, c("Interval", "Stable_prob", "Stable_percent", "Change_prob", "Change_percent"), drop = FALSE]
  openxlsx::writeData(wb, sheet_name, body, startRow = 5, startCol = 1, colNames = FALSE, withFilter = FALSE)
  openxlsx::addStyle(wb, sheet_name, header_style, rows = 3:4, cols = seq_len(ncol(body)), gridExpand = TRUE, stack = TRUE)
  if (nrow(body) > 0) {
    openxlsx::addStyle(wb, sheet_name, body_style, rows = 5:(nrow(body) + 4), cols = seq_len(ncol(body)), gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, sheet_name, left_style, rows = 5:(nrow(body) + 4), cols = 1, gridExpand = TRUE, stack = TRUE)
  }
  widths <- compute_col_widths(body, min_first = 12, min_other = 14)
  openxlsx::setColWidths(wb, sheet_name, cols = seq_len(ncol(body)), widths = widths)
  apply_table_rule_lines(wb, sheet_name, top_row = 3, header_rows = 4, bottom_row = 4 + nrow(body), n_cols = ncol(body))
  openxlsx::freezePane(wb, sheet_name, firstActiveRow = 5, firstActiveCol = 2)
}

write_interval_block_sheet <- function(wb, sheet_name, dat, title_text) {
  dat <- safe_df(dat)
  openxlsx::addWorksheet(wb, sheet_name)
  title_style <- openxlsx::createStyle(textDecoration = "bold", fontSize = 12, halign = "left")
  header_style <- openxlsx::createStyle(textDecoration = "bold", halign = "center", valign = "center", border = "bottom", borderStyle = "thick")
  section_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#EAF2F8", halign = "left")
  body_style <- openxlsx::createStyle(valign = "center", halign = "center")
  left_style <- openxlsx::createStyle(valign = "center", halign = "left")

  openxlsx::writeData(wb, sheet_name, data.frame(title_text), startRow = 1, startCol = 1, colNames = FALSE)
  openxlsx::mergeCells(wb, sheet_name, cols = 1:max(4, ncol(dat)), rows = 1)
  openxlsx::addStyle(wb, sheet_name, title_style, rows = 1, cols = 1, gridExpand = TRUE, stack = TRUE)

  if (nrow(dat) == 0) {
    openxlsx::writeData(wb, sheet_name, data.frame("No data"), startRow = 3, startCol = 1, colNames = FALSE)
    return(invisible(NULL))
  }

  intervals <- unique(as.character(dat$Interval))
  cur_row <- 3
  for (iv in intervals) {
    block <- dat[dat$Interval == iv, , drop = FALSE]
    block <- block[, names(block)[names(block) != "Interval"], drop = FALSE]

    openxlsx::writeData(wb, sheet_name, data.frame(iv), startRow = cur_row, startCol = 1, colNames = FALSE)
    openxlsx::mergeCells(wb, sheet_name, cols = 1:max(2, ncol(block)), rows = cur_row)
    openxlsx::addStyle(wb, sheet_name, section_style, rows = cur_row, cols = 1, gridExpand = TRUE, stack = TRUE)

    openxlsx::writeData(wb, sheet_name, block, startRow = cur_row + 1, startCol = 1, colNames = TRUE, withFilter = FALSE)
    openxlsx::addStyle(wb, sheet_name, header_style, rows = cur_row + 1, cols = seq_len(ncol(block)), gridExpand = TRUE, stack = TRUE)
    if (nrow(block) > 0) {
      openxlsx::addStyle(wb, sheet_name, body_style, rows = (cur_row + 2):(cur_row + 1 + nrow(block)), cols = seq_len(ncol(block)), gridExpand = TRUE, stack = TRUE)
      openxlsx::addStyle(wb, sheet_name, left_style, rows = (cur_row + 2):(cur_row + 1 + nrow(block)), cols = 1, gridExpand = TRUE, stack = TRUE)
    }
    apply_table_rule_lines(wb, sheet_name, top_row = cur_row, header_rows = cur_row + 1, bottom_row = cur_row + 1 + nrow(block), n_cols = ncol(block))
    cur_row <- cur_row + nrow(block) + 4
  }

  block_widths <- compute_col_widths(dat[, names(dat)[names(dat) != "Interval"], drop = FALSE], min_first = 12, min_other = 14)
  if (length(block_widths) > 0) openxlsx::setColWidths(wb, sheet_name, cols = seq_along(block_widths), widths = block_widths)
  openxlsx::freezePane(wb, sheet_name, firstActiveRow = 4, firstActiveCol = 2)
}

collapse_interval_block_labels <- function(dat, group_cols = "Comparison", subgroup_cols = "Covariate") {
  dat <- safe_df(dat)
  if (nrow(dat) == 0) return(dat)
  if (!"Interval" %in% names(dat)) return(dat)

  group_cols <- intersect(group_cols, names(dat))
  subgroup_cols <- intersect(subgroup_cols, names(dat))
  if (length(group_cols) == 0 && length(subgroup_cols) == 0) return(dat)

  src <- dat
  out <- dat
  prev_interval <- NA_character_
  prev_group_key <- NA_character_
  prev_subgroup_key <- NA_character_

  build_key <- function(x) paste(as.character(x), collapse = "\r")

  for (i in seq_len(nrow(out))) {
    interval_i <- as.character(out$Interval[i] %||% "")
    if (!identical(interval_i, prev_interval)) {
      prev_interval <- interval_i
      prev_group_key <- NA_character_
      prev_subgroup_key <- NA_character_
    }

    if (length(group_cols) > 0) {
      group_key_i <- build_key(src[i, group_cols, drop = FALSE])
      if (identical(group_key_i, prev_group_key)) {
        for (nm in group_cols) out[i, nm] <- ""
      } else {
        prev_group_key <- group_key_i
        prev_subgroup_key <- NA_character_
      }
    }

    if (length(subgroup_cols) > 0) {
      subgroup_key_i <- build_key(src[i, subgroup_cols, drop = FALSE])
      if (identical(subgroup_key_i, prev_subgroup_key)) {
        for (nm in subgroup_cols) out[i, nm] <- ""
      } else {
        prev_subgroup_key <- subgroup_key_i
      }
    }
  }

  out
}

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Package 'openxlsx' is required for state-transition workbook export.", call. = FALSE)
}

CFG <- load_step_rds("CFG", dir_rds = DIR_RDS, default = list())
DICT <- load_step_rds("DICT", dir_rds = DIR_RDS, default = list())
PANEL_LONG <- load_step_rds("PANEL_LONG", dir_rds = DIR_RDS, default = data.frame())
PANEL_META <- load_step_rds("PANEL_META", dir_rds = DIR_RDS, default = list())
LONGITUDINAL_SPEC <- load_step_rds("LONGITUDINAL_SPEC", dir_rds = DIR_RDS, default = list())
TRANSITION_DATA <- load_step_rds("TRANSITION_DATA", dir_rds = DIR_RDS, default = data.frame())
ESTIMATION_TRANSITIONS <- load_step_rds("ESTIMATION_TRANSITIONS", dir_rds = DIR_RDS, default = data.frame())
ESTIMATION_PREVALENCE <- load_step_rds("ESTIMATION_PREVALENCE", dir_rds = DIR_RDS, default = data.frame())
ESTIMATION_REGISTRY <- load_step_rds("ESTIMATION_REGISTRY", dir_rds = DIR_RDS, default = data.frame())
FIT_SUMMARY <- load_step_rds("FIT_SUMMARY", dir_rds = DIR_RDS, default = data.frame())
PREP_SUMMARY <- load_step_rds("PREP_SUMMARY", dir_rds = DIR_RDS, default = list())
BASELINE_WIDE <- load_step_rds("BASELINE_WIDE", dir_rds = DIR_RDS, default = data.frame())
ESTIMATION_COVARIATES <- load_step_rds("ESTIMATION_COVARIATES", dir_rds = DIR_RDS, default = data.frame())
ESTIMATION_COVARIATES_GLOBAL <- load_step_rds("ESTIMATION_COVARIATES_GLOBAL", dir_rds = DIR_RDS, default = data.frame())
ESTIMATION_COVARIATES_UNIVARIABLE <- load_step_rds("ESTIMATION_COVARIATES_UNIVARIABLE", dir_rds = DIR_RDS, default = data.frame())
ESTIMATION_COVARIATES_GLOBAL_UNIVARIABLE <- load_step_rds("ESTIMATION_COVARIATES_GLOBAL_UNIVARIABLE", dir_rds = DIR_RDS, default = data.frame())
ESTIMATION_COVARIATES_SELECTED <- load_step_rds("ESTIMATION_COVARIATES_SELECTED", dir_rds = DIR_RDS, default = data.frame())
ESTIMATION_COVARIATES_GLOBAL_SELECTED <- load_step_rds("ESTIMATION_COVARIATES_GLOBAL_SELECTED", dir_rds = DIR_RDS, default = data.frame())
BMI_MEDIATION_STAGE1 <- load_step_rds("BMI_MEDIATION_STAGE1", dir_rds = DIR_RDS, default = data.frame())
BMI_MEDIATION_STAGE2 <- load_step_rds("BMI_MEDIATION_STAGE2", dir_rds = DIR_RDS, default = data.frame())
BMI_MEDIATION_STAGE3 <- load_step_rds("BMI_MEDIATION_STAGE3", dir_rds = DIR_RDS, default = data.frame())
STATE_LABEL_MAP <- make_state_label_map(DICT, CFG, unname(LONGITUDINAL_SPEC$state_wide_map %||% character(0)))
DICT_META <- if (is.list(DICT) && is.data.frame(DICT$meta)) safe_df(DICT$meta) else data.frame()
DICT_LEVELS <- if (is.list(DICT) && is.data.frame(DICT$levels)) safe_df(DICT$levels) else data.frame()

resolve_covariate_label_tbl <- function(var_name) {
  if (is.data.frame(DICT_META) && nrow(DICT_META) > 0 && all(c("var_name", "var_label") %in% names(DICT_META))) {
    hit <- DICT_META[as.character(DICT_META$var_name) == var_name, "var_label", drop = TRUE]
    hit <- as.character(hit)
    hit <- hit[!is.na(hit) & nzchar(hit)]
    if (length(hit) > 0) return(hit[1])
  }
  var_name
}

resolve_covariate_type_tbl <- function(var_name) {
  if (is.data.frame(DICT_META) && nrow(DICT_META) > 0 && "var_name" %in% names(DICT_META)) {
    hit <- DICT_META[as.character(DICT_META$var_name) == var_name, , drop = FALSE]
    cand_cols <- intersect(c("type", "subtype"), names(hit))
    vals <- unique(tolower(trimws(as.character(unlist(hit[, cand_cols, drop = FALSE], use.names = FALSE)))))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (any(vals %in% c("categorical", "factor", "binary", "ordinal", "nominal"))) return("categorical")
  }
  "continuous"
}

resolve_covariate_level_label_tbl <- function(var_name, value) {
  if (is.data.frame(DICT_LEVELS) && nrow(DICT_LEVELS) > 0 &&
      all(c("var_name", "value", "value_label") %in% names(DICT_LEVELS))) {
    hit <- DICT_LEVELS[
      as.character(DICT_LEVELS$var_name) == var_name &
        as.character(DICT_LEVELS$value) == as.character(value),
      "value_label",
      drop = TRUE
    ]
    hit <- as.character(hit)
    hit <- hit[!is.na(hit) & nzchar(hit)]
    if (length(hit) > 0) return(hit[1])
  }
  as.character(value)
}

ordered_covariate_levels_tbl <- function(var_name, observed_levels = character(0)) {
  observed_levels <- as.character(observed_levels)
  observed_levels <- observed_levels[!is.na(observed_levels) & nzchar(observed_levels)]
  dict_levels <- character(0)
  if (is.data.frame(DICT_LEVELS) && nrow(DICT_LEVELS) > 0 &&
      all(c("var_name", "value") %in% names(DICT_LEVELS))) {
    dict_levels <- as.character(DICT_LEVELS$value[as.character(DICT_LEVELS$var_name) == var_name])
    dict_levels <- dict_levels[!is.na(dict_levels) & nzchar(dict_levels)]
  }
  vals <- unique(c(dict_levels, observed_levels))
  if (length(vals) <= 1) return(vals)
  vals_num <- suppressWarnings(as.numeric(vals))
  if (all(!is.na(vals_num))) {
    return(vals[order(vals_num, vals)])
  }
  vals
}

fmt_mean_sd_local <- function(x, digits = 2) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (length(x) == 0) return("")
  sd_x <- if (length(x) > 1) stats::sd(x) else 0
  paste0(sprintf(paste0("%.", digits, "f"), mean(x)), " (", sprintf(paste0("%.", digits, "f"), sd_x), ")")
}

fmt_n_pct_local <- function(n, denom, digits = 1) {
  if (is.na(n) || is.na(denom) || denom <= 0) return("")
  paste0(fmt_n(n), " (", sprintf(paste0("%.", digits, "f"), 100 * n / denom), ")")
}

build_baseline_characteristics_combined <- function(baseline_df, covariates, baseline_state_col = NULL, state_levels = character(0)) {
  baseline_df <- safe_df(baseline_df)
  covariates <- unique(as.character(covariates))
  covariates <- covariates[covariates %in% names(baseline_df)]
  if (nrow(baseline_df) == 0 || length(covariates) == 0) return(data.frame())

  use_state_groups <- !is.null(baseline_state_col) && baseline_state_col %in% names(baseline_df)
  state_cols <- character(0)
  if (use_state_groups) {
    state_vals <- as.character(baseline_df[[baseline_state_col]])
    if (length(state_levels) == 0) state_levels <- unique(state_vals)
    state_levels <- state_levels[state_levels %in% unique(state_vals)]
    state_levels <- state_levels[!is.na(state_levels) & nzchar(state_levels)]
    if (length(state_levels) <= 1) {
      use_state_groups <- FALSE
    } else {
      state_cols <- state_levels
    }
  }

  rows <- list()
  row_idx <- 1L
  for (cv in covariates) {
    x <- baseline_df[[cv]]
    cv_label <- resolve_covariate_label_tbl(cv)
    cv_type <- resolve_covariate_type_tbl(cv)
    miss_n <- sum(is.na(x) | trimws(as.character(x)) == "")

    if (identical(cv_type, "categorical")) {
      x_chr <- as.character(x)
      x_chr[trimws(x_chr) == ""] <- NA_character_
      level_values <- ordered_covariate_levels_tbl(cv, observed_levels = unique(x_chr[!is.na(x_chr)]))
      first_row <- TRUE
      for (lev in level_values) {
        n_lev <- sum(x_chr == lev, na.rm = TRUE)
        if (n_lev == 0) next
        row <- data.frame(
          Covariate = if (first_row) cv_label else "",
          Category = resolve_covariate_level_label_tbl(cv, lev),
          Overall = fmt_n_pct_local(n_lev, sum(!is.na(x_chr))),
          Missing_n = if (first_row) fmt_n(miss_n) else "",
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        if (use_state_groups) {
          for (st in state_cols) {
            idx <- baseline_df[[baseline_state_col]] == st & !is.na(baseline_df[[baseline_state_col]])
            denom_st <- sum(idx & !is.na(x_chr))
            row[[st]] <- fmt_n_pct_local(sum(idx & x_chr == lev, na.rm = TRUE), denom_st)
          }
          row <- row[, c("Covariate", "Category", "Overall", state_cols, "Missing_n"), drop = FALSE]
        }
        rows[[row_idx]] <- row
        row_idx <- row_idx + 1L
        first_row <- FALSE
      }
    } else {
      x_num <- suppressWarnings(as.numeric(x))
      row <- data.frame(
        Covariate = cv_label,
        Category = "M±SD",
        Overall = fmt_mean_sd_local(x_num),
        Missing_n = fmt_n(miss_n),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      if (use_state_groups) {
        for (st in state_cols) {
          idx <- baseline_df[[baseline_state_col]] == st & !is.na(baseline_df[[baseline_state_col]])
          row[[st]] <- fmt_mean_sd_local(x_num[idx])
        }
        row <- row[, c("Covariate", "Category", "Overall", state_cols, "Missing_n"), drop = FALSE]
      }
      rows[[row_idx]] <- row
      row_idx <- row_idx + 1L
    }
  }

  if (length(rows) == 0) return(data.frame())
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

build_baseline_characteristics_split <- function(baseline_df, covariates) {
  baseline_df <- safe_df(baseline_df)
  covariates <- unique(as.character(covariates))
  covariates <- covariates[covariates %in% names(baseline_df)]
  if (nrow(baseline_df) == 0 || length(covariates) == 0) return(data.frame())

  rows <- list()
  row_idx <- 1L
  for (cv in covariates) {
    x <- baseline_df[[cv]]
    cv_label <- resolve_covariate_label_tbl(cv)
    cv_type <- resolve_covariate_type_tbl(cv)
    miss_n <- sum(is.na(x) | trimws(as.character(x)) == "")

    if (identical(cv_type, "categorical")) {
      x_chr <- as.character(x)
      x_chr[trimws(x_chr) == ""] <- NA_character_
      denom <- sum(!is.na(x_chr))
      level_values <- ordered_covariate_levels_tbl(cv, observed_levels = unique(x_chr[!is.na(x_chr)]))
      first_row <- TRUE
      for (lev in level_values) {
        n_lev <- sum(x_chr == lev, na.rm = TRUE)
        if (n_lev == 0) next
        rows[[row_idx]] <- data.frame(
          Covariate = if (first_row) cv_label else "",
          Category = resolve_covariate_level_label_tbl(cv, lev),
          `n/M` = fmt_n(n_lev),
          `%/SD` = if (denom > 0) sprintf("%.1f", 100 * n_lev / denom) else "",
          Missing_n = if (first_row) fmt_n(miss_n) else "",
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        row_idx <- row_idx + 1L
        first_row <- FALSE
      }
    } else {
      x_num <- suppressWarnings(as.numeric(x))
      x_num_nonmiss <- x_num[!is.na(x_num)]
      rows[[row_idx]] <- data.frame(
        Covariate = cv_label,
        Category = "",
        `n/M` = if (length(x_num_nonmiss) > 0) sprintf("%.2f", mean(x_num_nonmiss)) else "",
        `%/SD` = if (length(x_num_nonmiss) > 1) sprintf("%.2f", stats::sd(x_num_nonmiss)) else if (length(x_num_nonmiss) == 1) "0.00" else "",
        Missing_n = fmt_n(miss_n),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      row_idx <- row_idx + 1L
    }
  }

  if (length(rows) == 0) return(data.frame())
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

covariate_order_map <- setNames(numeric(0), character(0))
if (is.data.frame(DICT_META) && nrow(DICT_META) > 0 && all(c("var_name", "display_order") %in% names(DICT_META))) {
  cov_meta <- DICT_META
  role_col <- NULL
  if ("analysis_role" %in% names(cov_meta)) role_col <- "analysis_role"
  if (is.null(role_col) && "role" %in% names(cov_meta)) role_col <- "role"
  if (!is.null(role_col)) {
    cov_meta <- cov_meta[tolower(as.character(cov_meta[[role_col]])) == "covariate", , drop = FALSE]
  }
  cov_names <- toupper(as.character(cov_meta$var_name))
  cov_ord <- suppressWarnings(as.numeric(cov_meta$display_order))
  keep <- !is.na(cov_names) & nzchar(cov_names) & !is.na(cov_ord)
  if (any(keep)) covariate_order_map <- stats::setNames(cov_ord[keep], cov_names[keep])
  if ("var_label" %in% names(cov_meta)) {
    cov_labels <- toupper(as.character(cov_meta$var_label))
    keep_lab <- !is.na(cov_labels) & nzchar(cov_labels) & !is.na(cov_ord)
    if (any(keep_lab)) {
      covariate_order_map <- c(covariate_order_map, stats::setNames(cov_ord[keep_lab], cov_labels[keep_lab]))
    }
  }
}

# Override local table-format helpers so downstream table builders follow the
# latest manuscript rules without depending on older intermediate layouts.
fmt_mean_sd_local <- function(x, digits = 2) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (length(x) == 0) return("")
  sd_x <- if (length(x) > 1) stats::sd(x) else 0
  plus_minus <- intToUtf8(177)
  paste0(sprintf(paste0("%.", digits, "f"), mean(x)), plus_minus, sprintf(paste0("%.", digits, "f"), sd_x))
}

build_baseline_characteristics_combined <- function(baseline_df, covariates, baseline_state_col = NULL, state_levels = character(0)) {
  baseline_df <- safe_df(baseline_df)
  covariates <- unique(as.character(covariates))
  covariates <- covariates[covariates %in% names(baseline_df)]
  if (nrow(baseline_df) == 0 || length(covariates) == 0) return(data.frame())

  use_state_groups <- !is.null(baseline_state_col) && baseline_state_col %in% names(baseline_df)
  state_cols <- character(0)
  if (use_state_groups) {
    state_vals <- as.character(baseline_df[[baseline_state_col]])
    if (length(state_levels) == 0) state_levels <- unique(state_vals)
    state_levels <- state_levels[state_levels %in% unique(state_vals)]
    state_levels <- state_levels[!is.na(state_levels) & nzchar(state_levels)]
    if (length(state_levels) <= 1) {
      use_state_groups <- FALSE
    } else {
      state_cols <- state_levels
    }
  }

  rows <- list()
  row_idx <- 1L
  for (cv in covariates) {
    x <- baseline_df[[cv]]
    cv_label <- resolve_covariate_label_tbl(cv)
    cv_type <- resolve_covariate_type_tbl(cv)
    miss_n <- sum(is.na(x) | trimws(as.character(x)) == "")

    if (identical(cv_type, "categorical")) {
      x_chr <- as.character(x)
      x_chr[trimws(x_chr) == ""] <- NA_character_
      level_values <- ordered_covariate_levels_tbl(cv, observed_levels = unique(x_chr[!is.na(x_chr)]))
      first_row <- TRUE
      for (lev in level_values) {
        n_lev <- sum(x_chr == lev, na.rm = TRUE)
        if (n_lev == 0) next
        row <- data.frame(
          Covariate = if (first_row) cv_label else "",
          Category = resolve_covariate_level_label_tbl(cv, lev),
          Overall = fmt_n_pct_local(n_lev, sum(!is.na(x_chr))),
          Missing_n = if (first_row) fmt_n(miss_n) else "",
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        if (use_state_groups) {
          for (st in state_cols) {
            idx <- baseline_df[[baseline_state_col]] == st & !is.na(baseline_df[[baseline_state_col]])
            denom_st <- sum(idx & !is.na(x_chr))
            row[[st]] <- fmt_n_pct_local(sum(idx & x_chr == lev, na.rm = TRUE), denom_st)
          }
          row <- row[, c("Covariate", "Category", "Overall", state_cols, "Missing_n"), drop = FALSE]
        }
        rows[[row_idx]] <- row
        row_idx <- row_idx + 1L
        first_row <- FALSE
      }
    } else {
      x_num <- suppressWarnings(as.numeric(x))
      row <- data.frame(
        Covariate = cv_label,
        Category = paste0("M", intToUtf8(177), "SD"),
        Overall = fmt_mean_sd_local(x_num),
        Missing_n = fmt_n(miss_n),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      if (use_state_groups) {
        for (st in state_cols) {
          idx <- baseline_df[[baseline_state_col]] == st & !is.na(baseline_df[[baseline_state_col]])
          row[[st]] <- fmt_mean_sd_local(x_num[idx])
        }
        row <- row[, c("Covariate", "Category", "Overall", state_cols, "Missing_n"), drop = FALSE]
      }
      rows[[row_idx]] <- row
      row_idx <- row_idx + 1L
    }
  }

  if (length(rows) == 0) return(data.frame())
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

build_baseline_characteristics_split <- function(baseline_df, covariates) {
  baseline_df <- safe_df(baseline_df)
  covariates <- unique(as.character(covariates))
  covariates <- covariates[covariates %in% names(baseline_df)]
  if (nrow(baseline_df) == 0 || length(covariates) == 0) return(data.frame())

  rows <- list()
  row_idx <- 1L
  for (cv in covariates) {
    x <- baseline_df[[cv]]
    cv_label <- resolve_covariate_label_tbl(cv)
    cv_type <- resolve_covariate_type_tbl(cv)
    miss_n <- sum(is.na(x) | trimws(as.character(x)) == "")

    if (identical(cv_type, "categorical")) {
      x_chr <- as.character(x)
      x_chr[trimws(x_chr) == ""] <- NA_character_
      denom <- sum(!is.na(x_chr))
      level_values <- ordered_covariate_levels_tbl(cv, observed_levels = unique(x_chr[!is.na(x_chr)]))
      first_row <- TRUE
      for (lev in level_values) {
        n_lev <- sum(x_chr == lev, na.rm = TRUE)
        if (n_lev == 0) next
        rows[[row_idx]] <- data.frame(
          Covariate = if (first_row) cv_label else "",
          Category = resolve_covariate_level_label_tbl(cv, lev),
          `n or M` = fmt_n(n_lev),
          `% or SD` = if (denom > 0) sprintf("%.1f", 100 * n_lev / denom) else "",
          Missing_n = if (first_row) fmt_n(miss_n) else "",
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        row_idx <- row_idx + 1L
        first_row <- FALSE
      }
    } else {
      x_num <- suppressWarnings(as.numeric(x))
      x_num_nonmiss <- x_num[!is.na(x_num)]
      rows[[row_idx]] <- data.frame(
        Covariate = cv_label,
        Category = "",
        `n or M` = if (length(x_num_nonmiss) > 0) sprintf("%.2f", mean(x_num_nonmiss)) else "",
        `% or SD` = if (length(x_num_nonmiss) > 1) sprintf("%.2f", stats::sd(x_num_nonmiss)) else if (length(x_num_nonmiss) == 1) "0.00" else "",
        Missing_n = fmt_n(miss_n),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      row_idx <- row_idx + 1L
    }
  }

  if (length(rows) == 0) return(data.frame())
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

sort_covariates_by_display_order <- function(vars, order_map) {
  vars <- unique(as.character(vars))
  vars <- vars[!is.na(vars) & nzchar(vars)]
  if (length(vars) == 0) return(character(0))
  ord <- unname(order_map[toupper(vars)])
  ord[is.na(ord)] <- 999999 + seq_along(vars)[is.na(ord)]
  vars[order(ord, vars)]
}

wave_order <- PANEL_META$wave_order %||% sort(unique(as.character(PANEL_LONG$panel_wave)))
interval_levels <- if (length(wave_order) >= 2) paste0(toupper(wave_order[-length(wave_order)]), "_to_", toupper(wave_order[-1])) else character(0)

S3 <- data.frame(
  Metric = c("Unique participants", "Panel rows", "Observed transition pairs", "Number of intervals", "Loglikelihood", "AIC", "BIC", "SABIC"),
  Value = c(
    fmt_n(PREP_SUMMARY$n_ids %||% NA_integer_),
    fmt_n(PREP_SUMMARY$n_panel_rows %||% NA_integer_),
    fmt_n(PREP_SUMMARY$n_transition_rows %||% NA_integer_),
    fmt_n(length(unique(as.character(ESTIMATION_TRANSITIONS$interval)))),
    fmt_stat3(FIT_SUMMARY$ll[1] %||% NA_real_),
    fmt_stat3(FIT_SUMMARY$aic[1] %||% NA_real_),
    fmt_stat3(FIT_SUMMARY$bic[1] %||% NA_real_),
    fmt_stat3(FIT_SUMMARY$sabic[1] %||% NA_real_)
  ),
  stringsAsFactors = FALSE
)

T1 <- data.frame()
A1 <- data.frame()
baseline_covariates <- PREP_SUMMARY$baseline_covariates %||% setdiff(names(BASELINE_WIDE), names(BASELINE_WIDE)[1])
baseline_covariates <- unique(as.character(unlist(baseline_covariates, use.names = FALSE)))
baseline_covariates <- baseline_covariates[baseline_covariates %in% names(BASELINE_WIDE)]
baseline_covariates <- sort_covariates_by_display_order(baseline_covariates, covariate_order_map)
if (is.data.frame(BASELINE_WIDE) && nrow(BASELINE_WIDE) > 0 && length(baseline_covariates) > 0) {
  baseline_wave <- toupper(wave_order[1] %||% "Y1")
  base_state_df <- data.frame()
  if (is.data.frame(PANEL_LONG) && nrow(PANEL_LONG) > 0) {
    base_state_df <- PANEL_LONG[toupper(as.character(PANEL_LONG$panel_wave)) == baseline_wave, c("panel_id", "state"), drop = FALSE]
    if (nrow(base_state_df) > 0) {
      base_state_df$Baseline_State <- state_label(base_state_df$state, STATE_LABEL_MAP)
      base_state_df$id_key <- as.character(base_state_df$panel_id)
      base_state_df <- unique(base_state_df[, c("id_key", "Baseline_State"), drop = FALSE])
    }
  }
  base_cov_df <- BASELINE_WIDE
  id_col_baseline <- names(base_cov_df)[1]
  base_cov_df$id_key <- as.character(base_cov_df[[id_col_baseline]])
  baseline_df <- merge(base_cov_df, base_state_df, by = "id_key", all.x = TRUE, sort = FALSE)
  state_levels_t1 <- if (is.data.frame(PANEL_LONG) && nrow(PANEL_LONG) > 0) {
    state_label(sort(unique(PANEL_LONG$state)), STATE_LABEL_MAP)
  } else {
    character(0)
  }
  T1 <- build_baseline_characteristics_split(
    baseline_df = baseline_df,
    covariates = baseline_covariates
  )
  A1 <- build_baseline_characteristics_combined(
    baseline_df = baseline_df,
    covariates = baseline_covariates,
    baseline_state_col = "Baseline_State",
    state_levels = state_levels_t1
  )
}

if (is.data.frame(PANEL_LONG) && nrow(PANEL_LONG) > 0) {
  raw_prev <- aggregate(
    list(n_num = PANEL_LONG$panel_id),
    by = list(Wave = toupper(as.character(PANEL_LONG$panel_wave)), State = PANEL_LONG$state),
    FUN = length
  )
  wave_totals <- aggregate(list(total_n = raw_prev$n_num), by = list(Wave = raw_prev$Wave), FUN = sum)
  raw_prev <- merge(raw_prev, wave_totals, by = "Wave", all.x = TRUE, sort = FALSE)
  raw_prev$Percent_num <- ifelse(raw_prev$total_n > 0, raw_prev$n_num / raw_prev$total_n, NA_real_)
  raw_prev <- raw_prev[order(match(raw_prev$Wave, toupper(wave_order)), raw_prev$State), , drop = FALSE]
  T2_long <- data.frame(
    Wave = raw_prev$Wave,
    State = raw_prev$State,
    n_num = raw_prev$n_num,
    Percent_num = raw_prev$Percent_num,
    n = fmt_n(raw_prev$n_num),
    Percent = fmt_pct1(raw_prev$Percent_num),
    stringsAsFactors = FALSE
  )
  T2 <- build_wave_state_wide(T2_long, wave_order = toupper(wave_order), prefix = "state", label_map = STATE_LABEL_MAP)
  A2 <- build_wave_state_chr(T2_long, wave_order = toupper(wave_order), label_map = STATE_LABEL_MAP)
} else {
  wave_n <- data.frame()
  T2_long <- data.frame()
  T2 <- data.frame()
  A2 <- data.frame()
}

T2 <- safe_df(T2)
A2 <- safe_df(A2)
T3 <- data.frame()
A3 <- data.frame()
T4 <- data.frame()
T5 <- data.frame()
T6 <- data.frame()
T6_1 <- data.frame()
T6_2 <- data.frame()
T7 <- data.frame()
T8 <- data.frame()
T8_1 <- data.frame()
T8_2 <- data.frame()
M1 <- data.frame()
M2 <- data.frame()
M3 <- data.frame()
A6_1 <- data.frame()
A6_2 <- data.frame()
S1 <- data.frame()
S2 <- data.frame()

if (is.data.frame(TRANSITION_DATA) && nrow(TRANSITION_DATA) > 0) {
  raw_counts <- aggregate(
    list(n = TRANSITION_DATA$panel_id),
    by = list(interval = TRANSITION_DATA$interval, from_state = TRANSITION_DATA$from_state, to_state = TRANSITION_DATA$to_state),
    FUN = length
  )
  raw_counts$from_state <- suppressWarnings(as.integer(as.character(raw_counts$from_state)))
  raw_counts$to_state <- suppressWarnings(as.integer(as.character(raw_counts$to_state)))
  raw_counts$row_total <- ave(raw_counts$n, raw_counts$interval, raw_counts$from_state, FUN = sum)
  raw_counts$row_prob <- ifelse(raw_counts$row_total > 0, raw_counts$n / raw_counts$row_total, NA_real_)
  raw_counts$cell <- paste0(fmt_n(raw_counts$n), " (", fmt_pct1(raw_counts$row_prob), ")")

  all_intervals <- if (length(interval_levels) > 0) interval_levels[interval_levels %in% unique(as.character(raw_counts$interval))] else unique(as.character(raw_counts$interval))
  T3 <- do.call(rbind, lapply(all_intervals, function(iv) {
    df_iv <- raw_counts[raw_counts$interval == iv, , drop = FALSE]
    out <- build_interval_matrix_split(df_iv, label_map = STATE_LABEL_MAP)
    if (nrow(out) == 0) return(out)
    out$Interval <- iv
    out[, c("Interval", names(out)[names(out) != "Interval"]), drop = FALSE]
  }))
  rownames(T3) <- NULL

  A3 <- do.call(rbind, lapply(all_intervals, function(iv) {
    df_iv <- raw_counts[raw_counts$interval == iv, , drop = FALSE]
    out <- build_interval_matrix_chr(df_iv, value_col = "cell", label_map = STATE_LABEL_MAP)
    if (nrow(out) == 0) return(out)
    out$Interval <- iv
    out[, c("Interval", names(out)[names(out) != "Interval"]), drop = FALSE]
  }))
  rownames(A3) <- NULL

  T4 <- do.call(rbind, lapply(all_intervals, function(iv) {
    df_iv <- raw_counts[raw_counts$interval == iv, , drop = FALSE]
    out <- build_interval_matrix(df_iv, value_col = "row_prob", label_map = STATE_LABEL_MAP)
    if (nrow(out) == 0) return(out)
    out$Interval <- iv
    out[, c("Interval", names(out)[names(out) != "Interval"]), drop = FALSE]
  }))
  rownames(T4) <- NULL

  stable_df <- raw_counts[raw_counts$from_state == raw_counts$to_state, , drop = FALSE]
  stable_n <- aggregate(list(stable_n = stable_df$n), by = list(Interval = stable_df$interval), FUN = sum)
  interval_totals <- aggregate(list(total_n = raw_counts$n), by = list(Interval = raw_counts$interval), FUN = sum)
  stable_prob <- merge(interval_totals, stable_n, by = "Interval", all.x = TRUE, sort = FALSE)
  stable_prob$stable_n[is.na(stable_prob$stable_n)] <- 0
  stable_prob$stable_prob <- ifelse(stable_prob$total_n > 0, stable_prob$stable_n / stable_prob$total_n, NA_real_)
  T5 <- data.frame(
    Interval = stable_prob$Interval,
    Stable_prob = fmt_est2(stable_prob$stable_prob),
    Stable_percent = fmt_pct1(stable_prob$stable_prob),
    Change_prob = fmt_est2(1 - stable_prob$stable_prob),
    Change_percent = fmt_pct1(1 - stable_prob$stable_prob),
    stringsAsFactors = FALSE
  )

  S1 <- do.call(rbind, lapply(all_intervals, function(iv) {
    df_iv <- raw_counts[raw_counts$interval == iv, , drop = FALSE]
    out <- build_interval_matrix(df_iv, value_col = "n", label_map = STATE_LABEL_MAP)
    if (nrow(out) == 0) return(out)
    out$Interval <- iv
    out[, c("Interval", names(out)[names(out) != "Interval"]), drop = FALSE]
  }))
  rownames(S1) <- NULL
}

if (is.data.frame(T3) && nrow(T3) > 0 && length(interval_levels) > 0) T3 <- T3[order(match(T3$Interval, interval_levels)), , drop = FALSE]
if (is.data.frame(A3) && nrow(A3) > 0 && length(interval_levels) > 0) A3 <- A3[order(match(A3$Interval, interval_levels)), , drop = FALSE]
if (is.data.frame(T4) && nrow(T4) > 0 && length(interval_levels) > 0) T4 <- T4[order(match(T4$Interval, interval_levels)), , drop = FALSE]
if (is.data.frame(T5) && nrow(T5) > 0 && length(interval_levels) > 0) T5 <- T5[order(match(T5$Interval, interval_levels)), , drop = FALSE]
if (is.data.frame(T7) && nrow(T7) > 0 && length(interval_levels) > 0) T7 <- T7[order(match(T7$Interval, interval_levels)), , drop = FALSE]
if (is.data.frame(S1) && nrow(S1) > 0 && length(interval_levels) > 0) S1 <- S1[order(match(S1$Interval, interval_levels)), , drop = FALSE]
if (is.data.frame(S2) && nrow(S2) > 0 && length(interval_levels) > 0) S2 <- S2[order(match(S2$Interval, interval_levels), S2$From_State, S2$To_State), , drop = FALSE]

if (is.data.frame(TRANSITION_DATA) && nrow(TRANSITION_DATA) > 0) {
  S2 <- aggregate(
    list(n = TRANSITION_DATA$panel_id),
    by = list(Interval = TRANSITION_DATA$interval, From_State = TRANSITION_DATA$from_state, To_State = TRANSITION_DATA$to_state),
    FUN = length
  )
  names(S2)[names(S2) == "n"] <- "Pair_Count"
  S2$From_State <- state_label(S2$From_State, STATE_LABEL_MAP)
  S2$To_State <- state_label(S2$To_State, STATE_LABEL_MAP)
  S2 <- S2[order(S2$Interval, S2$From_State, S2$To_State), , drop = FALSE]
  S2$Pair_Count <- fmt_n(S2$Pair_Count)
  S2 <- collapse_interval_block_labels(S2, group_cols = "From_State", subgroup_cols = character(0))
}

if (is.data.frame(ESTIMATION_TRANSITIONS) && nrow(ESTIMATION_TRANSITIONS) > 0) {
  est_transitions <- safe_df(ESTIMATION_TRANSITIONS)
  est_transitions$from_state <- suppressWarnings(as.integer(as.character(est_transitions$from_state)))
  est_transitions$to_state <- suppressWarnings(as.integer(as.character(est_transitions$to_state)))
  ref_state <- suppressWarnings(as.integer(PREP_SUMMARY$reference_state %||% max(est_transitions$to_state, na.rm = TRUE)))
  continuous_cov_means <- setNames(numeric(0), character(0))
  if (is.data.frame(BASELINE_WIDE) && nrow(BASELINE_WIDE) > 0 && is.data.frame(ESTIMATION_COVARIATES) && nrow(ESTIMATION_COVARIATES) > 0) {
    cont_vars <- unique(as.character(ESTIMATION_COVARIATES$source_var[ESTIMATION_COVARIATES$predictor_type == "continuous"]))
    cont_vars <- cont_vars[!is.na(cont_vars) & nzchar(cont_vars) & cont_vars %in% names(BASELINE_WIDE)]
    if (length(cont_vars) > 0) {
      continuous_cov_means <- stats::setNames(
        vapply(cont_vars, function(v) {
          xv <- suppressWarnings(as.numeric(BASELINE_WIDE[[v]]))
          xv <- xv[!is.na(xv)]
          if (length(xv) == 0) return(0)
          mean(xv)
        }, numeric(1)),
        cont_vars
      )
    }
  }

  adjusted_transitions <- est_transitions
  if (is.data.frame(ESTIMATION_COVARIATES) && nrow(ESTIMATION_COVARIATES) > 0 && !is.na(ref_state)) {
    cov_adj <- safe_df(ESTIMATION_COVARIATES)
    cov_adj$to_state <- suppressWarnings(as.integer(as.character(cov_adj$to_state)))
    cov_adj$adj_contrib <- 0
    is_cont <- cov_adj$predictor_type == "continuous" & !is.na(cov_adj$source_var)
    cov_adj$adj_contrib[is_cont] <- cov_adj$estimate[is_cont] * unname(continuous_cov_means[as.character(cov_adj$source_var[is_cont])])
    cov_adj_sum <- aggregate(
      list(adj_contrib = cov_adj$adj_contrib),
      by = list(interval = cov_adj$interval, to_state = cov_adj$to_state),
      FUN = sum
    )

    intervals_est <- unique(as.character(est_transitions$interval))
    from_states_est <- sort(unique(est_transitions$from_state))
    to_states_est <- sort(unique(est_transitions$to_state))
    nonref_states <- to_states_est[to_states_est != ref_state]
    out_rows <- list()
    out_idx <- 1L

    for (iv in intervals_est) {
      for (fs in from_states_est) {
        df_if <- est_transitions[est_transitions$interval == iv & est_transitions$from_state == fs, , drop = FALSE]
        if (nrow(df_if) == 0) next
        p_ref0 <- df_if$prob[df_if$to_state == ref_state]
        if (length(p_ref0) == 0 || is.na(p_ref0[1]) || p_ref0[1] <= 0) next
        eta_vals <- numeric(0)
        for (st in nonref_states) {
          p_st <- df_if$prob[df_if$to_state == st]
          if (length(p_st) == 0 || is.na(p_st[1]) || p_st[1] <= 0) {
            eta_vals[as.character(st)] <- -Inf
          } else {
            base_eta <- log(p_st[1] / p_ref0[1])
            adj_hit <- cov_adj_sum$adj_contrib[cov_adj_sum$interval == iv & cov_adj_sum$to_state == st]
            adj_val <- if (length(adj_hit) == 0 || is.na(adj_hit[1])) 0 else adj_hit[1]
            eta_vals[as.character(st)] <- base_eta + adj_val
          }
        }
        denom <- 1 + sum(exp(eta_vals[is.finite(eta_vals)]))
        for (st in to_states_est) {
          prob_st <- if (st == ref_state) {
            1 / denom
          } else {
            exp(eta_vals[as.character(st)]) / denom
          }
          out_rows[[out_idx]] <- data.frame(
            interval = iv,
            from_state = fs,
            to_state = st,
            prob = prob_st,
            stringsAsFactors = FALSE
          )
          out_idx <- out_idx + 1L
        }
      }
    }
    if (length(out_rows) > 0) {
      adjusted_transitions <- do.call(rbind, out_rows)
    }
  }

  all_intervals_est <- if (length(interval_levels) > 0) {
    interval_levels[interval_levels %in% unique(as.character(adjusted_transitions$interval))]
  } else {
    unique(as.character(adjusted_transitions$interval))
  }
  T7 <- do.call(rbind, lapply(all_intervals_est, function(iv) {
    df_iv <- adjusted_transitions[adjusted_transitions$interval == iv, , drop = FALSE]
    out <- build_interval_matrix(df_iv, value_col = "prob", label_map = STATE_LABEL_MAP)
    if (nrow(out) == 0) return(out)
    out$Interval <- iv
    out[, c("Interval", names(out)[names(out) != "Interval"]), drop = FALSE]
  }))
  rownames(T7) <- NULL
}

if (is.data.frame(ESTIMATION_TRANSITIONS) && nrow(ESTIMATION_TRANSITIONS) > 0) {
  est_transitions <- safe_df(ESTIMATION_TRANSITIONS)
  est_transitions$from_state <- suppressWarnings(as.integer(as.character(est_transitions$from_state)))
  est_transitions$to_state <- suppressWarnings(as.integer(as.character(est_transitions$to_state)))
  all_intervals_est <- if (length(interval_levels) > 0) {
    interval_levels[interval_levels %in% unique(as.character(est_transitions$interval))]
  } else {
    unique(as.character(est_transitions$interval))
  }
  T7 <- do.call(rbind, lapply(all_intervals_est, function(iv) {
    df_iv <- est_transitions[est_transitions$interval == iv, , drop = FALSE]
    out <- build_interval_matrix(df_iv, value_col = "prob", label_map = STATE_LABEL_MAP)
    if (nrow(out) == 0) return(out)
    out$Interval <- iv
    out[, c("Interval", names(out)[names(out) != "Interval"]), drop = FALSE]
  }))
  rownames(T7) <- NULL
}

if (is.data.frame(ESTIMATION_TRANSITIONS) && nrow(ESTIMATION_TRANSITIONS) > 0 &&
    is.data.frame(ESTIMATION_REGISTRY) && nrow(ESTIMATION_REGISTRY) > 0) {
  safe_read_lines_tbl <- function(path) {
    if (is.null(path) || length(path) == 0 || is.na(path) || !file.exists(path)) return(character(0))
    tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"),
      error = function(e) tryCatch(readLines(path, warn = FALSE), error = function(e2) character(0)))
  }

  parse_model_results_all_tbl <- function(out_file) {
    txt <- safe_read_lines_tbl(out_file)
    up <- toupper(txt)
    start_idx <- grep("^MODEL RESULTS\\s*$", up)
    if (length(start_idx) == 0) return(data.frame())
    block <- txt[seq.int(start_idx[1] + 1L, length(txt))]
    block_up <- toupper(block)
    out_list <- list()
    current_outcome <- NA_character_
    idx <- 1L
    for (i in seq_along(block)) {
      ln <- block[i]
      up_i <- block_up[i]
      if (grepl("^\\s*INTERCEPTS\\s*$", up_i) || grepl("^\\s*NEW/ADDITIONAL PARAMETERS\\s*$", up_i) || grepl("^\\s*TECHNICAL", up_i)) break
      if (grepl("^\\s*[A-Z0-9_#]+\\s+ON\\s*$", up_i)) {
        current_outcome <- trimws(sub("\\s+ON\\s*$", "", up_i))
        next
      }
      if (is.na(current_outcome) || !nzchar(current_outcome)) next
      parts <- strsplit(trimws(ln), "\\s+")[[1]]
      if (length(parts) < 5) next
      vals <- suppressWarnings(as.numeric(parts[2:5]))
      if (any(is.na(vals))) next
      out_list[[idx]] <- data.frame(
        outcome = current_outcome,
        predictor = toupper(parts[1]),
        estimate = vals[1],
        se = vals[2],
        z = vals[3],
        p = vals[4],
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
    out_list <- out_list[!vapply(out_list, is.null, logical(1))]
    if (length(out_list) == 0) return(data.frame())
    out <- do.call(rbind, out_list)
    rownames(out) <- NULL
    out
  }

  parse_nominal_intercepts_tbl <- function(out_file) {
    txt <- safe_read_lines_tbl(out_file)
    up <- toupper(txt)
    start_idx <- grep("^MODEL RESULTS\\s*$", up)
    if (length(start_idx) == 0) return(data.frame())
    block <- txt[seq.int(start_idx[1] + 1L, length(txt))]
    block_up <- toupper(block)
    in_intercepts <- FALSE
    out_list <- list()
    idx <- 1L
    for (i in seq_along(block)) {
      ln <- block[i]
      up_i <- block_up[i]
      if (grepl("^\\s*INTERCEPTS\\s*$", up_i)) {
        in_intercepts <- TRUE
        next
      }
      if (!in_intercepts) next
      if (grepl("^\\s*LOGISTIC REGRESSION ODDS RATIO RESULTS\\s*$", up_i) || grepl("^\\s*TECHNICAL", up_i) || grepl("^\\s*NEW/ADDITIONAL PARAMETERS\\s*$", up_i)) break
      parts <- strsplit(trimws(ln), "\\s+")[[1]]
      if (length(parts) < 5) next
      vals <- suppressWarnings(as.numeric(parts[2:5]))
      if (any(is.na(vals))) next
      out_list[[idx]] <- data.frame(
        outcome = toupper(parts[1]),
        estimate = vals[1],
        se = vals[2],
        z = vals[3],
        p = vals[4],
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
    out_list <- out_list[!vapply(out_list, is.null, logical(1))]
    if (length(out_list) == 0) return(data.frame())
    out <- do.call(rbind, out_list)
    rownames(out) <- NULL
    out
  }

  out_file_tbl <- as.character(ESTIMATION_REGISTRY$out_file[1] %||% NA_character_)
  model_results_all_tbl <- parse_model_results_all_tbl(out_file_tbl)
  intercepts_tbl <- parse_nominal_intercepts_tbl(out_file_tbl)

  transition_specs_tbl <- PREP_SUMMARY$transition_specs %||% list()
  state_values_tbl <- suppressWarnings(as.integer(PREP_SUMMARY$state_values %||% integer(0)))
  ref_state_tbl <- suppressWarnings(as.integer(PREP_SUMMARY$reference_state %||% NA_integer_))
  active_cov_tbl <- toupper(as.character(PREP_SUMMARY$active_baseline_covariates %||% character(0)))

  cov_spec_tbl <- PREP_SUMMARY$covariate_specs %||% list()
  cov_spec_tbl <- cov_spec_tbl[vapply(cov_spec_tbl, function(x) isTRUE(x$is_active), logical(1))]
  cov_profile_tbl <- setNames(rep(0, length(active_cov_tbl)), active_cov_tbl)
  if (length(cov_spec_tbl) > 0 && is.data.frame(BASELINE_WIDE) && nrow(BASELINE_WIDE) > 0) {
    for (sp in cov_spec_tbl) {
      pred_nm <- toupper(as.character(sp$name %||% ""))
      src_nm <- as.character(sp$source_var %||% "")
      pred_type <- tolower(as.character(sp$predictor_type %||% ""))
      if (!nzchar(pred_nm) || !(pred_nm %in% active_cov_tbl)) next
      if (identical(pred_type, "continuous") && nzchar(src_nm) && src_nm %in% names(BASELINE_WIDE)) {
        xv <- suppressWarnings(as.numeric(BASELINE_WIDE[[src_nm]]))
        xv <- xv[!is.na(xv)]
        cov_profile_tbl[pred_nm] <- if (length(xv) > 0) mean(xv) else 0
      } else {
        cov_profile_tbl[pred_nm] <- 0
      }
    }
  }

  if (is.data.frame(model_results_all_tbl) && nrow(model_results_all_tbl) > 0 &&
      is.data.frame(intercepts_tbl) && nrow(intercepts_tbl) > 0 &&
      length(transition_specs_tbl) > 0 && length(state_values_tbl) > 0 && !is.na(ref_state_tbl)) {
    nonref_states_tbl <- state_values_tbl[state_values_tbl != ref_state_tbl]
    out_rows_tbl <- list()
    out_idx_tbl <- 1L

    for (spec in transition_specs_tbl) {
      from_wave_i <- as.character(spec$from_wave %||% "")
      to_wave_i <- as.character(spec$to_wave %||% "")
      interval_i <- paste0(toupper(from_wave_i), "_to_", toupper(to_wave_i))
      dummy_vars_i <- toupper(as.character(spec$dummy_vars %||% character(0)))
      dummy_states_i <- suppressWarnings(as.integer(spec$dummy_states %||% integer(0)))

      for (fs in state_values_tbl) {
        eta_map <- setNames(rep(NA_real_, length(nonref_states_tbl)), as.character(nonref_states_tbl))
        for (ts in nonref_states_tbl) {
          outcome_i <- paste0(toupper(to_wave_i), "#", ts)
          eta_i <- intercepts_tbl$estimate[intercepts_tbl$outcome == outcome_i]
          if (length(eta_i) == 0 || is.na(eta_i[1])) next
          eta_val <- eta_i[1]

          if (length(dummy_vars_i) > 0 && length(dummy_states_i) > 0) {
            hit_dummy <- which(dummy_states_i == fs)
            if (length(hit_dummy) > 0) {
              pred_dummy <- dummy_vars_i[hit_dummy[1]]
              beta_dummy <- model_results_all_tbl$estimate[
                model_results_all_tbl$outcome == outcome_i &
                  model_results_all_tbl$predictor == pred_dummy
              ]
              if (length(beta_dummy) > 0 && !is.na(beta_dummy[1])) eta_val <- eta_val + beta_dummy[1]
            }
          }

          if (length(active_cov_tbl) > 0 && is.data.frame(ESTIMATION_COVARIATES) && nrow(ESTIMATION_COVARIATES) > 0) {
            cov_hits <- ESTIMATION_COVARIATES[
              as.character(ESTIMATION_COVARIATES$interval) == interval_i &
                suppressWarnings(as.integer(ESTIMATION_COVARIATES$to_state)) == ts &
                toupper(as.character(ESTIMATION_COVARIATES$predictor)) %in% active_cov_tbl,
              ,
              drop = FALSE
            ]
            if (nrow(cov_hits) > 0) {
              for (kk in seq_len(nrow(cov_hits))) {
                pred_cov <- toupper(as.character(cov_hits$predictor[kk]))
                x_cov <- cov_profile_tbl[pred_cov]
                if (length(x_cov) == 0 || is.na(x_cov)) x_cov <- 0
                beta_cov <- suppressWarnings(as.numeric(cov_hits$estimate[kk]))
                if (!is.na(beta_cov)) eta_val <- eta_val + beta_cov * x_cov
              }
            }
          }

          eta_map[as.character(ts)] <- eta_val
        }

        eta_vec <- as.numeric(eta_map)
        names(eta_vec) <- names(eta_map)
        exp_eta <- exp(eta_vec - max(c(0, eta_vec), na.rm = TRUE))
        denom <- 1 + sum(exp_eta, na.rm = TRUE)

        for (ts in state_values_tbl) {
          prob_ts <- if (ts == ref_state_tbl) {
            1 / denom
          } else {
            exp_eta[as.character(ts)] / denom
          }
          out_rows_tbl[[out_idx_tbl]] <- data.frame(
            interval = interval_i,
            from_state = fs,
            to_state = ts,
            prob = prob_ts,
            stringsAsFactors = FALSE
          )
          out_idx_tbl <- out_idx_tbl + 1L
        }
      }
    }

    out_rows_tbl <- out_rows_tbl[!vapply(out_rows_tbl, is.null, logical(1))]
    if (length(out_rows_tbl) > 0) {
      t7_long_tbl <- do.call(rbind, out_rows_tbl)
      all_intervals_est <- if (length(interval_levels) > 0) {
        interval_levels[interval_levels %in% unique(as.character(t7_long_tbl$interval))]
      } else {
        unique(as.character(t7_long_tbl$interval))
      }
      T7 <- do.call(rbind, lapply(all_intervals_est, function(iv) {
        df_iv <- t7_long_tbl[t7_long_tbl$interval == iv, , drop = FALSE]
        out <- build_interval_matrix(df_iv, value_col = "prob", label_map = STATE_LABEL_MAP)
        if (nrow(out) == 0) return(out)
        out$Interval <- iv
        out[, c("Interval", names(out)[names(out) != "Interval"]), drop = FALSE]
      }))
      rownames(T7) <- NULL
    }
  }
}

if (is.data.frame(T7) && nrow(T7) > 0 &&
    is.data.frame(TRANSITION_DATA) && nrow(TRANSITION_DATA) > 0) {
  observed_from_tbl <- unique(data.frame(
    Interval = as.character(TRANSITION_DATA$interval),
    from_state = suppressWarnings(as.integer(as.character(TRANSITION_DATA$from_state))),
    stringsAsFactors = FALSE
  ))
  observed_from_tbl$From_State <- state_label(observed_from_tbl$from_state, STATE_LABEL_MAP)
  observed_keys_tbl <- unique(paste(observed_from_tbl$Interval, observed_from_tbl$From_State, sep = "\r"))
  t7_keys_tbl <- paste(as.character(T7$Interval), as.character(T7$From_State), sep = "\r")
  keep_row_tbl <- t7_keys_tbl %in% observed_keys_tbl
  value_cols_tbl <- setdiff(names(T7), c("Interval", "From_State"))
  if (length(value_cols_tbl) > 0) {
    for (nm in value_cols_tbl) T7[[nm]][!keep_row_tbl] <- NA_real_
  }
}

if (is.data.frame(ESTIMATION_COVARIATES) && nrow(ESTIMATION_COVARIATES) > 0) {
  T6_src <- ESTIMATION_COVARIATES
  T6_src$Comparison <- paste0(state_label(T6_src$to_state, STATE_LABEL_MAP), " vs ", state_label(T6_src$reference_state, STATE_LABEL_MAP))
  if (!"source_var" %in% names(T6_src)) T6_src$source_var <- T6_src$predictor
  if (!"source_label" %in% names(T6_src)) T6_src$source_label <- T6_src$predictor
  if (!"predictor_type" %in% names(T6_src)) T6_src$predictor_type <- "continuous"
  if (!"value_label" %in% names(T6_src)) T6_src$value_label <- NA_character_
  if (!"reference_label" %in% names(T6_src)) T6_src$reference_label <- NA_character_

  T6 <- data.frame()
  A6_1 <- data.frame()
  A6_2 <- data.frame()

  add_reference_rrr_row <- function(dat, interval, comparison, covariate, category) {
    rbind(
      dat,
      data.frame(
        Interval = interval,
        Comparison = comparison,
        Covariate = covariate,
        Category = category,
        RRR = "1.00",
        LLCI = "",
        ULCI = "",
        p = "",
        sig = "",
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    )
  }

  add_reference_b_row <- function(dat, interval, comparison, covariate, category) {
    rbind(
      dat,
      data.frame(
        Interval = interval,
        Comparison = comparison,
        Covariate = covariate,
        Category = category,
        `B (SE)` = "reference",
        Statistic = "",
        p = "",
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    )
  }

  add_reference_rrr_ci_row <- function(dat, interval, comparison, covariate, category) {
    rbind(
      dat,
      data.frame(
        Interval = interval,
        Comparison = comparison,
        Covariate = covariate,
        Category = category,
        RRR = "1.00",
        `95% CI` = "",
        p = "",
        sig = "",
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    )
  }

  split_keys <- unique(T6_src[, c("interval", "Comparison", "source_var"), drop = FALSE])
  for (ii in seq_len(nrow(split_keys))) {
    key_i <- split_keys[ii, , drop = FALSE]
    df_i <- T6_src[
      T6_src$interval == key_i$interval &
        T6_src$Comparison == key_i$Comparison &
        T6_src$source_var == key_i$source_var,
      ,
      drop = FALSE
    ]
    if (nrow(df_i) == 0) next
    pred_type_i <- as.character(df_i$predictor_type[1] %||% "continuous")
    source_var_i <- as.character(df_i$source_var[1] %||% key_i$source_var[1] %||% "")
    cov_label_i <- as.character(df_i$source_label[1] %||% df_i$source_var[1])
    ref_label_i <- as.character(df_i$reference_label[1] %||% "")
    if (identical(pred_type_i, "categorical")) {
      ref_level_i <- as.character(df_i$reference_level[1] %||% "")
      ordered_levels_i <- ordered_covariate_levels_tbl(
        source_var_i,
        observed_levels = unique(c(as.character(df_i$level), ref_level_i))
      )
      for (lev_i in ordered_levels_i) {
        cat_i <- resolve_covariate_level_label_tbl(source_var_i, lev_i)
        if (!is.na(ref_level_i) && nzchar(ref_level_i) && identical(as.character(lev_i), ref_level_i)) {
          T6 <- add_reference_rrr_row(T6, key_i$interval, key_i$Comparison, cov_label_i, cat_i)
          A6_1 <- add_reference_b_row(A6_1, key_i$interval, key_i$Comparison, cov_label_i, cat_i)
          A6_2 <- add_reference_rrr_ci_row(A6_2, key_i$interval, key_i$Comparison, cov_label_i, cat_i)
        }
        df_lev <- df_i[as.character(df_i$level) == as.character(lev_i), , drop = FALSE]
        if (nrow(df_lev) == 0) next
        for (jj in seq_len(nrow(df_lev))) {
          row_j <- df_lev[jj, , drop = FALSE]
          T6 <- rbind(
            T6,
            data.frame(
              Interval = row_j$interval,
              Comparison = row_j$Comparison,
              Covariate = cov_label_i,
              Category = cat_i,
              RRR = fmt_est2(row_j$odds_ratio),
              LLCI = fmt_est2(row_j$or_lcl),
              ULCI = fmt_est2(row_j$or_ucl),
              p = fmt_pval(row_j$p),
              sig = fmt_sig(row_j$p),
              stringsAsFactors = FALSE,
              check.names = FALSE
            )
          )
          A6_1 <- rbind(
            A6_1,
            data.frame(
              Interval = row_j$interval,
              Comparison = row_j$Comparison,
              Covariate = cov_label_i,
              Category = cat_i,
              `B (SE)` = paste0(fmt_est2(row_j$estimate), " (", fmt_est2(row_j$se), ")"),
              Statistic = fmt_stat3(row_j$z),
              p = fmt_pval(row_j$p),
              stringsAsFactors = FALSE,
              check.names = FALSE
            )
          )
          A6_2 <- rbind(
            A6_2,
            data.frame(
              Interval = row_j$interval,
              Comparison = row_j$Comparison,
              Covariate = cov_label_i,
              Category = cat_i,
              RRR = fmt_est2(row_j$odds_ratio),
              `95% CI` = ifelse(
                is.na(row_j$or_lcl) | is.na(row_j$or_ucl),
                "",
                paste0(fmt_est2(row_j$or_lcl), "~", fmt_est2(row_j$or_ucl))
              ),
              p = fmt_pval(row_j$p),
              sig = fmt_sig(row_j$p),
              stringsAsFactors = FALSE,
              check.names = FALSE
            )
          )
        }
      }
    } else {
      for (jj in seq_len(nrow(df_i))) {
        row_j <- df_i[jj, , drop = FALSE]
        T6 <- rbind(
          T6,
          data.frame(
            Interval = row_j$interval,
            Comparison = row_j$Comparison,
            Covariate = cov_label_i,
            Category = "",
            RRR = fmt_est2(row_j$odds_ratio),
            LLCI = fmt_est2(row_j$or_lcl),
            ULCI = fmt_est2(row_j$or_ucl),
            p = fmt_pval(row_j$p),
            sig = fmt_sig(row_j$p),
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
        )
        A6_1 <- rbind(
          A6_1,
          data.frame(
            Interval = row_j$interval,
            Comparison = row_j$Comparison,
            Covariate = cov_label_i,
            Category = "",
            `B (SE)` = paste0(fmt_est2(row_j$estimate), " (", fmt_est2(row_j$se), ")"),
            Statistic = fmt_stat3(row_j$z),
            p = fmt_pval(row_j$p),
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
        )
        A6_2 <- rbind(
          A6_2,
          data.frame(
            Interval = row_j$interval,
            Comparison = row_j$Comparison,
            Covariate = cov_label_i,
            Category = "",
            RRR = fmt_est2(row_j$odds_ratio),
            `95% CI` = ifelse(
              is.na(row_j$or_lcl) | is.na(row_j$or_ucl),
              "",
              paste0(fmt_est2(row_j$or_lcl), "~", fmt_est2(row_j$or_ucl))
            ),
            p = fmt_pval(row_j$p),
            sig = fmt_sig(row_j$p),
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
        )
      }
    }
  }
  rownames(T6) <- NULL
  rownames(A6_1) <- NULL
  rownames(A6_2) <- NULL
  cov_ord_t6 <- unname(covariate_order_map[toupper(T6$Covariate)])
  cov_ord_a61 <- unname(covariate_order_map[toupper(A6_1$Covariate)])
  cov_ord_a62 <- unname(covariate_order_map[toupper(A6_2$Covariate)])
  cov_ord_t6[is.na(cov_ord_t6)] <- 999999
  cov_ord_a61[is.na(cov_ord_a61)] <- 999999
  cov_ord_a62[is.na(cov_ord_a62)] <- 999999
  if (length(interval_levels) > 0) {
    T6 <- T6[order(match(T6$Interval, interval_levels), T6$Comparison, cov_ord_t6, T6$Covariate), , drop = FALSE]
    A6_1 <- A6_1[order(match(A6_1$Interval, interval_levels), A6_1$Comparison, cov_ord_a61, A6_1$Covariate), , drop = FALSE]
    A6_2 <- A6_2[order(match(A6_2$Interval, interval_levels), A6_2$Comparison, cov_ord_a62, A6_2$Covariate), , drop = FALSE]
  }
  T6 <- collapse_interval_block_labels(T6, group_cols = "Comparison", subgroup_cols = "Covariate")
  A6_1 <- collapse_interval_block_labels(A6_1, group_cols = "Comparison", subgroup_cols = "Covariate")
  A6_2 <- collapse_interval_block_labels(A6_2, group_cols = "Comparison", subgroup_cols = "Covariate")
  if ("B (SE)" %in% names(A6_1)) {
    bse_raw <- as.character(A6_1[["B (SE)"]])
    bse_raw[is.na(bse_raw)] <- ""
    b_vals <- ifelse(grepl("^reference$", trimws(bse_raw), ignore.case = TRUE), "reference", sub("^\\s*([^ ]+)\\s*\\(.*$", "\\1", bse_raw))
    se_vals <- ifelse(grepl("^reference$", trimws(bse_raw), ignore.case = TRUE), "", sub("^.*\\(([^()]*)\\)\\s*$", "\\1", bse_raw))
    keep_cols <- setdiff(names(A6_1), "B (SE)")
    insert_at <- match("Category", keep_cols)
    if (is.na(insert_at)) insert_at <- length(keep_cols)
    A6_1$B <- b_vals
    A6_1$SE <- se_vals
    ordered_cols <- append(keep_cols, c("B", "SE"), after = insert_at)
    A6_1 <- A6_1[, ordered_cols, drop = FALSE]
  }
}

build_covariate_effect_table <- function(src, overall = FALSE) {
  src <- safe_df(src)
  if (nrow(src) == 0) return(data.frame())
  src$Comparison <- paste0(state_label(src$to_state, STATE_LABEL_MAP), " vs ", state_label(src$reference_state, STATE_LABEL_MAP))
  if (!"source_label" %in% names(src)) src$source_label <- src$predictor
  if (!"source_var" %in% names(src)) src$source_var <- src$predictor
  if (!"predictor_type" %in% names(src)) src$predictor_type <- "continuous"
  if (!"value_label" %in% names(src)) src$value_label <- NA_character_
  if (!"reference_label" %in% names(src)) src$reference_label <- NA_character_
  if (!"reference_level" %in% names(src)) src$reference_level <- NA_character_
  src$Category <- ifelse(
    src$predictor_type == "categorical",
    as.character(src$value_label %||% src$level),
    ""
  )
  out <- data.frame(
    Interval = if (overall) "Overall W1-W4" else as.character(src$interval),
    Comparison = src$Comparison,
    Covariate = as.character(src$source_label),
    Category = src$Category,
    RRR = fmt_est2(src$odds_ratio),
    LLCI = fmt_est2(src$or_lcl),
    ULCI = fmt_est2(src$or_ucl),
    p = fmt_pval(src$p),
    sig = fmt_sig(src$p),
    B = fmt_est2(src$estimate),
    SE = fmt_est2(src$se),
    Statistic = fmt_stat3(src$z),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  out$.source_var <- as.character(src$source_var)
  out$.predictor_type <- as.character(src$predictor_type)
  out$.is_ref <- 0L
  out$.reference_label <- as.character(src$reference_label %||% NA_character_)
  out$.reference_level <- as.character(src$reference_level %||% NA_character_)
  out$.is_ref <- 0L
  ref_rows <- list()
  ref_idx <- 1L
  cat_src <- src[tolower(as.character(src$predictor_type)) == "categorical", , drop = FALSE]
  if (nrow(cat_src) > 0) {
    split_keys <- unique(cat_src[, intersect(c("interval", "Comparison", "source_var"), names(cat_src)), drop = FALSE])
    for (ii in seq_len(nrow(split_keys))) {
      key_i <- split_keys[ii, , drop = FALSE]
      hit <- cat_src[
        as.character(cat_src$interval) == as.character(key_i$interval) &
          as.character(cat_src$Comparison) == as.character(key_i$Comparison) &
          as.character(cat_src$source_var) == as.character(key_i$source_var),
        ,
        drop = FALSE
      ]
      if (nrow(hit) == 0) next
      ref_label <- as.character(hit$reference_label[1] %||% "")
      ref_level <- as.character(hit$reference_level[1] %||% "")
      if (!nzchar(ref_label) && nzchar(ref_level)) ref_label <- resolve_covariate_level_label_tbl(as.character(hit$source_var[1]), ref_level)
      if (!nzchar(ref_label)) next
      ref_rows[[ref_idx]] <- data.frame(
        Interval = if (overall) "Overall W1-W4" else as.character(hit$interval[1]),
        Comparison = as.character(hit$Comparison[1]),
        Covariate = as.character(hit$source_label[1]),
        Category = ref_label,
        RRR = "1.00",
        LLCI = "",
        ULCI = "",
        p = "",
        sig = "",
        B = "reference",
        SE = "",
        Statistic = "",
        .source_var = as.character(hit$source_var[1]),
        .predictor_type = "categorical",
        .reference_label = ref_label,
        .reference_level = ref_level,
        .is_ref = -1L,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      ref_idx <- ref_idx + 1L
    }
  }
  if (length(ref_rows) > 0) {
    ref_df <- do.call(rbind, ref_rows)
    all_cols <- unique(c(names(out), names(ref_df)))
    for (nm in setdiff(all_cols, names(out))) out[[nm]] <- ""
    for (nm in setdiff(all_cols, names(ref_df))) ref_df[[nm]] <- ""
    out <- rbind(ref_df[, all_cols, drop = FALSE], out[, all_cols, drop = FALSE])
  }
  out$.is_ref <- ifelse(tolower(trimws(as.character(out$B))) == "reference", -1L, 0L)
  cov_ord <- unname(covariate_order_map[toupper(out$Covariate)])
  cov_ord[is.na(cov_ord)] <- 999999
  if (overall) {
    out <- out[order(out$Comparison, cov_ord, out$Covariate, out$.is_ref, out$Category), , drop = FALSE]
  } else if (length(interval_levels) > 0) {
    out <- out[order(match(out$Interval, interval_levels), out$Comparison, cov_ord, out$Covariate, out$.is_ref, out$Category), , drop = FALSE]
  } else {
    out <- out[order(out$Interval, out$Comparison, cov_ord, out$Covariate, out$.is_ref, out$Category), , drop = FALSE]
  }
  out <- out[, !grepl("^\\.", names(out)), drop = FALSE]
  collapse_interval_block_labels(out, group_cols = "Comparison", subgroup_cols = "Covariate")
}

T6_1 <- build_covariate_effect_table(ESTIMATION_COVARIATES_UNIVARIABLE, overall = FALSE)
T6_2 <- build_covariate_effect_table(ESTIMATION_COVARIATES_SELECTED, overall = FALSE)
T8_1 <- build_covariate_effect_table(ESTIMATION_COVARIATES_GLOBAL_UNIVARIABLE, overall = TRUE)
T8_2 <- build_covariate_effect_table(ESTIMATION_COVARIATES_GLOBAL_SELECTED, overall = TRUE)

build_bmi_mediation_stage_table <- function(src, overall = FALSE) {
  src <- safe_df(src)
  if (nrow(src) == 0) return(data.frame())
  if (!"reference_label" %in% names(src)) src$reference_label <- NA_character_
  if (!"reference_level" %in% names(src)) src$reference_level <- NA_character_
  if (!"mediator_type" %in% names(src)) src$mediator_type <- "continuous"
  if (!"mediator_label" %in% names(src)) src$mediator_label <- "BMI change"
  if (!"mediator_reference_label" %in% names(src)) src$mediator_reference_label <- NA_character_
  is_cat_mediator <- any(tolower(as.character(src$mediator_type)) == "categorical")
  if (is_cat_mediator) {
    src$Comparison <- paste0(as.character(src$mediator_label), " vs ", as.character(src$mediator_reference_label))
    out <- data.frame(
      Interval = if (overall) "Overall W1-W4" else as.character(src$interval),
      Comparison = src$Comparison,
      Covariate = as.character(src$source_label),
      Category = ifelse(tolower(as.character(src$predictor_type)) == "categorical", as.character(src$value_label), ""),
      RRR = fmt_est2(src$odds_ratio),
      LLCI = fmt_est2(src$or_lcl),
      ULCI = fmt_est2(src$or_ucl),
      p = fmt_pval(src$p),
      sig = fmt_sig(src$p),
      B = fmt_est2(src$estimate),
      SE = fmt_est2(src$se),
      Statistic = fmt_stat3(src$z),
      n = fmt_n(src$n),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    out$.source_var <- as.character(src$source_var)
    out$.predictor_type <- as.character(src$predictor_type)
    ref_rows <- list()
    ref_idx <- 1L
    cat_src <- src[tolower(as.character(src$predictor_type)) == "categorical", , drop = FALSE]
    if (nrow(cat_src) > 0) {
      split_cols <- intersect(c("interval", "Comparison", "source_var"), names(cat_src))
      split_keys <- unique(cat_src[, split_cols, drop = FALSE])
      for (ii in seq_len(nrow(split_keys))) {
        hit <- cat_src[as.character(cat_src$source_var) == as.character(split_keys$source_var[ii]) &
                         as.character(cat_src$Comparison) == as.character(split_keys$Comparison[ii]), , drop = FALSE]
        if (!overall && "interval" %in% names(split_keys)) {
          hit <- hit[as.character(hit$interval) == as.character(split_keys$interval[ii]), , drop = FALSE]
        }
        if (nrow(hit) == 0) next
        ref_label <- as.character(hit$reference_label[1] %||% "")
        ref_level <- as.character(hit$reference_level[1] %||% "")
        if (!nzchar(ref_label) && nzchar(ref_level)) ref_label <- resolve_covariate_level_label_tbl(as.character(hit$source_var[1]), ref_level)
        if (!nzchar(ref_label)) next
        ref_rows[[ref_idx]] <- data.frame(
          Interval = if (overall) "Overall W1-W4" else as.character(hit$interval[1]),
          Comparison = as.character(hit$Comparison[1]),
          Covariate = as.character(hit$source_label[1]),
          Category = ref_label,
          RRR = "1.00",
          LLCI = "",
          ULCI = "",
          p = "",
          sig = "",
          B = "reference",
          SE = "",
          Statistic = "",
          n = fmt_n(hit$n[1]),
          .source_var = as.character(hit$source_var[1]),
          .predictor_type = "categorical",
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        ref_idx <- ref_idx + 1L
      }
    }
    if (length(ref_rows) > 0) {
      ref_df <- do.call(rbind, ref_rows)
      all_cols <- unique(c(names(out), names(ref_df)))
      for (nm in setdiff(all_cols, names(out))) out[[nm]] <- ""
      for (nm in setdiff(all_cols, names(ref_df))) ref_df[[nm]] <- ""
      out <- rbind(ref_df[, all_cols, drop = FALSE], out[, all_cols, drop = FALSE])
    }
    out$.is_ref <- ifelse(tolower(trimws(as.character(out$B))) == "reference", -1L, 0L)
    cov_ord <- unname(covariate_order_map[toupper(out$Covariate)])
    cov_ord[is.na(cov_ord)] <- 999999
    if (overall) {
      out <- out[order(out$Comparison, cov_ord, out$Covariate, out$.is_ref, out$Category), , drop = FALSE]
    } else {
      out <- out[order(match(out$Interval, interval_levels), out$Comparison, cov_ord, out$Covariate, out$.is_ref, out$Category), , drop = FALSE]
    }
    out <- out[, !grepl("^\\.", names(out)), drop = FALSE]
    return(collapse_interval_block_labels(out, group_cols = "Comparison", subgroup_cols = "Covariate"))
  }
  out <- data.frame(
    Interval = if (overall) "Overall W1-W4" else as.character(src$interval),
    Mediator = "BMI change",
    Covariate = as.character(src$source_label),
    Category = ifelse(tolower(as.character(src$predictor_type)) == "categorical", as.character(src$value_label), ""),
    B = fmt_est2(src$estimate),
    SE = fmt_est2(src$se),
    Statistic = fmt_stat3(src$z),
    p = fmt_pval(src$p),
    sig = fmt_sig(src$p),
    n = fmt_n(src$n),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  out$.source_var <- as.character(src$source_var)
  out$.predictor_type <- as.character(src$predictor_type)
  ref_rows <- list()
  ref_idx <- 1L
  cat_src <- src[tolower(as.character(src$predictor_type)) == "categorical", , drop = FALSE]
  if (nrow(cat_src) > 0) {
    split_cols <- intersect(c("interval", "source_var"), names(cat_src))
    split_keys <- unique(cat_src[, split_cols, drop = FALSE])
    for (ii in seq_len(nrow(split_keys))) {
      hit <- cat_src[as.character(cat_src$source_var) == as.character(split_keys$source_var[ii]), , drop = FALSE]
      if (!overall && "interval" %in% names(split_keys)) {
        hit <- hit[as.character(hit$interval) == as.character(split_keys$interval[ii]), , drop = FALSE]
      }
      if (nrow(hit) == 0) next
      ref_label <- as.character(hit$reference_label[1] %||% "")
      ref_level <- as.character(hit$reference_level[1] %||% "")
      if (!nzchar(ref_label) && nzchar(ref_level)) ref_label <- resolve_covariate_level_label_tbl(as.character(hit$source_var[1]), ref_level)
      if (!nzchar(ref_label)) next
      ref_rows[[ref_idx]] <- data.frame(
        Interval = if (overall) "Overall W1-W4" else as.character(hit$interval[1]),
        Mediator = "BMI change",
        Covariate = as.character(hit$source_label[1]),
        Category = ref_label,
        B = "reference",
        SE = "",
        Statistic = "",
        p = "",
        sig = "",
        n = fmt_n(hit$n[1]),
        .source_var = as.character(hit$source_var[1]),
        .predictor_type = "categorical",
        .is_ref = -1L,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      ref_idx <- ref_idx + 1L
    }
  }
  if (length(ref_rows) > 0) {
    ref_df <- do.call(rbind, ref_rows)
    all_cols <- unique(c(names(out), names(ref_df)))
    for (nm in setdiff(all_cols, names(out))) out[[nm]] <- ""
    for (nm in setdiff(all_cols, names(ref_df))) ref_df[[nm]] <- ""
    out <- rbind(ref_df[, all_cols, drop = FALSE], out[, all_cols, drop = FALSE])
  }
  out$.is_ref <- ifelse(tolower(trimws(as.character(out$B))) == "reference", -1L, 0L)
  cov_ord <- unname(covariate_order_map[toupper(out$Covariate)])
  cov_ord[is.na(cov_ord)] <- 999999
  if (overall) {
    out <- out[order(cov_ord, out$Covariate, out$.is_ref, out$Category), , drop = FALSE]
  } else {
    out <- out[order(match(out$Interval, interval_levels), cov_ord, out$Covariate, out$.is_ref, out$Category), , drop = FALSE]
  }
  out <- out[, !grepl("^\\.", names(out)), drop = FALSE]
  collapse_interval_block_labels(out, group_cols = "Mediator", subgroup_cols = "Covariate")
}

M1 <- build_bmi_mediation_stage_table(BMI_MEDIATION_STAGE1, overall = FALSE)

M2 <- build_covariate_effect_table(BMI_MEDIATION_STAGE2, overall = FALSE)
M3 <- build_bmi_mediation_stage_table(BMI_MEDIATION_STAGE3, overall = TRUE)

if (is.data.frame(ESTIMATION_COVARIATES_GLOBAL) && nrow(ESTIMATION_COVARIATES_GLOBAL) > 0) {
  T8_src <- safe_df(ESTIMATION_COVARIATES_GLOBAL)
  T8_src$Comparison <- paste0(state_label(T8_src$to_state, STATE_LABEL_MAP), " vs ", state_label(T8_src$reference_state, STATE_LABEL_MAP))
  if (!"source_label" %in% names(T8_src)) T8_src$source_label <- T8_src$predictor
  if (!"source_var" %in% names(T8_src)) T8_src$source_var <- T8_src$predictor
  if (!"predictor_type" %in% names(T8_src)) T8_src$predictor_type <- "continuous"
  if (!"value_label" %in% names(T8_src)) T8_src$value_label <- NA_character_
  T8_src$Category <- ifelse(
    T8_src$predictor_type == "categorical",
    as.character(T8_src$value_label %||% T8_src$level),
    ""
  )
  T8 <- data.frame(
    Interval = "Overall W1-W4",
    Comparison = T8_src$Comparison,
    Covariate = as.character(T8_src$source_label),
    Category = T8_src$Category,
    RRR = fmt_est2(T8_src$odds_ratio),
    LLCI = fmt_est2(T8_src$or_lcl),
    ULCI = fmt_est2(T8_src$or_ucl),
    p = fmt_pval(T8_src$p),
    sig = fmt_sig(T8_src$p),
    B = fmt_est2(T8_src$estimate),
    SE = fmt_est2(T8_src$se),
    Statistic = fmt_stat3(T8_src$z),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  cov_ord_t8 <- unname(covariate_order_map[toupper(T8$Covariate)])
  cov_ord_t8[is.na(cov_ord_t8)] <- 999999
  T8 <- T8[order(T8$Comparison, cov_ord_t8, T8$Covariate, T8$Category), , drop = FALSE]
  T8 <- collapse_interval_block_labels(T8, group_cols = "Comparison", subgroup_cols = "Covariate")
}
T8 <- build_covariate_effect_table(ESTIMATION_COVARIATES_GLOBAL, overall = TRUE)

if (is.data.frame(T4) && nrow(T4) > 0) {
  num_cols <- setdiff(names(T4), c("Interval", "From_State"))
  for (nm in num_cols) T4[[nm]] <- fmt_est2(T4[[nm]])
}

if (is.data.frame(T7) && nrow(T7) > 0) {
  num_cols <- setdiff(names(T7), c("Interval", "From_State"))
  for (nm in num_cols) T7[[nm]] <- fmt_est2(T7[[nm]])
}

display_table_names <- c("T3", "A3", "T4", "T5", "T6", "T6_1", "T6_2", "T7", "T8", "T8_1", "T8_2", "M1", "M2", "M3", "A6_1", "A6_2", "S1", "S2")
for (nm_tbl in display_table_names) {
  if (exists(nm_tbl, inherits = FALSE)) {
    obj_tbl <- get(nm_tbl, inherits = FALSE)
    if (is.data.frame(obj_tbl) && nrow(obj_tbl) > 0 && "Interval" %in% names(obj_tbl)) {
      obj_tbl$Interval <- pretty_interval_label_tbl(obj_tbl$Interval)
      assign(nm_tbl, obj_tbl, inherits = FALSE)
    }
  }
}

if (is.data.frame(S1) && nrow(S1) > 0) {
  num_cols <- setdiff(names(S1), c("Interval", "From_State"))
  for (nm in num_cols) S1[[nm]] <- fmt_n(S1[[nm]])
}

TABLE_REGISTRY <- list(
  T1 = T1,
  T2 = T2,
  T3 = T3,
  T4 = T4,
  T5 = T5,
  T6 = T6,
  T6_1 = T6_1,
  T6_2 = T6_2,
  T7 = T7,
  T8 = T8,
  T8_1 = T8_1,
  T8_2 = T8_2,
  M1 = M1,
  M2 = M2,
  M3 = M3,
  A1 = A1,
  A2 = A2,
  A3 = A3,
  A6_1 = A6_1,
  A6_2 = A6_2,
  S1 = S1,
  S2 = S2,
  S3 = S3
)

TABLE_META <- list(
  T1 = list(caption = "T1. Baseline sample characteristics"),
  T2 = list(caption = "T2. State prevalence by wave"),
  T3 = list(caption = "T3. Observed transition matrix by interval"),
  T4 = list(caption = "T4. Observed transition probability matrices (wide)"),
  T5 = list(caption = "T5. Stability versus change summary"),
  T6 = list(caption = "T6. Baseline covariate effects in adjusted transition model (RRR scale)"),
  T6_1 = list(caption = "T6-1. Univariable covariate effects by interval (RRR scale)"),
  T6_2 = list(caption = "T6-2. Multivariable covariate effects by interval among univariable p < .05 predictors (RRR scale)"),
  T7 = list(caption = "T7. Model-estimated transition probability matrices (reference covariate profile)"),
  T8 = list(caption = "T8. Overall W1-W4 covariate effects with interval-invariant coefficients (RRR scale)"),
  T8_1 = list(caption = "T8-1. Univariable overall W1-W4 covariate effects with interval-invariant coefficients (RRR scale)"),
  T8_2 = list(caption = "T8-2. Multivariable overall W1-W4 covariate effects among univariable p < .05 predictors (RRR scale)"),
  M1 = list(caption = "M1. Stage 1 mediation model: baseline covariates predicting BMI change"),
  M2 = list(caption = "M2. Stage 2 mediation model: covariates and BMI change predicting dementia transition (RRR scale)"),
  M3 = list(caption = "M3. Overall W1-W4 mediation stage 1 model: baseline covariates predicting BMI change"),
  A1 = list(caption = "A1. Baseline sample characteristics, combined summary"),
  A2 = list(caption = "A2. State prevalence by wave, n (%)"),
  A3 = list(caption = "A3. Observed transition matrix by interval, n (%)"),
  A6_1 = list(caption = "A6-1. Baseline covariate effects, coefficient scale"),
  A6_2 = list(caption = "A6-2. Baseline covariate effects, RRR scale"),
  S1 = list(caption = "S1. Transition count matrices (wide)"),
  S2 = list(caption = "S2. Pair-level transition counts"),
  S3 = list(caption = "S3. Sample and model overview")
)

TABLE_INDEX <- data.frame(
  table_name = names(TABLE_REGISTRY),
  nrow = vapply(TABLE_REGISTRY, nrow, integer(1)),
  ncol = vapply(TABLE_REGISTRY, ncol, integer(1)),
  caption = vapply(TABLE_META, function(x) x$caption %||% "", character(1)),
  stringsAsFactors = FALSE
)

for (nm in names(TABLE_REGISTRY)) {
  write_csv_safe(TABLE_REGISTRY[[nm]], file.path(DIR_TABLES, paste0(nm, ".csv")))
}

write_csv_safe(TABLE_INDEX, file.path(DIR_TABLES, "TABLE_INDEX.csv"))
write_csv_safe(ESTIMATION_REGISTRY, file.path(DIR_TABLES, "estimation_registry.csv"))

TABLE_MANIFEST <- data.frame(
  table_name = c(names(TABLE_REGISTRY), "TABLE_INDEX", "estimation_registry"),
  file_path = c(
    file.path(DIR_TABLES, paste0(names(TABLE_REGISTRY), ".csv")),
    file.path(DIR_TABLES, "TABLE_INDEX.csv"),
    file.path(DIR_TABLES, "estimation_registry.csv")
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(TABLE_MANIFEST, file.path(DIR_TABLES, "TABLE_MANIFEST.csv"))

wb <- openxlsx::createWorkbook()
for (nm in names(TABLE_REGISTRY)) {
  if (identical(nm, "T2")) {
    write_t2_twoline_sheet(
      wb = wb,
      sheet_name = substr(nm, 1, 31),
      dat = TABLE_REGISTRY[[nm]],
      title_text = TABLE_META[[nm]]$caption %||% nm
    )
  } else if (identical(nm, "A2")) {
    write_review_sheet(
      wb = wb,
      sheet_name = substr(nm, 1, 31),
      dat = TABLE_REGISTRY[[nm]],
      title_text = TABLE_META[[nm]]$caption %||% nm
    )
  } else if (identical(nm, "T3")) {
    write_t3_twoline_sheet(
      wb = wb,
      sheet_name = substr(nm, 1, 31),
      dat = TABLE_REGISTRY[[nm]],
      title_text = TABLE_META[[nm]]$caption %||% nm
    )
  } else if (identical(nm, "A3")) {
    write_t3_matrix_sheet(
      wb = wb,
      sheet_name = substr(nm, 1, 31),
      dat = TABLE_REGISTRY[[nm]],
      title_text = TABLE_META[[nm]]$caption %||% nm
    )
  } else if (identical(nm, "T4")) {
    write_t3_matrix_sheet(
      wb = wb,
      sheet_name = substr(nm, 1, 31),
      dat = TABLE_REGISTRY[[nm]],
      title_text = TABLE_META[[nm]]$caption %||% nm
    )
  } else if (identical(nm, "T7")) {
    write_t3_matrix_sheet(
      wb = wb,
      sheet_name = substr(nm, 1, 31),
      dat = TABLE_REGISTRY[[nm]],
      title_text = TABLE_META[[nm]]$caption %||% nm
    )
  } else if (identical(nm, "T5")) {
    write_t5_twoline_sheet(
      wb = wb,
      sheet_name = substr(nm, 1, 31),
      dat = TABLE_REGISTRY[[nm]],
      title_text = TABLE_META[[nm]]$caption %||% nm
    )
  } else if (nm %in% c("T6", "T6_1", "T6_2", "T8", "T8_1", "T8_2", "M1", "M2", "M3")) {
    write_interval_block_sheet(
      wb = wb,
      sheet_name = substr(nm, 1, 31),
      dat = TABLE_REGISTRY[[nm]],
      title_text = TABLE_META[[nm]]$caption %||% nm
    )
  } else if (identical(nm, "A6_1")) {
    write_interval_block_sheet(
      wb = wb,
      sheet_name = "A6-1",
      dat = TABLE_REGISTRY[[nm]],
      title_text = TABLE_META[[nm]]$caption %||% nm
    )
  } else if (identical(nm, "A6_2")) {
    write_interval_block_sheet(
      wb = wb,
      sheet_name = "A6-2",
      dat = TABLE_REGISTRY[[nm]],
      title_text = TABLE_META[[nm]]$caption %||% nm
    )
  } else if (identical(nm, "S1")) {
    write_t3_matrix_sheet(
      wb = wb,
      sheet_name = substr(nm, 1, 31),
      dat = TABLE_REGISTRY[[nm]],
      title_text = TABLE_META[[nm]]$caption %||% nm
    )
  } else if (identical(nm, "S2")) {
    write_interval_block_sheet(
      wb = wb,
      sheet_name = substr(nm, 1, 31),
      dat = TABLE_REGISTRY[[nm]],
      title_text = TABLE_META[[nm]]$caption %||% nm
    )
  } else {
    write_review_sheet(
      wb = wb,
      sheet_name = substr(nm, 1, 31),
      dat = TABLE_REGISTRY[[nm]],
      title_text = TABLE_META[[nm]]$caption %||% nm
    )
  }
}
write_review_sheet(wb, "TABLE_INDEX", TABLE_INDEX, "TABLE_INDEX. Table registry")

dir.create(dirname(PATH_FINAL_EXCEL), recursive = TRUE, showWarnings = FALSE)
excel_saved_path <- PATH_FINAL_EXCEL
save_ok <- tryCatch(
  {
    openxlsx::saveWorkbook(wb, PATH_FINAL_EXCEL, overwrite = TRUE)
    TRUE
  },
  warning = function(w) FALSE,
  error = function(e) FALSE
)
if (!isTRUE(save_ok)) {
  excel_saved_path <- sub("\\.xlsx$", "_updated.xlsx", PATH_FINAL_EXCEL, ignore.case = TRUE)
  openxlsx::saveWorkbook(wb, excel_saved_path, overwrite = TRUE)
  log_info("Excel file was locked; saved updated workbook as ", excel_saved_path)
}

TABLE_SUMMARY <- list(
  table_dir = DIR_TABLES,
  n_tables = length(TABLE_REGISTRY),
  excel_file = excel_saved_path,
  created_at = Sys.time()
)

save_named_rds_list(
  list(
    TABLE_REGISTRY = TABLE_REGISTRY,
    TABLE_INDEX = TABLE_INDEX,
    TABLE_MANIFEST = TABLE_MANIFEST,
    TABLE_SUMMARY = TABLE_SUMMARY
  ),
  dir_rds = DIR_RDS
)

log_info("Excel file = ", excel_saved_path)
log_info("n_tables   = ", length(TABLE_REGISTRY))
log_step_end("tables", round(as.numeric(difftime(Sys.time(), T0_TABLES, units = "secs")), 2), ok = TRUE)
