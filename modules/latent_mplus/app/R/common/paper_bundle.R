# ============================================================
# paper_bundle.R
# Paper bundle builder
# ------------------------------------------------------------
# 역할
# 1) figures/png, tiff, pdf 수집
# 2) 최종 표(csv, xlsx) 수집
# 3) manuscript / appendix용 bundle 폴더 구성
# 4) manifest.csv 생성
# ============================================================

# ------------------------------------------------------------
# 0. helpers
# ------------------------------------------------------------
ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

safe_list_files <- function(path, pattern = NULL, full.names = TRUE) {
  if (!dir.exists(path)) return(character(0))
  list.files(path, pattern = pattern, full.names = full.names)
}

copy_if_any <- function(files, to) {
  if (length(files) == 0) return(invisible(FALSE))
  ensure_dir(to)
  ok <- file.copy(files, to, overwrite = TRUE)
  invisible(ok)
}

infer_asset_group <- function(x) {
  xlow <- tolower(x)

  if (grepl("^fig", xlow)) return("main_figure")
  if (grepl("^t[0-9]", xlow)) return("main_table")
  if (grepl("^s[0-9]", xlow)) return("appendix_table")
  if (grepl("^a[0-9]", xlow)) return("appendix_asset")
  "other"
}

build_manifest_df <- function(files, type) {
  if (length(files) == 0) {
    return(data.frame(
      file = character(0),
      ext = character(0),
      type = character(0),
      group = character(0),
      stringsAsFactors = FALSE
    ))
  }

  base <- basename(files)
  data.frame(
    file = base,
    ext = tools::file_ext(base),
    type = type,
    group = vapply(tools::file_path_sans_ext(base), infer_asset_group, character(1)),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# 1. main function
# ------------------------------------------------------------
build_paper_bundle <- function(dir_output) {
  stopifnot(!missing(dir_output))

  bundle_dir <- file.path(dir_output, "paper_bundle")
  fig_dir    <- file.path(bundle_dir, "figures")
  tab_dir    <- file.path(bundle_dir, "tables")

  fig_dir_png  <- file.path(fig_dir, "png")
  fig_dir_tiff <- file.path(fig_dir, "tiff")
  fig_dir_pdf  <- file.path(fig_dir, "pdf")

  ensure_dir(bundle_dir)
  ensure_dir(fig_dir)
  ensure_dir(tab_dir)
  ensure_dir(fig_dir_png)
  ensure_dir(fig_dir_tiff)
  ensure_dir(fig_dir_pdf)

  # ----------------------------------------------------------
  # figures
  # ----------------------------------------------------------
  figs_png <- safe_list_files(
    file.path(dir_output, "figures", "png"),
    pattern = "\\.png$",
    full.names = TRUE
  )

  figs_tiff <- safe_list_files(
    file.path(dir_output, "figures", "tiff"),
    pattern = "\\.tif$|\\.tiff$",
    full.names = TRUE
  )

  figs_pdf <- safe_list_files(
    file.path(dir_output, "figures", "pdf"),
    pattern = "\\.pdf$",
    full.names = TRUE
  )

  copy_if_any(figs_png,  fig_dir_png)
  copy_if_any(figs_tiff, fig_dir_tiff)
  copy_if_any(figs_pdf,  fig_dir_pdf)

  # ----------------------------------------------------------
  # tables
  # ----------------------------------------------------------
  tab_xlsx <- safe_list_files(dir_output, pattern = "\\.xlsx$", full.names = TRUE)
  tab_csv  <- safe_list_files(dir_output, pattern = "\\.csv$",  full.names = TRUE)

  # paper_bundle 내부에서 생성되는 manifest류 중복 방지
  tab_xlsx <- tab_xlsx[dirname(tab_xlsx) == normalizePath(dir_output, winslash = "/", mustWork = FALSE)]
  tab_csv  <- tab_csv[dirname(tab_csv) == normalizePath(dir_output, winslash = "/", mustWork = FALSE)]

  copy_if_any(tab_xlsx, tab_dir)
  copy_if_any(tab_csv,  tab_dir)

  # ----------------------------------------------------------
  # manifest
  # ----------------------------------------------------------
  manifest_fig_png  <- build_manifest_df(figs_png,  "figure_png")
  manifest_fig_tiff <- build_manifest_df(figs_tiff, "figure_tiff")
  manifest_fig_pdf  <- build_manifest_df(figs_pdf,  "figure_pdf")
  manifest_tab_xlsx <- build_manifest_df(tab_xlsx,  "table_xlsx")
  manifest_tab_csv  <- build_manifest_df(tab_csv,   "table_csv")

  manifest <- dplyr::bind_rows(
    manifest_fig_png,
    manifest_fig_tiff,
    manifest_fig_pdf,
    manifest_tab_xlsx,
    manifest_tab_csv
  )

  if (nrow(manifest) > 0) {
    manifest <- manifest |>
      dplyr::mutate(
        asset_id = tools::file_path_sans_ext(file)
      ) |>
      dplyr::select(asset_id, file, ext, type, group)
  }

  utils::write.csv(
    manifest,
    file.path(bundle_dir, "manifest.csv"),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  # ----------------------------------------------------------
  # lightweight summaries
  # ----------------------------------------------------------
  summary_df <- data.frame(
    category = c("png_figures", "tiff_figures", "pdf_figures", "xlsx_tables", "csv_tables"),
    n = c(length(figs_png), length(figs_tiff), length(figs_pdf), length(tab_xlsx), length(tab_csv)),
    stringsAsFactors = FALSE
  )

  utils::write.csv(
    summary_df,
    file.path(bundle_dir, "bundle_summary.csv"),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  invisible(list(
    bundle_dir = bundle_dir,
    figures_png = figs_png,
    figures_tiff = figs_tiff,
    figures_pdf = figs_pdf,
    tables_xlsx = tab_xlsx,
    tables_csv = tab_csv,
    manifest = manifest,
    summary = summary_df
  ))
}