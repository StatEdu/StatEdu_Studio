source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")

argument_value <- function(arguments, name, default = "") {
  prefix <- paste0("--", name, "=")
  match <- arguments[startsWith(arguments, prefix)]
  if (length(match)) substring(match[[1L]], nchar(prefix) + 1L) else default
}

tam_data_path <- argument_value(commandArgs(trailingOnly = TRUE), "data", Sys.getenv("SMARTPLS_TAM_DATA", ""))
if (!nzchar(tam_data_path) && .Platform$OS.type == "windows") {
  local_candidate <- "C:/StatEdu/SmartPLS_Workspace/Example - TAM 100/Data.txt"
  if (file.exists(local_candidate)) tam_data_path <- local_candidate
}
if (!nzchar(tam_data_path) || !file.exists(tam_data_path)) {
  message("SmartPLS TAM optional validation SKIP: provide --data=PATH or SMARTPLS_TAM_DATA to the locally installed 100-row TAM data.")
  quit(save = "no", status = 0L)
}

tam_manifest_path <- file.path(
  "docs", "evidence", "release_1_2_4", "pls",
  "smartpls_4_1_1_8_tam100_supplement", "evidence_manifest.json"
)
tam_manifest <- jsonlite::fromJSON(tam_manifest_path, simplifyVector = TRUE)
expected_data <- tam_manifest$optional_local_data
stopifnot(
  basename(tam_data_path) == expected_data$file_basename,
  file.info(tam_data_path)$size == as.numeric(expected_data$bytes),
  identical(
    tolower(digest::digest(file = tam_data_path, algo = "sha256", serialize = FALSE)),
    tolower(expected_data$sha256)
  )
)

tam_data <- utils::read.table(
  tam_data_path, header = TRUE, sep = ";", check.names = FALSE,
  stringsAsFactors = FALSE
)
blocks <- list(
  PU = paste0("USEF", 1:5),
  PEOU = paste0("EOU", 1:5),
  BI = paste0("BI", 1:3),
  ATT = paste0("ATT", 1:5),
  USE = paste0("USE", 1:4)
)
indicators <- unlist(blocks, use.names = FALSE)
stopifnot(nrow(tam_data) == 100L, all(indicators %in% names(tam_data)), !anyNA(tam_data[indicators]))

latent_nodes <- Map(function(id, label) list(
  id = id, role = "latent", name = id, canvasLabel = label, x = 0, y = 0,
  measurementMode = "reflective", constructType = "commonFactor", weightingMode = "auto"
), names(blocks), c(
  "Perceived Usefulness", "Perceived Ease of Use", "Behavioral Intention to Use",
  "Attitude Toward Using", "Actual System Use"
))
indicator_nodes <- lapply(indicators, function(indicator) list(
  id = indicator, role = "indicator", name = indicator, variableId = indicator,
  canvasLabel = indicator, x = 0, y = 0
))
measurement_edges <- list()
edge_index <- 0L
for (construct in names(blocks)) {
  for (indicator in blocks[[construct]]) {
    edge_index <- edge_index + 1L
    measurement_edges[[edge_index]] <- list(
      id = paste0("m", edge_index), from = construct, to = indicator
    )
  }
}
path_pairs <- list(
  c("PEOU", "PU"), c("PU", "BI"), c("PU", "ATT"), c("PEOU", "ATT"),
  c("ATT", "BI"), c("ATT", "USE"), c("BI", "USE")
)
path_edges <- Map(function(pair, index) list(
  id = paste0("p", index), from = pair[[1L]], to = pair[[2L]], pathType = "regression"
), path_pairs, seq_along(path_pairs))
snapshot <- list(
  modelSchemaVersion = 7, analysisType = "plssem",
  nodes = c(latent_nodes, indicator_nodes), edges = c(measurement_edges, path_edges),
  moderations = list(), covariates = list()
)
