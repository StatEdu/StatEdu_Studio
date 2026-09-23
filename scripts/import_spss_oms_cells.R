all_args <- commandArgs(trailingOnly = TRUE)
value_arg <- function(prefix, default = "") {
  hit <- all_args[startsWith(all_args, prefix)]
  if (length(hit) == 0L) return(default)
  sub(prefix, "", hit[[1]], fixed = TRUE)
}

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1]]) else "scripts/import_spss_oms_cells.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
input_path <- value_arg("--input=", file.path(repo_root, "outputs", "spss_classical_validation.xml"))
output_path <- value_arg("--output=", file.path(repo_root, "sample", "spss31_analysis_cells.csv"))

if (!requireNamespace("xml2", quietly = TRUE)) stop("The xml2 package is required.")
if (!file.exists(input_path)) stop("SPSS OXML output was not found: ", input_path)

doc <- xml2::read_xml(input_path)
xml2::xml_ns_strip(doc)
commands <- xml2::xml_find_all(doc, ".//command")
rows <- list()

for (command_index in seq_along(commands)) {
  command <- commands[[command_index]]
  command_name <- xml2::xml_attr(command, "command")
  pivots <- xml2::xml_find_all(command, ".//pivotTable")
  for (pivot_index in seq_along(pivots)) {
    pivot <- pivots[[pivot_index]]
    subtype <- xml2::xml_attr(pivot, "subType")
    table <- xml2::xml_attr(pivot, "text")
    cells <- xml2::xml_find_all(pivot, ".//cell[@number]")
    for (cell_index in seq_along(cells)) {
      cell <- cells[[cell_index]]
      ancestors <- rev(xml2::xml_parents(cell))
      pivot_position <- which(vapply(ancestors, identical, logical(1), pivot))
      if (length(pivot_position)) ancestors <- ancestors[(pivot_position[[1]] + 1L):length(ancestors)]
      components <- vapply(ancestors, function(node) {
        kind <- xml2::xml_name(node)
        text <- xml2::xml_attr(node, "text")
        if (kind == "dimension") {
          paste0(xml2::xml_attr(node, "axis"), ":", text)
        } else if (kind == "group") {
          paste0("group:", text)
        } else if (kind == "category") {
          paste0("cat:", text)
        } else {
          ""
        }
      }, character(1))
      components <- components[nzchar(components)]
      rows[[length(rows) + 1L]] <- data.frame(
        CommandIndex = command_index,
        Command = command_name,
        PivotIndex = pivot_index,
        SubType = subtype,
        Table = table,
        Path = paste(components, collapse = " > "),
        Number = suppressWarnings(as.numeric(xml2::xml_attr(cell, "number"))),
        Text = xml2::xml_attr(cell, "text"),
        stringsAsFactors = FALSE
      )
    }
  }
}

result <- do.call(rbind, rows)
if (!nrow(result) || any(!is.finite(result$Number))) stop("No finite numeric SPSS OMS cells were extracted.")
duplicate_keys <- duplicated(result[c("CommandIndex", "PivotIndex", "Path")])
if (any(duplicate_keys)) stop("OMS cell paths are not unique within command/pivot: ", result$Path[which(duplicate_keys)[[1]]])
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(result, output_path, row.names = FALSE, na = "")
cat(sprintf("Imported %d numeric SPSS OMS cells to %s\n", nrow(result), normalizePath(output_path, winslash = "/", mustWork = TRUE)))
