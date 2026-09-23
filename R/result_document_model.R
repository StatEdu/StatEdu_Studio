# Snapshot-only document model shared by the editable Word and native HWPX writers.
result_document_table_style <- function() {
  list(font = "Arial", size = 9, padding_pt = 0.2 * 72 / 25.4,
    padding_hwp = 56L, outer_pt = 0.5 * 72 / 25.4,
    inner_pt = 0.1 * 72 / 25.4)
}

# Plain grids use the same gdtools metrics and
# padding arithmetic as autofit; complex text and merges retain its full path.
result_document_table_geometry <- function(info, fallback = TRUE) {
  source <- info$screen
  values <- source$values
  simple <- identical(flextable::get_flextable_defaults()$theme_fun, "theme_booktabs") &&
    is.matrix(values) && is.character(values) && nrow(values) > source$header_rows &&
    ncol(values) > 0L && !anyNA(values) &&
    !any(grepl("[\\r\\n\\t]|<br>|<tab>", values, perl = TRUE)) &&
    all(vapply(source$cells, function(cell) cell$rowspan == 1L && cell$colspan == 1L &&
      !nzchar(cell$superscript %||% ""), logical(1)))
  if (!simple) {
    if (!fallback) return(NULL)
    table <- result_document_table(info, layout_only = TRUE)
    return(list(widths = unname(table$body$colwidths),
      heights = unname(c(table$header$rowheights, table$body$rowheights))))
  }
  theme <- result_document_table_style()
  text <- unique(as.vector(values))
  metric <- gdtools::strings_sizes(text, fontname = theme$font,
    fontsize = theme$size, bold = FALSE, italic = FALSE)
  index <- match(as.vector(values), text)
  padding <- 2 * theme$padding_pt * (4/3)/72
  widths <- apply(matrix(metric$width[index], nrow(values)) + padding, 2L, max) + 0.1
  heights <- apply(matrix((metric$ascent + metric$descent)[index], nrow(values)) + padding, 1L, max) + 0.1
  target <- result_docx_page_spec(result_docx_wide_table(info))$table_width
  captured <- source$column_widths
  widths <- if (length(captured) == length(widths) && all(is.finite(captured) & captured >= 0) && sum(captured) > 0) {
    weights <- pmax(captured, 0.001)
    target * weights / sum(weights)
  } else target * 0.6 / length(widths) + target * 0.4 * widths / sum(widths)
  list(widths = unname(widths), heights = unname(heights))
}

# Boundaries belong to the captured grid, including merged header cells.
result_document_cell_rules <- function(source, cell) {
  r <- cell$row; last <- r + cell$rowspan - 1L; nh <- source$header_rows
  header <- r <= nh
  value <- trimws(source$values[r, cell$col])
  summary <- r > nh && grepl("^F\\s*\\(", trimws(source$values[r, 1L]))
  list(top = if (r == 1L) "outer" else if (summary || r == nh + 1L) "inner" else "none",
    bottom = if (last == nrow(source$values)) "outer" else if (header &&
      (last == nh || (r == 1L && nzchar(value)) ||
        (cell$colspan > 1L && grepl("95%\\s*CI", value)))) "inner" else "none")
}

result_document_content_types <- function() c("main", "appendix", "explanation", "figure")

result_document_selection <- function(contents) {
  if (is.null(contents)) return(result_document_content_types())
  intersect(as.character(contents), result_document_content_types())
}

# Classify captured nodes, preserving titles and table footnotes with their
# owning content. Explicit table roles take precedence over legacy heuristics.
result_document_selected_items <- function(items, document, contents) {
  if (setequal(contents, result_document_content_types())) return(items)
  if (!length(items)) return(items)
  orders <- vapply(items, `[[`, numeric(1), "output_order")
  selected <- rep(FALSE, length(items))
  headings <- which(vapply(items, function(x) isTRUE(x$heading), logical(1)))
  for (i in seq_along(items)) {
    item <- items[[i]]
    source <- xml2::xml_find_first(document, sprintf("//*[@data-result-export-order='%s']", item$output_order))
    if (is.null(item$text)) {
      if (!is.null(item$path)) selected[i] <- "figure" %in% contents else {
        role <- xml2::xml_attr(xml2::xml_find_first(source, "ancestor-or-self::*[@data-result-table-role][1]"), "data-result-table-role")
        if (is.na(role) || !nzchar(role)) role <- if (result_docx_main_table(item)) "main" else "appendix"
        selected[i] <- result_table_role(role) %in% contents
      }
    } else if (!isTRUE(item$heading)) selected[i] <- "explanation" %in% contents
  }
  # Notes are an integral part of the selected table, not optional narrative.
  for (i in which(vapply(items, function(x) !is.null(x$text) && !isTRUE(x$heading), logical(1)))) {
    source <- xml2::xml_find_first(document, sprintf("//*[@data-result-export-order='%s']", orders[i]))
    wrapper <- xml2::xml_find_first(source, "ancestor::*[contains(concat(' ',normalize-space(@class),' '),' result-table-with-note ')][1]")
    note <- xml2::xml_find_first(source, "ancestor-or-self::*[contains(concat(' ',normalize-space(@class),' '),' coefficient-note ') or contains(concat(' ',normalize-space(@class),' '),' coefficient-warning ')][1]")
    if (!inherits(wrapper, "xml_missing") || !inherits(note, "xml_missing")) {
      owner <- if (!inherits(wrapper, "xml_missing")) xml2::xml_find_first(wrapper, ".//table") else xml2::xml_find_first(source, "preceding::table[1]")
      match <- match(result_node_order(owner), orders)
      selected[i] <- !is.na(match) && selected[match]
    }
  }
  for (i in rev(headings)) {
    subsequent <- headings[headings > i]
    subsequent <- subsequent[vapply(subsequent, function(j) items[[j]]$level <= items[[i]]$level, logical(1))]
    end <- if (length(subsequent)) subsequent[1] - 1L else length(items)
    selected[i] <- end > i && any(selected[seq.int(i + 1L, end)])
  }
  items[selected]
}

result_document_model <- function(entries, contents = NULL, layout_only = FALSE) {
  contents <- result_document_selection(contents)
  if (!length(contents)) stop(statedu_t("result.document.no_selection", fallback = "Select at least one content type."), call. = FALSE)
  nodes <- list(); paths <- character(); previous_wide <- FALSE
  completed <- FALSE
  on.exit(if (!completed && length(paths)) unlink(paths), add=TRUE)
  add <- function(node) nodes[[length(nodes)+1L]] <<- node
  for (index in seq_along(entries)) {
    entry <- entries[[index]]
    document <- result_entry_document(entry)
    paragraphs <- result_entry_paragraphs(entry,document=document)
    tables <- result_entry_tables(entry,index,include_docx=FALSE,document=document,text_items=paragraphs)
    images <- if ("figure" %in% contents) result_entry_images(entry,document=document) else list()
    paths <- c(paths,vapply(images,`[[`,character(1),"path"))
    paragraph_text <- vapply(paragraphs, `[[`, character(1), "text")
    items <- c(tables,images,paragraphs)
    items <- items[order(vapply(items,`[[`,numeric(1),"output_order"))]
    items <- result_document_selected_items(items, document, contents)
    pending_gap <- FALSE
    for (item in items) {
      if (pending_gap && (is.null(item$text) || isTRUE(item$heading))) {
        add(list(kind="gap",landscape=previous_wide));pending_gap <- FALSE
      }
      wide <- previous_wide
      if (is.null(item$text)) wide <- if (!is.null(item$path)) identical(item$orientation,"landscape") else result_docx_wide_table(item)
      else if (isTRUE(item$heading)) {
        following <- Filter(function(x)is.null(x$text) && x$output_order>item$output_order,items)
        if(length(following))wide <- if(!is.null(following[[1]]$path))identical(following[[1]]$orientation,"landscape") else result_docx_wide_table(following[[1]])
      }
      previous_wide <- wide
      if (!is.null(item$text)) add(list(kind="paragraph",landscape=wide,text=item$text,heading=isTRUE(item$heading)))
      else if (!is.null(item$path)) add(list(kind="image",landscape=wide,image=item,dimensions=result_docx_image_dimensions(item,landscape=wide)))
      else {
        if (layout_only) {
          geometry <- result_document_table_geometry(item)
          add(list(kind="table",landscape=wide,info=item,widths=geometry$widths,heights=geometry$heights))
        } else {
          table <- result_document_table(item)
          add(list(kind="table",landscape=wide,info=item,table=table,widths=table$body$colwidths,
            heights=c(table$header$rowheights,table$body$rowheights)))
        }
        for(note in as.character(item$notes %||% character()))if(nzchar(note) && !note %in% paragraph_text)
          add(list(kind="paragraph",landscape=wide,text=note,heading=FALSE))
        pending_gap <- TRUE
      }
    }
    if(pending_gap)add(list(kind="gap",landscape=previous_wide))
  }
  if (!length(nodes)) stop(statedu_t("result.document.no_content", fallback = "There is no result content matching the selection."), call. = FALSE)
  completed <- TRUE
  list(nodes=nodes,temporary_images=paths)
}

result_document_cleanup <- function(model) {
  if(length(model$temporary_images))unlink(model$temporary_images)
}

# Share paragraph layout properties across editable Word table cells. Run
# toggle properties stay explicit: bold/italic have different style semantics.
# Cell borders and widths also remain explicit for Hancom import.
result_docx_shared_table_writer <- function(document) {
  ns <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")
  style_path <- file.path(document$package_dir, "word/styles.xml")
  styles <- xml2::read_xml(style_path)
  used <- xml2::xml_attr(xml2::xml_find_all(styles, "/w:styles/w:style", ns), "w:styleId", ns = ns)
  definitions <- character(); keys <- character(); ids <- character()
  replacements <- new.env(parent = emptyenv())
  run_replacements <- new.env(parent = emptyenv())
  run_keys <- character(); run_ids <- character()
  # The bundled flextable serializer is the same one used by
  # body_add_flextable; intercept before parsing the large table XML.
  serializer <- get0("gen_raw_wml", envir = asNamespace("flextable"), inherits = FALSE)
  add <- function(document, table) {
    if (!is.function(serializer)) return(flextable::body_add_flextable(document, table))
    original <- table
    defaults <- flextable::get_flextable_defaults()
    table <- defaults$post_process_all(table)
    table <- defaults$post_process_docx(table)
    # Captions and custom hooks may add content; keep their standard path.
    if (length(table$caption$value)) return(flextable::body_add_flextable(document, original))
    table <- flextable::fix_border_issues(table)
    text <- serializer(table, doc = document)
    pattern <- "(?s)<w:pPr>.*?</w:pPr>"
    properties <- regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1L]]
    for (property in unique(properties)) {
      if (exists(property, replacements, inherits = FALSE)) {
        text <- gsub(property, replacements[[property]], text, fixed = TRUE)
        next
      }
      # Only the known simple properties emitted by our table builder qualify.
      # Keep uncommon fields (numbering, section breaks, etc.) untouched.
      root <- xml2::read_xml(paste0('<root xmlns:w="', ns[[1]], '">', property, '</root>'))
      p <- xml2::xml_child(root, 1L)
      names <- xml2::xml_name(xml2::xml_children(p))
      if (!all(names %in% c("pStyle", "jc", "pBdr", "spacing", "ind", "rPr", "keepNext", "keepLines", "widowControl"))) next
      base <- xml2::xml_attr(xml2::xml_find_first(p, "./w:pStyle", ns), "w:val", ns = ns)
      if (is.na(base)) {
        label <- xml2::xml_attr(xml2::xml_find_first(p, "./w:pStyle", ns), "w:pstlname", ns = ns)
        match <- which(document$styles$style_type == "paragraph" & document$styles$style_name == label)
        if (length(match)) base <- document$styles$style_id[match[1L]]
      }
      if (is.na(base) || !nzchar(base)) next
      run <- regmatches(property, regexpr("(?s)<w:rPr>.*?</w:rPr>", property, perl = TRUE))
      layout <- gsub("(?s)<w:rPr>.*?</w:rPr>|<w:pStyle[^>]*/>", "", property, perl = TRUE)
      key <- paste(base, layout)
      index <- match(key, keys)
      if (is.na(index)) {
        id <- paste0("StatEduTableP", length(ids) + 1L)
        while (id %in% used) id <- paste0(id, "X")
        used <<- c(used, id); keys <<- c(keys, key); ids <<- c(ids, id); index <- length(ids)
        definitions <<- c(definitions, paste0('<w:style xmlns:w="', ns[[1]],
          '" w:type="paragraph" w:customStyle="1" w:styleId="', id,
          '"><w:name w:val="', id, '"/><w:basedOn w:val="',
          htmltools::htmlEscape(base, attribute = TRUE), '"/>', layout, '</w:style>'))
      }
      replacement <- paste0('<w:pPr><w:pStyle w:val="', ids[index], '"/>', paste(run, collapse = ""), '</w:pPr>')
      replacements[[property]] <- replacement
      text <- gsub(property, replacement, text, fixed = TRUE)
    }
    runs <- regmatches(text, gregexpr("(?s)<w:rPr>.*?</w:rPr>", text, perl = TRUE))[[1L]]
    for (run in unique(runs)) {
      if (!exists(run, run_replacements, inherits = FALSE)) {
        root <- xml2::read_xml(paste0('<root xmlns:w="', ns[[1]], '">', run, '</root>'))
        fields <- xml2::xml_name(xml2::xml_children(xml2::xml_child(root, 1L)))
        if (!all(fields %in% c("rFonts", "i", "b", "u", "strike", "sz", "szCs", "color", "vertAlign"))) next
        inline_pattern <- "<w:(?:i|b|strike|vertAlign)\\b[^>]*/>"
        inline <- paste(regmatches(run, gregexpr(inline_pattern, run, perl = TRUE))[[1L]], collapse = "")
        shared <- gsub(inline_pattern, "", run, perl = TRUE)
        index <- match(shared, run_keys)
        if (is.na(index)) {
          id <- paste0("StatEduTableC", length(run_ids) + 1L)
          while (id %in% used) id <- paste0(id, "X")
          used <<- c(used, id); run_keys <<- c(run_keys, shared); run_ids <<- c(run_ids, id); index <- length(run_ids)
          definitions <<- c(definitions, paste0('<w:style xmlns:w="', ns[[1]],
            '" w:type="character" w:customStyle="1" w:styleId="', id,
            '"><w:name w:val="', id, '"/>', shared, '</w:style>'))
        }
        run_replacements[[run]] <- paste0('<w:rPr><w:rStyle w:val="', run_ids[index], '"/>', inline, '</w:rPr>')
      }
      text <- gsub(run, run_replacements[[run]], text, fixed = TRUE)
    }
    officer::body_add_xml(document, text)
  }
  finish <- function() {
    for (definition in definitions) xml2::xml_add_child(styles, xml2::read_xml(definition))
    if (length(definitions)) xml2::write_xml(styles, style_path)
    invisible(NULL)
  }
  list(add = add, finish = finish)
}

# Batch equal cell properties into rectangles rather than repeatedly copying
# the flextable for each captured cell. Merged cells retain their full grid span.
result_document_apply_cell_formats <- function(ft, formats, header_rows, borders) {
  for (part in c("header", "body")) {
    source_rows <- seq_len(nrow(formats$align))
    source_rows <- source_rows[if (part == "header") source_rows <= header_rows else source_rows > header_rows]
    if (!length(source_rows)) next
    for (property in names(formats)) {
      values <- formats[[property]][source_rows, , drop = FALSE]
      for (value in unique(as.vector(values[!is.na(values)]))) {
        columns <- lapply(seq_len(nrow(values)), function(i) which(!is.na(values[i, ]) & values[i, ] == value))
        keys <- vapply(columns, paste, collapse = ",", FUN.VALUE = character(1))
        for (rows in split(which(nzchar(keys)), keys[nzchar(keys)])) {
          cols <- columns[[rows[1L]]]
          if (property == "align") ft <- flextable::align(ft, i = rows, j = cols, align = value, part = part)
          else if (property == "valign") ft <- flextable::valign(ft, i = rows, j = cols, valign = value, part = part)
          else if (property == "top") ft <- flextable::border(ft, i = rows, j = cols, border.top = borders[[value]], part = part)
          else ft <- flextable::border(ft, i = rows, j = cols, border.bottom = borders[[value]], part = part)
        }
      }
    }
  }
  ft
}

# One private artifact per format, owned by the Results session. Repeated saves
# reuse only byte-identical snapshot inputs; no analysis or output is persisted
# in a process-global cache. The uncached writers remain the source of truth.
result_document_export_cache <- function(max_bytes = 32 * 1024^2) {
  artifacts <- list()
  clear <- function() {
    for (item in artifacts) unlink(item$path)
    artifacts <<- list()
    invisible(NULL)
  }
  save <- function(entries, file, format, contents = NULL, language = statedu_current_language()) {
    stopifnot(format %in% c("word", "hwpx"))
    selected <- result_document_content_types()
    selected <- selected[selected %in% result_document_selection(contents)]
    dependencies <- c("www/style.css", list.files("www/hwpx-base", full.names = TRUE))
    signature <- list(entries = entries, contents = selected, language = language,
      config = read_app_config(), style = result_document_table_style(),
      defaults = flextable::get_flextable_defaults(),
      dependencies = tools::md5sum(dependencies[file.exists(dependencies)]))
    item <- artifacts[[format]]
    if (!is.null(item) && identical(item$signature, signature) && file.exists(item$path) &&
        identical(unname(tools::md5sum(item$path)), item$checksum)) {
      # Preserve the writer's open-document protection on the fast path too.
      if (.Platform$OS.type == "windows" && file.exists(file) &&
          !suppressWarnings(file.rename(file, file))) stop("Please close the document before saving: ", file)
      if (!file.copy(item$path, file, overwrite = TRUE)) stop("Could not save the document: ", file)
      return(invisible(file))
    }
    # A failed replacement must never leave a stale artifact eligible for reuse.
    if (!is.null(item)) unlink(item$path)
    artifacts[[format]] <<- NULL
    if (format == "word") write_result_collection_docx(entries, file, contents = selected)
    else write_result_collection_hwpx(entries, file, contents = selected)
    size <- file.info(file)$size
    if (is.finite(size) && size <= max_bytes && as.numeric(object.size(signature)) <= max_bytes) {
      path <- tempfile("statedu_document_cache_", fileext = if (format == "word") ".docx" else ".hwpx")
      if (suppressWarnings(file.copy(file, path))) {
        artifacts[[format]] <<- list(signature = signature, path = path,
          checksum = unname(tools::md5sum(path)))
      } else unlink(path)
    }
    invisible(file)
  }
  list(save = save, clear = clear)
}
