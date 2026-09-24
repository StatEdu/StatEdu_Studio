survival_normalize_event_map <- function(values, event_map = NULL, event_of_interest = "1") {
  observed <- unique(trimws(as.character(values[!is.na(values)])))
  observed <- observed[nzchar(observed)]
  interest <- trimws(as.character(event_of_interest %||% "1")[[1]])
  if (is.data.frame(event_map) && all(c("raw_value", "role") %in% names(event_map))) {
    map <- event_map[, intersect(c("raw_value", "role", "label"), names(event_map)), drop = FALSE]
    map$raw_value <- trimws(as.character(map$raw_value))
    map$role <- trimws(tolower(as.character(map$role)))
    if (!"label" %in% names(map)) map$label <- map$raw_value
    map$label <- as.character(map$label)
    return(map)
  }
  data.frame(
    raw_value = observed,
    role = ifelse(observed == interest, "event_of_interest", "censored"),
    label = observed,
    stringsAsFactors = FALSE
  )
}

