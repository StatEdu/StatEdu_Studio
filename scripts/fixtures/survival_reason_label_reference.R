survival_preflight <- function(data, settings) {
  if (!is.data.frame(data)) stop("No data is loaded.")
  if (!is.list(settings)) stop("Survival settings must be a list.")
  roles <- settings$roles %||% list()
  shape <- as.character(settings$data_shape %||% "single_record")[[1]]
  time <- as.character(roles$time %||% "")[[1]]
  event <- as.character(roles$event %||% "")[[1]]
  entry <- as.character(roles$entry %||% "")[[1]]
  start <- as.character(roles$start %||% "")[[1]]
  stop <- as.character(roles$stop %||% "")[[1]]
  subject_id <- as.character(roles$subject_id %||% "")[[1]]
  group <- survival_selected_names(roles$group)
  group <- if (length(group) > 0) group[[1]] else ""
  covariates <- survival_selected_names(roles$covariates)
  role_variables <- unique(c(time, event, entry, start, stop, subject_id, group, covariates))
  role_variables <- role_variables[nzchar(role_variables)]
  issues <- list()
  required <- switch(shape,
    entry_exit = c(entry, time, event),
    start_stop = c(subject_id, start, stop, event),
    c(time, event)
  )
  required <- required[nzchar(required)]
  if ((identical(shape, "single_record") || identical(shape, "entry_exit")) && !nzchar(time)) {
    issues[[length(issues) + 1L]] <- survival_issue("error", "missing_time_role", message = "Select a time variable.")
  }
  if (!nzchar(event)) {
    issues[[length(issues) + 1L]] <- survival_issue("error", "missing_event_role", message = "Select an event variable.")
  }
  if (identical(shape, "entry_exit") && !nzchar(entry)) {
    issues[[length(issues) + 1L]] <- survival_issue("error", "missing_entry_role", message = "Select an entry-time variable.")
  }
  if (identical(shape, "start_stop")) {
    if (!nzchar(subject_id)) issues[[length(issues) + 1L]] <- survival_issue("block", "missing_subject_id", message = "Start-stop data require a subject ID.")
    if (!nzchar(start) || !nzchar(stop)) issues[[length(issues) + 1L]] <- survival_issue("error", "missing_interval_role", message = "Select start and stop variables.")
  }
  missing_columns <- setdiff(role_variables, names(data))
  if (length(missing_columns) > 0) {
    issues[[length(issues) + 1L]] <- survival_issue(
      "error", "missing_columns", paste(missing_columns, collapse = ", "), length(missing_columns),
      paste("Variables not found:", paste(missing_columns, collapse = ", ")),
      "Select variables that exist in the current data."
    )
  }
  duplicate_required <- unique(required[duplicated(required)])
  if (length(duplicate_required) > 0) {
    issues[[length(issues) + 1L]] <- survival_issue(
      "error", "conflicting_roles", paste(duplicate_required, collapse = ", "), length(duplicate_required),
      "The same variable is assigned to conflicting required survival roles.",
      "Assign a different variable to each required role."
    )
  }
  current_issues <- survival_bind_issues(issues)
  if (any(current_issues$severity %in% c("error", "block")) && length(missing_columns) > 0) {
    return(list(ok = FALSE, schema = shape, settings = settings, analysis_data = data.frame(), row_audit = data.frame(), counts = list(source_rows = nrow(data), source_subjects = nrow(data), analysis_rows = 0L, analysis_subjects = 0L, events = 0L, competing_events = 0L, censored = 0L), issues = current_issues, transformations = list()))
  }
  frame <- data[, role_variables, drop = FALSE]
  row_id <- seq_len(nrow(frame))
  reasons <- vector("list", nrow(frame))
  recorded_covariate_rows <- integer(0)
  add_reason <- function(mask, code) {
    indexes <- which(!is.na(mask) & mask)
    if (length(indexes) > 0L && identical(code, "missing_covariate")) {
      # Keep every variable check, but record this shared reason once per row.
      if (length(recorded_covariate_rows)) indexes <- setdiff(indexes, recorded_covariate_rows)
      recorded_covariate_rows <<- c(recorded_covariate_rows, indexes)
    }
    for (index in indexes) {
      existing <- reasons[[index]]
      if (is.null(existing)) reasons[[index]] <<- code
      else if (!code %in% existing) reasons[[index]] <<- c(existing, code)
    }
  }
  numeric_roles <- unique(c(time, entry, start, stop))
  numeric_roles <- numeric_roles[nzchar(numeric_roles) & numeric_roles %in% names(frame)]
  duration_status <- list()
  for (name in numeric_roles) {
    duration_status[[name]] <- survival_coerce_duration(frame[[name]])
    frame[[name]] <- duration_status[[name]]$values
    invalid_encoding <- duration_status[[name]]$invalid_encoding
    if (any(invalid_encoding)) {
      add_reason(invalid_encoding, "invalid_time_encoding")
      issues[[length(issues) + 1L]] <- survival_issue(
        "block", "invalid_time_encoding", name, sum(invalid_encoding),
        "Time-role values must be numeric durations; date/time fields must be converted to elapsed durations before analysis.",
        "Convert the selected time variable to a numeric elapsed duration and verify its unit."
      )
    }
  }
  if (nzchar(time) && time %in% names(frame)) {
    add_reason(duration_status[[time]]$missing, "missing_time")
    invalid_time <- !is.na(frame[[time]]) & (!is.finite(frame[[time]]) | frame[[time]] < 0)
    add_reason(invalid_time, "invalid_time")
    if (any(invalid_time)) issues[[length(issues) + 1L]] <- survival_issue("block", "invalid_time", time, sum(invalid_time), "Time values must be finite and nonnegative.", "Correct the time values.")
  }
  if (identical(shape, "entry_exit") && all(c(entry, time) %in% names(frame))) {
    add_reason(duration_status[[entry]]$missing, "missing_entry")
    invalid_order <- !is.na(frame[[entry]]) & !is.na(frame[[time]]) & frame[[entry]] >= frame[[time]]
    add_reason(invalid_order, "invalid_time_order")
    if (any(invalid_order)) issues[[length(issues) + 1L]] <- survival_issue("block", "entry_not_before_exit", paste(entry, time, sep = ", "), sum(invalid_order), "Entry time must be earlier than exit time.", "Correct entry and exit times.")
  }
  if (identical(shape, "start_stop") && all(c(start, stop) %in% names(frame))) {
    add_reason(duration_status[[start]]$missing | duration_status[[stop]]$missing, "missing_interval")
    invalid_interval <- !is.na(frame[[start]]) & (!is.finite(frame[[start]]) | frame[[start]] < 0) |
      !is.na(frame[[stop]]) & (!is.finite(frame[[stop]]) | frame[[stop]] < 0)
    add_reason(invalid_interval, "invalid_interval_time")
    if (any(invalid_interval)) issues[[length(issues) + 1L]] <- survival_issue("block", "invalid_interval_time", paste(start, stop, sep = ", "), sum(invalid_interval), "Interval times must be finite and nonnegative.", "Correct the start-stop values.")
    invalid_order <- !is.na(frame[[start]]) & !is.na(frame[[stop]]) & frame[[start]] >= frame[[stop]]
    add_reason(invalid_order, "invalid_time_order")
    if (any(invalid_order)) issues[[length(issues) + 1L]] <- survival_issue("block", "start_not_before_stop", paste(start, stop, sep = ", "), sum(invalid_order), "Start time must be earlier than stop time.", "Correct the risk intervals.")
    if (nzchar(subject_id) && subject_id %in% names(frame)) {
      missing_id <- is.na(frame[[subject_id]]) | !nzchar(trimws(as.character(frame[[subject_id]])))
      add_reason(missing_id, "missing_subject_id_value")
      if (any(missing_id)) issues[[length(issues) + 1L]] <- survival_issue("block", "missing_subject_id_value", subject_id, sum(missing_id), "Every start-stop row requires a subject ID.", "Complete the subject IDs.")
    }
  }
  if (nzchar(event) && event %in% names(frame)) {
    raw_event <- frame[[event]]
    add_reason(is.na(raw_event) | !nzchar(event_text <- trimws(as.character(raw_event))), "missing_event")
    event_map <- survival_normalize_event_map(raw_event, settings$event_map, settings$event_of_interest)
    allowed_roles <- c("censored", "event_of_interest", "competing_event", "other_state", "exclude", "unknown")
    duplicate_map <- duplicated(event_map$raw_value) | duplicated(event_map$raw_value, fromLast = TRUE)
    invalid_map <- !event_map$role %in% allowed_roles
    # The default map already contains every observed plain-vector event code.
    unmapped <- if (is.null(settings$event_map) && !is.object(raw_event) &&
        is.atomic(raw_event) && is.null(dim(raw_event))) character(0) else {
      observed <- if (typeof(raw_event) %in% c("integer", "double") && is.null(attributes(raw_event)) &&
          identical(class(settings$event_map), "data.frame") &&
          all(vapply(settings$event_map, function(x) is.character(x) && is.null(attributes(x)), logical(1))) &&
          is.character(settings$event_of_interest) && is.null(attributes(settings$event_of_interest)))
        unique(event_text[!is.na(raw_event)]) else
        unique(trimws(as.character(raw_event[!is.na(raw_event)])))
      setdiff(observed[nzchar(observed)], event_map$raw_value)
    }
    if (any(duplicate_map) || any(invalid_map) || length(unmapped) > 0 || any(event_map$role == "unknown")) {
      issues[[length(issues) + 1L]] <- survival_issue("block", "invalid_event_map", event, length(unique(c(event_map$raw_value[duplicate_map | invalid_map | event_map$role == "unknown"], unmapped))), "Every observed event code must have one valid role.", "Review the event-code mapping.")
    }
    role_lookup <- stats::setNames(event_map$role, event_map$raw_value)
    # Reuse plain-vector text; retain conversion dispatch for classed inputs.
    event_roles <- unname(role_lookup[if (!is.object(raw_event) && is.atomic(raw_event) && is.null(dim(raw_event)))
      event_text else trimws(as.character(raw_event))])
    add_reason(event_roles == "exclude", "excluded_event_code")
    competing_event <- event_roles == "competing_event"
    competing_workflow <- identical(as.character(settings$objective %||% "")[[1]], "competing") ||
      identical(as.character(settings$event_structure %||% "")[[1]], "competing")
    if (any(competing_event, na.rm = TRUE) && !competing_workflow) {
      add_reason(competing_event, "competing_event_requires_competing_risk")
      issues[[length(issues) + 1L]] <- survival_issue(
        "block", "competing_event_requires_competing_risk", event, sum(competing_event, na.rm = TRUE),
        "Competing-event codes cannot be analyzed as ordinary censoring in the standard Kaplan-Meier or Cox workflow.",
        "Use the Competing Risks analysis for cumulative incidence, Gray's test, cause-specific Cox, or Fine-Gray regression."
      )
    }
    other_state <- event_roles == "other_state"
    add_reason(other_state, "unsupported_other_state")
    if (any(other_state, na.rm = TRUE)) {
      issues[[length(issues) + 1L]] <- survival_issue(
        "block", "unsupported_other_state", event, sum(other_state, na.rm = TRUE),
        "Event codes assigned as other_state require a multi-state survival model, which is not supported in survival v1.",
        "Use a supported terminal-event definition or a dedicated multi-state analysis."
      )
    }
    frame[[paste0(event, "__raw")]] <- raw_event
    frame[[event]] <- event_roles == "event_of_interest"
  } else {
    event_map <- data.frame(raw_value = character(), role = character(), label = character(), stringsAsFactors = FALSE)
    event_roles <- rep(NA_character_, nrow(frame))
  }
  if (identical(shape, "start_stop") && all(c(subject_id, start, stop, event) %in% names(frame))) {
    usable <- !is.na(frame[[subject_id]]) & is.finite(frame[[start]]) & is.finite(frame[[stop]]) & frame[[start]] < frame[[stop]]
    subject_rows <- split(which(usable), as.character(frame[[subject_id]][usable]), drop = TRUE)
    overlap_rows <- integer(0)
    post_event_rows <- integer(0)
    extra_event_rows <- integer(0)
    unordered_subjects <- character(0)
    # Direct list access avoids repeated data-frame dispatch without changing columns.
    intervals <- if (identical(class(frame), "data.frame")) unclass(frame) else frame
    subject_names <- names(subject_rows)
    for (subject_index in seq_along(subject_rows)) {
      id_value <- subject_names[[subject_index]]
      # Empty names retain the original failed name lookup and downstream error.
      indexes <- if (identical(id_value, "")) NULL else subject_rows[[subject_index]]
      # One usable interval cannot overlap, follow another event, or be out of order.
      if (length(indexes) == 1L) next
      subject_starts <- if (!is.object(intervals)) intervals[[start]][indexes] else NULL
      if (length(indexes) > 1L && is.unsorted(subject_starts %||% intervals[[start]][indexes], strictly = FALSE)) unordered_subjects <- c(unordered_subjects, id_value)
      # Strictly increasing starts already determine the complete ordering.
      ordered <- if (length(indexes) > 1L && !is.null(subject_starts) &&
          !is.unsorted(subject_starts, strictly = TRUE)) indexes else
        indexes[order(intervals[[start]][indexes], intervals[[stop]][indexes], indexes)]
      if (length(ordered) > 1L) {
        prior_max_stop <- cummax(intervals[[stop]][ordered])[-length(ordered)]
        overlapping <- intervals[[start]][ordered[-1L]] < prior_max_stop
        overlap_rows <- c(overlap_rows, ordered[-1L][overlapping])
      }
      event_indexes <- ordered[intervals[[event]][ordered] %in% TRUE]
      if (length(event_indexes) > 1L) extra_event_rows <- c(extra_event_rows, event_indexes[-1L])
      if (length(event_indexes) > 0L) {
        first_event <- event_indexes[[1]]
        later <- ordered[intervals[[start]][ordered] >= intervals[[stop]][first_event] & ordered != first_event]
        post_event_rows <- c(post_event_rows, later)
      }
    }
    overlap_rows <- unique(overlap_rows)
    post_event_rows <- unique(post_event_rows)
    extra_event_rows <- unique(extra_event_rows)
    if (length(overlap_rows)) {
      add_reason(seq_len(nrow(frame)) %in% overlap_rows, "overlapping_interval")
      issues[[length(issues) + 1L]] <- survival_issue("block", "overlapping_intervals", paste(start, stop, sep = ", "), length(overlap_rows), "Risk intervals overlap within a subject.", "Remove or reconcile overlapping intervals.")
    }
    if (length(post_event_rows)) {
      add_reason(seq_len(nrow(frame)) %in% post_event_rows, "interval_after_event")
      issues[[length(issues) + 1L]] <- survival_issue("block", "interval_after_event", subject_id, length(post_event_rows), "Risk intervals remain after a subject's event.", "End follow-up at the first terminal event or choose a recurrent-event model.")
    }
    if (length(extra_event_rows)) {
      add_reason(seq_len(nrow(frame)) %in% extra_event_rows, "multiple_subject_events")
      issues[[length(issues) + 1L]] <- survival_issue("block", "multiple_subject_events", event, length(extra_event_rows), "A subject has multiple event rows in a standard time-dependent Cox setup.", "Use the first terminal event or a recurrent-event model.")
    }
    if (length(unordered_subjects)) issues[[length(issues) + 1L]] <- survival_issue("warning", "intervals_not_source_order", subject_id, length(unique(unordered_subjects)), "Some subject intervals are not stored in chronological order; analysis uses start-time order.", "Review the source row order.")
  }
  for (name in c(group, covariates)) {
    if (!nzchar(name) || !name %in% names(frame)) next
    add_reason(is.na(frame[[name]]), if (identical(name, group)) "missing_group" else "missing_covariate")
  }
  excluded <- lengths(reasons) > 0
  exclusion_labels <- rep("", length(reasons))
  exclusion_labels[excluded] <- vapply(reasons[excluded], paste, collapse = ";", character(1))
  row_audit <- data.frame(
    source_row = row_id,
    included = !excluded,
    exclusion_reasons = exclusion_labels,
    stringsAsFactors = FALSE
  )
  analysis_data <- frame[!excluded, , drop = FALSE]
  if (identical(shape, "start_stop") && nrow(analysis_data) > 1L && all(c(subject_id, start, stop) %in% names(analysis_data))) {
    analysis_data <- analysis_data[order(as.character(analysis_data[[subject_id]]), analysis_data[[start]], analysis_data[[stop]]), , drop = FALSE]
  }
  if (nzchar(group) && group %in% names(analysis_data)) analysis_data[[group]] <- droplevels(as.factor(analysis_data[[group]]))
  measurements <- character(0)
  variable_info <- settings$variable_info
  if (is.data.frame(variable_info) && all(c("name", "measurement") %in% names(variable_info))) measurements <- stats::setNames(tolower(as.character(variable_info$measurement)), as.character(variable_info$name))
  for (name in covariates) {
    if (!name %in% names(analysis_data)) next
    measurement <- named_value(measurements, name, "")
    if (measurement %in% c("binary", "category", "ordered", "ordinal", "nominal")) analysis_data[[name]] <- as.factor(analysis_data[[name]])
  }
  interest_events <- if (nzchar(event) && event %in% names(analysis_data)) sum(analysis_data[[event]], na.rm = TRUE) else 0L
  competing_events <- sum(event_roles[!excluded] == "competing_event", na.rm = TRUE)
  censored <- sum(event_roles[!excluded] == "censored", na.rm = TRUE)
  if (nrow(analysis_data) == 0) issues[[length(issues) + 1L]] <- survival_issue("block", "no_analysis_rows", n = nrow(data), message = "No complete valid rows remain after applying survival variables.", action = "Review the exclusion reasons.")
  if (nrow(analysis_data) > 0 && interest_events == 0) issues[[length(issues) + 1L]] <- survival_issue("block", "no_events", event, 0L, "No events were found for the selected event of interest.", "Review the event-code mapping.")
  if (isTRUE(settings$legacy) && nrow(event_map) >= 3L) issues[[length(issues) + 1L]] <- survival_issue("warning", "potential_competing_events", event, nrow(event_map) - 1L, "Multiple non-interest event codes were treated as censoring by the legacy setup.", "Confirm whether any code represents a competing event.")
  issues_table <- survival_bind_issues(issues)
  source_subjects <- if (nzchar(subject_id) && subject_id %in% names(data)) length(unique(data[[subject_id]][!is.na(data[[subject_id]])])) else nrow(data)
  analysis_subjects <- if (nzchar(subject_id) && subject_id %in% names(analysis_data)) length(unique(analysis_data[[subject_id]][!is.na(analysis_data[[subject_id]])])) else nrow(analysis_data)
  list(
    ok = !any(issues_table$severity %in% c("error", "block")),
    schema = shape,
    settings = settings,
    analysis_data = analysis_data,
    row_audit = row_audit,
    counts = list(source_rows = nrow(data), source_subjects = source_subjects, analysis_rows = nrow(analysis_data), analysis_subjects = analysis_subjects, events = as.integer(interest_events), competing_events = as.integer(competing_events), censored = as.integer(censored)),
    issues = issues_table,
    transformations = list(event_map = event_map, raw_event_column = if (nzchar(event)) paste0(event, "__raw") else "", event_column = event)
  )
}

