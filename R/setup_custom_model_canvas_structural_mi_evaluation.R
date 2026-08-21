# Structural modification-index evaluation helpers.

structural_canvas_allowed_mi <- function(snapshot, fit, mode = "theory") {
  nodes <- snapshot$nodes %||% list()
  edges <- snapshot$edges %||% list()
  latents <- Filter(function(node) identical(node$role, "latent"), nodes)
  latent_ids <- vapply(latents, function(node) as.character(node$id), character(1))
  latent_names <- stats::setNames(vapply(latents, structural_canvas_name, character(1)), latent_ids)
  name_to_id <- stats::setNames(names(latent_names), unname(latent_names))
  structural_edges <- Filter(function(edge) {
    if (identical(edge$kind, "covariance")) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    identical(from$role, "latent") && identical(to$role, "latent")
  }, edges)
  incoming <- unique(vapply(structural_edges, function(edge) as.character(edge$to), character(1)))
  exogenous <- setdiff(latent_ids, incoming)
  endogenous <- intersect(latent_ids, incoming)
  indicator_parent <- character(0)
  for (edge in edges) {
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    if (identical(from$role, "latent") && identical(to$role, "indicator")) indicator_parent[[structural_canvas_name(to)]] <- as.character(from$id)
    if (identical(to$role, "latent") && identical(from$role, "indicator")) indicator_parent[[structural_canvas_name(from)]] <- as.character(to$id)
  }
  reachable <- function(from_id, to_id) {
    visit <- function(current, seen = character(0)) {
      if (current %in% seen) return(FALSE)
      targets <- vapply(Filter(function(edge) identical(as.character(edge$from), current), structural_edges), function(edge) as.character(edge$to), character(1))
      if (to_id %in% targets) return(TRUE)
      any(vapply(targets, visit, logical(1), seen = c(seen, current)))
    }
    visit(from_id)
  }
  mi <- lavaan::modindices(fit)
  if (!nrow(mi)) return(mi)
  valid_mi <- is.finite(mi$mi) & mi$mi >= 0
  mi$`MI p` <- NA_real_
  mi$`BH-adjusted p` <- NA_real_
  mi$`Multiplicity family size` <- sum(valid_mi)
  mi$`MI p`[valid_mi] <- stats::pchisq(mi$mi[valid_mi], df = 1L, lower.tail = FALSE)
  mi$`BH-adjusted p`[valid_mi] <- stats::p.adjust(mi$`MI p`[valid_mi], method = "BH")
  mi <- mi[valid_mi & mi$mi >= 4, , drop = FALSE]
  if (!nrow(mi)) return(mi)
  if (identical(mode, "conventional")) {
    mi$Allowed <- TRUE
    mi$Reason <- "Conventional modification-index output"
    return(mi[order(-mi$mi), , drop = FALSE])
  }
  mi$Allowed <- FALSE
  mi$Reason <- "Not allowed"
  for (index in seq_len(nrow(mi))) {
    if (!identical(mi$op[[index]], "~~") || identical(mi$lhs[[index]], mi$rhs[[index]])) next
    lhs <- mi$lhs[[index]]
    rhs <- mi$rhs[[index]]
    lhs_parent <- if (lhs %in% names(indicator_parent)) indicator_parent[[lhs]] else ""
    rhs_parent <- if (rhs %in% names(indicator_parent)) indicator_parent[[rhs]] else ""
    if (nzchar(lhs_parent) && identical(lhs_parent, rhs_parent)) {
      mi$Allowed[[index]] <- TRUE
      mi$Reason[[index]] <- "Measurement errors within the same latent variable"
      next
    }
    lhs_id <- if (lhs %in% names(name_to_id)) name_to_id[[lhs]] else ""
    rhs_id <- if (rhs %in% names(name_to_id)) name_to_id[[rhs]] else ""
    if (nzchar(lhs_id) && nzchar(rhs_id) && lhs_id %in% exogenous && rhs_id %in% exogenous) {
      mi$Allowed[[index]] <- TRUE
      mi$Reason[[index]] <- "Covariance between exogenous latent variables"
      next
    }
    if (nzchar(lhs_id) && nzchar(rhs_id) && lhs_id %in% endogenous && rhs_id %in% endogenous &&
        !reachable(lhs_id, rhs_id) && !reachable(rhs_id, lhs_id)) {
      mi$Allowed[[index]] <- TRUE
      mi$Reason[[index]] <- "Covariance between structurally unrelated disturbances"
    }
  }
  mi <- mi[mi$Allowed, , drop = FALSE]
  mi[order(-mi$mi), , drop = FALSE]
}

structural_canvas_mi_refits <- function(snapshot, result, data, analysis_type, estimator, missing, std_lv, mode = "theory", ordered = character(0), ml_likelihood = "normal") {
  mi <- structural_canvas_allowed_mi(snapshot, result$fit, mode = mode)
  if (!nrow(mi)) return(mi)
  for (column in c("cfi_after", "tli_after", "rmsea_after", "srmr_after")) mi[[column]] <- NA_real_
  if (identical(mode, "conventional")) return(mi)

  current_fit <- result$fit
  cumulative_syntax <- result$syntax
  steps <- list()
  for (step in seq_len(5L)) {
    candidates <- structural_canvas_allowed_mi(snapshot, current_fit, mode = "theory")
    if (!nrow(candidates)) break
    candidate <- NULL
    candidate_row <- NULL
    candidate_syntax <- NULL
    skipped_candidates <- 0L
    skipped_details <- character(0)
    for (candidate_index in seq_len(nrow(candidates))) {
      trial_row <- candidates[candidate_index, , drop = FALSE]
      trial_syntax <- paste(
        cumulative_syntax,
        paste(trial_row$lhs[[1L]], trial_row$op[[1L]], trial_row$rhs[[1L]]),
        sep = "\n"
      )
      trial_error <- ""
      parameterization <- if (length(ordered)) "theta" else "delta"
      trial <- tryCatch({
        arguments <- list(
          model = trial_syntax, data = data, estimator = estimator, missing = missing,
          std.lv = std_lv, ordered = ordered, auto.cov.lv.x = FALSE,
          parameterization = parameterization
        )
        if (identical(toupper(as.character(estimator)), "ML")) arguments$likelihood <- ml_likelihood
        do.call(if (identical(analysis_type, "cfa")) lavaan::cfa else lavaan::sem, arguments)
      }, error = function(error) {
        trial_error <<- conditionMessage(error)
        NULL
      })
      trial_admissibility <- if (!is.null(trial)) structural_canvas_fit_admissibility(trial) else list(admissible = FALSE)
      if (!is.null(trial) && isTRUE(trial_admissibility$admissible)) {
        candidate <- trial
        candidate_row <- trial_row
        candidate_syntax <- trial_syntax
        skipped_candidates <- candidate_index - 1L
        break
      }
      path_label <- paste(trial_row$lhs[[1L]], trial_row$op[[1L]], trial_row$rhs[[1L]])
      reason <- if (nzchar(trial_error)) paste0("fit error: ", trial_error) else paste(trial_admissibility$reasons %||% "inadmissible trial fit", collapse = "; ")
      skipped_details <- c(skipped_details, paste0(path_label, " [", reason, "]"))
    }
    if (is.null(candidate)) break

    cumulative_syntax <- candidate_syntax
    current_fit <- candidate
    indices <- structural_canvas_fit_measures(candidate, estimator, .90)$values
    candidate_row$step <- step
    candidate_row$skipped_inadmissible <- skipped_candidates
    candidate_row$skipped_details <- paste(skipped_details, collapse = " | ")
    candidate_row$cfi_after <- indices[[5L]]
    candidate_row$tli_after <- indices[[6L]]
    candidate_row$rmsea_after <- indices[[8L]]
    candidate_row$srmr_after <- indices[[7L]]
    steps[[length(steps) + 1L]] <- candidate_row
  }

  if (!length(steps)) return(mi[0L, , drop = FALSE])
  do.call(rbind, steps)
}
