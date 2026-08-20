structural_canvas_moderation_add_gradient <- function(gradient, label, value) {
  if (!nzchar(label) || !is.finite(value)) return(gradient)
  current <- if (label %in% names(gradient)) gradient[[label]] else 0
  gradient[[label]] <- current + value
  gradient
}

structural_canvas_moderation_product_except <- function(values, index) {
  if (!length(values)) return(1)
  if (length(values) == 1L) return(1)
  prod(values[-index])
}

structural_canvas_moderation_delta_se <- function(gradient, covariance) {
  gradient <- gradient[is.finite(gradient) & abs(gradient) > 0]
  if (!length(gradient) || !all(names(gradient) %in% rownames(covariance))) return(NA_real_)
  variance <- as.numeric(t(gradient) %*% covariance[names(gradient), names(gradient), drop = FALSE] %*% gradient)
  if (!is.finite(variance)) return(NA_real_)
  sqrt(max(0, variance))
}

structural_canvas_moderation_interval_roots <- function(effect_fn, se_fn, lower, upper, critical) {
  if (!all(is.finite(c(lower, upper))) || lower >= upper) return(numeric(0))
  grid <- seq(lower, upper, length.out = 201L)
  value_fn <- function(value) {
    effect <- effect_fn(value)
    se <- se_fn(value)
    if (!is.finite(effect) || !is.finite(se) || se <= 0) return(NA_real_)
    abs(effect / se) - critical
  }
  values <- vapply(grid, value_fn, numeric(1))
  roots <- grid[is.finite(values) & abs(values) < 1e-7]
  for (index in seq_len(length(grid) - 1L)) {
    left <- values[[index]]
    right <- values[[index + 1L]]
    if (!all(is.finite(c(left, right)))) next
    if (left == 0 || right == 0 || sign(left) == sign(right)) next
    root <- tryCatch(
      stats::uniroot(function(value) value_fn(value), lower = grid[[index]], upper = grid[[index + 1L]])$root,
      error = function(error) NA_real_
    )
    if (is.finite(root)) roots <- c(roots, root)
  }
  sort(unique(round(roots[roots >= lower & roots <= upper], 10)))
}

structural_canvas_moderation_interval_rows <- function(effect_type, path, moderator, moderator_mean, lower, upper, effect_fn, se_fn, alpha) {
  critical <- stats::qnorm(1 - alpha / 2)
  roots <- structural_canvas_moderation_interval_roots(effect_fn, se_fn, lower, upper, critical)
  cuts <- sort(unique(c(lower, roots, upper)))
  if (length(cuts) < 2L) cuts <- c(lower, upper)
  rows <- list()
  for (index in seq_len(length(cuts) - 1L)) {
    interval_lower <- cuts[[index]]
    interval_upper <- cuts[[index + 1L]]
    midpoint <- mean(c(interval_lower, interval_upper))
    effect <- effect_fn(midpoint)
    se <- se_fn(midpoint)
    if (!is.finite(effect) || !is.finite(se) || se <= 0) next
    z <- effect / se
    p <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)
    rows[[length(rows) + 1L]] <- data.frame(
      Effect = effect_type,
      Path = path,
      Moderator = moderator,
      `Moderator range` = paste(format_decimal3(interval_lower + moderator_mean), "to", format_decimal3(interval_upper + moderator_mean)),
      `Midpoint effect` = format_decimal3(effect),
      SE = format_decimal3(se),
      z = format_decimal3(z),
      p = format_p(p),
      Significant = if (isTRUE(is.finite(p) && p < alpha)) "Yes" else "No",
      check.names = FALSE
    )
  }
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

structural_canvas_direct_moderation_jn_rows <- function(definition, coefficients, covariance, estimates, alpha) {
  direct_label <- as.character(definition$direct_label %||% "")
  interaction_label <- as.character(definition$interaction_label %||% "")
  if (!all(c(direct_label, interaction_label) %in% names(coefficients))) return(data.frame())
  if (!all(c(direct_label, interaction_label) %in% rownames(covariance))) return(data.frame())
  interaction_row <- estimates[estimates$label == interaction_label, , drop = FALSE]
  interaction_p <- if (nrow(interaction_row)) suppressWarnings(as.numeric(interaction_row$pvalue[[1L]])) else NA_real_
  if (!is.finite(interaction_p) || interaction_p >= alpha) return(data.frame())
  b1 <- unname(coefficients[[direct_label]])
  b3 <- unname(coefficients[[interaction_label]])
  moderator_mean <- as.numeric(definition$moderator_mean %||% 0)
  centered_min <- as.numeric(definition$moderator_min %||% NA_real_) - moderator_mean
  centered_max <- as.numeric(definition$moderator_max %||% NA_real_) - moderator_mean
  if (!all(is.finite(c(b1, b3, centered_min, centered_max))) || centered_min >= centered_max) return(data.frame())
  effect_fn <- function(value) b1 + value * b3
  se_fn <- function(value) {
    gradient <- c(stats::setNames(1, direct_label), stats::setNames(value, interaction_label))
    structural_canvas_moderation_delta_se(gradient, covariance)
  }
  structural_canvas_moderation_interval_rows(
    "Direct",
    paste(definition$predictor, "->", definition$outcome),
    definition$moderator,
    moderator_mean,
    centered_min,
    centered_max,
    effect_fn,
    se_fn,
    alpha
  )
}

structural_canvas_indirect_moderation_jn_rows <- function(effect_definitions, moderation_definitions, coefficients, covariance, alpha) {
  if (!length(effect_definitions) || !length(moderation_definitions)) return(data.frame())
  rows <- list()
  for (effect in effect_definitions) {
    if (!identical(as.character(effect$type %||% ""), "Indirect")) next
    paths <- effect$paths %||% list()
    path_labels <- effect$path_labels %||% list()
    for (path_index in seq_along(paths)) {
      path <- paths[[path_index]]
      labels <- path_labels[[path_index]] %||% character(0)
      if (length(path) < 3L || length(labels) != length(path) - 1L) next
      for (definition in moderation_definitions) {
        edge_position <- which(
          path[-length(path)] == as.character(definition$predictor %||% "") &
            path[-1L] == as.character(definition$outcome %||% "")
        )
        if (!length(edge_position)) next
        edge_position <- edge_position[[1L]]
        direct_label <- as.character(definition$direct_label %||% "")
        interaction_label <- as.character(definition$interaction_label %||% "")
        other_labels <- labels[-edge_position]
        required_labels <- unique(c(direct_label, interaction_label, other_labels))
        if (!all(required_labels %in% names(coefficients)) || !all(required_labels %in% rownames(covariance))) next
        other_values <- unname(coefficients[other_labels])
        other_product <- if (length(other_values)) prod(other_values) else 1
        b1 <- unname(coefficients[[direct_label]])
        b3 <- unname(coefficients[[interaction_label]])
        if (!all(is.finite(c(other_values, other_product, b1, b3)))) next
        index_value <- other_product * b3
        index_gradient <- stats::setNames(numeric(0), character(0))
        index_gradient <- structural_canvas_moderation_add_gradient(index_gradient, interaction_label, other_product)
        for (other_index in seq_along(other_labels)) {
          index_gradient <- structural_canvas_moderation_add_gradient(
            index_gradient,
            other_labels[[other_index]],
            structural_canvas_moderation_product_except(other_values, other_index) * b3
          )
        }
        index_se <- structural_canvas_moderation_delta_se(index_gradient, covariance)
        index_p <- if (is.finite(index_se) && index_se > 0) {
          2 * stats::pnorm(abs(index_value / index_se), lower.tail = FALSE)
        } else {
          NA_real_
        }
        moderator_mean <- as.numeric(definition$moderator_mean %||% 0)
        centered_min <- as.numeric(definition$moderator_min %||% NA_real_) - moderator_mean
        centered_max <- as.numeric(definition$moderator_max %||% NA_real_) - moderator_mean
        if (!all(is.finite(c(centered_min, centered_max))) || centered_min >= centered_max) next
        effect_fn <- function(value) other_product * (b1 + value * b3)
        se_fn <- function(value) {
          gradient <- stats::setNames(numeric(0), character(0))
          gradient <- structural_canvas_moderation_add_gradient(gradient, direct_label, other_product)
          gradient <- structural_canvas_moderation_add_gradient(gradient, interaction_label, other_product * value)
          for (other_index in seq_along(other_labels)) {
            gradient <- structural_canvas_moderation_add_gradient(
              gradient,
              other_labels[[other_index]],
              structural_canvas_moderation_product_except(other_values, other_index) * (b1 + value * b3)
            )
          }
          structural_canvas_moderation_delta_se(gradient, covariance)
        }
        rows[[length(rows) + 1L]] <- structural_canvas_moderation_interval_rows(
          "Indirect",
          paste(path, collapse = " -> "),
          definition$moderator,
          moderator_mean,
          centered_min,
          centered_max,
          effect_fn,
          se_fn,
          alpha
        )
      }
    }
  }
  rows <- Filter(function(row) is.data.frame(row) && nrow(row), rows)
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

structural_canvas_moderation_factor_score_values <- function(fit, latent_name) {
  scores <- tryCatch(lavaan::lavPredict(fit, method = "regression"), error = function(error) NULL)
  if (is.null(scores)) return(numeric(0))
  if (is.list(scores)) scores <- do.call(rbind, lapply(scores, as.data.frame))
  scores <- as.data.frame(scores)
  if (!latent_name %in% names(scores)) return(numeric(0))
  values <- suppressWarnings(as.numeric(scores[[latent_name]]))
  values[is.finite(values)]
}

structural_canvas_moderation_update_factor_score_ranges <- function(definitions, fit) {
  lapply(definitions, function(definition) {
    if (!identical(as.character(definition$moderator_role %||% ""), "latent")) return(definition)
    values <- structural_canvas_moderation_factor_score_values(fit, as.character(definition$moderator %||% ""))
    if (length(values) < 2L) {
      definition$moderator_range_available <- FALSE
      definition$moderator_min <- NA_real_
      definition$moderator_max <- NA_real_
      return(definition)
    }
    definition$moderator_range_available <- TRUE
    definition$moderator_mean <- 0
    definition$moderator_min <- min(values, na.rm = TRUE)
    definition$moderator_max <- max(values, na.rm = TRUE)
    definition
  })
}

structural_canvas_moderation_jn_table <- function(bundle, alpha = .05) {
  definitions <- bundle$diagnostics$moderation_definitions %||% bundle$moderation_definitions %||% list()
  if (!length(definitions) || !inherits(bundle$fit, "lavaan")) return(data.frame())
  definitions <- structural_canvas_moderation_update_factor_score_ranges(definitions, bundle$fit)
  coefficients <- lavaan::coef(bundle$fit)
  covariance <- tryCatch(as.matrix(lavaan::lavInspect(bundle$fit, "vcov")), error = function(error) matrix(numeric(0), 0L, 0L))
  estimates <- lavaan::parameterEstimates(bundle$fit)
  rows <- lapply(definitions, structural_canvas_direct_moderation_jn_rows, coefficients, covariance, estimates, alpha)
  indirect_rows <- structural_canvas_indirect_moderation_jn_rows(
    bundle$diagnostics$effect_definitions %||% bundle$effect_definitions %||% list(),
    definitions,
    coefficients,
    covariance,
    alpha
  )
  rows <- c(rows, list(indirect_rows))
  rows <- Filter(function(row) is.data.frame(row) && nrow(row), rows)
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

structural_canvas_register_moderation_outputs <- function(output, prefix, fit_result, app_language_fn = NULL) {
  output[[paste0(prefix, "_result_moderation_jn")]] <- renderUI({
    table <- structural_canvas_moderation_jn_table(fit_result())
    if (!is.data.frame(table) || !nrow(table)) return(NULL)
    ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
    div(class = "result-section regression-result-panel structural-moderation-jn-result",
      h4(if (ko) "Johnson-Neyman 조절효과" else "Johnson-Neyman moderation"),
      structural_canvas_basic_html_table(table),
      tags$p(class = "structural-result-note", if (ko) "이 표는 관측 연속형 또는 잠재 조절변수가 연결된 잠재 구조경로에서 product-indicator 상호작용항 또는 조절된 매개효과 index가 유의한 경우에만 표시됩니다. 잠재 조절변수의 범위는 lavaan 회귀 요인점수 척도로 표시됩니다." else "This table is shown only when a product-indicator interaction term or moderated-mediation index is significant for a latent structural path moderated by an observed continuous or latent moderator. Latent moderator ranges are reported on the lavaan regression factor-score scale.")
    )
  })
  invisible(TRUE)
}
