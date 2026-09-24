generalized_missing_pattern_reference <-
function (raw_prepared, complete_index, outcome, predictors, 
    exposure = character(0)) 
{
    if (!is.data.frame(raw_prepared) || nrow(raw_prepared) == 
        0) {
        return(data.frame(Item = character(0), Value = character(0), 
            stringsAsFactors = FALSE, check.names = FALSE))
    }
    complete_index <- as.logical(complete_index %||% stats::complete.cases(raw_prepared))
    complete_index[is.na(complete_index)] <- FALSE
    outcome <- utils::head(intersect(as.character(outcome %||% 
        character(0)), names(raw_prepared)), 1)
    predictors <- intersect(as.character(predictors %||% character(0)), 
        names(raw_prepared))
    exposure <- utils::head(intersect(as.character(exposure %||% 
        character(0)), names(raw_prepared)), 1)
    missing_matrix <- is.na(raw_prepared)
    pattern <- apply(missing_matrix, 1, function(row) {
        missing_names <- names(raw_prepared)[row]
        if (length(missing_names) == 0) 
            "Complete"
        else paste(missing_names, collapse = ", ")
    })
    pattern_counts <- sort(table(pattern), decreasing = TRUE)
    most_common_pattern <- if (length(pattern_counts) > 0) {
        sprintf("%s (n=%s)", names(pattern_counts)[[1]], as.integer(pattern_counts[[1]]))
    }
    else {
        ""
    }
    outcome_missing <- if (length(outcome) == 1) 
        sum(is.na(raw_prepared[[outcome]]))
    else NA_integer_
    predictor_missing <- if (length(predictors) > 0) 
        sum(rowSums(missing_matrix[, predictors, drop = FALSE]) > 
            0)
    else 0L
    exposure_missing <- if (length(exposure) == 1) 
        sum(is.na(raw_prepared[[exposure]]))
    else 0L
    data.frame(Item = c("Raw rows", "Complete model rows", "Rows excluded by complete-case screen", 
        "Rows with missing dependent variable", "Rows with any missing independent variable", 
        "Rows with missing exposure / offset", "Distinct missingness patterns", 
        "Most common missingness pattern"), Value = c(as.character(nrow(raw_prepared)), 
        as.character(sum(complete_index)), as.character(sum(!complete_index)), 
        if (is.na(outcome_missing)) "" else as.character(outcome_missing), 
        as.character(predictor_missing), as.character(exposure_missing), 
        as.character(length(pattern_counts)), most_common_pattern), 
        stringsAsFactors = FALSE, check.names = FALSE)
}
