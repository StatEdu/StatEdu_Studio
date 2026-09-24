# Data tab DT table renderers and callbacks.

message_table_datatable <- function(
  message,
  language = statedu_initial_language(),
  options = list(dom = "t", paging = FALSE, ordering = FALSE),
  escape = FALSE
) {
  DT::datatable(
    data.frame(Message = as.character(message %||% ""), check.names = FALSE),
    rownames = FALSE,
    colnames = data_table_colnames("Message", language),
    escape = escape,
    selection = "none",
    options = options
  )
}
