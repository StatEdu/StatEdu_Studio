# Server handlers for client-side messages.

client_js_error_message <- function(error) {
  sprintf(
    "Client JS error: %s (%s:%s:%s)",
    error$message %||% "",
    error$source %||% "",
    error$line %||% "",
    error$column %||% ""
  )
}

register_client_error_handler <- function(input) {
  observeEvent(input$client_js_error, {
    message(client_js_error_message(input$client_js_error))
  })

  invisible(TRUE)
}
