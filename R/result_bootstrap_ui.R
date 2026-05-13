# Bootstrap progress UI.

bootstrap_progress_ui <- function(status) {
  if (!is.list(status)) {
    return(NULL)
  }
  done <- as.integer(status$done %||% 0L)
  total <- as.integer(status$r %||% 0L)
  percent <- if (total > 0) min(100, max(0, round(done / total * 100))) else 0
  message <- status$message %||% "Running regression"
  label <- if (total > 0) {
    sprintf("%s Bootstrap %s / %s", message, done, total)
  } else {
    message
  }

  div(
    class = "bootstrap-progress-row bootstrap-progress-only",
    div(
      class = "bootstrap-progress-box",
      div(
        class = "bootstrap-progress-track",
        div(class = "bootstrap-progress-bar", style = sprintf("width:%s%%;", percent))
      ),
      div(class = "bootstrap-progress-label", label)
    )
  )
}

bootstrap_stop_button <- function() {
  tags$button(
    "Stop bootstrap",
    id = "stop_bootstrap",
    type = "button",
    class = "btn btn-default btn-sm bootstrap-stop-button",
    onmousedown = paste(
      "this.disabled = true;",
      "this.innerText = 'Stopping...';",
      "if (window.Shiny) Shiny.setInputValue('stop_bootstrap_now', Date.now() + Math.random(), {priority: 'event'});"
    ),
    onclick = paste(
      "if (window.Shiny) Shiny.setInputValue('stop_bootstrap_now', Date.now() + Math.random(), {priority: 'event'});"
    )
  )
}
