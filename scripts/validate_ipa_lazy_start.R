if (.Platform$OS.type == "windows") Sys.setlocale("LC_CTYPE", "Korean_Korea.utf8")
Sys.setenv(STATEDU_MODULE_CACHE = "false")
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE)
source_app_modules()

# Full-app handlers are registered before the browser binds the new menu inputs.
# Flush once with no IPA inputs, then bind them, just as lazy menu entry does.
for (language in c("ko", "en")) {
  shiny::testServer(function(input, output, session) {
    handlers <- register_ipa_handlers(input, output, session,
      function() data.frame(i = seq(3, 5, length.out = 30), p = seq(2, 4, length.out = 30)),
      function() c("i", "p"), function() language, function() NULL)
  }, {
    withCallingHandlers(session$flushReact(), warning = function(w) stop(w))
    stopifnot(!session$isClosed(), nzchar(output$ipa_setup$html),
      nzchar(output$ipa_point_setup$html))
    session$setInputs(ipa_design = "overall", ipa_mode = "direct", ipa_layout = "wide")
    session$setInputs(ipa_available = "i", ipa_available_active = 1, ipa_move_importance = 1)
    session$setInputs(ipa_available = "p", ipa_available_active = 2, ipa_move_performance = 1)
    session$setInputs(run_ipa = 1)
    stopifnot(!session$isClosed(), !is.null(handlers$result()),
      abs(handlers$result()$coordinates$Importance - 4) < 1e-10,
      abs(handlers$result()$coordinates$Performance - 3) < 1e-10)
  })
}
cat("PASS: IPA lazy startup and analysis before/after input binding (ko/en)\n")
