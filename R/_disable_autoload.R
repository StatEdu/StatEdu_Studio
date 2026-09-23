# Shiny recognizes this marker before loading R/*.R. app.R already loads the
# complete module list in dependency order through source_app_modules().
# Keep one loader for both desktop launchers and direct shiny::runApp() use.
