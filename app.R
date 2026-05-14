source(file.path("R", "app_bootstrap.R"), local = FALSE)
load_app_packages()

app_config <- read_app_config()
app_version <- app_config$version

source_app_modules()

ui <- app_ui(app_version)
server <- create_app_server(app_version)

shinyApp(ui, server)
