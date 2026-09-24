# Generate the real variable table for the browser regression check.
.libPaths(R.home('library'))
Sys.setlocale('LC_CTYPE', 'English_United States.utf8')
source('R/app_bootstrap.R', encoding = 'UTF-8')
load_app_packages(check = FALSE)
source_app_modules()
out <- 'tmp/variable-pagination'
dir.create(out, recursive = TRUE, showWarnings = FALSE)
info <- data.frame(source_order = 1:100, name = paste0('v', 1:100),
  var_label = '', measurement = 'category', storage_type = 'numeric',
  n_unique = 5L, n_missing = 0L, min_value = '1', max_value = '5')
state <- variable_table_render_state(info, checked_names = character(),
  selected_names = character(), selection_applied = FALSE, language = 'en')
widget <- DT::datatable(state$table_data, rownames = FALSE, escape = FALSE,
  selection = 'none', options = variable_table_options('en', compact = FALSE),
  callback = variable_table_callback(selected_names = character(),
    dependent_only = FALSE, single_select_role = FALSE,
    script = variable_table_callback_script('en')))
widget <- htmlwidgets::prependContent(widget, htmltools::tags$script(htmltools::HTML(
  'window.Shiny = {setInputValue: function() {}, bindAll: function() {}, unbindAll: function() {}};')))
htmlwidgets::saveWidget(widget, file.path(out, 'table.html'), selfcontained = FALSE)
cat('Generated variable pagination browser fixture\n')
