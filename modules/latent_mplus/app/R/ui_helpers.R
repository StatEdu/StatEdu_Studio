empty_message <- function(text) {
  div(class = "empty-message", text)
}

app_brand_title <- function(version) {
  div(
    class = "brand-title",
    tags$img(src = paste0("logo-horizontal.png?v=", version, "-statedu-studio-latent"), class = "brand-logo-horizontal", alt = "StatEdu Studio logo"),
    span(class = "latent-brand-text", "Latent Mplus"),
    span(class = "version", paste0("v", version))
  )
}

app_stylesheet_link <- function(version) {
  tags$link(rel = "stylesheet", type = "text/css", href = paste0("style.css?v=", version, "-efs-0933-latent"))
}

app_script_link <- function(version) {
  tags$script(src = paste0("easyflow.js?v=", version, "-efs-0933-latent"))
}

latent_default_project_root <- function() {
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

latent_project_root_value <- function(value = NULL) {
  value <- trimws(as.character(value %||% ""))
  normalized <- gsub("\\\\", "/", value)
  if (!nzchar(normalized) || identical(tolower(normalized), "d:/latent")) {
    return(latent_default_project_root())
  }
  normalized
}

app_head_tags <- function(version) {
  tags$head(
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = paste0("logo-favicon-32.png?v=", version, "-concept-02-8")),
    tags$link(rel = "icon", type = "image/png", sizes = "64x64", href = paste0("logo-favicon-64.png?v=", version, "-concept-02-8")),
    app_stylesheet_link(version),
    tags$script(HTML(
      "window.MathJax = {
        tex: {
          inlineMath: [['$', '$'], ['\\\\(', '\\\\)']],
          displayMath: [['$$', '$$'], ['\\\\[', '\\\\]']],
          processEscapes: true
        },
        options: {
          skipHtmlTags: ['script', 'noscript', 'style', 'textarea', 'pre', 'code']
        }
      };"
    )),
    tags$script(
      id = "MathJax-script",
      defer = "defer",
      onload = "if (window.easyflowMathJaxReady) window.easyflowMathJaxReady();",
      src = paste0("mathjax/tex-svg.js?v=", version, "-local")
    ),
    app_script_link(version),
    tags$script(HTML("
      $(document).on('click', '.latent-step-tab', function() {
        var target = $(this).data('target');
        var scope = $(this).closest('.latent-workflow');
        scope.find('.latent-step-tab').removeClass('active');
        $(this).addClass('active');
        scope.find('.latent-step-panel').removeClass('active');
        scope.find('#' + target).addClass('active');
      });
      $(document).on('click', '.latent-setup-topic', function() {
        var target = $(this).data('target');
        var scope = $(this).closest('.latent-setup-workspace');
        scope.find('.latent-setup-topic').removeClass('active');
        $(this).addClass('active');
        scope.find('.latent-setup-panel').removeClass('active');
        scope.find('#' + target).addClass('active');
      });
      Shiny.addCustomMessageHandler('latent-show-results', function(message) {
        var moduleId = message.module || '';
        var target = 'latent_' + moduleId + '_run';
        var panel = $('#' + target);
        if (!panel.length) return;
        var workflow = panel.closest('.latent-workflow');
        workflow.find('.latent-step-tab').removeClass('active');
        workflow.find('.latent-step-tab[data-target=\"' + target + '\"]').addClass('active');
        workflow.find('.latent-step-panel').removeClass('active');
        panel.addClass('active');
        window.setTimeout(function() {
          var top = Math.max(0, workflow.offset().top - 12);
          window.scrollTo(window.pageXOffset || 0, top);
        }, 0);
      });
      function latentActiveRole(moduleId) {
        var input = $('#' + moduleId + '_active_role');
        if (!input.length) return '';
        if (input[0].selectize) return input[0].selectize.getValue() || '';
        return input.val() || '';
      }
      function updateLatentActiveRoleVisual(moduleId) {
        var role = latentActiveRole(moduleId);
        var input = $('#' + moduleId + '_active_role');
        input.attr('data-active-role', role);
        $('#' + moduleId + '_role_summary table tbody tr').removeClass('latent-active-role-row');
        $('#' + moduleId + '_role_summary table tbody tr').each(function() {
          var firstCell = $(this).find('td').first().text().trim();
          if (firstCell === role) $(this).addClass('latent-active-role-row');
        });
      }
      $(document).on('change', '.latent-role-select', function() {
        if (!window.Shiny) return;
        var el = $(this);
        var moduleId = el.data('module');
        var variable = el.data('variable');
        if (!moduleId || !variable) return;
        if (window.easyflowLatentRememberViewport) window.easyflowLatentRememberViewport(moduleId);
        if (window.easyflowLatentRememberPage) window.easyflowLatentRememberPage(moduleId);
        var activeRole = latentActiveRole(moduleId);
        el.closest('tr').find('.latent-role-checkbox')
          .attr('data-role', activeRole)
          .data('role', activeRole)
          .prop('checked', activeRole && el.val() === activeRole);
        Shiny.setInputValue(moduleId + '_role_cell_update', {
          variable: variable,
          role: el.val() || '',
          nonce: Date.now() + Math.random()
        }, {priority: 'event'});
      });
      $(document).on('change', '.latent-role-checkbox', function() {
        if (!window.Shiny) return;
        var el = $(this);
        var moduleId = el.data('module');
        var variable = el.data('variable');
        var role = latentActiveRole(moduleId);
        if (!moduleId || !variable || !role) return;
        if (window.easyflowLatentRememberViewport) window.easyflowLatentRememberViewport(moduleId);
        if (window.easyflowLatentRememberPage) window.easyflowLatentRememberPage(moduleId);
        el.attr('data-role', role).data('role', role);
        el.closest('tr').find('.latent-role-select').val(el.prop('checked') ? role : '');
        Shiny.setInputValue(moduleId + '_role_checkbox_update', {
          variable: variable,
          role: el.prop('checked') ? role : '',
          active_role: role,
          nonce: Date.now() + Math.random()
        }, {priority: 'event'});
      });
      $(document).on('change', 'select[id$=\"_active_role\"]', function() {
        var moduleId = this.id.replace(/_active_role$/, '');
        var activeRole = latentActiveRole(moduleId);
        updateLatentActiveRoleVisual(moduleId);
        var table = $('#' + moduleId + '_variable_preview table');
        if (!table.length) return;
        table.find('.latent-role-checkbox').each(function() {
          var checkbox = $(this);
          var roleValue = checkbox.closest('tr').find('.latent-role-select').val() || '';
          checkbox.attr('data-role', activeRole).data('role', activeRole).prop('checked', activeRole && roleValue === activeRole);
        });
        if (window.easyflowLatentUpdatePageToggle) window.easyflowLatentUpdatePageToggle(moduleId);
      });
      $(document).on('draw.dt', '#mixture_role_summary table, #lta_role_summary table, #state_transition_role_summary table', function() {
        var moduleId = $(this).closest('.datatables').attr('id') || '';
        moduleId = moduleId.replace(/_role_summary$/, '');
        if (moduleId) updateLatentActiveRoleVisual(moduleId);
      });
      Shiny.addCustomMessageHandler('latent-role-clear', function(message) {
        var moduleId = message.module || '';
        var role = message.role || '';
        var all = !!message.all;
        var table = $('#' + moduleId + '_variable_preview table');
        if (!table.length) return;
        table.find('.latent-role-select').each(function() {
          var select = $(this);
          if (all || select.val() === role) {
            select.val('');
          }
        });
        table.find('.latent-role-checkbox').prop('checked', false);
        if (window.easyflowLatentUpdatePageToggle) window.easyflowLatentUpdatePageToggle(moduleId);
      });
      $(document).on('click', '.latent-select-current-page', function() {
        if (!window.Shiny) return;
        var moduleId = $(this).data('module');
        var role = $('#' + moduleId + '_active_role').val() || '';
        if (window.easyflowLatentRememberViewport) window.easyflowLatentRememberViewport(moduleId);
        var table = $('#' + moduleId + '_variable_preview table').DataTable();
        var variables = [];
        $(table.rows({page: 'current'}).nodes()).find('.latent-role-checkbox').each(function() {
          var variable = $(this).data('variable');
          if (variable) variables.push(variable);
        });
        Shiny.setInputValue(moduleId + '_select_current_page', {
          variables: variables,
          role: role,
          nonce: Date.now() + Math.random()
        }, {priority: 'event'});
      });
    "))
  )
}

set_data_step_view <- function(active_step_setter, data_view_setter, step, view = "info") {
  active_step_setter(step)
  data_view_setter(view)
}

metric_tile <- function(label, value, detail = NULL) {
  div(
    class = "latent-metric-tile",
    div(class = "latent-metric-value", value),
    div(class = "latent-metric-label", label),
    if (!is.null(detail)) div(class = "latent-metric-detail", detail)
  )
}

placeholder_table <- function(id) {
  DT::dataTableOutput(id)
}

app_ui <- function(version) {
  navbarPage(
    title = app_brand_title(version),
    id = "main_menu",
    header = app_head_tags(version),
    data_tab_panel(),
    latent_home_tab(),
    latent_menu_tab(),
    result_library_tab(),
    about_tab(version)
  )
}
