# Custom mediation/moderation model canvas UI.

custom_model_canvas_translation_key <- function(en) {
  keys <- c(
    "Mediation / Moderation Custom Model" = "title",
    "Cancel" = "cancel",
    "Apply" = "apply",
    "Close" = "close",
    "none" = "none",
    "Label" = "label",
    "Role" = "role",
    "Properties" = "properties",
    "Variable name" = "variable_name",
    "Font size" = "font_size",
    "Mode: Select" = "mode_select",
    "Mode: Connect" = "mode_connect",
    "Mode: Delete" = "mode_delete",
    "Mode: Properties" = "mode_properties",
    "Covariates" = "covariates",
    "Covariates: none" = "covariates_none",
    "Paper settings" = "paper_settings",
    "Paper" = "paper",
    "Orientation" = "orientation",
    "Landscape" = "landscape",
    "Portrait" = "portrait",
    "Start" = "start",
    "Start auto" = "start_auto",
    "Start top" = "start_top",
    "Start right" = "start_right",
    "Start bottom" = "start_bottom",
    "Start left" = "start_left",
    "End" = "end",
    "End auto" = "end_auto",
    "End top" = "end_top",
    "End right" = "end_right",
    "End bottom" = "end_bottom",
    "End left" = "end_left",
    "Style settings" = "style_settings",
    "Box line color" = "box_line_color",
    "Box line width" = "box_line_width",
    "Arrow line color" = "arrow_line_color",
    "Arrow line width" = "arrow_line_width",
    "Arrow head" = "arrow_head",
    "Triangle" = "triangle",
    "Arrow" = "arrow",
    "Open triangle" = "open_triangle",
    "Circle" = "circle",
    "B(p) font" = "b_p_font",
    "Export model" = "export_model",
    "Export" = "export",
    "Format" = "format",
    "Invalid model JSON." = "invalid_json",
    "Select a variable first." = "select_variable_first",
    "The result diagram is available after running analysis." = "result_unavailable",
    "%s can currently be selected only once." = "role_limit",
    "Role: independent, mediator, moderator, dependent" = "role_prompt",
    "Covariate settings" = "covariate_settings",
    "Black" = "color_black",
    "Dark gray" = "color_dark_gray",
    "Blue" = "color_blue",
    "Green" = "color_green",
    "Red" = "color_red",
    "Purple" = "color_purple",
    "Orange" = "color_orange",
    "Custom" = "color_custom",
    "Independent" = "role_independent",
    "Mediator" = "role_mediator",
    "Moderator" = "role_moderator",
    "Dependent" = "role_dependent",
    "Covariate" = "role_covariate",
    "Complete Step 2 in the Data tab before drawing a model." = "complete_step2",
    "File" = "file",
    "Analysis" = "analysis",
    "Tools" = "tools",
    "View" = "view",
    "Result" = "result",
    "Load" = "load",
    "Save" = "save",
    "Run" = "run",
    "Analysis options" = "analysis_options",
    "Select" = "select",
    "Connect" = "connect",
    "Delete" = "delete",
    "Undo" = "undo",
    "Redo" = "redo",
    "Zoom in" = "zoom_in",
    "Zoom out" = "zoom_out",
    "Fit" = "fit",
    "Grid" = "grid",
    "Auto align" = "auto_align",
    "Style" = "style",
    "Reset" = "reset",
    "Reset model" = "reset_model",
    "Clear all boxes and arrows?" = "clear_boxes_arrows",
    "Result diagram" = "result_diagram",
    "Edit" = "edit",
    "Non-significant dashed" = "nonsignificant_dashed",
    "Straight" = "straight",
    "Curve up" = "curve_up",
    "Curve down" = "curve_down",
    "The drawn model does not match one of the currently supported mediation/moderation model numbers." = "unsupported_model",
    "Draw a mediation/moderation model by placing variables on the canvas." = "subtitle",
    "Running custom mediation / moderation model" = "running",
    "Custom model analysis finished." = "finished",
    "Custom Model Canvas Data Viewer" = "data_viewer_title"
  )
  en <- as.character(en %||% "")[[1]]
  if (!en %in% names(keys)) {
    return("")
  }
  value <- unname(keys[[en]])
  if (is.null(value)) "" else value
}

custom_model_canvas_builtin_text <- function(language, key) {
  language <- normalize_app_language(language)
  if (!identical(language, "ja")) {
    return("")
  }
  values <- c(
    title = "\u5a92\u4ecb\u30fb\u8abf\u6574\u30ab\u30b9\u30bf\u30e0\u30e2\u30c7\u30eb",
    cancel = "\u30ad\u30e3\u30f3\u30bb\u30eb",
    apply = "\u9069\u7528",
    close = "\u9589\u3058\u308b",
    none = "\u306a\u3057",
    label = "\u30e9\u30d9\u30eb",
    role = "\u5f79\u5272",
    properties = "\u5c5e\u6027",
    variable_name = "\u5909\u6570\u540d",
    font_size = "\u30d5\u30a9\u30f3\u30c8\u30b5\u30a4\u30ba",
    mode_select = "\u30e2\u30fc\u30c9: \u9078\u629e",
    mode_connect = "\u30e2\u30fc\u30c9: \u63a5\u7d9a",
    mode_delete = "\u30e2\u30fc\u30c9: \u524a\u9664",
    mode_properties = "\u30e2\u30fc\u30c9: \u5c5e\u6027",
    covariates = "\u5171\u5909\u91cf",
    covariates_none = "\u5171\u5909\u91cf: \u306a\u3057",
    paper_settings = "\u7528\u7d19\u8a2d\u5b9a",
    paper = "\u7528\u7d19",
    orientation = "\u5411\u304d",
    landscape = "\u6a2a",
    portrait = "\u7e26",
    style_settings = "\u30b9\u30bf\u30a4\u30eb\u8a2d\u5b9a",
    box_line_color = "\u30dc\u30c3\u30af\u30b9\u7dda\u306e\u8272",
    box_line_width = "\u30dc\u30c3\u30af\u30b9\u7dda\u306e\u592a\u3055",
    arrow_line_color = "\u77e2\u5370\u7dda\u306e\u8272",
    arrow_line_width = "\u77e2\u5370\u7dda\u306e\u592a\u3055",
    arrow_head = "\u77e2\u5370\u306e\u5148\u7aef",
    triangle = "\u4e09\u89d2\u5f62",
    arrow = "\u77e2\u5370",
    open_triangle = "\u958b\u3044\u305f\u4e09\u89d2\u5f62",
    circle = "\u5186\u5f62",
    b_p_font = "B(p) \u30d5\u30a9\u30f3\u30c8",
    export_model = "\u30e2\u30c7\u30eb\u3092\u66f8\u304d\u51fa\u3057",
    export = "\u66f8\u304d\u51fa\u3057",
    format = "\u5f62\u5f0f",
    invalid_json = "\u30e2\u30c7\u30ebJSON\u304c\u6b63\u3057\u304f\u3042\u308a\u307e\u305b\u3093\u3002",
    select_variable_first = "\u5148\u306b\u5909\u6570\u3092\u9078\u629e\u3057\u3066\u304f\u3060\u3055\u3044\u3002",
    result_unavailable = "\u7d50\u679c\u56f3\u306f\u5206\u6790\u5b9f\u884c\u5f8c\u306b\u78ba\u8a8d\u3067\u304d\u307e\u3059\u3002",
    role_limit = "%s\u306f\u73fe\u57281\u3064\u3060\u3051\u9078\u629e\u3067\u304d\u307e\u3059\u3002",
    role_prompt = "\u5f79\u5272: independent, mediator, moderator, dependent",
    covariate_settings = "\u5171\u5909\u91cf\u8a2d\u5b9a",
    color_black = "\u9ed2",
    color_dark_gray = "\u6fc3\u3044\u30b0\u30ec\u30fc",
    color_blue = "\u9752",
    color_green = "\u7dd1",
    color_red = "\u8d64",
    color_purple = "\u7d2b",
    color_orange = "\u30aa\u30ec\u30f3\u30b8",
    color_custom = "\u30ab\u30b9\u30bf\u30e0",
    role_independent = "\u72ec\u7acb",
    role_mediator = "\u5a92\u4ecb",
    role_moderator = "\u8abf\u6574",
    role_dependent = "\u5f93\u5c5e",
    role_covariate = "\u5171\u5909\u91cf",
    complete_step2 = "\u30e2\u30c7\u30eb\u3092\u63cf\u304f\u524d\u306b\u3001\u30c7\u30fc\u30bf\u30bf\u30d6\u306e\u30b9\u30c6\u30c3\u30d72\u3092\u9069\u7528\u3057\u3066\u304f\u3060\u3055\u3044\u3002",
    file = "\u30d5\u30a1\u30a4\u30eb",
    analysis = "\u5206\u6790",
    tools = "\u30c4\u30fc\u30eb",
    view = "\u8868\u793a",
    result = "\u7d50\u679c",
    load = "\u8aad\u307f\u8fbc\u307f",
    save = "\u4fdd\u5b58",
    run = "\u5b9f\u884c",
    analysis_options = "\u5206\u6790\u30aa\u30d7\u30b7\u30e7\u30f3",
    select = "\u9078\u629e",
    connect = "\u63a5\u7d9a",
    delete = "\u524a\u9664",
    undo = "\u5143\u306b\u623b\u3059",
    redo = "\u3084\u308a\u76f4\u3057",
    zoom_in = "\u62e1\u5927",
    zoom_out = "\u7e2e\u5c0f",
    fit = "\u753b\u9762\u306b\u5408\u308f\u305b\u308b",
    grid = "\u30b0\u30ea\u30c3\u30c9",
    auto_align = "\u81ea\u52d5\u6574\u5217",
    style = "\u30b9\u30bf\u30a4\u30eb",
    reset = "\u521d\u671f\u5316",
    reset_model = "\u30e2\u30c7\u30eb\u3092\u521d\u671f\u5316",
    clear_boxes_arrows = "\u3059\u3079\u3066\u306e\u30dc\u30c3\u30af\u30b9\u3068\u77e2\u5370\u3092\u521d\u671f\u5316\u3057\u307e\u3059\u304b\uff1f",
    result_diagram = "\u7d50\u679c\u56f3",
    edit = "\u7de8\u96c6",
    nonsignificant_dashed = "\u6709\u610f\u3067\u306a\u3044\u7d4c\u8def\u3092\u70b9\u7dda",
    straight = "\u76f4\u7dda",
    curve_up = "\u4e0a\u5411\u304d\u66f2\u7dda",
    curve_down = "\u4e0b\u5411\u304d\u66f2\u7dda",
    unsupported_model = "\u63cf\u753b\u3057\u305f\u30e2\u30c7\u30eb\u306f\u73fe\u5728\u30b5\u30dd\u30fc\u30c8\u3055\u308c\u3066\u3044\u308b\u5a92\u4ecb\u30fb\u8abf\u6574\u30e2\u30c7\u30eb\u756a\u53f7\u306b\u4e00\u81f4\u3057\u307e\u305b\u3093\u3002",
    subtitle = "\u30ad\u30e3\u30f3\u30d0\u30b9\u306b\u5909\u6570\u3092\u914d\u7f6e\u3057\u3066\u5a92\u4ecb\u30fb\u8abf\u6574\u30e2\u30c7\u30eb\u3092\u4f5c\u6210\u3057\u307e\u3059\u3002",
    running = "\u30ab\u30b9\u30bf\u30e0\u5a92\u4ecb\u30fb\u8abf\u6574\u30e2\u30c7\u30eb\u3092\u5b9f\u884c\u4e2d",
    finished = "\u30ab\u30b9\u30bf\u30e0\u30e2\u30c7\u30eb\u5206\u6790\u304c\u5b8c\u4e86\u3057\u307e\u3057\u305f\u3002",
    data_viewer_title = "\u30ab\u30b9\u30bf\u30e0\u30e2\u30c7\u30eb\u30c7\u30fc\u30bf\u30d3\u30e5\u30fc\u30a2"
  )
  if (!key %in% names(values)) {
    return("")
  }
  value <- unname(values[[key]])
  if (is.null(value)) "" else value
}

custom_model_canvas_text <- function(language, en, ko) {
  language <- normalize_app_language(language)
  fallback <- if (identical(language, "ko")) ko else en
  key <- custom_model_canvas_translation_key(en)
  if (nzchar(key)) {
    builtin <- custom_model_canvas_builtin_text(language, key)
    if (nzchar(builtin)) {
      return(builtin)
    }
    return(statedu_t(paste0("custom_model_canvas.", key), language, fallback))
  }
  fallback
}

custom_model_canvas_title <- function(language = statedu_initial_language()) {
  custom_model_canvas_text(
    language,
    "Mediation / Moderation Custom Model",
    "\ub9e4\uac1c\u00b7\uc870\uc808 \uc0ac\uc6a9\uc790 \uc815\uc758 \ubaa8\ub378"
  )
}

custom_model_canvas_i18n <- function(language = statedu_initial_language()) {
  list(
    cancel = custom_model_canvas_text(language, "Cancel", "\ucde8\uc18c"),
    apply = custom_model_canvas_text(language, "Apply", "\uc801\uc6a9"),
    close = custom_model_canvas_text(language, "Close", "\ub2eb\uae30"),
    none = custom_model_canvas_text(language, "none", "\uc5c6\uc74c"),
    label = custom_model_canvas_text(language, "Label", "\ub77c\ubca8"),
    role = custom_model_canvas_text(language, "Role", "\uc5ed\ud560"),
    properties = custom_model_canvas_text(language, "Properties", "\uc18d\uc131"),
    variable_name = custom_model_canvas_text(language, "Variable name", "\ubcc0\uc218\uba85"),
    font_size = custom_model_canvas_text(language, "Font size", "\ud3f0\ud2b8 \ud06c\uae30"),
    mode_select = custom_model_canvas_text(language, "Mode: Select", "\ubaa8\ub4dc: \uc120\ud0dd"),
    mode_connect = custom_model_canvas_text(language, "Mode: Connect", "\ubaa8\ub4dc: \uc5f0\uacb0"),
    mode_delete = custom_model_canvas_text(language, "Mode: Delete", "\ubaa8\ub4dc: \uc0ad\uc81c"),
    mode_properties = custom_model_canvas_text(language, "Mode: Properties", "\ubaa8\ub4dc: \uc18d\uc131"),
    covariates = custom_model_canvas_text(language, "Covariates", "\uacf5\ubcc0\ub7c9"),
    covariates_none = custom_model_canvas_text(language, "Covariates: none", "\uacf5\ubcc0\ub7c9: \uc5c6\uc74c"),
    paper_settings = custom_model_canvas_text(language, "Paper settings", "\uc6a9\uc9c0 \uc124\uc815"),
    paper = custom_model_canvas_text(language, "Paper", "\uc6a9\uc9c0"),
    orientation = custom_model_canvas_text(language, "Orientation", "\ubc29\ud5a5"),
    landscape = custom_model_canvas_text(language, "Landscape", "\uac00\ub85c"),
    portrait = custom_model_canvas_text(language, "Portrait", "\uc138\ub85c"),
    style_settings = custom_model_canvas_text(language, "Style settings", "\uc2a4\ud0c0\uc77c \uc124\uc815"),
    box_line_color = custom_model_canvas_text(language, "Box line color", "\ubc15\uc2a4 \uc120 \uc0c9"),
    box_line_width = custom_model_canvas_text(language, "Box line width", "\ubc15\uc2a4 \uc120 \uad75\uae30"),
    arrow_line_color = custom_model_canvas_text(language, "Arrow line color", "\ud654\uc0b4\ud45c \uc120 \uc0c9"),
    arrow_line_width = custom_model_canvas_text(language, "Arrow line width", "\ud654\uc0b4\ud45c \uc120 \uad75\uae30"),
    arrow_head = custom_model_canvas_text(language, "Arrow head", "\ud654\uc0b4\ud45c \ub05d"),
    triangle = custom_model_canvas_text(language, "Triangle", "\uc0bc\uac01\ud615"),
    arrow = custom_model_canvas_text(language, "Arrow", "\ud654\uc0b4\ud45c"),
    open_triangle = custom_model_canvas_text(language, "Open triangle", "\uc5f4\ub9b0 \uc0bc\uac01\ud615"),
    circle = custom_model_canvas_text(language, "Circle", "\uc6d0\ud615"),
    b_p_font = custom_model_canvas_text(language, "B(p) font", "B(p) \ud3f0\ud2b8"),
    export_model = custom_model_canvas_text(language, "Export model", "\ubaa8\ub378 \ub0b4\ubcf4\ub0b4\uae30"),
    export = custom_model_canvas_text(language, "Export", "\ub0b4\ubcf4\ub0b4\uae30"),
    format = custom_model_canvas_text(language, "Format", "\ud615\uc2dd"),
    invalid_json = custom_model_canvas_text(language, "Invalid model JSON.", "\uc798\ubabb\ub41c \ubaa8\ub378 JSON\uc785\ub2c8\ub2e4."),
    select_variable_first = custom_model_canvas_text(language, "Select a variable first.", "\ubcc0\uc218\ub97c \uba3c\uc800 \uc120\ud0dd\ud558\uc138\uc694."),
    result_unavailable = custom_model_canvas_text(language, "The result diagram is available after running analysis.", "\uacb0\uacfc \uadf8\ub9bc\uc740 \ubd84\uc11d \uc2e4\ud589 \ud6c4\uc5d0 \ud655\uc778\ud560 \uc218 \uc788\uc2b5\ub2c8\ub2e4."),
    role_limit = custom_model_canvas_text(language, "%s can currently be selected only once.", "%s\uc740(\ub294) \ud604\uc7ac 1\uac1c\ub9cc \uc120\ud0dd\ud560 \uc218 \uc788\uc2b5\ub2c8\ub2e4."),
    role_prompt = custom_model_canvas_text(language, "Role: independent, mediator, moderator, dependent", "\uc5ed\ud560: independent, mediator, moderator, dependent"),
    covariate_settings = custom_model_canvas_text(language, "Covariate settings", "\uacf5\ubcc0\ub7c9 \uc124\uc815"),
    color_black = custom_model_canvas_text(language, "Black", "\uac80\uc815"),
    color_dark_gray = custom_model_canvas_text(language, "Dark gray", "\ud68c\uac80\uc815"),
    color_blue = custom_model_canvas_text(language, "Blue", "\ud30c\ub791"),
    color_green = custom_model_canvas_text(language, "Green", "\ucd08\ub85d"),
    color_red = custom_model_canvas_text(language, "Red", "\ube68\uac15"),
    color_purple = custom_model_canvas_text(language, "Purple", "\ubcf4\ub77c"),
    color_orange = custom_model_canvas_text(language, "Orange", "\uc8fc\ud669"),
    color_custom = custom_model_canvas_text(language, "Custom", "\uc0ac\uc6a9\uc790 \uc9c0\uc815"),
    role_independent = custom_model_canvas_text(language, "Independent", "\ub3c5\ub9bd"),
    role_mediator = custom_model_canvas_text(language, "Mediator", "\ub9e4\uac1c"),
    role_moderator = custom_model_canvas_text(language, "Moderator", "\uc870\uc808"),
    role_dependent = custom_model_canvas_text(language, "Dependent", "\uc885\uc18d"),
    role_covariate = custom_model_canvas_text(language, "Covariate", "\uacf5\ubcc0\ub7c9")
  )
}

custom_model_canvas_display_label <- function(name, variable_table = NULL, labels = character(0)) {
  data_label <- display_variable_name_static(name, variable_table, labels, label_only = TRUE)
  if (!nzchar(data_label)) name else data_label
}

custom_model_canvas_variable_items <- function(selected_names, variable_table = NULL, labels = character(0)) {
  selected_names <- as.character(selected_names %||% character(0))
  selected_names <- selected_names[nzchar(selected_names)]
  if (length(selected_names) == 0) {
    return(list())
  }

  measurements <- character(0)
  if (!is.null(variable_table) && all(c("name", "measurement") %in% names(variable_table))) {
    measurements <- stats::setNames(as.character(variable_table$measurement), as.character(variable_table$name))
  }

  lapply(selected_names, function(name) {
    list(
      name = name,
      dataLabel = custom_model_canvas_display_label(name, variable_table, labels),
      measurement = named_value(measurements, name, "")
    )
  })
}

custom_model_canvas_variable_panel <- function(items, language = statedu_initial_language()) {
  if (length(items) == 0) {
    return(div(
      class = "custom-model-empty-variable-list",
      custom_model_canvas_text(
        language,
        "Complete Step 2 in the Data tab before drawing a model.",
        "\ub370\uc774\ud130 \ud0ed\uc758 2\ub2e8\uacc4\ub97c \uba3c\uc800 \uc801\uc6a9\ud55c \ub2e4\uc74c \ubaa8\ub378\uc744 \uadf8\ub9b4 \uc218 \uc788\uc2b5\ub2c8\ub2e4."
      )
    ))
  }

  div(
    class = "custom-model-variable-panel-body",
    div(
      class = "custom-model-variable-list analysis-transfer-listbox",
      role = "listbox",
      tabindex = "0",
      lapply(items, function(item) {
        div(
          class = "custom-model-variable-item analysis-transfer-option",
          role = "option",
          draggable = "true",
          `data-variable-name` = item$name,
          `data-value` = item$name,
          `data-data-label` = item$dataLabel,
          `data-measurement` = item$measurement,
          measurement_symbol_tag(item$measurement),
          span(class = "analysis-transfer-option-label custom-model-variable-name", item$dataLabel)
        )
      })
    ),
    div(
      class = "custom-model-role-actions",
      div(class = "custom-model-role-title", custom_model_canvas_text(language, "Role", "\uc5ed\ud560")),
      tags$button(type = "button", class = "custom-model-role-button", `data-role` = "dependent", custom_model_canvas_text(language, "Dependent", "\uc885\uc18d")),
      tags$button(type = "button", class = "custom-model-role-button", `data-role` = "independent", custom_model_canvas_text(language, "Independent", "\ub3c5\ub9bd")),
      tags$button(type = "button", class = "custom-model-role-button", `data-role` = "mediator", custom_model_canvas_text(language, "Mediator", "\ub9e4\uac1c")),
      tags$button(type = "button", class = "custom-model-role-button", `data-role` = "moderator", custom_model_canvas_text(language, "Moderator", "\uc870\uc808")),
      tags$button(type = "button", class = "custom-model-role-button custom-model-role-button-covariate", `data-role` = "covariate", custom_model_canvas_text(language, "Covariate", "\uacf5\ubcc0\ub7c9"))
    )
  )
}

custom_model_canvas_button <- function(action, label, title = label, mode = FALSE, extra_class = "", icon = NULL) {
  tags$button(
    type = "button",
    class = paste("custom-model-toolbar-button", extra_class, if (isTRUE(mode)) "is-mode-button" else ""),
    `data-action` = action,
    title = title,
    span(class = "custom-model-toolbar-icon", icon),
    span(class = "custom-model-toolbar-label", label)
  )
}

custom_model_canvas_edge_shape_tools <- function(language = statedu_initial_language()) {
  div(
    class = "custom-model-edge-shape-tools",
    `data-edge-shape-tools` = "true",
    tags$button(type = "button", class = "custom-model-edge-shape-button", `data-edge-shape` = "straight", title = custom_model_canvas_text(language, "Straight", "\uc9c1\uc120"), "\u2500"),
    tags$button(type = "button", class = "custom-model-edge-shape-button", `data-edge-shape` = "curveUp", title = custom_model_canvas_text(language, "Curve up", "\uc704\ub85c \uace1\uc120"), "\u2322"),
    tags$button(type = "button", class = "custom-model-edge-shape-button", `data-edge-shape` = "curveDown", title = custom_model_canvas_text(language, "Curve down", "\uc544\ub798\ub85c \uace1\uc120"), "\u2323")
  )
}

custom_model_canvas_edge_anchor_tools <- function(language = statedu_initial_language()) {
  side_button <- function(endpoint, side, label, title) {
    tags$button(
      type = "button",
      class = "custom-model-edge-anchor-button",
      `data-edge-anchor-endpoint` = endpoint,
      `data-edge-anchor-side` = side,
      title = title,
      label
    )
  }
  div(
    class = "custom-model-edge-anchor-tools",
    `data-edge-anchor-tools` = "true",
    span(class = "custom-model-edge-anchor-label", custom_model_canvas_text(language, "Start", "\uc2dc\uc791")),
    side_button("from", "auto", "A", custom_model_canvas_text(language, "Start auto", "\uc2dc\uc791 \uc790\ub3d9")),
    side_button("from", "top", "\u2191", custom_model_canvas_text(language, "Start top", "\uc2dc\uc791 \uc704")),
    side_button("from", "right", "\u2192", custom_model_canvas_text(language, "Start right", "\uc2dc\uc791 \uc624\ub978\ucabd")),
    side_button("from", "bottom", "\u2193", custom_model_canvas_text(language, "Start bottom", "\uc2dc\uc791 \uc544\ub798")),
    side_button("from", "left", "\u2190", custom_model_canvas_text(language, "Start left", "\uc2dc\uc791 \uc67c\ucabd")),
    span(class = "custom-model-edge-anchor-label", custom_model_canvas_text(language, "End", "\ub05d")),
    side_button("to", "auto", "A", custom_model_canvas_text(language, "End auto", "\ub05d \uc790\ub3d9")),
    side_button("to", "top", "\u2191", custom_model_canvas_text(language, "End top", "\ub05d \uc704")),
    side_button("to", "right", "\u2192", custom_model_canvas_text(language, "End right", "\ub05d \uc624\ub978\ucabd")),
    side_button("to", "bottom", "\u2193", custom_model_canvas_text(language, "End bottom", "\ub05d \uc544\ub798")),
    side_button("to", "left", "\u2190", custom_model_canvas_text(language, "End left", "\ub05d \uc67c\ucabd"))
  )
}

custom_model_canvas_toolbar <- function(input = NULL, language = statedu_initial_language()) {
  group_tab <- function(group, label, active = FALSE) {
    tags$button(
      type = "button",
      class = paste("custom-model-toolbar-tab", if (isTRUE(active)) "is-active" else ""),
      `data-toolbar-group` = group,
      label
    )
  }

  group_panel <- function(group, ..., active = FALSE) {
    div(
      class = paste("custom-model-toolbar-panel", if (isTRUE(active)) "is-active" else ""),
      `data-toolbar-panel` = group,
      ...
    )
  }

  div(
    class = "custom-model-toolbar",
    div(
      class = "custom-model-toolbar-tabs",
      group_tab("file", custom_model_canvas_text(language, "File", "\ud30c\uc77c"), active = TRUE),
      group_tab("analysis", custom_model_canvas_text(language, "Analysis", "\ubd84\uc11d")),
      group_tab("tools", custom_model_canvas_text(language, "Tools", "\ub3c4\uad6c")),
      group_tab("view", custom_model_canvas_text(language, "View", "\ubcf4\uae30")),
      group_tab("result", custom_model_canvas_text(language, "Result", "\uacb0\uacfc"))
    ),
    div(
      class = "custom-model-toolbar-panels",
      group_panel(
        "file",
        custom_model_canvas_button("load", custom_model_canvas_text(language, "Load", "\ubd88\ub7ec\uc624\uae30")),
        custom_model_canvas_button("save", custom_model_canvas_text(language, "Save", "\uc800\uc7a5")),
        custom_model_canvas_button("export", custom_model_canvas_text(language, "Export", "\ub0b4\ubcf4\ub0b4\uae30")),
        active = TRUE
      ),
      group_panel(
        "analysis",
        custom_model_canvas_button("run", custom_model_canvas_text(language, "Run", "\uc2e4\ud589")),
        div(
          class = "custom-model-run-options-popover",
          div(class = "custom-model-run-options-title", custom_model_canvas_text(language, "Analysis options", "\ubd84\uc11d \uc635\uc158")),
          custom_model_canvas_analysis_options(input, language),
          div(
            class = "custom-model-run-options-actions",
            tags$button(type = "button", class = "btn btn-default btn-sm custom-model-run-options-cancel", `data-action` = "runCancel", custom_model_canvas_text(language, "Cancel", "\ucde8\uc18c")),
            tags$button(type = "button", class = "btn btn-primary btn-sm custom-model-run-options-confirm", `data-action` = "runConfirm", custom_model_canvas_text(language, "Run", "\uc2e4\ud589"))
          )
        )
      ),
      group_panel(
        "tools",
        custom_model_canvas_button("select", custom_model_canvas_text(language, "Select", "\uc120\ud0dd"), mode = TRUE),
        custom_model_canvas_button("connect", custom_model_canvas_text(language, "Connect", "\uc5f0\uacb0"), mode = TRUE),
        custom_model_canvas_button("delete", custom_model_canvas_text(language, "Delete", "\uc0ad\uc81c"), mode = TRUE, extra_class = "custom-model-delete-button"),
        custom_model_canvas_button("properties", custom_model_canvas_text(language, "Properties", "\uc18d\uc131"), mode = TRUE),
        custom_model_canvas_button("undo", custom_model_canvas_text(language, "Undo", "\uc2e4\ud589\ucde8\uc18c")),
        custom_model_canvas_button("redo", custom_model_canvas_text(language, "Redo", "\ub2e4\uc2dc\uc2e4\ud589")),
        custom_model_canvas_edge_shape_tools(language),
        custom_model_canvas_edge_anchor_tools(language)
      ),
      group_panel(
        "view",
        custom_model_canvas_button("zoomIn", custom_model_canvas_text(language, "Zoom in", "\ud655\ub300")),
        custom_model_canvas_button("zoomOut", custom_model_canvas_text(language, "Zoom out", "\ucd95\uc18c")),
        custom_model_canvas_button("fit", custom_model_canvas_text(language, "Fit", "\ud654\uba74\ub9de\ucda4")),
        custom_model_canvas_button("paper", custom_model_canvas_text(language, "Paper", "\uc6a9\uc9c0\uc124\uc815")),
        custom_model_canvas_button("grid", custom_model_canvas_text(language, "Grid", "\uaca9\uc790"), mode = TRUE),
        custom_model_canvas_button("autoAlign", custom_model_canvas_text(language, "Auto align", "\uc790\ub3d9 \ub9de\ucda4"), mode = TRUE),
        custom_model_canvas_button("style", custom_model_canvas_text(language, "Style", "\uc2a4\ud0c0\uc77c")),
        custom_model_canvas_button("reset", custom_model_canvas_text(language, "Reset", "\ucd08\uae30\ud654"), extra_class = "custom-model-reset-button"),
        div(
          class = "custom-model-reset-confirm-popover",
          div(class = "custom-model-reset-confirm-title", custom_model_canvas_text(language, "Reset model", "\ubaa8\ud615 \ucd08\uae30\ud654")),
          div(class = "custom-model-reset-confirm-message", custom_model_canvas_text(language, "Clear all boxes and arrows?", "\ubaa8\ub4e0 \ubc15\uc2a4\uc640 \ud654\uc0b4\ud45c\ub97c \ucd08\uae30\ud654\ud560\uae4c\uc694?")),
          div(
            class = "custom-model-reset-confirm-actions",
            tags$button(type = "button", class = "btn btn-default btn-sm", `data-action` = "resetCancel", custom_model_canvas_text(language, "Cancel", "\ucde8\uc18c")),
            tags$button(type = "button", class = "btn btn-warning btn-sm", `data-action` = "resetConfirm", custom_model_canvas_text(language, "Reset", "\ucd08\uae30\ud654"))
          )
        )
      ),
      group_panel(
        "result",
        custom_model_canvas_button("resultView", custom_model_canvas_text(language, "Result diagram", "\uacb0\uacfc \uadf8\ub9bc")),
        custom_model_canvas_button("resultEdit", custom_model_canvas_text(language, "Edit", "\ud3b8\uc9d1"), mode = TRUE),
        custom_model_canvas_button("dashNonsignificant", custom_model_canvas_text(language, "Non-significant dashed", "\uc720\uc758\ud558\uc9c0 \uc54a\uc740 \uacbd\ub85c \uc810\uc120"), mode = TRUE),
        custom_model_canvas_button("style", custom_model_canvas_text(language, "Style", "\uc2a4\ud0c0\uc77c")),
        custom_model_canvas_edge_shape_tools(language),
        custom_model_canvas_edge_anchor_tools(language)
      )
    )
  )
}

custom_model_canvas_edge_label_from_result <- function(result, equation, term, response = NULL) {
  custom_model_canvas_edge_info_from_result(result, equation, term, response = response)$label
}

custom_model_canvas_edge_info_from_result <- function(result, equation, term, response = NULL) {
  term <- as.character(term %||% "")[[1]]
  equation <- as.character(equation %||% "")[[1]]
  response <- as.character(response %||% "")[[1]]
  if (!nzchar(term) || !nzchar(equation)) {
    return(list(label = "", p = NA_real_, significant = FALSE, matched = FALSE))
  }
  for (path_result in result$path_results %||% list()) {
    if (!is.list(path_result)) next
    if (!identical(as.character(path_result$equation %||% "")[[1]], equation)) next
    if (nzchar(response)) {
      path_response <- tryCatch(all.vars(stats::formula(path_result$model))[[1]], error = function(e) "")
      if (!identical(path_response, response)) next
    }
    info <- mediation_moderation_path_coefficient_info(path_result, term)
    label <- as.character(info$label %||% "")[[1]]
    if (nzchar(label)) {
      info$matched <- TRUE
      return(info)
    }
  }
  list(label = "", p = NA_real_, significant = FALSE, matched = FALSE)
}

custom_model_canvas_result_edge_label <- function(result, from_node, to_node) {
  custom_model_canvas_result_edge_info(result, from_node, to_node)$label
}

custom_model_canvas_result_edge_info <- function(result, from_node, to_node) {
  from_role <- custom_model_canvas_record_value(from_node, "role", "")
  to_role <- custom_model_canvas_record_value(to_node, "role", "")
  from_var <- custom_model_canvas_node_variable(from_node)
  to_var <- custom_model_canvas_node_variable(to_node)
  if (identical(from_role, "independent") && identical(to_role, "mediator")) {
    return(custom_model_canvas_edge_info_from_result(result, paste("M model:", to_var), from_var, response = to_var))
  }
  if (identical(from_role, "mediator") && identical(to_role, "mediator")) {
    return(custom_model_canvas_edge_info_from_result(result, paste("M model:", to_var), from_var, response = to_var))
  }
  if (identical(from_role, "mediator") && identical(to_role, "dependent")) {
    return(custom_model_canvas_edge_info_from_result(result, "Y model", from_var, response = to_var))
  }
  if (identical(from_role, "independent") && identical(to_role, "dependent")) {
    return(custom_model_canvas_edge_info_from_result(result, "Y model", from_var, response = to_var))
  }
  list(label = "", p = NA_real_, significant = FALSE, matched = FALSE)
}

custom_model_canvas_result_moderation_label <- function(result, moderation, nodes, edge_by_id) {
  custom_model_canvas_result_moderation_info(result, moderation, nodes, edge_by_id)$label
}

custom_model_canvas_result_moderation_info <- function(result, moderation, nodes, edge_by_id) {
  source <- nodes[[custom_model_canvas_record_value(moderation, "from")]] %||% NULL
  target_edge <- edge_by_id[[custom_model_canvas_record_value(moderation, "toEdge")]] %||% NULL
  if (is.null(source) || is.null(target_edge)) {
    return(list(label = "", p = NA_real_, significant = FALSE, matched = FALSE))
  }
  from_node <- nodes[[custom_model_canvas_record_value(target_edge, "from")]] %||% NULL
  to_node <- nodes[[custom_model_canvas_record_value(target_edge, "to")]] %||% NULL
  if (is.null(from_node) || is.null(to_node)) {
    return(list(label = "", p = NA_real_, significant = FALSE, matched = FALSE))
  }
  moderator <- custom_model_canvas_node_variable(source)
  moderated_var <- custom_model_canvas_node_variable(from_node)
  from_role <- custom_model_canvas_record_value(from_node, "role", "")
  to_role <- custom_model_canvas_record_value(to_node, "role", "")
  if (identical(from_role, "independent") && identical(to_role, "mediator")) {
    return(custom_model_canvas_edge_info_from_result(
      result,
      paste("M model:", custom_model_canvas_node_variable(to_node)),
      paste0(moderated_var, ":", moderator),
      response = custom_model_canvas_node_variable(to_node)
    ))
  }
  if (identical(from_role, "mediator") && identical(to_role, "dependent")) {
    return(custom_model_canvas_edge_info_from_result(result, "Y model", paste0(moderated_var, ":", moderator), response = custom_model_canvas_node_variable(to_node)))
  }
  if (identical(from_role, "independent") && identical(to_role, "dependent")) {
    return(custom_model_canvas_edge_info_from_result(result, "Y model", paste0(moderated_var, ":", moderator), response = custom_model_canvas_node_variable(to_node)))
  }
  list(label = "", p = NA_real_, significant = FALSE, matched = FALSE)
}

custom_model_canvas_result_snapshot <- function(snapshot, result) {
  snapshot <- snapshot %||% list()
  snapshot$nonce <- NULL
  style <- snapshot$style %||% list()
  label_size <- custom_model_canvas_numeric_value(style$labelFontSize %||% style$fontSize, 12)
  if (is.na(label_size)) label_size <- 12
  nodes <- custom_model_canvas_records(snapshot$nodes)
  node_ids <- vapply(nodes, custom_model_canvas_record_value, character(1), key = "id")
  names(nodes) <- node_ids
  edges <- custom_model_canvas_records(snapshot$edges)
  edge_ids <- vapply(edges, custom_model_canvas_record_value, character(1), key = "id")
  names(edges) <- edge_ids
  edge_by_id <- edges

  edges <- lapply(edges, function(edge) {
    from_node <- nodes[[custom_model_canvas_record_value(edge, "from")]] %||% NULL
    to_node <- nodes[[custom_model_canvas_record_value(edge, "to")]] %||% NULL
    info <- if (is.null(from_node) || is.null(to_node)) {
      list(label = "", p = NA_real_, significant = FALSE, matched = FALSE)
    } else {
      custom_model_canvas_result_edge_info(result, from_node, to_node)
    }
    edge$label <- as.character(info$label %||% "")[[1]]
    edge$p <- custom_model_canvas_numeric_value(info$p, NA_real_)
    edge$significant <- isTRUE(info$significant) && nzchar(edge$label)
    edge$resultMatched <- isTRUE(info$matched)
    edge$labelPosition <- custom_model_canvas_numeric_value(edge$labelPosition, 50)
    if (is.na(edge$labelPosition)) edge$labelPosition <- 50
    edge$labelOffsetX <- custom_model_canvas_numeric_value(edge$labelOffsetX, 0)
    if (is.na(edge$labelOffsetX)) edge$labelOffsetX <- 0
    edge$labelOffsetY <- custom_model_canvas_numeric_value(edge$labelOffsetY, -10)
    if (is.na(edge$labelOffsetY)) edge$labelOffsetY <- -10
    edge$labelFontSize <- custom_model_canvas_numeric_value(edge$labelFontSize, label_size)
    if (is.na(edge$labelFontSize)) edge$labelFontSize <- label_size
    edge
  })

  moderations <- custom_model_canvas_records(snapshot$moderations)
  moderations <- lapply(moderations, function(moderation) {
    info <- custom_model_canvas_result_moderation_info(result, moderation, nodes, edge_by_id)
    moderation$label <- as.character(info$label %||% "")[[1]]
    moderation$p <- custom_model_canvas_numeric_value(info$p, NA_real_)
    moderation$significant <- isTRUE(info$significant) && nzchar(moderation$label)
    moderation$resultMatched <- isTRUE(info$matched)
    moderation$labelOffsetX <- custom_model_canvas_numeric_value(moderation$labelOffsetX, 0)
    if (is.na(moderation$labelOffsetX)) moderation$labelOffsetX <- 0
    moderation$labelOffsetY <- custom_model_canvas_numeric_value(moderation$labelOffsetY, -10)
    if (is.na(moderation$labelOffsetY)) moderation$labelOffsetY <- -10
    moderation$labelFontSize <- custom_model_canvas_numeric_value(moderation$labelFontSize, label_size)
    if (is.na(moderation$labelFontSize)) moderation$labelFontSize <- label_size
    moderation
  })

  snapshot$edges <- unname(edges)
  snapshot$moderations <- unname(moderations)
  snapshot$dashNonsignificant <- isTRUE(snapshot$dashNonsignificant %||% TRUE)
  snapshot
}

custom_model_canvas_analysis_options <- function(input = NULL, language = statedu_initial_language()) {
  bootstrap_choices <- bootstrap_resample_choices(language)
  options_tab <- if (!is.null(input)) isolate(input$custom_mm_options_tab) else NULL
  options_tab <- if (as.character(options_tab %||% "Model") %in% c("Model", "Bootstrap", "Output")) as.character(options_tab %||% "Model") else "Model"
  output_table_style <- analysis_output_table_style(if (!is.null(input)) isolate(input$custom_mm_output_table_style) else NULL)
  div(
    class = "custom-model-analysis-options analysis-options-column",
    analysis_options_tabs_panel(
      id = "custom_mm_options_tab",
      selected = options_tab,
      class = "mm-model-panel custom-mm-options",
      tabPanel(
        analysis_ui_text("Model", language),
        value = "Model",
        div(
          class = "factor-options-tab-content regression-options-tab-content mm-options-tab-content",
          div(
            class = "analysis-option-group",
            div(class = "analysis-option-title", mediation_moderation_text(language, "Analysis method", "\ubd84\uc11d \ubc29\ubc95")),
            selectInput(
              "custom_mm_analysis_method",
              NULL,
              choices = mediation_moderation_analysis_method_choices(language),
              selected = mediation_moderation_scalar_choice(
                if (!is.null(input)) isolate(input$custom_mm_analysis_method) else NULL,
                "statedu",
                c("statedu", "process_ols")
              ),
              selectize = FALSE
            )
          ),
          analysis_option_group(
            "Residual diagnostics",
            list(
              list(
                id = "custom_mm_residual_diagnostics",
                label = "Residual diagnostics",
                value = isTRUE(if (!is.null(input)) isolate(input$custom_mm_residual_diagnostics %||% TRUE) else TRUE)
              ),
              list(
                id = "custom_mm_auto_method",
                label = "Automatic method selection",
                value = isTRUE(if (!is.null(input)) isolate(input$custom_mm_auto_method %||% TRUE) else TRUE) &&
                  isTRUE(if (!is.null(input)) isolate(input$custom_mm_residual_diagnostics %||% TRUE) else TRUE),
                disabled = !isTRUE(if (!is.null(input)) isolate(input$custom_mm_residual_diagnostics %||% TRUE) else TRUE)
              )
            ),
            language = language
          ),
          analysis_option_group(
            "Effect size",
            list(
              list(
                id = "custom_mm_effect_size_y",
                label = "Y model",
                value = isTRUE(if (!is.null(input)) isolate(input$custom_mm_effect_size_y %||% TRUE) else TRUE)
              ),
              list(
                id = "custom_mm_effect_size_m",
                label = "M model",
                value = isTRUE(if (!is.null(input)) isolate(input$custom_mm_effect_size_m %||% FALSE) else FALSE)
              )
            ),
            language = language
          ),
          analysis_option_group(
            "Covariate control",
            list(
              list(
                id = "custom_mm_covariate_control_y",
                label = mediation_moderation_text(language, "Dependent variable", "\uc885\uc18d\ubcc0\uc218"),
                value = isTRUE(if (!is.null(input)) isolate(input$custom_mm_covariate_control_y %||% TRUE) else TRUE)
              ),
              list(
                id = "custom_mm_covariate_control_m",
                label = mediation_moderation_text(language, "Mediator variable", "\ub9e4\uac1c\ubcc0\uc218"),
                value = isTRUE(if (!is.null(input)) isolate(input$custom_mm_covariate_control_m %||% TRUE) else TRUE)
              )
            ),
            language = language
          )
        )
      ),
      tabPanel(
        analysis_ui_text("Bootstrap", language),
        value = "Bootstrap",
        div(
          class = "factor-options-tab-content regression-options-tab-content mm-options-tab-content",
          div(
            class = "analysis-option-group",
            selectInput(
              "custom_mm_boot_r",
              analysis_ui_text("Number of bootstrap samples", language),
              choices = bootstrap_choices,
              selected = mediation_moderation_scalar_choice(
                if (!is.null(input)) isolate(input$custom_mm_boot_r) else NULL,
                "5000",
                unname(bootstrap_choices)
              ),
              selectize = FALSE
            ),
            numericInput(
              "custom_mm_seed",
              analysis_ui_text("Seed number", language),
              value = mediation_moderation_numeric_choice(if (!is.null(input)) isolate(input$custom_mm_seed) else NULL, default_seed()),
              min = 1,
              step = 1
            ),
            selectInput(
              "custom_mm_ci_method",
              mediation_moderation_text(language, "Bootstrap CI method", "Bootstrap CI \ubc29\uc2dd"),
              choices = mediation_moderation_ci_method_choices(language),
              selected = mediation_moderation_scalar_choice(
                if (!is.null(input)) isolate(input$custom_mm_ci_method) else NULL,
                "bias_corrected",
                c("bias_corrected", "percentile")
              ),
              selectize = FALSE
            )
          )
        )
      ),
      tabPanel(
        analysis_ui_text("Output", language),
        value = "Output",
        div(
          class = "factor-options-tab-content regression-options-tab-content mm-options-tab-content",
          analysis_output_table_style_tabs("custom_mm_output_table_style", output_table_style, language)
        )
      )
    )
  )
}

custom_model_canvas_analysis_options_modal <- function(input = NULL, language = statedu_initial_language()) {
  modalDialog(
    title = custom_model_canvas_text(language, "Analysis options", "\ubd84\uc11d \uc635\uc158"),
    div(
      class = "custom-model-analysis-options-modal",
      custom_model_canvas_analysis_options(input, language)
    ),
    footer = tagList(
      modalButton(custom_model_canvas_text(language, "Cancel", "\ucde8\uc18c")),
      actionButton("custom_model_canvas_run_confirm", custom_model_canvas_text(language, "Run", "\uc2e4\ud589"), class = "btn btn-primary")
    ),
    easyClose = TRUE
  )
}

custom_model_canvas_workspace <- function(selected_names, variable_table = NULL, labels = character(0), input = NULL, language = statedu_initial_language()) {
  items <- custom_model_canvas_variable_items(selected_names, variable_table, labels)
  variables_json <- htmltools::htmlEscape(
    jsonlite::toJSON(items, auto_unbox = TRUE, null = "null"),
    attribute = TRUE
  )
  i18n_json <- htmltools::htmlEscape(
    jsonlite::toJSON(custom_model_canvas_i18n(language), auto_unbox = TRUE, null = "null"),
    attribute = TRUE
  )

  tagList(
    div(
      id = "custom-model-canvas-root",
      class = "custom-model-canvas-root",
      `data-variables` = variables_json,
      `data-language` = normalize_app_language(language),
      `data-i18n` = i18n_json,
      div(
        class = "custom-model-variable-panel analysis-transfer-column analysis-transfer-panel",
        analysis_field_label_tag("Variables", language = language),
        custom_model_canvas_variable_panel(items, language)
      ),
      div(
        class = "custom-model-diagram-panel",
        custom_model_canvas_toolbar(input, language),
        div(
          class = "custom-model-statusbar",
          span(class = "custom-model-mode-status", custom_model_canvas_text(language, "Mode: Select", "\ubaa8\ub4dc: \uc120\ud0dd")),
          span(class = "custom-model-paper-status", "B5 landscape"),
          span(class = "custom-model-covariate-status", custom_model_canvas_text(language, "Covariates: none", "\uacf5\ubcc0\ub7c9: \uc5c6\uc74c"))
        ),
        div(
          class = "custom-model-canvas-scroll",
          div(
            class = "custom-model-paper is-grid-visible",
            `data-width` = "971",
            `data-height` = "688",
            tags$svg(class = "custom-model-edge-layer", width = "971", height = "688"),
            div(class = "custom-model-node-layer")
          )
        )
      )
    ),
    tags$script(HTML("window.StatEduModelCanvas && window.StatEduModelCanvas.canvas && window.StatEduModelCanvas.canvas.initAll();"))
  )
}

structural_measurement_icon <- function(kind) {
  marker_id <- paste0("sem-arrow-", kind)
  svg <- function(...) tags$svg(class = "structural-measurement-svg", viewBox = "0 0 68 42", `aria-hidden` = "true", ...)
  marker <- tags$defs(tags$marker(id = marker_id, viewBox = "0 0 6 6", refX = "5", refY = "3", markerWidth = "5", markerHeight = "5", orient = "auto", tags$path(d = "M0,0 L6,3 L0,6 Z", fill = "currentColor")))
  line <- function(x1, y1, x2, y2, arrow = FALSE) tags$line(x1 = x1, y1 = y1, x2 = x2, y2 = y2, `marker-end` = if (arrow) paste0("url(#", marker_id, ")") else NULL)
  box <- function(x, y) tags$rect(x = x, y = y, width = "17", height = "7", rx = "1")
  latent <- function(cx, cy) tags$ellipse(cx = cx, cy = cy, rx = "10", ry = "7")
  if (kind == "left") return(svg(marker, latent(55, 21), box(3, 5), box(3, 18), box(3, 31), line(20, 8.5, 45, 18), line(20, 21.5, 45, 21), line(20, 34.5, 45, 24)))
  if (kind == "right") return(svg(marker, latent(13, 21), box(48, 5), box(48, 18), box(48, 31), line(23, 18, 48, 8.5), line(23, 21, 48, 21.5), line(23, 24, 48, 34.5)))
  if (kind == "top") return(svg(marker, latent(34, 34), box(3, 3), box(25.5, 3), box(48, 3), line(29, 28, 11.5, 10), line(34, 27, 34, 10), line(39, 28, 56.5, 10)))
  if (kind == "bottom") return(svg(marker, latent(34, 8), box(3, 32), box(25.5, 32), box(48, 32), line(29, 14, 11.5, 32), line(34, 15, 34, 32), line(39, 14, 56.5, 32)))
  reflective <- identical(kind, "reflective")
  svg(marker, latent(13, 21), box(48, 5), box(48, 18), box(48, 31),
      line(if (reflective) 23 else 48, if (reflective) 18 else 8.5, if (reflective) 48 else 23, if (reflective) 8.5 else 18, TRUE),
      line(if (reflective) 23 else 48, 21, if (reflective) 48 else 23, 21, TRUE),
      line(if (reflective) 23 else 48, if (reflective) 24 else 34.5, if (reflective) 48 else 23, if (reflective) 34.5 else 24, TRUE))
}

structural_file_icon <- function(kind) {
  svg <- function(...) tags$svg(
    class = "structural-common-toolbar-svg",
    viewBox = "0 0 24 24",
    fill = "none",
    stroke = "currentColor",
    `stroke-width` = "1.8",
    `stroke-linecap` = "round",
    `stroke-linejoin` = "round",
    `aria-hidden` = "true",
    ...
  )
  if (identical(kind, "load")) {
    return(svg(
      tags$path(d = "M3 7.5h6l2-2h4.5a2 2 0 0 1 2 2v1"),
      tags$path(d = "M3.5 9.5h17l-2.2 9H5.7z"),
      tags$path(d = "M12 17v-5"),
      tags$path(d = "m9.8 14.2 2.2-2.2 2.2 2.2")
    ))
  }
  if (identical(kind, "save")) {
    return(svg(
      tags$path(d = "M4 3.5h13l3 3V20H4z"),
      tags$path(d = "M8 3.5v6h8v-6"),
      tags$rect(x = "7", y = "13", width = "10", height = "7", rx = "1")
    ))
  }
  svg(
    tags$path(d = "M4 7h10"), tags$circle(cx = "17", cy = "7", r = "2"), tags$path(d = "M19 7h1"),
    tags$path(d = "M4 12h3"), tags$circle(cx = "10", cy = "12", r = "2"), tags$path(d = "M12 12h8"),
    tags$path(d = "M4 17h8"), tags$circle(cx = "15", cy = "17", r = "2"), tags$path(d = "M17 17h3")
  )
}

structural_equation_variable_panel <- function(items, language = statedu_initial_language()) {
  list_ui <- custom_model_canvas_variable_panel(items, language)
  if (length(items) > 0) list_ui$children[[2]] <- NULL
  tagList(
    list_ui,
    div(
      class = "structural-selection-settings",
      div(class = "structural-selection-settings-body", if (identical(normalize_app_language(language), "ko")) "캔버스의 변수를 선택하세요." else "Select a variable on the canvas.")
    )
  )
}

structural_equation_title <- function(language = statedu_initial_language()) {
  if (identical(normalize_app_language(language), "ko")) "구조방정식" else "Structural Equation Modeling"
}

structural_analysis_title <- function(analysis_type = "cbsem", language = statedu_initial_language()) {
  ko <- identical(normalize_app_language(language), "ko")
  switch(
    analysis_type,
    cfa = if (ko) "확인적 요인분석" else "Confirmatory Factor Analysis",
    plssem = if (ko) "PLS 구조방정식" else "PLS Structural Equation Modeling",
    if (ko) "구조방정식" else "Structural Equation Modeling"
  )
}

structural_analysis_prefix <- function(analysis_type = "cbsem") {
  paste0("structural_", analysis_type)
}

structural_capture_truthy <- function(value) {
  tolower(trimws(as.character(value %||% ""))) %in% c("1", "true", "yes", "y", "run", "auto")
}

structural_capture_initial_snapshot <- function(analysis_type = "cbsem") {
  if (!identical(analysis_type, "cfa")) return(list(snapshot = NULL, auto_run = FALSE))
  candidates <- c(
    Sys.getenv("STATEDU_CAPTURE_CFA_MODEL_FILE", ""),
    Sys.getenv("STATEDU_CAPTURE_STRUCTURAL_MODEL_FILE", "")
  )
  candidates <- candidates[nzchar(candidates)]
  path <- if (length(candidates)) candidates[[1L]] else ""
  if (!nzchar(path)) return(list(snapshot = NULL, auto_run = FALSE))
  if (!file.exists(path)) {
    warning(sprintf("CFA capture model file does not exist: %s", path), call. = FALSE)
    return(list(snapshot = NULL, auto_run = FALSE))
  }
  snapshot <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(error) {
      warning(sprintf("Failed to read CFA capture model file: %s", conditionMessage(error)), call. = FALSE)
      NULL
    }
  )
  if (!is.list(snapshot) || !is.list(snapshot$nodes) || !is.list(snapshot$edges)) {
    warning("CFA capture model file must contain model nodes and edges.", call. = FALSE)
    return(list(snapshot = NULL, auto_run = FALSE))
  }
  list(
    snapshot = snapshot,
    auto_run = structural_capture_truthy(Sys.getenv("STATEDU_CAPTURE_CFA_RUN", ""))
  )
}

structural_analysis_package <- function(analysis_type = "cbsem") {
  if (identical(analysis_type, "plssem")) "seminr" else "lavaan"
}

structural_equation_toolbar <- function(analysis_type = "cbsem", language = statedu_initial_language()) {
  ko <- identical(normalize_app_language(language), "ko")
  div(
    class = "custom-model-toolbar",
    div(
      class = "custom-model-toolbar-panel is-active",
      `data-toolbar-panel` = "tools",
      div(
        class = "structural-primary-toolbar-tools",
      custom_model_canvas_button("load", if (ko) "모형 불러오기" else "Load model", title = if (ko) "저장한 모형 불러오기" else "Load a saved model", icon = structural_file_icon("load")),
      custom_model_canvas_button("save", if (ko) "모형 저장" else "Save model", title = if (ko) "현재 모형 저장하기" else "Save the current model", icon = structural_file_icon("save")),
      custom_model_canvas_button("export", if (ko) "모형 내보내기" else "Export model"),
      if (!identical(analysis_type, "cfa")) tagList(
        tags$button(
          type = "button",
          class = "custom-model-toolbar-button structural-covariate-toolbar-button",
          `data-role` = "covariate",
          title = if (ko) "선택한 관측변수를 공변량으로 지정" else "Assign selected variables as covariates",
          span(class = "custom-model-toolbar-icon", "C"),
          span(class = "custom-model-toolbar-label", if (ko) "공변량 지정" else "Assign covariate")
        ),
        custom_model_canvas_button("structuralCovariateTargets", if (ko) "공변량 설정" else "Covariate targets", title = if (ko) "공변량별 통제 대상 설정" else "Set control targets for each covariate", icon = structural_file_icon("settings"))
      ),
      custom_model_canvas_button("addLatent", if (ko) "잠재변수" else "Latent variable", extra_class = "structural-add-latent"),
      custom_model_canvas_button("select", custom_model_canvas_text(language, "Select", "선택"), mode = TRUE),
      if (identical(analysis_type, "cfa")) custom_model_canvas_button("flipCfa", if (ko) "좌우 반전" else "Flip sides", title = if (ko) "잠재변수와 측정변수 좌우 반전" else "Flip latent variables and indicators", mode = TRUE),
      custom_model_canvas_button("connect", custom_model_canvas_text(language, "Connect", "연결"), mode = TRUE),
      custom_model_canvas_button("covariance", if (ko) "공분산" else "Covariance", title = if (ko) "공분산 연결" else "Draw covariance", mode = TRUE),
      custom_model_canvas_button("properties", custom_model_canvas_text(language, "Properties", "속성"), mode = TRUE),
      custom_model_canvas_button("detachIndicator", if (ko) "지표 분리" else "Detach indicator", title = if (ko) "선택 측정변수를 잠재변수에서 분리" else "Detach selected indicator"),
      custom_model_canvas_button("indicatorUp", if (ko) "지표 앞으로" else "Indicator up", title = if (ko) "측정변수 순서를 앞으로" else "Move indicator earlier"),
      custom_model_canvas_button("indicatorDown", if (ko) "지표 뒤로" else "Indicator down", title = if (ko) "측정변수 순서를 뒤로" else "Move indicator later"),
      custom_model_canvas_button("alignLeft", if (ko) "왼쪽 정렬" else "Align left", title = if (ko) "선택 항목 왼쪽 정렬" else "Align selected left"),
      custom_model_canvas_button("alignTop", if (ko) "위 정렬" else "Align top", title = if (ko) "선택 항목 위 정렬" else "Align selected top"),
      custom_model_canvas_button("alignCenter", if (ko) "가운데 정렬" else "Center", title = if (ko) "선택 항목 가로 중앙 정렬" else "Align horizontal centers"),
      custom_model_canvas_button("alignMiddle", if (ko) "세로 중앙" else "Middle", title = if (ko) "선택 항목 세로 중앙 정렬" else "Align vertical centers"),
      custom_model_canvas_button("distributeH", if (ko) "가로 분배" else "Distribute", title = if (ko) "선택 항목 가로 균등 배치" else "Distribute horizontally"),
      custom_model_canvas_button("distributeV", if (ko) "세로 분배" else "Distribute vertical", title = if (ko) "선택 항목 세로 균등 배치" else "Distribute vertically"),
      custom_model_canvas_button("delete", custom_model_canvas_text(language, "Delete", "삭제"), mode = TRUE),
      custom_model_canvas_button("undo", custom_model_canvas_text(language, "Undo", "실행 취소")),
      custom_model_canvas_button("redo", custom_model_canvas_text(language, "Redo", "다시 실행")),
      custom_model_canvas_button("grid", custom_model_canvas_text(language, "Grid", "격자")),
      custom_model_canvas_button("zoomIn", if (ko) "모형 확대" else "Zoom in", title = if (ko) "캔버스 안의 모형 확대" else "Zoom model in"),
      custom_model_canvas_button("zoomOut", if (ko) "모형 축소" else "Zoom out", title = if (ko) "캔버스 안의 모형 축소" else "Zoom model out"),
      custom_model_canvas_button("fit", custom_model_canvas_text(language, "Fit", "화면 맞춤")),
      custom_model_canvas_button("reset", if (ko) "모형 초기화" else "Reset model", title = if (ko) "캔버스의 모형 전체 초기화" else "Clear the entire canvas model", extra_class = "custom-model-reset-button"),
      div(
        class = "custom-model-reset-confirm-popover",
        div(class = "custom-model-reset-confirm-title", if (ko) "모형 초기화" else "Reset model"),
        div(class = "custom-model-reset-confirm-message", if (ko) "모든 변수와 연결선을 초기화할까요?" else "Clear all variables and paths?"),
        div(
          class = "custom-model-reset-confirm-actions",
          tags$button(type = "button", class = "btn btn-default btn-sm", `data-action` = "resetCancel", if (ko) "취소" else "Cancel"),
          tags$button(type = "button", class = "btn btn-warning btn-sm", `data-action` = "resetConfirm", if (ko) "초기화" else "Reset")
        )
      ),
      custom_model_canvas_button("run", if (ko) "분석 실행" else "Run analysis", title = if (ko) "현재 모형 분석 실행" else "Run the current model"),
      div(
        class = "custom-model-run-options-popover structural-run-options-popover",
        div(class = "custom-model-run-options-title", if (ko) "분석 옵션" else "Analysis options"),
        div(
          class = "custom-model-analysis-options structural-run-options-tabs analysis-tabbed-options",
          tabsetPanel(
            type = "tabs",
            tabPanel(
              if (ko) "추정" else "Estimation",
          selectInput(paste0(structural_analysis_prefix(analysis_type), "_estimator"), if (ko) "추정 방법" else "Estimator", choices = if (analysis_type == "plssem") c("PLS" = "PLS") else c("ML" = "ML", "MLR" = "MLR", "WLSMV" = "WLSMV")),
          if (analysis_type != "plssem") selectInput(paste0(structural_analysis_prefix(analysis_type), "_missing"), if (ko) "결측치 처리" else "Missing data", choices = stats::setNames(c("fiml", "listwise"), c("FIML", if (ko) "목록 삭제" else "Listwise deletion"))),
          if (analysis_type != "plssem") selectInput(paste0(structural_analysis_prefix(analysis_type), "_scale"), if (ko) "잠재변수 스케일" else "Latent scale", choices = stats::setNames(c("marker", "variance"), c(if (ko) "첫 지표 부하량 = 1" else "Marker loading = 1", if (ko) "잠재변수 분산 = 1" else "Latent variance = 1"))),
          if (analysis_type != "plssem") selectInput(paste0(structural_analysis_prefix(analysis_type), "_rmsea_ci"), if (ko) "RMSEA 신뢰수준" else "RMSEA confidence level", choices = c("90% CI" = "0.90", "95% CI" = "0.95", "99% CI" = "0.99"), selected = "0.90"),
          if (identical(analysis_type, "cfa")) checkboxInput(paste0(structural_analysis_prefix(analysis_type), "_invariance_enabled"), if (ko) "측정불변성 분석" else "Measurement invariance analysis", value = FALSE),
          if (identical(analysis_type, "cfa")) selectInput(paste0(structural_analysis_prefix(analysis_type), "_invariance_group"), if (ko) "집단변수" else "Grouping variable", choices = character(0)),
            ),
            tabPanel(
              if (ko) "타당도" else "Validity",
          if (analysis_type != "plssem") selectInput(
            paste0(structural_analysis_prefix(analysis_type), "_validity_formula"),
            if (ko) "AVE·CR 계산 방식" else "AVE/CR formula",
            choices = stats::setNames(c("standardized", "model_implied"), c(if (ko) "표준화 부하량(Fornell-Larcker)" else "Standardized loadings (Fornell-Larcker)", if (ko) "모형모수 방식(Raykov 계열)" else "Model-implied parameters (Raykov)")),
            selected = "standardized"
          ),
          if (identical(analysis_type, "cfa")) selectInput(
            paste0(structural_analysis_prefix(analysis_type), "_reliability_bootstrap"),
            if (ko) "AVE·신뢰도 bootstrap CI" else "AVE/reliability bootstrap CI",
            choices = c("Do not compute" = "0", "500 resamples" = "500", "1,000 resamples" = "1000", "2,000 resamples" = "2000"), selected = "0"
          ),
          if (identical(analysis_type, "cfa")) numericInput(
            paste0(structural_analysis_prefix(analysis_type), "_reliability_seed"),
            if (ko) "AVE·신뢰도 bootstrap seed" else "AVE/reliability bootstrap seed",
            value = 24680L, min = 1L, step = 1L
          ),
          if (identical(analysis_type, "cfa")) selectInput(
            paste0(structural_analysis_prefix(analysis_type), "_reliability_ci_method"),
            if (ko) "AVE/reliability CI method" else "AVE/reliability CI method",
            choices = c("Percentile" = "percentile", "BCa (slower)" = "bca"),
            selected = "percentile"
          ),
          if (identical(analysis_type, "cfa")) selectInput(
            paste0(structural_analysis_prefix(analysis_type), "_bollen_stine_bootstrap"),
            if (ko) "Bollen-Stine 전체 적합도 bootstrap" else "Bollen-Stine global-fit bootstrap",
            choices = c("Do not compute" = "0", "500 resamples" = "500", "1,000 resamples" = "1000", "2,000 resamples" = "2000"), selected = "0"
          ),
          if (identical(analysis_type, "cfa")) numericInput(
            paste0(structural_analysis_prefix(analysis_type), "_bollen_stine_seed"),
            if (ko) "Bollen-Stine bootstrap seed" else "Bollen-Stine bootstrap seed",
            value = 97531L, min = 1L, step = 1L
          ),
          if (identical(analysis_type, "cfa")) tags$p(class = "structural-option-note", if (ko) "Bollen-Stine 검정은 결측이 없는 연속형 단일집단 ML CFA에서만 실행됩니다." else "Bollen-Stine is available only for complete continuous single-group CFA estimated with ML."),
          if (identical(analysis_type, "cfa")) checkboxInput(paste0(structural_analysis_prefix(analysis_type), "_mi_holdout_enabled"), if (ko) "MI 탐색·검증 표본분할" else "MI exploration/validation split", value = FALSE),
          if (identical(analysis_type, "cfa")) selectInput(paste0(structural_analysis_prefix(analysis_type), "_mi_holdout_fraction"), if (ko) "검증표본 비율" else "Validation-sample fraction", choices = c("20%" = "0.20", "30%" = "0.30", "40%" = "0.40"), selected = "0.30"),
          if (identical(analysis_type, "cfa")) numericInput(paste0(structural_analysis_prefix(analysis_type), "_mi_holdout_seed"), if (ko) "표본분할 seed" else "Sample-split seed", value = 13579L, min = 1L, step = 1L),
          if (identical(analysis_type, "cfa")) tags$p(class = "structural-option-note", if (ko) "MI 표본분할은 연속형 ML/MLR CFA 전용이며 측정불변성 또는 Heywood 제약 재분석과 동시에 사용할 수 없습니다." else "MI splitting is for continuous ML/MLR CFA and cannot be combined with measurement invariance or Heywood-constrained reanalysis."),
            ),
            tabPanel(
              if (ko) "진단" else "Diagnostics",
          if (analysis_type != "plssem") selectInput(
            paste0(structural_analysis_prefix(analysis_type), "_htmt_threshold"),
            if (ko) "HTMT 기준" else "HTMT threshold",
            choices = c("Strict (.85)" = "0.85", "Lenient (.90)" = "0.90"),
            selected = "0.85"
          ),
          if (analysis_type != "plssem") selectInput(
            paste0(structural_analysis_prefix(analysis_type), "_htmt_bootstrap"),
            if (ko) "HTMT 부트스트랩 CI" else "HTMT bootstrap CI",
            choices = c("Do not compute" = "0", "500 resamples" = "500", "1,000 resamples" = "1000", "2,000 resamples" = "2000"),
            selected = "0"
          ),
          if (analysis_type != "plssem") numericInput(
            paste0(structural_analysis_prefix(analysis_type), "_htmt_seed"),
            if (ko) "HTMT 부트스트랩 seed" else "HTMT bootstrap seed",
            value = 12345L, min = 1L, step = 1L
          ),
          if (analysis_type != "plssem") selectInput(
            paste0(structural_analysis_prefix(analysis_type), "_htmt_ci_method"),
            if (ko) "HTMT CI method" else "HTMT CI method",
            choices = c("Percentile" = "percentile", "BCa (slower)" = "bca"),
            selected = "percentile"
          ),
          if (analysis_type != "plssem") selectInput(
            paste0(structural_analysis_prefix(analysis_type), "_mi_mode"),
            if (ko) "MI 출력 기준" else "MI output method",
            choices = stats::setNames(
              c("theory", "conventional"),
              c(if (ko) "이론적 허용 MI + 누적 적합도" else "Theory-allowed MI with cumulative fit", if (ko) "일반 프로그램 방식(전체 MI)" else "Conventional output (all MI)")
            ),
            selected = "theory"
          ),
          selectInput(
            paste0(structural_analysis_prefix(analysis_type), "_result_coefficient"),
            if (ko) "결과 모형 계수" else "Result diagram coefficient",
            choices = c("beta(p)" = "beta", "B(p)" = "b"),
            selected = "beta"
          )
            )
          )
        ),
        div(
          class = "custom-model-run-options-actions",
          tags$button(type = "button", class = "btn btn-default btn-sm", `data-action` = "runCancel", if (ko) "취소" else "Cancel"),
          tags$button(type = "button", class = "btn btn-primary btn-sm", `data-action` = "runConfirm", if (ko) "실행" else "Run")
        )
      )
      ),
      if (!identical(analysis_type, "cfa")) div(
        class = "structural-advanced-analysis-tools",
        custom_model_canvas_button("multiGroup", if (ko) "다집단 분석" else "Multigroup", title = if (ko) "다집단 분석 설정" else "Multigroup analysis settings"),
        custom_model_canvas_button("moderator", if (ko) "조절변수" else "Moderator", title = if (ko) "조절효과 설정" else "Moderation settings")
      ),
      if (!identical(analysis_type, "cfa")) div(
        class = "structural-latent-tools",
        span(class = "structural-latent-tools-label", if (ko) "측정모형" else "Measurement"),
        custom_model_canvas_button("placementLeft", if (ko) "왼쪽" else "Left", title = if (ko) "측정변수를 왼쪽으로" else "Indicators left", icon = structural_measurement_icon("left")),
        custom_model_canvas_button("placementRight", if (ko) "오른쪽" else "Right", title = if (ko) "측정변수를 오른쪽으로" else "Indicators right", icon = structural_measurement_icon("right")),
        custom_model_canvas_button("placementTop", if (ko) "위" else "Top", title = if (ko) "측정변수를 위로" else "Indicators above", icon = structural_measurement_icon("top")),
        custom_model_canvas_button("placementBottom", if (ko) "아래" else "Bottom", title = if (ko) "측정변수를 아래로" else "Indicators below", icon = structural_measurement_icon("bottom")),
        if (identical(analysis_type, "plssem")) tagList(
          span(class = "structural-toolbar-separator"),
          custom_model_canvas_button("reflective", if (ko) "반영지표" else "Reflective", title = if (ko) "반영지표: 잠재변수 → 측정변수" else "Reflective measurement", icon = structural_measurement_icon("reflective")),
          custom_model_canvas_button("formative", if (ko) "형성지표" else "Formative", title = if (ko) "형성지표: 측정변수 → 잠재변수" else "Formative measurement", icon = structural_measurement_icon("formative"))
        )
      ),
      custom_model_canvas_edge_shape_tools(language),
      custom_model_canvas_edge_anchor_tools(language),
      div(
        class = "structural-result-tools",
        custom_model_canvas_button("resultView", if (ko) "결과 모형" else "Result diagram"),
        custom_model_canvas_button("resultEdit", if (ko) "결과 편집" else "Edit result", mode = TRUE),
        custom_model_canvas_button("dashNonsignificant", if (ko) "비유의 점선" else "Non-significant dashed", mode = TRUE),
        custom_model_canvas_button("style", if (ko) "스타일" else "Style")
      ),
      div(class = "structural-disturbance-toolbar", `aria-live` = "polite")
    )
  )
}

structural_equation_workspace <- function(selected_names, variable_table = NULL, labels = character(0), analysis_type = "cbsem", language = statedu_initial_language()) {
  prefix <- structural_analysis_prefix(analysis_type)
  items <- custom_model_canvas_variable_items(selected_names, variable_table, labels)
  variables_json <- htmltools::htmlEscape(jsonlite::toJSON(items, auto_unbox = TRUE, null = "null"), attribute = TRUE)
  labels_i18n <- custom_model_canvas_i18n(language)
  labels_i18n$role_latent <- if (identical(normalize_app_language(language), "ko")) "잠재변수" else "Latent variable"
  i18n_json <- htmltools::htmlEscape(jsonlite::toJSON(labels_i18n, auto_unbox = TRUE, null = "null"), attribute = TRUE)
  capture <- structural_capture_initial_snapshot(analysis_type)
  capture_attrs <- list()
  if (!is.null(capture$snapshot)) {
    capture_attrs[["data-initial-snapshot"]] <- htmltools::htmlEscape(
      jsonlite::toJSON(capture$snapshot, auto_unbox = TRUE, null = "null"),
      attribute = TRUE
    )
    if (isTRUE(capture$auto_run)) capture_attrs[["data-initial-run"]] <- "true"
  }
  root <- div(
      id = paste0(prefix, "-canvas-root"),
      class = "custom-model-canvas-root structural-equation-canvas-root",
      `data-input-prefix` = paste0(prefix, "_canvas"),
      `data-analysis-type` = analysis_type,
      `data-analysis-package` = structural_analysis_package(analysis_type),
      `data-canvas-width` = "1600",
      `data-canvas-height` = "1000",
      `data-canvas-paper` = "Large",
      `data-variables` = variables_json,
      `data-language` = normalize_app_language(language),
      `data-i18n` = i18n_json,
      div(
        class = "custom-model-variable-panel analysis-transfer-column analysis-transfer-panel",
        analysis_field_label_tag(if (identical(normalize_app_language(language), "ko")) "관측변수" else "Observed variables", language = language),
        structural_equation_variable_panel(items, language)
      ),
      div(
        class = "custom-model-diagram-panel",
        structural_equation_toolbar(analysis_type, language),
        div(class = "custom-model-statusbar",
            span(class = "custom-model-mode-status", custom_model_canvas_text(language, "Mode: Select", "모드: 선택")),
            span(class = "custom-model-paper-status", "Large 1600×1000"),
            span(class = "custom-model-covariate-status", ""),
            span(class = "structural-validation-status", "오류 0 · 경고 0")),
        div(class = "custom-model-canvas-scroll",
            div(class = "custom-model-paper is-grid-visible", `data-width` = "1600", `data-height` = "1000",
                tags$svg(class = "custom-model-edge-layer", width = "1600", height = "1000"),
                div(class = "custom-model-node-layer")))
      )
    )
  if (length(capture_attrs)) {
    root <- do.call(htmltools::tagAppendAttributes, c(list(root), capture_attrs))
  }
  tagList(
    root,
    uiOutput(paste0(prefix, "_results")),
    tags$script(HTML("window.StatEduModelCanvas && window.StatEduModelCanvas.canvas && window.StatEduModelCanvas.canvas.initAll();"))
  )
}

structural_equation_tab_panel <- function(analysis_type = "cbsem", language = statedu_initial_language()) {
  prefix <- structural_analysis_prefix(analysis_type)
  title <- structural_analysis_title(analysis_type, language)
  tabPanel(
    title,
    value = paste0("analysis_", prefix),
    div(class = "page-shell",
        div(class = "app-heading", h1(title), div(if (identical(normalize_app_language(language), "ko")) "관측변수와 잠재변수를 배치하여 CFA와 SEM 모형을 작성합니다." else "Build CFA and SEM models with observed and latent variables.", class = "app-subtitle")),
        div(class = "workspace-panel frequencies-workspace-panel custom-model-workspace-panel structural-equation-workspace-panel", style = "min-width:1450px;overflow-x:auto;",
            div(
              class = paste("analysis-workspace-heading", paste0(prefix, "-workspace-heading")),
              div(class = "analysis-workspace-heading-main", h3(analysis_ui_text(title, language)))
            ),
            analysis_workspace_body(prefix, uiOutput(paste0(prefix, "_canvas_setup")), NULL, NULL)))
  )
}

custom_model_canvas_tab_panel <- function(title = custom_model_canvas_title(), language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  tabPanel(
    title,
    value = "analysis_custom_model_canvas",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(custom_model_canvas_title(language)),
        div(
          custom_model_canvas_text(
            language,
            "Draw a mediation/moderation model by placing variables on the canvas.",
            "\ubcc0\uc218\ub97c \uce94\ubc84\uc2a4\uc5d0 \ubc30\uce58\ud558\uc5ec \ub9e4\uac1c\u00b7\uc870\uc808 \uc0ac\uc6a9\uc790 \ubaa8\ub378\uc744 \uc791\uc131\ud569\ub2c8\ub2e4."
          ),
          class = "app-subtitle"
        )
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel custom-model-workspace-panel",
        style = "min-width:1450px;overflow-x:auto;",
        analysis_workspace_heading(custom_model_canvas_title(language), "custom_model_canvas", language),
        analysis_workspace_body(
          "custom_model_canvas",
          uiOutput("custom_model_canvas_setup"),
          uiOutput("custom_model_canvas_save_control"),
          uiOutput("custom_model_canvas_results")
        )
      )
    )
  )
}
