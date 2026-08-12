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

structural_canvas_node <- function(snapshot, id) {
  nodes <- snapshot$nodes %||% list()
  matches <- Filter(function(node) identical(as.character(node$id %||% ""), as.character(id)), nodes)
  if (length(matches)) matches[[1]] else NULL
}

structural_canvas_name <- function(node) {
  as.character(node$name %||% node$variableId %||% node$dataLabel %||% "")
}

structural_canvas_parameter_term <- function(edge, target_name) {
  target_name <- as.character(target_name)
  free <- edge$free
  fixed_value <- suppressWarnings(as.numeric(edge$fixedValue %||% NA_real_))
  start_value <- suppressWarnings(as.numeric(edge$startValue %||% NA_real_))
  equality_label <- trimws(as.character(edge$equalityLabel %||% ""))
  parameter_name <- trimws(as.character(edge$parameterName %||% ""))
  label <- if (nzchar(equality_label)) equality_label else parameter_name
  if (nzchar(label) && !grepl("^[A-Za-z][A-Za-z0-9_.]*$", label)) {
    stop(sprintf("Invalid lavaan parameter label '%s'. Use a letter first, followed by letters, numbers, underscores, or periods.", label))
  }
  if (identical(free, FALSE)) {
    if (!is.finite(fixed_value)) stop(sprintf("A finite fixed value is required for the fixed path to %s.", target_name))
    return(paste0(format(fixed_value, scientific = FALSE, digits = 15, trim = TRUE), "*", target_name))
  }
  modifiers <- character(0)
  if (is.finite(start_value)) modifiers <- c(modifiers, paste0("start(", format(start_value, scientific = FALSE, digits = 15, trim = TRUE), ")"))
  if (nzchar(label)) modifiers <- c(modifiers, label)
  if (length(modifiers)) paste0(paste(modifiers, collapse = "*"), "*", target_name) else target_name
}

structural_canvas_has_parameter_modifier <- function(edge) {
  identical(edge$free, FALSE) ||
    is.finite(suppressWarnings(as.numeric(edge$startValue %||% NA_real_))) ||
    nzchar(trimws(as.character(edge$parameterName %||% ""))) ||
    nzchar(trimws(as.character(edge$equalityLabel %||% "")))
}

structural_canvas_result_snapshot <- function(snapshot, fit, coefficient = "beta") {
  snapshot <- snapshot %||% list()
  snapshot$nonce <- NULL
  if (!inherits(fit, "lavaan")) return(snapshot)

  parameters <- lavaan::parameterEstimates(fit, standardized = TRUE)
  estimate_column <- if (identical(coefficient, "b")) "est" else "std.all"
  result_info <- function(lhs, op, rhs) {
    row <- parameters[parameters$lhs == lhs & parameters$op == op & parameters$rhs == rhs, , drop = FALSE]
    if (!nrow(row) && identical(op, "~~")) {
      row <- parameters[parameters$lhs == rhs & parameters$op == op & parameters$rhs == lhs, , drop = FALSE]
    }
    if (!nrow(row)) return(list(label = "", p = NA_real_, matched = FALSE))
    value <- suppressWarnings(as.numeric(row[[estimate_column]][[1L]]))
    p_value <- suppressWarnings(as.numeric(row$pvalue[[1L]]))
    if (!is.finite(value)) return(list(label = "", p = p_value, matched = FALSE))
    list(
      label = if (is.finite(p_value)) {
        sprintf("%s(%s)", format_decimal3(value), format_p(p_value))
      } else {
        format_decimal3(value)
      },
      p = p_value,
      matched = TRUE
    )
  }
  edges <- snapshot$edges %||% list()
  target_name <- function(node) {
    if (is.null(node)) return("")
    if (node$role %in% c("latent", "indicator")) return(structural_canvas_name(node))
    target <- Filter(function(edge) {
      !identical(edge$kind, "covariance") && identical(as.character(edge$from), as.character(node$id))
    }, edges)
    if (length(target)) structural_canvas_name(structural_canvas_node(snapshot, target[[1L]]$to)) else ""
  }

  snapshot$edges <- lapply(edges, function(edge) {
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    from_role <- as.character(from$role %||% "")
    to_role <- as.character(to$role %||% "")
    is_measurement_path <-
      (identical(from_role, "latent") && identical(to_role, "indicator")) ||
      (identical(from_role, "indicator") && identical(to_role, "latent"))
    is_structural_path <-
      !identical(edge$kind, "covariance") &&
      identical(from_role, "latent") && identical(to_role, "latent") &&
      !identical(as.character(edge$pathType %||% "regression"), "higherOrder")
    info <- list(label = "", p = NA_real_, matched = FALSE)
    if (identical(edge$kind, "covariance")) {
      info <- result_info(target_name(from), "~~", target_name(to))
    } else if (!is.null(from) && !is.null(to) && identical(from$role, "latent") && identical(to$role, "indicator")) {
      info <- result_info(structural_canvas_name(from), "=~", structural_canvas_name(to))
    } else if (!is.null(from) && !is.null(to) && identical(from$role, "indicator") && identical(to$role, "latent")) {
      info <- result_info(structural_canvas_name(to), "=~", structural_canvas_name(from))
    } else if (!is.null(from) && !is.null(to) && identical(from$role, "latent") && identical(to$role, "latent")) {
      if (identical(as.character(edge$pathType %||% "regression"), "higherOrder")) {
        info <- result_info(structural_canvas_name(from), "=~", structural_canvas_name(to))
      } else {
        info <- result_info(structural_canvas_name(to), "~", structural_canvas_name(from))
      }
    }
    edge$label <- info$label
    edge$p <- info$p
    edge$significant <- isTRUE(info$matched) && (!is.finite(info$p) || info$p < .05)
    edge$dashEligible <- is_measurement_path || is_structural_path
    edge$resultMatched <- isTRUE(info$matched)
    edge$labelPosition <- 50
    edge$labelOffsetX <- 0
    edge$labelOffsetY <- -10
    edge$labelTextAnchor <- "middle"
    if (is_measurement_path && !is.null(from) && !is.null(to)) {
      dx <- abs(as.numeric(to$x %||% 0) - as.numeric(from$x %||% 0))
      dy <- abs(as.numeric(to$y %||% 0) - as.numeric(from$y %||% 0))
      if (is.finite(dx) && is.finite(dy) && dx >= dy) edge$labelTextAnchor <- "start"
    }
    edge
  })
  snapshot$dashNonsignificant <- TRUE
  snapshot$resultCoefficient <- coefficient
  snapshot
}

structural_canvas_apply_mi <- function(snapshot, mi_row) {
  nodes <- snapshot$nodes %||% list()
  edges <- snapshot$edges %||% list()
  node_by_name <- function(name, role = NULL) {
    matches <- Filter(function(node) {
      identical(structural_canvas_name(node), as.character(name)) &&
        (is.null(role) || node$role %in% role)
    }, nodes)
    if (length(matches)) matches[[1L]] else NULL
  }
  residual_for <- function(target, role) {
    if (is.null(target)) return(NULL)
    links <- Filter(function(edge) {
      !identical(edge$kind, "covariance") && identical(as.character(edge$to), as.character(target$id))
    }, edges)
    sources <- lapply(links, function(edge) structural_canvas_node(snapshot, edge$from))
    sources <- Filter(function(node) !is.null(node) && node$role %in% role, sources)
    if (length(sources)) sources[[1L]] else NULL
  }

  lhs <- node_by_name(mi_row$lhs[[1L]], c("latent", "indicator"))
  rhs <- node_by_name(mi_row$rhs[[1L]], c("latent", "indicator"))
  lhs_target <- lhs
  rhs_target <- rhs
  reason <- as.character(mi_row$Reason[[1L]] %||% "")
  curve_direction <- NULL
  if (grepl("Measurement errors", reason, fixed = TRUE)) {
    lhs <- residual_for(lhs, "error")
    rhs <- residual_for(rhs, "error")
    if (!is.null(lhs) && !is.null(rhs) && !is.null(lhs_target) && !is.null(rhs_target)) {
      error_x <- mean(c(as.numeric(lhs$x), as.numeric(rhs$x)), na.rm = TRUE)
      error_y <- mean(c(as.numeric(lhs$y), as.numeric(rhs$y)), na.rm = TRUE)
      target_x <- mean(c(as.numeric(lhs_target$x), as.numeric(rhs_target$x)), na.rm = TRUE)
      target_y <- mean(c(as.numeric(lhs_target$y), as.numeric(rhs_target$y)), na.rm = TRUE)
      dx <- error_x - target_x
      dy <- error_y - target_y
      curve_direction <- if (abs(dx) >= abs(dy)) if (dx >= 0) "right" else "left" else if (dy >= 0) "bottom" else "top"
    }
  } else if (grepl("disturbances", reason, fixed = TRUE)) {
    lhs <- residual_for(lhs, "disturbance")
    rhs <- residual_for(rhs, "disturbance")
  }
  if (is.null(lhs) || is.null(rhs)) stop("The covariance endpoints could not be found on the canvas.")

  duplicate <- any(vapply(edges, function(edge) {
    identical(edge$kind, "covariance") &&
      ((identical(as.character(edge$from), as.character(lhs$id)) && identical(as.character(edge$to), as.character(rhs$id))) ||
       (identical(as.character(edge$from), as.character(rhs$id)) && identical(as.character(edge$to), as.character(lhs$id))))
  }, logical(1)))
  if (!duplicate) {
    snapshot$edges <- c(edges, list(list(
      id = paste0("edge-mi-", as.integer(Sys.time()), "-", sample.int(999999L, 1L)),
      from = as.character(lhs$id),
      to = as.character(rhs$id),
      kind = "covariance",
      label = "",
      shape = "curveUp",
      curveDirection = curve_direction,
      curveOffset = 52,
      free = TRUE,
      parameterName = "",
      equalityLabel = ""
    )))
  }
  snapshot$nonce <- NULL
  snapshot
}

structural_canvas_mi_signature <- function(lhs, op, rhs) {
  lhs <- as.character(lhs)
  rhs <- as.character(rhs)
  op <- as.character(op)
  if (identical(op, "~~")) paste(sort(c(lhs, rhs)), collapse = "~~") else paste(lhs, op, rhs, sep = "|")
}

structural_canvas_mi_history_rows <- function(mi, selected_rows, existing = data.frame(), justification = "") {
  if (is.null(mi) || !nrow(mi) || !length(selected_rows)) return(existing)
  selected_rows <- selected_rows[selected_rows >= 1L & selected_rows <= nrow(mi)]
  if (!length(selected_rows)) return(existing)
  existing_signatures <- if (nrow(existing) && "Signature" %in% names(existing)) as.character(existing$Signature) else character(0)
  additions <- list()
  for (index in selected_rows) {
    signature <- structural_canvas_mi_signature(mi$lhs[[index]], mi$op[[index]], mi$rhs[[index]])
    if (signature %in% c(existing_signatures, vapply(additions, function(item) item$Signature[[1L]], character(1)))) next
    additions[[length(additions) + 1L]] <- data.frame(
      Step = nrow(existing) + length(additions) + 1L,
      Parameter = paste(mi$lhs[[index]], mi$op[[index]], mi$rhs[[index]]),
      Signature = signature,
      MI = as.numeric(mi$mi[[index]] %||% NA_real_),
      EPC = as.numeric(if ("epc" %in% names(mi)) mi$epc[[index]] else NA_real_),
      CFI = as.numeric(if ("cfi_after" %in% names(mi)) mi$cfi_after[[index]] else NA_real_),
      TLI = as.numeric(if ("tli_after" %in% names(mi)) mi$tli_after[[index]] else NA_real_),
      RMSEA = as.numeric(if ("rmsea_after" %in% names(mi)) mi$rmsea_after[[index]] else NA_real_),
      SRMR = as.numeric(if ("srmr_after" %in% names(mi)) mi$srmr_after[[index]] else NA_real_),
      Justification = as.character(justification %||% ""),
      stringsAsFactors = FALSE
    )
  }
  if (!length(additions)) existing else rbind(existing, do.call(rbind, additions))
}

structural_canvas_missing_diagnostics <- function(data, variables) {
  variables <- intersect(unique(as.character(variables)), names(data))
  if (!length(variables)) return(list(available = FALSE))
  values <- data[variables]
  n <- nrow(values)
  missing_count <- vapply(values, function(value) sum(is.na(value)), integer(1))
  variable_table <- data.frame(
    Variable = variables, Missing = unname(missing_count),
    Percent = if (n > 0) 100 * unname(missing_count) / n else NA_real_,
    stringsAsFactors = FALSE
  )
  patterns <- apply(is.na(values), 1L, function(row) paste(ifelse(row, "1", "0"), collapse = ""))
  pattern_table <- as.data.frame(table(patterns), stringsAsFactors = FALSE)
  names(pattern_table) <- c("Pattern", "Count")
  pattern_table <- pattern_table[order(-pattern_table$Count), , drop = FALSE]
  pattern_table$Description <- vapply(pattern_table$Pattern, function(pattern) {
    bits <- strsplit(pattern, "", fixed = TRUE)[[1L]]
    missing_variables <- variables[bits == "1"]
    if (length(missing_variables)) paste("Missing:", paste(missing_variables, collapse = ", ")) else "Complete"
  }, character(1))
  list(
    available = TRUE, n = n, complete_n = sum(stats::complete.cases(values)),
    incomplete_n = sum(!stats::complete.cases(values)), pattern_count = nrow(pattern_table),
    variables = variable_table, patterns = pattern_table
  )
}

structural_canvas_mahalanobis_diagnostics <- function(data, variables, alpha = .001) {
  variables <- intersect(unique(as.character(variables)), names(data))
  if (length(variables) < 2L || !all(vapply(data[variables], is.numeric, logical(1)))) return(list(available = FALSE, reason = "At least two numeric continuous indicators are required."))
  complete_rows <- which(stats::complete.cases(data[variables]))
  values <- as.matrix(data[complete_rows, variables, drop = FALSE])
  p <- ncol(values)
  if (nrow(values) <= p + 1L) return(list(available = FALSE, reason = "Too few complete cases for multivariate outlier diagnostics."))
  covariance <- stats::cov(values)
  inverse <- tryCatch(solve(covariance), error = function(error) NULL)
  if (is.null(inverse)) return(list(available = FALSE, reason = "The complete-case covariance matrix is singular."))
  centered <- sweep(values, 2L, colMeans(values), "-")
  distances <- rowSums((centered %*% inverse) * centered)
  pvalues <- stats::pchisq(distances, df = p, lower.tail = FALSE)
  flagged <- pvalues < alpha
  table <- data.frame(
    Row = complete_rows[flagged], Mahalanobis = distances[flagged], df = p,
    p = pvalues[flagged], stringsAsFactors = FALSE
  )
  if (nrow(table)) table <- table[order(-table$Mahalanobis), , drop = FALSE]
  list(available = TRUE, n = nrow(values), p = p, alpha = alpha, flagged_n = sum(flagged), table = table)
}

structural_canvas_fit_guidance <- function(fit_values) {
  values <- as.numeric(fit_values)
  if (length(values) < 8L) stop("Fit guidance requires the standard fit-measure vector.")
  df <- values[[2L]]
  metrics <- c(CFI = values[[5L]], TLI = values[[6L]], SRMR = values[[7L]], RMSEA = values[[8L]])
  if (!is.finite(df) || df <= 0) {
    return(data.frame(Metric = names(metrics), Value = metrics, Guidance = "Not assessed", Reference = "Saturated/df <= 0", stringsAsFactors = FALSE))
  }
  classify_incremental <- function(value) if (!is.finite(value)) "Not assessed" else if (value >= .95) "Good" else if (value >= .90) "Marginal" else "Review"
  classify_rmsea <- function(value) if (!is.finite(value)) "Not assessed" else if (value <= .06) "Good" else if (value <= .08) "Marginal" else "Review"
  classify_srmr <- function(value) if (!is.finite(value)) "Not assessed" else if (value <= .08) "Good" else if (value <= .10) "Marginal" else "Review"
  guidance <- c(classify_incremental(metrics[["CFI"]]), classify_incremental(metrics[["TLI"]]), classify_srmr(metrics[["SRMR"]]), classify_rmsea(metrics[["RMSEA"]]))
  references <- c("Good >= .95; Marginal >= .90", "Good >= .95; Marginal >= .90", "Good <= .08; Marginal <= .10", "Good <= .06; Marginal <= .08")
  data.frame(Metric = names(metrics), Value = unname(metrics), Guidance = guidance, Reference = references, stringsAsFactors = FALSE)
}

structural_canvas_ordered_indicators <- function(snapshot, variable_table = NULL) {
  if (is.null(variable_table) || !all(c("name", "measurement") %in% names(variable_table))) return(character(0))
  indicators <- unique(vapply(Filter(function(node) identical(node$role, "indicator"), snapshot$nodes %||% list()), structural_canvas_name, character(1)))
  measurements <- tolower(as.character(variable_table$measurement))
  names(measurements) <- as.character(variable_table$name)
  # Nominal indicators have no defensible category ordering for lavaan's
  # threshold/polychoric CFA. Do not silently impose their factor level order.
  ordered_levels <- c("binary", "ordinal", "ordered")
  is_ordered <- indicators %in% names(measurements) & measurements[indicators] %in% ordered_levels
  indicators[!is.na(is_ordered) & is_ordered]
}

structural_canvas_nominal_indicators <- function(snapshot, variable_table = NULL) {
  if (is.null(variable_table) || !all(c("name", "measurement") %in% names(variable_table))) return(character(0))
  indicators <- unique(vapply(Filter(function(node) identical(node$role, "indicator"), snapshot$nodes %||% list()), structural_canvas_name, character(1)))
  measurements <- tolower(as.character(variable_table$measurement))
  names(measurements) <- as.character(variable_table$name)
  nominal_levels <- c("category", "categorical", "factor", "nominal")
  is_nominal <- indicators %in% names(measurements) & measurements[indicators] %in% nominal_levels
  indicators[!is.na(is_nominal) & is_nominal]
}

structural_canvas_missing_exogenous_covariances <- function(snapshot) {
  nodes <- snapshot$nodes %||% list()
  edges <- snapshot$edges %||% list()
  latents <- Filter(function(node) identical(node$role, "latent"), nodes)
  endogenous_ids <- unique(vapply(Filter(function(edge) {
    if (identical(edge$kind, "covariance")) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    !is.null(from) && !is.null(to) && identical(from$role, "latent") && identical(to$role, "latent")
  }, edges), function(edge) as.character(edge$to), character(1)))
  exogenous <- Filter(function(node) !as.character(node$id) %in% endogenous_ids, latents)
  if (length(exogenous) < 2L) return(character(0))
  pairs <- utils::combn(exogenous, 2L, simplify = FALSE)
  missing <- Filter(function(pair) {
    ids <- vapply(pair, function(node) as.character(node$id), character(1))
    !any(vapply(edges, function(edge) {
      if (!identical(edge$kind, "covariance")) return(FALSE)
      endpoints <- c(as.character(edge$from), as.character(edge$to))
      setequal(endpoints, ids)
    }, logical(1)))
  }, pairs)
  vapply(missing, function(pair) paste(vapply(pair, structural_canvas_name, character(1)), collapse = " ↔ "), character(1))
}

structural_canvas_fornell_larcker <- function(ave, correlations, indicator_counts = NULL, assessable = TRUE) {
  latent_names <- names(ave)
  correlations <- as.matrix(correlations)
  if (is.null(indicator_counts)) indicator_counts <- stats::setNames(rep(2L, length(latent_names)), latent_names)
  max_correlation <- stats::setNames(rep(NA_real_, length(latent_names)), latent_names)
  criterion <- stats::setNames(rep("Not assessed", length(latent_names)), latent_names)
  if (length(latent_names) < 2L || !isTRUE(assessable)) return(list(max_correlation = max_correlation, criterion = criterion))
  for (name in latent_names) {
    others <- setdiff(latent_names, name)
    values <- abs(correlations[name, others, drop = TRUE])
    values <- values[is.finite(values)]
    if (length(values)) max_correlation[[name]] <- max(values)
    if ((indicator_counts[[name]] %||% 0L) < 2L || !is.finite(ave[[name]]) || !is.finite(max_correlation[[name]])) next
    criterion[[name]] <- if (sqrt(ave[[name]]) > max_correlation[[name]]) "Criterion met" else "Review needed"
  }
  list(max_correlation = max_correlation, criterion = criterion)
}

structural_canvas_htmt <- function(correlations, indicators_by_factor, threshold = .85) {
  correlations <- as.matrix(correlations)
  factor_names <- names(indicators_by_factor)
  matrix_result <- matrix(NA_real_, length(factor_names), length(factor_names), dimnames = list(factor_names, factor_names))
  pairs <- list()
  if (length(factor_names) < 2L) return(list(matrix = matrix_result, pairs = data.frame(), threshold = threshold))
  pair_index <- 0L
  for (indices in utils::combn(seq_along(factor_names), 2L, simplify = FALSE)) {
    first <- factor_names[[indices[[1L]]]]
    second <- factor_names[[indices[[2L]]]]
    first_indicators <- unique(indicators_by_factor[[first]])
    second_indicators <- unique(indicators_by_factor[[second]])
    reason <- ""
    value <- NA_real_
    if (length(first_indicators) < 2L || length(second_indicators) < 2L) {
      reason <- "At least two indicators per factor are required"
    } else if (length(intersect(first_indicators, second_indicators))) {
      reason <- "Cross-loaded indicators prevent standard HTMT calculation"
    } else if (!all(c(first_indicators, second_indicators) %in% rownames(correlations))) {
      reason <- "Indicator correlations are unavailable"
    } else {
      heterotrait <- abs(correlations[first_indicators, second_indicators, drop = FALSE])
      first_monotrait <- abs(correlations[first_indicators, first_indicators, drop = FALSE][lower.tri(correlations[first_indicators, first_indicators, drop = FALSE])])
      second_monotrait <- abs(correlations[second_indicators, second_indicators, drop = FALSE][lower.tri(correlations[second_indicators, second_indicators, drop = FALSE])])
      denominator <- sqrt(mean(first_monotrait, na.rm = TRUE) * mean(second_monotrait, na.rm = TRUE))
      if (is.finite(denominator) && denominator > 0) value <- mean(heterotrait, na.rm = TRUE) / denominator else reason <- "Within-factor correlations are insufficient"
    }
    matrix_result[first, second] <- matrix_result[second, first] <- value
    pair_index <- pair_index + 1L
    pairs[[pair_index]] <- data.frame(
      Factor1 = first, Factor2 = second, HTMT = value,
      Criterion = if (is.finite(value)) if (value < threshold) "Criterion met" else "Review needed" else "Not assessed",
      Reason = reason,
      stringsAsFactors = FALSE
    )
  }
  list(matrix = matrix_result, pairs = do.call(rbind, pairs), threshold = threshold)
}

structural_canvas_htmt_bootstrap <- function(data, indicators_by_factor, reps = 0L, confidence = .95, seed = 20260812L, ordered = character(0), threshold = .85, ci_method = "percentile", progress = NULL, cancel = NULL) {
  reps <- suppressWarnings(as.integer(reps))
  confidence <- suppressWarnings(as.numeric(confidence))
  threshold <- suppressWarnings(as.numeric(threshold))
  ci_method <- structural_canvas_bootstrap_ci_method(ci_method)
  variables <- unique(unlist(indicators_by_factor, use.names = FALSE))
  if (!is.data.frame(data) || reps < 2L || length(indicators_by_factor) < 2L ||
      !all(variables %in% names(data)) ||
      !is.finite(confidence) || confidence <= 0 || confidence >= 1 || !is.finite(threshold) || threshold <= 0) {
    return(NULL)
  }
  values <- data[variables]
  ordered <- intersect(as.character(ordered), variables)
  continuous <- setdiff(variables, ordered)
  if (length(continuous) && !all(vapply(values[continuous], is.numeric, logical(1)))) return(NULL)
  n <- nrow(values)
  if (n < 3L) return(NULL)
  factor_names <- names(indicators_by_factor)
  pairs <- utils::combn(factor_names, 2L, simplify = FALSE)
  estimates <- matrix(NA_real_, nrow = reps, ncol = length(pairs))
  compute_correlations <- function(frame) {
    if (length(ordered)) {
      suppressWarnings(tryCatch(
        as.matrix(lavaan::lavCor(frame, ordered = ordered, missing = "pairwise", estimator = "two.step", se = "none", test = "none", output = "cor", cor.smooth = TRUE)),
        error = function(error) NULL
      ))
    } else {
      suppressWarnings(stats::cor(frame, use = "pairwise.complete.obs"))
    }
  }
  compute_pair_values <- function(frame) {
    correlations <- compute_correlations(frame)
    if (is.null(correlations) || !all(variables %in% rownames(correlations))) return(rep(NA_real_, length(pairs)))
    htmt <- structural_canvas_htmt(correlations, indicators_by_factor, threshold = 1)
    vapply(seq_along(pairs), function(pair_index) {
      pair <- pairs[[pair_index]]
      as.numeric(htmt$matrix[pair[[1L]], pair[[2L]]])
    }, numeric(1))
  }
  original_values <- if (identical(ci_method, "bca")) compute_pair_values(values) else rep(NA_real_, length(pairs))
  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (old_seed_exists) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(as.integer(seed))
  total_iterations <- reps + if (identical(ci_method, "bca")) n else 0L
  progress_step <- max(1L, floor(total_iterations / 100L))
  if (is.function(progress)) progress(0L, total_iterations, 0L)
  for (index in seq_len(reps)) {
    if (is.function(cancel) && isTRUE(cancel())) stop("HTMT bootstrap canceled.")
    sampled <- values[sample.int(n, n, replace = TRUE), , drop = FALSE]
    estimates[index, ] <- compute_pair_values(sampled)
    if (is.function(progress) && (index == 1L || index == total_iterations || index %% progress_step == 0L)) {
      valid_counts <- colSums(is.finite(estimates[seq_len(index), , drop = FALSE]))
      progress(index, total_iterations, if (length(valid_counts)) min(valid_counts) else 0L)
    }
  }
  jackknife <- NULL
  if (identical(ci_method, "bca")) {
    jackknife <- matrix(NA_real_, nrow = n, ncol = length(pairs))
    for (index in seq_len(n)) {
      if (is.function(cancel) && isTRUE(cancel())) stop("HTMT bootstrap canceled.")
      jackknife[index, ] <- compute_pair_values(values[-index, , drop = FALSE])
      completed <- reps + index
      if (is.function(progress) && (completed == total_iterations || completed %% progress_step == 0L)) {
        valid_counts <- colSums(is.finite(estimates))
        progress(completed, total_iterations, if (length(valid_counts)) min(valid_counts) else 0L)
      }
    }
  }
  alpha <- (1 - confidence) / 2
  rows <- lapply(seq_along(pairs), function(pair_index) {
    pair_values <- estimates[, pair_index]
    pair_values <- pair_values[is.finite(pair_values)]
    interval <- if (identical(ci_method, "bca")) {
      structural_canvas_bca_interval(pair_values, original_values[[pair_index]], jackknife[, pair_index], confidence)
    } else if (length(pair_values) >= max(20L, ceiling(.5 * reps))) {
      as.numeric(stats::quantile(pair_values, probs = c(alpha, 1 - alpha), names = FALSE, type = 6, na.rm = TRUE))
    } else c(NA_real_, NA_real_)
    upper_one_sided <- if (length(pair_values) >= max(20L, ceiling(.5 * reps))) {
      as.numeric(stats::quantile(pair_values, probs = confidence, names = FALSE, type = 6, na.rm = TRUE))
    } else NA_real_
    data.frame(
      `Factor 1` = pairs[[pair_index]][[1L]], `Factor 2` = pairs[[pair_index]][[2L]],
      Lower = interval[[1L]], Upper = interval[[2L]],
      `One-sided upper` = upper_one_sided,
      `Upper < threshold` = if (is.finite(upper_one_sided)) if (upper_one_sided < threshold) "Yes" else "No" else "Not assessed",
      `Upper < 1` = if (is.finite(interval[[2L]])) if (interval[[2L]] < 1) "Yes" else "No" else "Not assessed",
      `CI method` = if (identical(ci_method, "bca")) {
        if (all(is.finite(interval))) "BCa" else "BCa unavailable"
      } else "Percentile",
      `Valid replicates` = length(pair_values), `Requested replicates` = reps,
      `Valid %` = 100 * length(pair_values) / reps,
      Status = structural_canvas_bootstrap_status(length(pair_values), reps), check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

structural_canvas_mardia <- function(data, variables, max_n = 2000L) {
  variables <- intersect(unique(as.character(variables)), names(data))
  if (length(variables) < 2L) return(list(available = FALSE, reason = "At least two continuous indicators are required."))
  values <- data[variables]
  if (!all(vapply(values, is.numeric, logical(1)))) return(list(available = FALSE, reason = "All indicators must be numeric and continuous."))
  values <- values[stats::complete.cases(values), , drop = FALSE]
  original_n <- nrow(values)
  p <- ncol(values)
  if (original_n <= p + 1L) return(list(available = FALSE, reason = "Too few complete cases for the number of indicators."))
  sampled <- original_n > max_n
  if (sampled) values <- values[unique(round(seq(1, original_n, length.out = max_n))), , drop = FALSE]
  n <- nrow(values)
  centered <- sweep(as.matrix(values), 2L, colMeans(values), "-")
  covariance <- crossprod(centered) / n
  inverse <- tryCatch(solve(covariance), error = function(error) NULL)
  if (is.null(inverse)) return(list(available = FALSE, reason = "The indicator covariance matrix is singular."))
  distances <- centered %*% inverse %*% t(centered)
  skewness <- mean(distances^3)
  skew_statistic <- n * skewness / 6
  skew_df <- p * (p + 1L) * (p + 2L) / 6
  skew_p <- stats::pchisq(skew_statistic, df = skew_df, lower.tail = FALSE)
  kurtosis <- mean(diag(distances)^2)
  expected_kurtosis <- p * (p + 2L)
  kurtosis_z <- (kurtosis - expected_kurtosis) / sqrt(8 * p * (p + 2L) / n)
  kurtosis_p <- 2 * stats::pnorm(abs(kurtosis_z), lower.tail = FALSE)
  nonnormal <- is.finite(skew_p) && is.finite(kurtosis_p) && (skew_p < .05 || kurtosis_p < .05)
  list(
    available = TRUE, n = n, original_n = original_n, p = p, sampled = sampled,
    skewness = skewness, skew_statistic = skew_statistic, skew_df = skew_df, skew_p = skew_p,
    kurtosis = kurtosis, expected_kurtosis = expected_kurtosis, kurtosis_z = kurtosis_z, kurtosis_p = kurtosis_p,
    recommendation = if (nonnormal) "MLR recommended" else "ML is acceptable",
    nonnormal = nonnormal
  )
}

structural_canvas_estimator_recommendation <- function(snapshot, data, variable_table, analysis_type = "cfa", estimator = "ML") {
  estimator <- toupper(as.character(estimator %||% "ML"))
  if (!analysis_type %in% c("cfa", "cbsem") || !identical(estimator, "ML")) {
    return(list(recommend = FALSE, reason = "Estimator recommendation is only evaluated for ML CFA/SEM."))
  }
  ordered <- structural_canvas_ordered_indicators(snapshot, variable_table)
  if (length(ordered)) return(list(recommend = FALSE, reason = "Ordered indicators are handled by WLSMV selection."))
  nodes <- snapshot$nodes %||% list()
  indicators <- unique(vapply(Filter(function(node) identical(node$role, "indicator"), nodes), structural_canvas_name, character(1)))
  indicators <- intersect(indicators, names(data %||% data.frame()))
  diagnosis <- structural_canvas_mardia(data, indicators)
  if (!isTRUE(diagnosis$available)) return(list(recommend = FALSE, reason = diagnosis$reason %||% "Mardia diagnostic unavailable.", diagnosis = diagnosis))
  list(
    recommend = isTRUE(diagnosis$nonnormal),
    recommended_estimator = if (isTRUE(diagnosis$nonnormal)) "MLR" else "ML",
    reason = if (isTRUE(diagnosis$nonnormal)) "Mardia skewness or kurtosis test indicates nonnormal continuous indicators." else "Mardia diagnostics do not flag nonnormality.",
    diagnosis = diagnosis
  )
}

structural_canvas_cronbach_alpha <- function(covariance, indicators) {
  covariance <- as.matrix(covariance)
  indicators <- unique(as.character(indicators))
  if (length(indicators) < 2L || !all(indicators %in% rownames(covariance))) return(NA_real_)
  item_covariance <- covariance[indicators, indicators, drop = FALSE]
  total_variance <- sum(item_covariance, na.rm = TRUE)
  if (!is.finite(total_variance) || total_variance <= 0) return(NA_real_)
  k <- length(indicators)
  k / (k - 1) * (1 - sum(diag(item_covariance), na.rm = TRUE) / total_variance)
}

structural_canvas_reliability_estimates <- function(fit, formula_mode = "standardized") {
  standardized <- lavaan::standardizedSolution(fit)
  observed <- lavaan::lavNames(fit, "ov")
  loadings <- standardized[standardized$op == "=~" & standardized$rhs %in% observed, c("lhs", "rhs", "est.std"), drop = FALSE]
  factors <- unique(loadings$lhs)
  theta_raw <- as.matrix(lavaan::lavInspect(fit, "theta"))
  theta_standardized <- matrix(0, nrow = length(observed), ncol = length(observed), dimnames = list(observed, observed))
  theta_rows <- standardized$op == "~~" & standardized$lhs %in% observed & standardized$rhs %in% observed
  for (index in which(theta_rows)) {
    lhs <- standardized$lhs[[index]]
    rhs <- standardized$rhs[[index]]
    theta_standardized[lhs, rhs] <- standardized$est.std[[index]]
    theta_standardized[rhs, lhs] <- standardized$est.std[[index]]
  }
  sample_covariance <- lavaan::lavInspect(fit, "sampstat")$cov %||% NULL
  rows <- lapply(factors, function(factor) {
    factor_rows <- loadings$lhs == factor
    indicators <- loadings$rhs[factor_rows]
    lambda <- loadings$est.std[factor_rows]
    if (identical(formula_mode, "model_implied")) {
      parameters <- lavaan::parameterEstimates(fit)
      raw <- parameters$est[parameters$op == "=~" & parameters$lhs == factor & parameters$rhs %in% indicators]
      latent_variance <- diag(as.matrix(lavaan::lavInspect(fit, "cov.lv")))[[factor]]
      residual_variances <- diag(theta_raw)[indicators]
      ave_common <- sum(raw^2 * latent_variance, na.rm = TRUE)
      ave <- ave_common / (ave_common + sum(residual_variances, na.rm = TRUE))
      common <- sum(raw)^2 * latent_variance
    } else {
      ave <- mean(lambda^2, na.rm = TRUE)
      common <- sum(lambda)^2
    }
    theta <- if (identical(formula_mode, "model_implied")) theta_raw else theta_standardized
    error_total <- if (all(indicators %in% rownames(theta))) sum(theta[indicators, indicators, drop = FALSE], na.rm = TRUE) else NA_real_
    cr <- if (is.finite(error_total) && common + error_total > 0) common / (common + error_total) else NA_real_
    data.frame(
      Factor = factor, AVE = ave, CR = cr,
      Alpha = if (!is.null(sample_covariance)) structural_canvas_cronbach_alpha(sample_covariance, indicators) else NA_real_,
      Omega = cr, check.names = FALSE
    )
  })
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

structural_canvas_group_reliability_estimates <- function(fit, formula_mode = "standardized") {
  group_labels <- as.character(lavaan::lavInspect(fit, "group.label"))
  group_count <- as.integer(lavaan::lavInspect(fit, "ngroups"))
  if (!is.finite(group_count) || group_count < 2L) return(data.frame())
  if (length(group_labels) != group_count) group_labels <- paste("Group", seq_len(group_count))
  standardized_all <- lavaan::standardizedSolution(fit)
  parameters_all <- lavaan::parameterEstimates(fit)
  observed <- lavaan::lavNames(fit, "ov")
  theta_all <- lavaan::lavInspect(fit, "theta")
  latent_covariance_all <- lavaan::lavInspect(fit, "cov.lv")
  sample_statistics_all <- lavaan::lavInspect(fit, "sampstat")
  rows <- lapply(seq_len(group_count), function(group_index) {
    standardized <- standardized_all[standardized_all$group == group_index, , drop = FALSE]
    parameters <- parameters_all[parameters_all$group == group_index, , drop = FALSE]
    loadings <- standardized[standardized$op == "=~" & standardized$rhs %in% observed, c("lhs", "rhs", "est.std"), drop = FALSE]
    factors <- unique(loadings$lhs)
    if (!length(factors)) return(data.frame())
    theta_raw <- as.matrix(if (is.list(theta_all) && !is.null(theta_all[[group_index]])) theta_all[[group_index]] else theta_all)
    theta_standardized <- matrix(0, nrow = length(observed), ncol = length(observed), dimnames = list(observed, observed))
    theta_rows <- standardized$op == "~~" & standardized$lhs %in% observed & standardized$rhs %in% observed
    for (index in which(theta_rows)) {
      lhs <- standardized$lhs[[index]]
      rhs <- standardized$rhs[[index]]
      theta_standardized[lhs, rhs] <- standardized$est.std[[index]]
      theta_standardized[rhs, lhs] <- standardized$est.std[[index]]
    }
    sample_statistics <- if (is.list(sample_statistics_all) && !is.null(sample_statistics_all[[group_index]])) sample_statistics_all[[group_index]] else sample_statistics_all
    sample_covariance <- sample_statistics$cov %||% NULL
    latent_covariance <- as.matrix(if (is.list(latent_covariance_all) && !is.null(latent_covariance_all[[group_index]])) latent_covariance_all[[group_index]] else latent_covariance_all)
    group_rows <- lapply(factors, function(factor) {
      factor_rows <- loadings$lhs == factor
      indicators <- loadings$rhs[factor_rows]
      lambda <- loadings$est.std[factor_rows]
      if (identical(formula_mode, "model_implied")) {
        raw <- parameters$est[parameters$op == "=~" & parameters$lhs == factor & parameters$rhs %in% indicators]
        latent_variance <- diag(latent_covariance)[[factor]]
        residual_variances <- diag(theta_raw)[indicators]
        ave_common <- sum(raw^2 * latent_variance, na.rm = TRUE)
        ave <- ave_common / (ave_common + sum(residual_variances, na.rm = TRUE))
        common <- sum(raw)^2 * latent_variance
      } else {
        ave <- mean(lambda^2, na.rm = TRUE)
        common <- sum(lambda)^2
      }
      theta <- if (identical(formula_mode, "model_implied")) theta_raw else theta_standardized
      error_total <- if (all(indicators %in% rownames(theta))) sum(theta[indicators, indicators, drop = FALSE], na.rm = TRUE) else NA_real_
      cr <- if (is.finite(error_total) && common + error_total > 0) common / (common + error_total) else NA_real_
      data.frame(
        Group = group_labels[[group_index]], Factor = factor, k = length(indicators),
        AVE = ave, CR = cr,
        `Cronbach's alpha` = if (!is.null(sample_covariance)) structural_canvas_cronbach_alpha(sample_covariance, indicators) else NA_real_,
        `Omega total` = cr,
        check.names = FALSE
      )
    })
    do.call(rbind, group_rows)
  })
  rows <- Filter(function(value) is.data.frame(value) && nrow(value), rows)
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

structural_canvas_group_htmt <- function(fit, threshold = .85) {
  group_labels <- as.character(lavaan::lavInspect(fit, "group.label"))
  group_count <- as.integer(lavaan::lavInspect(fit, "ngroups"))
  if (!is.finite(group_count) || group_count < 2L) return(data.frame())
  if (length(group_labels) != group_count) group_labels <- paste("Group", seq_len(group_count))
  standardized <- lavaan::standardizedSolution(fit)
  observed <- lavaan::lavNames(fit, "ov")
  sample_statistics_all <- lavaan::lavInspect(fit, "sampstat")
  rows <- lapply(seq_len(group_count), function(group_index) {
    group_standardized <- standardized[standardized$group == group_index, , drop = FALSE]
    loadings <- group_standardized[group_standardized$op == "=~" & group_standardized$rhs %in% observed, c("lhs", "rhs"), drop = FALSE]
    factor_names <- unique(loadings$lhs)
    if (length(factor_names) < 2L) return(data.frame())
    indicators_by_factor <- stats::setNames(lapply(factor_names, function(name) unique(loadings$rhs[loadings$lhs == name])), factor_names)
    sample_statistics <- if (is.list(sample_statistics_all) && !is.null(sample_statistics_all[[group_index]])) sample_statistics_all[[group_index]] else sample_statistics_all
    covariance <- as.matrix(sample_statistics$cov %||% matrix(numeric(0), 0L, 0L))
    if (!nrow(covariance) || !all(observed %in% rownames(covariance))) return(data.frame())
    variances <- diag(covariance)
    correlations <- if (all(is.finite(variances)) && all(variances > 0)) stats::cov2cor(covariance) else matrix(NA_real_, nrow(covariance), ncol(covariance), dimnames = dimnames(covariance))
    htmt <- structural_canvas_htmt(correlations, indicators_by_factor, threshold = threshold)
    if (is.null(htmt) || !nrow(htmt$pairs)) return(data.frame())
    data.frame(Group = group_labels[[group_index]], htmt$pairs, check.names = FALSE)
  })
  rows <- Filter(function(value) is.data.frame(value) && nrow(value), rows)
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

structural_canvas_bootstrap_status <- function(valid, requested) {
  ratio <- as.numeric(valid) / as.numeric(requested)
  ifelse(!is.finite(ratio) | ratio < .50, "Unreliable", ifelse(ratio < .80, "Caution", "Adequate"))
}

structural_canvas_bootstrap_ci_method <- function(value) {
  value <- tolower(trimws(as.character(value %||% "percentile")))
  if (grepl("^bca", value)) return("bca")
  if (value %in% c("bca", "bc_a", "bias-corrected accelerated", "bias corrected accelerated")) return("bca")
  "percentile"
}

structural_canvas_bca_interval <- function(bootstrap_values, original_value, jackknife_values, confidence = .95) {
  bootstrap_values <- as.numeric(bootstrap_values)
  bootstrap_values <- bootstrap_values[is.finite(bootstrap_values)]
  jackknife_values <- as.numeric(jackknife_values)
  jackknife_values <- jackknife_values[is.finite(jackknife_values)]
  confidence <- as.numeric(confidence)
  if (length(bootstrap_values) < 20L || length(jackknife_values) < 10L ||
      !is.finite(original_value) || !is.finite(confidence) || confidence <= 0 || confidence >= 1) {
    return(c(NA_real_, NA_real_))
  }
  alpha <- (1 - confidence) / 2
  prop_less <- (sum(bootstrap_values < original_value) + .5) / (length(bootstrap_values) + 1)
  z0 <- stats::qnorm(prop_less)
  jackknife_mean <- mean(jackknife_values)
  jackknife_delta <- jackknife_mean - jackknife_values
  denominator <- 6 * (sum(jackknife_delta^2)^(3 / 2))
  acceleration <- if (is.finite(denominator) && denominator > 0) sum(jackknife_delta^3) / denominator else 0
  adjusted <- vapply(c(alpha, 1 - alpha), function(probability) {
    z_alpha <- stats::qnorm(probability)
    denominator <- 1 - acceleration * (z0 + z_alpha)
    if (!is.finite(denominator) || abs(denominator) < .Machine$double.eps) return(NA_real_)
    stats::pnorm(z0 + (z0 + z_alpha) / denominator)
  }, numeric(1))
  if (any(!is.finite(adjusted)) || any(adjusted <= 0 | adjusted >= 1)) return(c(NA_real_, NA_real_))
  as.numeric(stats::quantile(bootstrap_values, probs = adjusted, names = FALSE, type = 6, na.rm = TRUE))
}

structural_canvas_bollen_stine <- function(fit, reps = 500L, seed = 97531L) {
  reps <- as.integer(reps)
  if (!is.finite(reps) || reps < 1L) stop("Bollen-Stine bootstrap requires at least one resample.")
  eligibility <- structural_canvas_bollen_stine_eligibility(fit)
  if (!isTRUE(eligibility$available)) stop(eligibility$reason)
  observed <- unname(lavaan::fitMeasures(fit, "chisq"))
  draws <- suppressWarnings(lavaan::bootstrapLavaan(
    fit, r = reps, type = "bollen.stine", iseed = as.integer(seed),
    fun = function(candidate) {
      if (!isTRUE(structural_canvas_fit_admissibility(candidate)$admissible)) return(NA_real_)
      unname(lavaan::fitMeasures(candidate, "chisq"))
    }
  ))
  draws <- as.numeric(draws)
  valid_draws <- draws[is.finite(draws)]
  valid <- length(valid_draws)
  exceedances <- if (valid) sum(valid_draws >= observed) else 0L
  trials <- valid + 1L
  successes <- exceedances + 1L
  p_bootstrap <- if (valid) successes / trials else NA_real_
  mcse <- if (valid) sqrt(p_bootstrap * (1 - p_bootstrap) / trials) else NA_real_
  z_95 <- stats::qnorm(.975)
  wilson_denominator <- 1 + z_95^2 / trials
  wilson_center <- if (valid) (p_bootstrap + z_95^2 / (2 * trials)) / wilson_denominator else NA_real_
  wilson_margin <- if (valid) z_95 * sqrt(p_bootstrap * (1 - p_bootstrap) / trials + z_95^2 / (4 * trials^2)) / wilson_denominator else NA_real_
  data.frame(
    `Observed chi-square` = observed,
    `Bootstrap p` = p_bootstrap,
    `Monte Carlo SE` = mcse,
    `Monte Carlo 95% lower` = if (valid) max(0, wilson_center - wilson_margin) else NA_real_,
    `Monte Carlo 95% upper` = if (valid) min(1, wilson_center + wilson_margin) else NA_real_,
    `Valid replicates` = valid,
    `Requested replicates` = reps,
    `Valid %` = 100 * valid / reps,
    Status = structural_canvas_bootstrap_status(valid, reps),
    Seed = as.integer(seed),
    check.names = FALSE
  )
}

structural_canvas_bollen_stine_eligibility <- function(fit) {
  estimator <- toupper(as.character(lavaan::lavInspect(fit, "options")$estimator %||% ""))
  if (!identical(estimator, "ML")) return(list(available = FALSE, reason = "Bollen-Stine bootstrap is available only for ML estimation."))
  if (length(lavaan::lavNames(fit, "ov.ord"))) return(list(available = FALSE, reason = "Bollen-Stine bootstrap is not available for ordered indicators."))
  if (as.integer(lavaan::lavInspect(fit, "ngroups")) != 1L) return(list(available = FALSE, reason = "Bollen-Stine bootstrap is currently available only for single-group CFA."))
  admissibility <- structural_canvas_fit_admissibility(fit)
  if (!isTRUE(admissibility$admissible)) return(list(
    available = FALSE,
    reason = paste0("Bollen-Stine bootstrap requires an admissible fitted model: ", paste(admissibility$reasons, collapse = "; "), ".")
  ))
  degrees_of_freedom <- unname(lavaan::fitMeasures(fit, "df"))
  if (!is.finite(degrees_of_freedom) || degrees_of_freedom <= 0) return(list(available = FALSE, reason = "Bollen-Stine bootstrap is not informative for a saturated model with df = 0."))
  analyzed_data <- as.matrix(lavaan::lavInspect(fit, "data"))
  if (anyNA(analyzed_data)) return(list(available = FALSE, reason = "Bollen-Stine bootstrap requires complete analyzed data in this implementation."))
  list(available = TRUE, reason = "Eligible complete continuous single-group ML model with positive degrees of freedom.")
}

structural_canvas_normalize_missing_option <- function(value) {
  value <- tolower(trimws(as.character(value %||% "")))
  if (value %in% c("fiml", "ml", "direct")) return("ml")
  if (value %in% c("fiml.x", "ml.x")) return("ml.x")
  if (value %in% c("default", "listwise", "")) return("listwise")
  value
}

structural_canvas_reliability_bootstrap <- function(syntax, data, reps = 500L, confidence = .95, seed = 12345L, estimator = "ML", missing = "fiml", std_lv = FALSE, ordered = character(0), formula_mode = "standardized", original_fit = NULL, ci_method = "percentile", progress = NULL, cancel = NULL) {
  reps <- as.integer(reps)
  ci_method <- structural_canvas_bootstrap_ci_method(ci_method)
  if (!is.finite(reps) || reps < 1L) stop("Reliability bootstrap requires at least one resample.")
  if (is.null(original_fit)) original_fit <- tryCatch(lavaan::cfa(
      syntax, data = data, estimator = estimator, missing = missing,
      std.lv = isTRUE(std_lv), ordered = ordered, auto.cov.lv.x = FALSE
    ), error = function(error) stop(paste0("AVE/reliability bootstrap could not fit the original CFA model: ", conditionMessage(error))))
  if (!inherits(original_fit, "lavaan")) stop("AVE/reliability bootstrap original_fit must be a fitted lavaan object.")
  if (as.integer(lavaan::lavInspect(original_fit, "ngroups")) != 1L) stop("AVE/reliability bootstrap currently supports only single-group CFA models.")
  original_options <- lavaan::lavInspect(original_fit, "options")
  original_estimator <- original_options$estimator.orig %||% original_options$estimator
  if (!identical(toupper(as.character(original_estimator)), toupper(as.character(estimator)))) stop("AVE/reliability bootstrap original_fit estimator does not match the requested estimator.")
  fitted_missing <- structural_canvas_normalize_missing_option(original_options$missing)
  requested_missing <- structural_canvas_normalize_missing_option(missing)
  if (!identical(fitted_missing, requested_missing)) stop("AVE/reliability bootstrap original_fit missing-data option does not match the requested missing-data option.")
  if (!identical(isTRUE(original_options$std.lv), isTRUE(std_lv))) stop("AVE/reliability bootstrap original_fit latent-scaling option does not match std_lv.")
  fitted_ordered <- sort(lavaan::lavNames(original_fit, "ov.ord"))
  requested_ordered <- sort(unique(as.character(ordered %||% character(0))))
  if (!identical(fitted_ordered, requested_ordered)) stop("AVE/reliability bootstrap original_fit ordered-indicator specification does not match the requested ordered variables.")
  original_parameterization <- tolower(as.character(original_options$parameterization %||% "delta"))
  if (!original_parameterization %in% c("delta", "theta")) stop("AVE/reliability bootstrap original_fit uses an unsupported parameterization.")
  if (isTRUE(original_options$auto.cov.lv.x) && length(lavaan::lavNames(original_fit, "lv")) > 1L) stop("AVE/reliability bootstrap original_fit enables automatic exogenous latent covariances, but resamples use the explicit canvas covariance specification.")
  fitted_observed <- sort(lavaan::lavNames(original_fit, "ov"))
  syntax_observed <- sort(unique(unlist(regmatches(syntax, gregexpr("[[:alnum:]_.]+", syntax)), use.names = FALSE)))
  if (!all(fitted_observed %in% syntax_observed)) stop("AVE/reliability bootstrap original_fit observed variables do not match the supplied syntax.")
  normalize_user_parameters <- function(parameters) {
    parameters <- parameters[parameters$user == 1L, c("lhs", "op", "rhs", "ustart", "label"), drop = FALSE]
    covariance <- parameters$op == "~~" & parameters$lhs > parameters$rhs
    lhs <- ifelse(covariance, parameters$rhs, parameters$lhs)
    rhs <- ifelse(covariance, parameters$lhs, parameters$rhs)
    fixed <- ifelse(is.finite(parameters$ustart), format(parameters$ustart, digits = 15, scientific = FALSE, trim = TRUE), "free")
    sort(paste(lhs, parameters$op, rhs, fixed, as.character(parameters$label %||% ""), sep = "\r"))
  }
  fitted_structure <- normalize_user_parameters(lavaan::parameterTable(original_fit))
  supplied_parameters <- tryCatch(lavaan::lavaanify(
    syntax, model.type = "cfa", auto = TRUE, std.lv = isTRUE(std_lv),
    auto.fix.first = !isTRUE(std_lv), auto.fix.single = TRUE,
    auto.var = TRUE, auto.cov.lv.x = FALSE, auto.th = TRUE,
    auto.delta = identical(original_parameterization, "delta"),
    parameterization = original_parameterization
  ), error = function(error) stop(paste0("AVE/reliability bootstrap could not parse the supplied syntax: ", conditionMessage(error))))
  supplied_structure <- normalize_user_parameters(supplied_parameters)
  if (!identical(fitted_structure, supplied_structure)) stop("AVE/reliability bootstrap original_fit parameter structure does not match the supplied syntax.")
  fitted_data <- as.matrix(lavaan::lavInspect(original_fit, "data"))
  if (!is.null(colnames(fitted_data))) fitted_data <- fitted_data[, fitted_observed, drop = FALSE]
  supplied_data <- data[, fitted_observed, drop = FALSE]
  supplied_data <- as.data.frame(lapply(supplied_data, function(values) {
    if (is.factor(values)) as.numeric(values) else as.numeric(values)
  }), check.names = FALSE)
  supplied_matrix <- as.matrix(supplied_data)
  colnames(supplied_matrix) <- fitted_observed
  missing_option <- fitted_missing
  if (missing_option %in% c("listwise", "default") && nrow(fitted_data) != nrow(supplied_matrix)) supplied_matrix <- supplied_matrix[stats::complete.cases(supplied_matrix), , drop = FALSE]
  if (!isTRUE(all.equal(fitted_data, supplied_matrix, check.attributes = FALSE))) stop("AVE/reliability bootstrap original_fit does not use the same analyzed observations and values as the supplied data.")
  structural_canvas_validate_model_based_bootstrap(original_fit, "AVE/reliability bootstrap")
  fit_reliability <- function(frame) {
    fit <- tryCatch(lavaan::cfa(
      syntax, data = frame, estimator = estimator, missing = missing,
      std.lv = isTRUE(std_lv), ordered = ordered, auto.cov.lv.x = FALSE,
      parameterization = original_parameterization
    ), error = function(error) NULL)
    if (is.null(fit) || !isTRUE(structural_canvas_fit_admissibility(fit)$admissible)) return(NULL)
    tryCatch(structural_canvas_reliability_estimates(fit, formula_mode), error = function(error) NULL)
  }
  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (old_seed_exists) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(as.integer(seed))
  n <- nrow(data)
  estimates <- vector("list", reps)
  total_iterations <- reps + if (identical(ci_method, "bca")) n else 0L
  progress_step <- max(1L, floor(total_iterations / 100L))
  if (is.function(progress)) progress(0L, total_iterations, 0L)
  for (index in seq_len(reps)) {
    if (is.function(cancel) && isTRUE(cancel())) stop("AVE/reliability bootstrap canceled.")
    sampled <- data[sample.int(n, n, replace = TRUE), , drop = FALSE]
    estimates[[index]] <- fit_reliability(sampled)
    if (is.function(progress) && (index == 1L || index == total_iterations || index %% progress_step == 0L)) {
      progress(index, total_iterations, length(Filter(function(value) !is.null(value) && nrow(value), estimates[seq_len(index)])))
    }
  }
  valid <- Filter(function(value) !is.null(value) && nrow(value), estimates)
  if (!length(valid)) return(data.frame())
  combined <- do.call(rbind, lapply(seq_along(valid), function(index) transform(valid[[index]], Replicate = index)))
  original_estimates <- structural_canvas_reliability_estimates(original_fit, formula_mode)
  jackknife <- data.frame()
  if (identical(ci_method, "bca")) {
    jackknife_values <- vector("list", n)
    for (index in seq_len(n)) {
      if (is.function(cancel) && isTRUE(cancel())) stop("AVE/reliability bootstrap canceled.")
      jackknife_values[[index]] <- fit_reliability(data[-index, , drop = FALSE])
      completed <- reps + index
      if (is.function(progress) && (completed == total_iterations || completed %% progress_step == 0L)) {
        progress(completed, total_iterations, length(valid))
      }
    }
    jackknife_values <- Filter(function(value) !is.null(value) && nrow(value), jackknife_values)
    if (length(jackknife_values)) jackknife <- do.call(rbind, lapply(seq_along(jackknife_values), function(index) transform(jackknife_values[[index]], Replicate = index)))
  }
  alpha <- (1 - confidence) / 2
  factors <- unique(combined$Factor)
  do.call(rbind, lapply(factors, function(factor) {
    values <- combined[combined$Factor == factor, , drop = FALSE]
    do.call(rbind, lapply(c("AVE", "CR", "Alpha", "Omega"), function(statistic) {
      finite <- values[[statistic]][is.finite(values[[statistic]])]
      original <- original_estimates[original_estimates$Factor == factor, statistic, drop = TRUE]
      original_value <- if (length(original)) as.numeric(original[[1L]]) else NA_real_
      jackknife_finite <- if (nrow(jackknife)) jackknife[jackknife$Factor == factor, statistic, drop = TRUE] else numeric(0)
      interval <- if (identical(ci_method, "bca")) {
        structural_canvas_bca_interval(finite, original_value, jackknife_finite, confidence)
      } else if (length(finite)) {
        c(
          unname(stats::quantile(finite, alpha, names = FALSE)),
          unname(stats::quantile(finite, 1 - alpha, names = FALSE))
        )
      } else c(NA_real_, NA_real_)
      data.frame(Factor = factor, Statistic = statistic,
        Lower = interval[[1L]], Upper = interval[[2L]],
        `CI method` = if (identical(ci_method, "bca")) {
          if (all(is.finite(interval))) "BCa" else "BCa unavailable"
        } else "Percentile",
        `Valid replicates` = length(finite), `Requested replicates` = reps,
        `Valid %` = 100 * length(finite) / reps,
        Status = structural_canvas_bootstrap_status(length(finite), reps),
        check.names = FALSE)
    }))
  }))
}

structural_canvas_validate_model_based_bootstrap <- function(fit, label = "Model-based bootstrap") {
  if (is.null(fit) || !inherits(fit, "lavaan")) stop(paste0(label, " requires a fitted lavaan CFA model."))
  diagnostics <- structural_canvas_fit_admissibility(fit)
  if (!isTRUE(diagnostics$admissible)) stop(paste0(
    label, " requires an admissible original CFA model: ",
    paste(diagnostics$reasons, collapse = "; "), "."
  ))
  invisible(TRUE)
}

structural_canvas_measurement_quality_guidance <- function(ave, cr, alpha, omega) {
  values <- c(AVE = ave, CR = cr, `Cronbach's α` = alpha, `ωtotal` = omega)
  if (any(!is.finite(values)) || any(values < 0 | values > 1)) return("Review inadmissible coefficient(s)")
  cutoffs <- c(AVE = .50, CR = .70, `Cronbach's α` = .70, `ωtotal` = .70)
  below <- names(values)[values < cutoffs]
  if (!length(below)) "Meets common cutoffs" else paste0(paste(below, collapse = ", "), " below common cutoff")
}

structural_canvas_constrained_single_indicators <- function(snapshot) {
  nodes <- snapshot$nodes %||% list()
  edges <- snapshot$edges %||% list()
  latents <- Filter(function(node) identical(node$role, "latent"), nodes)
  result <- character(0)
  for (latent in latents) {
    latent_id <- as.character(latent$id %||% "")
    measurement_edges <- Filter(function(edge) {
      if (identical(edge$kind, "covariance")) return(FALSE)
      from <- structural_canvas_node(snapshot, edge$from)
      to <- structural_canvas_node(snapshot, edge$to)
      (identical(as.character(from$id %||% ""), latent_id) && identical(to$role, "indicator")) ||
        (identical(as.character(to$id %||% ""), latent_id) && identical(from$role, "indicator"))
    }, edges)
    indicators <- unique(vapply(measurement_edges, function(edge) {
      from <- structural_canvas_node(snapshot, edge$from)
      to <- structural_canvas_node(snapshot, edge$to)
      as.character(if (identical(from$role, "indicator")) from$id else to$id)
    }, character(1)))
    if (length(indicators) != 1L) next
    constrained <- any(vapply(edges, function(edge) {
      from <- structural_canvas_node(snapshot, edge$from)
      identical(as.character(edge$to %||% ""), indicators[[1L]]) && !is.null(from) && identical(from$role, "error") &&
        identical(edge$free, FALSE) && is.finite(suppressWarnings(as.numeric(edge$fixedValue %||% NA_real_))) &&
        suppressWarnings(as.numeric(edge$fixedValue %||% NA_real_)) > 0
    }, logical(1)))
    if (constrained) result <- c(result, structural_canvas_name(latent))
  }
  unique(result)
}

structural_canvas_fixed_residual_scale_diagnostics <- function(snapshot, data, ordered = character(0)) {
  if (!is.data.frame(data)) return(data.frame())
  ordered <- unique(as.character(ordered))
  rows <- list()
  for (edge in snapshot$edges %||% list()) {
    if (!identical(edge$free, FALSE) || identical(edge$kind, "covariance")) next
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    if (is.null(from) || is.null(to) || !from$role %in% c("error", "disturbance") || !identical(to$role, "indicator")) next
    indicator <- structural_canvas_name(to)
    if (!indicator %in% names(data) || indicator %in% ordered || !is.numeric(data[[indicator]])) next
    fixed_value <- suppressWarnings(as.numeric(edge$fixedValue %||% NA_real_))
    observed_variance <- stats::var(data[[indicator]], na.rm = TRUE)
    if (!is.finite(fixed_value) || !is.finite(observed_variance)) next
    indicator_id <- as.character(to$id %||% "")
    parent_latents <- unique(vapply(Filter(function(measurement_edge) {
      if (identical(measurement_edge$kind, "covariance")) return(FALSE)
      measurement_from <- structural_canvas_node(snapshot, measurement_edge$from)
      measurement_to <- structural_canvas_node(snapshot, measurement_edge$to)
      (identical(as.character(measurement_from$id %||% ""), indicator_id) && identical(measurement_to$role, "latent")) ||
        (identical(as.character(measurement_to$id %||% ""), indicator_id) && identical(measurement_from$role, "latent"))
    }, snapshot$edges %||% list()), function(measurement_edge) {
      measurement_from <- structural_canvas_node(snapshot, measurement_edge$from)
      measurement_to <- structural_canvas_node(snapshot, measurement_edge$to)
      as.character(if (identical(measurement_from$role, "latent")) measurement_from$id else measurement_to$id)
    }, character(1)))
    parent_indicator_counts <- vapply(parent_latents, function(parent_id) {
      length(unique(vapply(Filter(function(measurement_edge) {
        if (identical(measurement_edge$kind, "covariance")) return(FALSE)
        measurement_from <- structural_canvas_node(snapshot, measurement_edge$from)
        measurement_to <- structural_canvas_node(snapshot, measurement_edge$to)
        (identical(as.character(measurement_from$id %||% ""), parent_id) && identical(measurement_to$role, "indicator")) ||
          (identical(as.character(measurement_to$id %||% ""), parent_id) && identical(measurement_from$role, "indicator"))
      }, snapshot$edges %||% list()), function(measurement_edge) {
        measurement_from <- structural_canvas_node(snapshot, measurement_edge$from)
        measurement_to <- structural_canvas_node(snapshot, measurement_edge$to)
        as.character(if (identical(measurement_from$role, "indicator")) measurement_from$id else measurement_to$id)
      }, character(1))))
    }, integer(1))
    single_indicator_factor <- length(parent_indicator_counts) > 0L && any(parent_indicator_counts == 1L)
    status <- if (fixed_value > observed_variance) "Exceeds observed variance" else if (fixed_value == observed_variance) "Equals observed variance" else "Within observed variance"
    rows[[length(rows) + 1L]] <- data.frame(
      Indicator = indicator, `Fixed residual variance` = fixed_value,
      `Observed variance` = observed_variance,
      `Implied maximum common variance` = observed_variance - fixed_value,
      `Single-indicator factor` = single_indicator_factor,
      Status = status, check.names = FALSE
    )
  }
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

structural_canvas_indicator_loading_guidance <- function(beta, ci_lower, ci_upper, residual_variance, cross_loaded = FALSE) {
  if (!is.finite(residual_variance) || residual_variance < 0 || residual_variance > 1) return("Review residual variance")
  if (isTRUE(cross_loaded)) return("Review cross-loading")
  if (!is.finite(beta) || !is.finite(ci_lower) || !is.finite(ci_upper)) return("Not assessed")
  if (ci_lower <= 0 && ci_upper >= 0) return("Loading CI includes 0")
  if (abs(beta) < .40) return("Weak loading review")
  "No loading flag"
}

structural_canvas_residual_diagnostics <- function(fit, cutoff = 1.96, top_n = 20L) {
  residual_covariance <- function(type) {
    value <- tryCatch(lavaan::resid(fit, type = type), error = function(error) NULL)
    if (is.null(value)) return(NULL)
    if (!is.null(value$cov)) return(value$cov)
    groups <- Filter(function(group_value) is.list(group_value) && !is.null(group_value$cov), value)
    if (!length(groups)) return(NULL)
    result <- lapply(groups, `[[`, "cov")
    names(result) <- names(groups)
    result
  }
  standardized_raw <- residual_covariance("standardized")
  correlation_raw <- residual_covariance("cor")
  if (is.null(correlation_raw)) return(list(available = FALSE))
  standardized_available <- !is.null(standardized_raw)
  if (is.null(standardized_raw)) standardized_raw <- correlation_raw
  group_labels <- tryCatch(as.character(lavaan::lavInspect(fit, "group.label")), error = function(error) character(0))
  group_count <- tryCatch(as.integer(lavaan::lavInspect(fit, "ngroups")), error = function(error) 1L)
  normalize_residual_list <- function(value) {
    if (is.list(value) && !is.data.frame(value)) {
      result <- lapply(value, as.matrix)
      if (is.null(names(result)) || any(!nzchar(names(result)))) {
        labels <- group_labels
        if (length(labels) != length(result)) labels <- paste("Group", seq_along(result))
        names(result) <- labels
      }
      result
    } else {
      result <- list(as.matrix(value))
      names(result) <- if (length(group_labels) == 1L) group_labels else "Overall"
      result
    }
  }
  standardized_list <- normalize_residual_list(standardized_raw)
  correlation_list <- normalize_residual_list(correlation_raw)
  common_groups <- intersect(names(standardized_list), names(correlation_list))
  if (!length(common_groups)) return(list(available = FALSE))
  pair_table <- function(standardized, correlation, group_name = NULL) {
    standardized <- as.matrix(standardized)
    correlation <- as.matrix(correlation)
    if (!nrow(standardized) || !nrow(correlation)) return(data.frame())
    standardized[upper.tri(standardized, diag = TRUE)] <- NA_real_
    correlation[upper.tri(correlation, diag = TRUE)] <- NA_real_
    locations <- which(is.finite(standardized), arr.ind = TRUE)
    if (!nrow(locations)) return(data.frame())
    result <- data.frame(
      Indicator1 = rownames(standardized)[locations[, "row"]],
      Indicator2 = colnames(standardized)[locations[, "col"]],
      `Standardized residual` = standardized[locations],
      `Correlation residual` = correlation[locations],
      `Residual scale` = if (standardized_available) "Standardized" else "Correlation residual fallback",
      `Exceeds cutoff` = abs(standardized[locations]) >= cutoff,
      check.names = FALSE
    )
    if (!is.null(group_name)) result <- data.frame(Group = group_name, result, row.names = NULL, check.names = FALSE)
    result[order(-abs(result[["Standardized residual"]])), , drop = FALSE]
  }
  by_group <- stats::setNames(lapply(common_groups, function(group_name) {
    standardized <- standardized_list[[group_name]]
    correlation <- correlation_list[[group_name]]
    pairs <- pair_table(standardized, correlation, group_name)
    list(
      standardized = {
        value <- as.matrix(standardized)
        value[upper.tri(value, diag = TRUE)] <- NA_real_
        value
      },
      correlation = {
        value <- as.matrix(correlation)
        value[upper.tri(value, diag = TRUE)] <- NA_real_
        value
      },
      pairs = pairs,
      largest = pairs[pairs[["Exceeds cutoff"]], , drop = FALSE]
    )
  }), common_groups)
  all_pairs <- Filter(function(value) nrow(value), lapply(by_group, `[[`, "pairs"))
  all_pairs <- if (length(all_pairs)) do.call(rbind, all_pairs) else data.frame()
  group_largest <- if (nrow(all_pairs)) all_pairs[all_pairs[["Exceeds cutoff"]], , drop = FALSE] else data.frame()
  if (nrow(group_largest)) group_largest <- utils::head(group_largest[order(-abs(group_largest[["Standardized residual"]])), , drop = FALSE], as.integer(top_n))
  group_summary <- do.call(rbind, lapply(names(by_group), function(group_name) {
    pairs <- by_group[[group_name]]$pairs
    if (!nrow(pairs)) return(data.frame(Group = group_name, `Max |standardized residual|` = NA_real_, Indicator1 = NA_character_, Indicator2 = NA_character_, `Flagged residuals` = 0L, check.names = FALSE))
    top <- pairs[1L, , drop = FALSE]
    data.frame(
      Group = group_name,
      `Max |standardized residual|` = abs(top[["Standardized residual"]][[1L]]),
      Indicator1 = top$Indicator1[[1L]], Indicator2 = top$Indicator2[[1L]],
      `Flagged residuals` = sum(pairs[["Exceeds cutoff"]], na.rm = TRUE),
      check.names = FALSE
    )
  }))
  first_group <- common_groups[[1L]]
  largest <- by_group[[first_group]]$largest
  if (!is.null(group_count) && is.finite(group_count) && group_count == 1L && "Group" %in% names(largest)) {
    largest <- largest[, setdiff(names(largest), c("Group", "Exceeds cutoff")), drop = FALSE]
  }
  list(
    available = TRUE,
    standardized_available = standardized_available,
    standardized = by_group[[first_group]]$standardized,
    correlation = by_group[[first_group]]$correlation,
    largest = largest,
    cutoff = cutoff,
    by_group = by_group,
    group_summary = group_summary,
    group_largest = group_largest,
    group_pairs = all_pairs
  )
}

structural_canvas_factor_correlation_diagnostics <- function(fit) {
  correlations <- as.matrix(lavaan::lavInspect(fit, "cor.lv"))
  if (nrow(correlations) < 2L) return(data.frame())
  locations <- which(lower.tri(correlations), arr.ind = TRUE)
  values <- correlations[locations]
  severity <- vapply(abs(values), function(value) {
    if (!is.finite(value)) "Unavailable"
    else if (value >= 1) "Inadmissible"
    else if (value >= .95) "Severe"
    else if (value >= .90) "High"
    else if (value >= .85) "Review"
    else "Acceptable"
  }, character(1))
  data.frame(
    Factor1 = rownames(correlations)[locations[, "row"]],
    Factor2 = colnames(correlations)[locations[, "col"]],
    Correlation = values, `Absolute correlation` = abs(values), Severity = severity,
    check.names = FALSE
  )
}

structural_canvas_minimum_eigenvalue <- function(matrix_value) {
  matrix_value <- as.matrix(matrix_value)
  if (!length(matrix_value) || any(!is.finite(matrix_value))) return(NA_real_)
  suppressWarnings(min(eigen((matrix_value + t(matrix_value)) / 2, symmetric = TRUE, only.values = TRUE)$values))
}

structural_canvas_symmetric_condition_number <- function(matrix_value) {
  matrix_value <- as.matrix(matrix_value)
  if (!length(matrix_value) || any(!is.finite(matrix_value))) return(NA_real_)
  values <- suppressWarnings(abs(eigen((matrix_value + t(matrix_value)) / 2, symmetric = TRUE, only.values = TRUE)$values))
  if (!length(values) || max(values) == 0 || min(values) == 0) return(Inf)
  max(values) / min(values)
}

structural_canvas_latent_correlation_intervals <- function(fit, level = .95) {
  latent <- lavaan::lavNames(fit, "lv")
  if (length(latent) < 2L) return(data.frame())
  standardized <- lavaan::standardizedSolution(fit, type = "std.lv", ci = TRUE, level = level)
  rows <- standardized$op == "~~" & standardized$lhs != standardized$rhs &
    standardized$lhs %in% latent & standardized$rhs %in% latent
  values <- standardized[rows, c("lhs", "rhs", "est.std", "ci.lower", "ci.upper", "pvalue"), drop = FALSE]
  if (!nrow(values)) return(data.frame())
  duplicated_pair <- duplicated(vapply(seq_len(nrow(values)), function(index) {
    paste(sort(c(values$lhs[[index]], values$rhs[[index]])), collapse = "\r")
  }, character(1)))
  values <- values[!duplicated_pair, , drop = FALSE]
  parameter_table <- lavaan::parameterTable(fit)
  covariance_parameters <- parameter_table[
    parameter_table$op == "~~" & parameter_table$lhs != parameter_table$rhs &
      parameter_table$lhs %in% latent & parameter_table$rhs %in% latent,
    c("lhs", "rhs", "free"), drop = FALSE
  ]
  pair_key <- function(lhs, rhs) paste(sort(c(lhs, rhs)), collapse = "\r")
  value_keys <- vapply(seq_len(nrow(values)), function(index) pair_key(values$lhs[[index]], values$rhs[[index]]), character(1))
  parameter_keys <- vapply(seq_len(nrow(covariance_parameters)), function(index) pair_key(covariance_parameters$lhs[[index]], covariance_parameters$rhs[[index]]), character(1))
  free <- covariance_parameters$free[match(value_keys, parameter_keys)]
  data.frame(
    `Factor 1` = values$lhs, `Factor 2` = values$rhs,
    r = values$est.std, `CI lower` = values$ci.lower, `CI upper` = values$ci.upper,
    p = values$pvalue, Type = ifelse(!is.na(free) & free > 0L, "Estimated", "Fixed"),
    `CI reaches |1|` = ifelse(
      is.finite(values$ci.lower) & is.finite(values$ci.upper),
      ifelse(values$ci.lower <= -1 | values$ci.upper >= 1, "Yes", "No"), "Not assessed"
    ),
    check.names = FALSE
  )
}

structural_canvas_ordered_category_diagnostics <- function(data, variables) {
  variables <- intersect(unique(as.character(variables)), names(data))
  rows <- list()
  for (name in variables) {
    value <- data[[name]]
    levels_value <- if (is.factor(value)) levels(value) else sort(unique(value[!is.na(value)]))
    counts <- table(factor(value, levels = levels_value), useNA = "no")
    valid_n <- sum(counts)
    sparse_limit <- max(5L, ceiling(.01 * valid_n))
    for (index in seq_along(counts)) {
      count <- as.integer(counts[[index]])
      rows[[length(rows) + 1L]] <- data.frame(
        Indicator = name, Category = names(counts)[[index]], Count = count,
        Percent = if (valid_n > 0) 100 * count / valid_n else NA_real_,
        Status = if (count == 0L) "Empty" else if (count <= sparse_limit) "Sparse" else if (valid_n > 0 && count / valid_n >= .95) "Dominant (>=95%)" else "Adequate",
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

structural_canvas_ordered_pair_diagnostics <- function(data, variables) {
  variables <- intersect(unique(as.character(variables)), names(data))
  if (length(variables) < 2L) return(data.frame())
  pairs <- utils::combn(variables, 2L, simplify = FALSE)
  rows <- lapply(pairs, function(pair) {
    first <- data[[pair[[1L]]]]
    second <- data[[pair[[2L]]]]
    first_levels <- if (is.factor(first)) levels(first) else sort(unique(first[!is.na(first)]))
    second_levels <- if (is.factor(second)) levels(second) else sort(unique(second[!is.na(second)]))
    contingency <- table(factor(first, levels = first_levels), factor(second, levels = second_levels), useNA = "no")
    valid_n <- sum(contingency)
    sparse_limit <- max(5L, ceiling(.01 * valid_n))
    counts <- as.integer(contingency)
    zero <- sum(counts == 0L)
    sparse <- sum(counts > 0L & counts <= sparse_limit)
    data.frame(
      `Indicator 1` = pair[[1L]], `Indicator 2` = pair[[2L]], `Valid pairs` = valid_n,
      `Cells` = length(counts), `Empty cells` = zero, `Sparse nonempty cells` = sparse,
      `Minimum nonzero count` = if (any(counts > 0L)) min(counts[counts > 0L]) else 0L,
      `Empty %` = if (length(counts)) 100 * zero / length(counts) else NA_real_,
      Status = if (zero > 0L || sparse > 0L) "Review" else "Adequate", check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

structural_canvas_error_covariance_diagnostics <- function(snapshot) {
  edges <- snapshot$edges %||% list()
  covariance_edges <- Filter(function(edge) {
    if (!identical(edge$kind, "covariance")) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    !is.null(from) && !is.null(to) && identical(from$role, "error") && identical(to$role, "error")
  }, edges)
  indicator_count <- sum(vapply(snapshot$nodes %||% list(), function(node) identical(node$role, "indicator"), logical(1)))
  possible <- if (indicator_count >= 2L) choose(indicator_count, 2L) else 0
  count <- length(covariance_edges)
  ratio <- if (possible > 0) count / possible else 0
  list(count = count, possible = possible, ratio = ratio, status = if (count == 0L) "None" else if (count >= 3L || ratio > .20) "Review complexity" else "Limited")
}

structural_canvas_identification_diagnostics <- function(snapshot) {
  nodes <- snapshot$nodes %||% list()
  edges <- snapshot$edges %||% list()
  latents <- Filter(function(node) identical(node$role, "latent"), nodes)
  issues <- list()
  add <- function(severity, element, code, message) {
    issues[[length(issues) + 1L]] <<- data.frame(Severity = severity, Element = element, Code = code, Message = message, stringsAsFactors = FALSE)
  }
  higher_edges <- Filter(function(edge) identical(as.character(edge$pathType %||% ""), "higherOrder"), edges)
  for (latent in latents) {
    latent_id <- as.character(latent$id)
    latent_name <- structural_canvas_name(latent)
    observed <- unique(vapply(Filter(function(edge) {
      if (identical(edge$kind, "covariance")) return(FALSE)
      from <- structural_canvas_node(snapshot, edge$from)
      to <- structural_canvas_node(snapshot, edge$to)
      (identical(as.character(from$id %||% ""), latent_id) && identical(to$role, "indicator")) ||
        (identical(as.character(to$id %||% ""), latent_id) && identical(from$role, "indicator"))
    }, edges), function(edge) {
      from <- structural_canvas_node(snapshot, edge$from)
      to <- structural_canvas_node(snapshot, edge$to)
      structural_canvas_name(if (identical(from$role, "indicator")) from else to)
    }, character(1)))
    children <- unique(vapply(Filter(function(edge) identical(as.character(edge$from), latent_id), higher_edges), function(edge) structural_canvas_name(structural_canvas_node(snapshot, edge$to)), character(1)))
    if (!length(observed) && !length(children)) add("Error", latent_name, "unmeasured_latent", "The latent variable has neither observed indicators nor lower-order factors.")
    single_indicator_constrained <- FALSE
    if (length(observed) == 1L && !length(children)) {
      indicator_node <- Filter(function(node) identical(node$role, "indicator") && identical(structural_canvas_name(node), observed[[1L]]), nodes)
      indicator_id <- if (length(indicator_node)) as.character(indicator_node[[1L]]$id) else ""
      single_indicator_constrained <- any(vapply(edges, function(edge) {
        from <- structural_canvas_node(snapshot, edge$from)
        identical(as.character(edge$to), indicator_id) && !is.null(from) && identical(from$role, "error") &&
          identical(edge$free, FALSE) && is.finite(suppressWarnings(as.numeric(edge$fixedValue %||% NA_real_))) &&
          suppressWarnings(as.numeric(edge$fixedValue %||% NA_real_)) > 0
      }, logical(1)))
      if (!single_indicator_constrained) add("Error", latent_name, "single_indicator", "A single-indicator factor requires an externally justified fixed residual variance on its error path.")
      else add("Warning", latent_name, "single_indicator_constrained", "The single-indicator factor is identified using a fixed residual variance; document the external reliability basis for this constraint.")
    }
    if (length(observed) == 2L && !length(children)) add("Warning", latent_name, "two_indicators", "A two-indicator factor may require additional constraints or structural information for stable identification.")
    if (length(children) > 0L && length(children) < 3L) add("Error", latent_name, "few_lower_order_factors", "A higher-order factor requires at least three lower-order factors in the current automatic identification scheme.")
    if (length(observed) && length(children)) add("Warning", latent_name, "mixed_measurement_level", "The factor has both observed indicators and lower-order factors; verify that this hybrid measurement specification is intentional.")
  }
  if (length(higher_edges)) {
    child_ids <- vapply(higher_edges, function(edge) as.character(edge$to), character(1))
    for (child_id in unique(child_ids[duplicated(child_ids)])) {
      child <- structural_canvas_node(snapshot, child_id)
      add("Warning", structural_canvas_name(child), "multiple_higher_order_parents", "The lower-order factor loads on more than one higher-order factor; standard higher-order reliability summaries may not apply.")
    }
  }
  measurement_edges <- Filter(function(edge) {
    if (identical(edge$kind, "covariance") || identical(as.character(edge$pathType %||% ""), "higherOrder")) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    !is.null(from) && !is.null(to) &&
      ((identical(from$role, "latent") && identical(to$role, "indicator")) ||
        (identical(to$role, "latent") && identical(from$role, "indicator")))
  }, edges)
  indicator_parents <- split(measurement_edges, vapply(measurement_edges, function(edge) {
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    structural_canvas_name(if (identical(from$role, "indicator")) from else to)
  }, character(1)))
  for (indicator_name in names(indicator_parents)) {
    parents <- unique(vapply(indicator_parents[[indicator_name]], function(edge) {
      from <- structural_canvas_node(snapshot, edge$from)
      to <- structural_canvas_node(snapshot, edge$to)
      structural_canvas_name(if (identical(from$role, "latent")) from else to)
    }, character(1)))
    if (length(parents) > 1L) add("Warning", indicator_name, "cross_loading", paste0("The indicator loads on multiple factors: ", paste(parents, collapse = ", "), ". Review simple-structure assumptions and reliability/validity summaries."))
  }
  fixed_residual_edges <- Filter(function(edge) {
    if (!identical(edge$free, FALSE) || identical(edge$kind, "covariance")) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    !is.null(from) && !is.null(to) && from$role %in% c("error", "disturbance") && to$role %in% c("indicator", "latent")
  }, edges)
  for (edge in fixed_residual_edges) {
    target <- structural_canvas_node(snapshot, edge$to)
    value <- suppressWarnings(as.numeric(edge$fixedValue %||% NA_real_))
    if (!is.finite(value)) {
      add("Error", structural_canvas_name(target), "invalid_fixed_residual", "A fixed residual variance must be a finite nonnegative number.")
    } else if (value < 0) {
      add("Error", structural_canvas_name(target), "negative_fixed_residual", "A residual variance cannot be fixed to a negative value.")
    } else if (value == 0) {
      add("Warning", structural_canvas_name(target), "boundary_fixed_residual", "A zero fixed residual variance is a boundary constraint implying perfect residual-free measurement; provide strong substantive justification.")
    }
  }
  directed <- Filter(function(edge) !identical(edge$kind, "covariance"), edges)
  signatures <- vapply(directed, function(edge) paste(edge$from, edge$to, edge$pathType %||% "regression", sep = "|"), character(1))
  if (anyDuplicated(signatures)) add("Error", "Model", "duplicate_path", "Duplicate directed paths were found between the same endpoints.")
  result <- if (length(issues)) do.call(rbind, issues) else data.frame(Severity = character(0), Element = character(0), Code = character(0), Message = character(0), stringsAsFactors = FALSE)
  rownames(result) <- NULL
  result
}

structural_canvas_higher_order_results <- function(snapshot, fit) {
  higher_edges <- Filter(function(edge) identical(as.character(edge$pathType %||% ""), "higherOrder"), snapshot$edges %||% list())
  if (!length(higher_edges)) return(list(available = FALSE, reason = "No higher-order loading paths are specified."))
  raw <- lavaan::parameterEstimates(fit)
  standardized <- lavaan::standardizedSolution(fit, ci = TRUE, level = .95)
  rows <- lapply(higher_edges, function(edge) {
    parent <- structural_canvas_name(structural_canvas_node(snapshot, edge$from))
    child <- structural_canvas_name(structural_canvas_node(snapshot, edge$to))
    raw_row <- raw[raw$lhs == parent & raw$op == "=~" & raw$rhs == child, , drop = FALSE]
    std_row <- standardized[standardized$lhs == parent & standardized$op == "=~" & standardized$rhs == child, , drop = FALSE]
    child_r2 <- lavaan::lavInspect(fit, "r2")
    residual_row <- standardized[standardized$lhs == child & standardized$op == "~~" & standardized$rhs == child, , drop = FALSE]
    data.frame(
      HigherOrderFactor = parent, LowerOrderFactor = child,
      B = if (nrow(raw_row)) raw_row$est[[1L]] else NA_real_,
      BCILower = if (nrow(raw_row)) raw_row$ci.lower[[1L]] else NA_real_,
      BCIUpper = if (nrow(raw_row)) raw_row$ci.upper[[1L]] else NA_real_,
      SE = if (nrow(raw_row)) raw_row$se[[1L]] else NA_real_,
      z = if (nrow(raw_row)) raw_row$z[[1L]] else NA_real_,
      p = if (nrow(raw_row)) raw_row$pvalue[[1L]] else NA_real_,
      Beta = if (nrow(std_row)) std_row$est.std[[1L]] else NA_real_,
      BetaCILower = if (nrow(std_row)) std_row$ci.lower[[1L]] else NA_real_,
      BetaCIUpper = if (nrow(std_row)) std_row$ci.upper[[1L]] else NA_real_,
      R2 = as.numeric(child_r2[child]),
      R2CILower = if (nrow(residual_row)) 1 - residual_row$ci.upper[[1L]] else NA_real_,
      R2CIUpper = if (nrow(residual_row)) 1 - residual_row$ci.lower[[1L]] else NA_real_,
      ResidualVariance = if (nrow(residual_row)) residual_row$est.std[[1L]] else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  list(available = TRUE, table = do.call(rbind, rows), higher_edges = higher_edges)
}

structural_canvas_omega_h <- function(snapshot, fit) {
  higher <- structural_canvas_higher_order_results(snapshot, fit)
  if (!isTRUE(higher$available)) return(list(available = FALSE, reason = higher$reason))
  parents <- unique(higher$table$HigherOrderFactor)
  if (length(parents) != 1L) return(list(available = FALSE, reason = "Omega-h requires exactly one higher-order general factor."))
  if (anyDuplicated(higher$table$LowerOrderFactor)) return(list(available = FALSE, reason = "Omega-h is not reported when a lower-order factor has multiple higher-order loadings."))
  standardized <- lavaan::standardizedSolution(fit)
  observed <- lavaan::lavNames(fit, "ov")
  first_loadings <- standardized[standardized$op == "=~" & standardized$rhs %in% observed, c("lhs", "rhs", "est.std"), drop = FALSE]
  children <- higher$table$LowerOrderFactor
  first_loadings <- first_loadings[first_loadings$lhs %in% children, , drop = FALSE]
  if (!nrow(first_loadings)) return(list(available = FALSE, reason = "No observed indicators were found under the lower-order factors."))
  if (anyDuplicated(first_loadings$rhs)) return(list(available = FALSE, reason = "Omega-h is not reported with cross-loaded observed indicators."))
  second_loadings <- stats::setNames(higher$table$Beta, higher$table$LowerOrderFactor)
  general_loadings <- first_loadings$est.std * second_loadings[first_loadings$lhs]
  implied_covariance <- as.matrix(lavaan::fitted(fit)$cov)
  indicator_names <- first_loadings$rhs
  if (!all(indicator_names %in% rownames(implied_covariance))) return(list(available = FALSE, reason = "The model-implied indicator covariance matrix is unavailable."))
  implied_correlation <- stats::cov2cor(implied_covariance[indicator_names, indicator_names, drop = FALSE])
  denominator <- sum(implied_correlation, na.rm = TRUE)
  omega_h <- if (is.finite(denominator) && denominator > 0) sum(general_loadings, na.rm = TRUE)^2 / denominator else NA_real_
  list(
    available = is.finite(omega_h), omega_h = omega_h, higher_order_factor = parents[[1L]],
    indicators = length(indicator_names), general_loadings = stats::setNames(as.numeric(general_loadings), indicator_names),
    reason = if (is.finite(omega_h)) "" else "Omega-h denominator is not positive and finite."
  )
}

structural_canvas_higher_order_loading_guidance <- function(value) {
  if (!is.finite(value)) "Not assessed"
  else if (abs(value) < .40) "Weak loading review"
  else "No loading flag"
}

structural_canvas_omega_h_guidance <- function(value) {
  if (!is.finite(value) || value < 0 || value > 1) "Review inadmissible coefficient"
  else if (value < .70) "Below common .70 guideline"
  else "Meets common .70 guideline"
}

structural_canvas_factor_score_quality <- function(fit) {
  predicted <- tryCatch(lavaan::lavPredict(fit, method = "regression", rel = TRUE), error = function(error) NULL)
  if (is.null(predicted)) return(data.frame())
  reliability <- attr(predicted, "rel", exact = TRUE)
  if (is.list(reliability)) reliability <- unlist(reliability, use.names = TRUE)
  reliability <- as.numeric(reliability)
  factor_names <- names(unlist(attr(predicted, "rel", exact = TRUE), use.names = TRUE))
  if (!length(factor_names) || length(factor_names) != length(reliability)) factor_names <- colnames(as.matrix(predicted))
  if (!length(reliability) || length(factor_names) != length(reliability)) return(data.frame())
  determinacy <- ifelse(is.finite(reliability) & reliability >= 0, sqrt(reliability), NA_real_)
  guidance <- vapply(determinacy, function(value) {
    if (!is.finite(value) || value > 1) "Not assessed"
    else if (value >= .90) "Strong"
    else if (value >= .80) "Acceptable for cautious use"
    else "Low; avoid individual-score use"
  }, character(1))
  data.frame(Factor = factor_names, Determinacy = determinacy, `Score reliability` = reliability, Guidance = guidance, check.names = FALSE)
}

run_structural_canvas_analysis <- function(snapshot, data, analysis_type, estimator = "ML", missing = "fiml", std_lv = FALSE, ordered = character(0), nominal = character(0), residual_variance_fixes = numeric(0)) {
  nodes <- snapshot$nodes %||% list()
  edges <- snapshot$edges %||% list()
  residual_constraint_diagnostics <- if (analysis_type %in% c("cfa", "cbsem")) structural_canvas_identification_diagnostics(snapshot) else data.frame()
  residual_constraint_errors <- if (nrow(residual_constraint_diagnostics)) residual_constraint_diagnostics[
    residual_constraint_diagnostics$Severity == "Error" & residual_constraint_diagnostics$Code %in% c("single_indicator", "invalid_fixed_residual", "negative_fixed_residual"),
    , drop = FALSE
  ] else data.frame()
  if (nrow(residual_constraint_errors)) {
    stop(paste(residual_constraint_errors$Message, collapse = " "))
  }
  residual_scale <- if (analysis_type %in% c("cfa", "cbsem")) structural_canvas_fixed_residual_scale_diagnostics(snapshot, data, ordered) else data.frame()
  invalid_residual_scale <- if (nrow(residual_scale)) residual_scale[
    residual_scale$Status != "Within observed variance" & residual_scale[["Single-indicator factor"]], , drop = FALSE
  ] else data.frame()
  if (nrow(invalid_residual_scale)) {
    details <- vapply(seq_len(nrow(invalid_residual_scale)), function(index) paste0(
      invalid_residual_scale$Indicator[[index]], ": fixed residual = ", format_decimal3(invalid_residual_scale[["Fixed residual variance"]][[index]]),
      ", observed variance = ", format_decimal3(invalid_residual_scale[["Observed variance"]][[index]])
    ), character(1))
    stop(paste0("For a continuous single-indicator factor, the fixed residual variance must be smaller than the observed variance; otherwise nonpositive common variance is imposed by the single-indicator decomposition. ", paste(details, collapse = "; "), "."))
  }
  latents <- Filter(function(node) identical(node$role, "latent"), nodes)
  measurement_lines <- vapply(latents, function(latent) {
    indicator_edges <- Filter(function(edge) {
      from <- structural_canvas_node(snapshot, edge$from)
      to <- structural_canvas_node(snapshot, edge$to)
      (identical(from$id, latent$id) && identical(to$role, "indicator")) ||
        (identical(to$id, latent$id) && identical(from$role, "indicator"))
    }, edges)
    indicators <- vapply(indicator_edges, function(edge) {
      from <- structural_canvas_node(snapshot, edge$from)
      to <- structural_canvas_node(snapshot, edge$to)
      indicator_name <- structural_canvas_name(if (identical(from$role, "indicator")) from else to)
      structural_canvas_parameter_term(edge, indicator_name)
    }, character(1))
    paste(structural_canvas_name(latent), "=~", paste(indicators, collapse = " + "))
  }, character(1))
  measurement_lines <- measurement_lines[grepl("\\S+\\s*=~\\s*\\S+", measurement_lines)]
  higher_order_edges <- Filter(function(edge) {
    if (identical(edge$kind, "covariance") || !identical(as.character(edge$pathType %||% "regression"), "higherOrder")) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    !is.null(from) && !is.null(to) && identical(from$role, "latent") && identical(to$role, "latent")
  }, edges)
  higher_order_groups <- split(higher_order_edges, vapply(higher_order_edges, function(edge) as.character(edge$from), character(1)))
  higher_order_lines <- vapply(higher_order_groups, function(group) {
    parent <- structural_canvas_node(snapshot, group[[1L]]$from)
    children <- vapply(group, function(edge) {
      child_name <- structural_canvas_name(structural_canvas_node(snapshot, edge$to))
      structural_canvas_parameter_term(edge, child_name)
    }, character(1))
    paste(structural_canvas_name(parent), "=~", paste(children, collapse = " + "))
  }, character(1))
  structural_lines <- vapply(Filter(function(edge) {
    if (identical(edge$kind, "covariance")) return(FALSE)
    if (identical(as.character(edge$pathType %||% "regression"), "higherOrder")) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    identical(from$role, "latent") && identical(to$role, "latent")
  }, edges), function(edge) {
    predictor_name <- structural_canvas_name(structural_canvas_node(snapshot, edge$from))
    paste(structural_canvas_name(structural_canvas_node(snapshot, edge$to)), "~", structural_canvas_parameter_term(edge, predictor_name))
  }, character(1))
  covariance_target_name <- function(node) {
    if (is.null(node)) return("")
    if (identical(node$role, "latent") || identical(node$role, "indicator")) return(structural_canvas_name(node))
    if (node$role %in% c("error", "disturbance")) {
      target_edge <- Filter(function(edge) !identical(edge$kind, "covariance") && identical(as.character(edge$from), as.character(node$id)), edges)
      if (length(target_edge)) return(structural_canvas_name(structural_canvas_node(snapshot, target_edge[[1]]$to)))
    }
    ""
  }
  covariance_lines <- vapply(Filter(function(edge) identical(edge$kind, "covariance"), edges), function(edge) {
    from_name <- covariance_target_name(structural_canvas_node(snapshot, edge$from))
    to_name <- covariance_target_name(structural_canvas_node(snapshot, edge$to))
    if (!nzchar(from_name) || !nzchar(to_name)) "" else paste(from_name, "~~", structural_canvas_parameter_term(edge, to_name))
  }, character(1))
  covariance_lines <- covariance_lines[nzchar(covariance_lines)]
  residual_parameter_edges <- Filter(function(edge) {
    if (identical(edge$kind, "covariance") || !structural_canvas_has_parameter_modifier(edge)) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    !is.null(from) && !is.null(to) && from$role %in% c("error", "disturbance") && to$role %in% c("indicator", "latent")
  }, edges)
  residual_parameter_lines <- vapply(residual_parameter_edges, function(edge) {
    target_name <- structural_canvas_name(structural_canvas_node(snapshot, edge$to))
    paste(target_name, "~~", structural_canvas_parameter_term(edge, target_name))
  }, character(1))
  residual_fix_input <- residual_variance_fixes %||% numeric(0)
  residual_variance_fixes <- as.numeric(residual_fix_input)
  names(residual_variance_fixes) <- names(residual_fix_input)
  if (length(residual_variance_fixes)) {
    fix_names <- names(residual_variance_fixes)
    if (is.null(fix_names) || any(is.na(fix_names) | !nzchar(fix_names))) stop("Every residual-variance sensitivity value must have an indicator name.")
    if (anyDuplicated(fix_names)) stop("Residual-variance sensitivity values contain duplicate indicator names.")
    unknown_fix_names <- setdiff(fix_names, names(data))
    if (length(unknown_fix_names)) stop(paste0("Residual-variance sensitivity indicators were not found in the data: ", paste(unknown_fix_names, collapse = ", "), "."))
    if (any(!is.finite(residual_variance_fixes) | residual_variance_fixes <= 0)) stop("Residual-variance sensitivity values must be finite and greater than zero.")
    existing_residual_targets <- unique(vapply(residual_parameter_edges, function(edge) structural_canvas_name(structural_canvas_node(snapshot, edge$to)), character(1)))
    conflicting_fix_names <- intersect(fix_names, existing_residual_targets)
    if (length(conflicting_fix_names)) stop(paste0("Residual-variance sensitivity values conflict with existing canvas residual constraints for: ", paste(conflicting_fix_names, collapse = ", "), ". Remove the existing fixed/start/labeled constraint or do not apply the sensitivity fix."))
  }
  residual_fix_lines <- if (length(residual_variance_fixes)) vapply(names(residual_variance_fixes), function(name) {
    paste(structural_canvas_name(list(name = name)), "~~", paste0(format(residual_variance_fixes[[name]], scientific = FALSE, digits = 15, trim = TRUE), "*", structural_canvas_name(list(name = name))))
  }, character(1)) else character(0)

  if (analysis_type %in% c("cfa", "cbsem")) {
    syntax <- paste(c(measurement_lines, higher_order_lines, structural_lines, covariance_lines, residual_parameter_lines, residual_fix_lines), collapse = "\n")
    if (!nzchar(syntax)) stop("The model does not contain estimable paths.")
    estimator <- toupper(as.character(estimator %||% "ML"))
    missing <- as.character(missing %||% "fiml")
    if (identical(estimator, "WLSMV") && identical(missing, "fiml")) missing <- "pairwise"
    ordered <- intersect(unique(as.character(ordered %||% character(0))), names(data))
    nominal <- intersect(unique(as.character(nominal %||% character(0))), names(data))
    if (length(nominal)) {
      stop(sprintf("Nominal indicators are not supported by standard CFA/SEM: %s.", paste(nominal, collapse = ", ")))
    }
    if (length(residual_variance_fixes) && (length(ordered) || estimator %in% c("WLSMV", "DWLS"))) {
      stop("Fixed residual-variance sensitivity analysis is supported only for continuous indicators estimated with ML or MLR.")
    }
    if (length(residual_variance_fixes)) {
      fix_observed_variances <- vapply(names(residual_variance_fixes), function(name) {
        if (!is.numeric(data[[name]])) return(NA_real_)
        stats::var(data[[name]], na.rm = TRUE)
      }, numeric(1))
      if (any(!is.finite(fix_observed_variances) | fix_observed_variances <= 0)) stop("Residual-variance sensitivity analysis requires a positive observed variance for every continuous indicator.")
      if (any(residual_variance_fixes >= fix_observed_variances)) stop("Each residual-variance sensitivity value must be smaller than its indicator's observed variance.")
    }
    if (identical(estimator, "WLSMV") && !length(ordered)) {
      stop("WLSMV requires at least one binary, categorical, or ordinal indicator.")
    }
    fit <- if (identical(analysis_type, "cfa")) {
      lavaan::cfa(syntax, data = data, estimator = estimator, missing = missing, std.lv = isTRUE(std_lv), ordered = ordered, auto.cov.lv.x = FALSE)
    } else {
      lavaan::sem(syntax, data = data, estimator = estimator, missing = missing, std.lv = isTRUE(std_lv), ordered = ordered, auto.cov.lv.x = FALSE)
    }
    converged <- isTRUE(lavaan::lavInspect(fit, "converged"))
    post_check <- isTRUE(lavaan::lavInspect(fit, "post.check"))
    model_df <- suppressWarnings(as.numeric(lavaan::fitMeasures(fit, "df")[[1L]]))
    theta <- as.matrix(lavaan::lavInspect(fit, "theta"))
    negative_residuals <- if (length(theta)) rownames(theta)[diag(theta) < 0] else character(0)
    latent_covariance <- as.matrix(lavaan::lavInspect(fit, "cov.lv"))
    parameter_covariance <- tryCatch(as.matrix(lavaan::lavInspect(fit, "vcov")), error = function(error) matrix(numeric(0), 0L, 0L))
    negative_latent_variances <- if (length(latent_covariance)) rownames(latent_covariance)[diag(latent_covariance) < 0] else character(0)
    theta_min_eigenvalue <- structural_canvas_minimum_eigenvalue(theta)
    latent_min_eigenvalue <- structural_canvas_minimum_eigenvalue(latent_covariance)
    parameter_min_eigenvalue <- structural_canvas_minimum_eigenvalue(parameter_covariance)
    theta_tolerance <- sqrt(.Machine$double.eps) * max(1, if (length(theta)) max(abs(diag(theta)), na.rm = TRUE) else 1)
    latent_tolerance <- sqrt(.Machine$double.eps) * max(1, if (length(latent_covariance)) max(abs(diag(latent_covariance)), na.rm = TRUE) else 1)
    parameter_scale <- if (length(parameter_covariance)) max(abs(diag(parameter_covariance)), na.rm = TRUE) else NA_real_
    parameter_tolerance <- if (is.finite(parameter_scale)) sqrt(.Machine$double.eps) * parameter_scale else NA_real_
    non_psd_theta <- is.finite(theta_min_eigenvalue) && theta_min_eigenvalue < -theta_tolerance
    non_psd_latent_covariance <- is.finite(latent_min_eigenvalue) && latent_min_eigenvalue < -latent_tolerance
    near_singular_theta <- is.finite(theta_min_eigenvalue) && !non_psd_theta && theta_min_eigenvalue <= theta_tolerance
    near_singular_latent_covariance <- is.finite(latent_min_eigenvalue) && !non_psd_latent_covariance && latent_min_eigenvalue <= latent_tolerance
    non_psd_parameter_covariance <- is.finite(parameter_min_eigenvalue) && parameter_min_eigenvalue < -parameter_tolerance
    near_singular_parameter_covariance <- is.finite(parameter_min_eigenvalue) && !non_psd_parameter_covariance && parameter_min_eigenvalue <= parameter_tolerance
    theta_condition_number <- structural_canvas_symmetric_condition_number(theta)
    latent_condition_number <- structural_canvas_symmetric_condition_number(latent_covariance)
    parameter_condition_number <- structural_canvas_symmetric_condition_number(parameter_covariance)
    ill_conditioned_theta <- is.finite(theta_condition_number) && theta_condition_number > 1e8
    ill_conditioned_latent_covariance <- is.finite(latent_condition_number) && latent_condition_number > 1e8
    ill_conditioned_parameter_covariance <- is.finite(parameter_condition_number) && parameter_condition_number > 1e8
    latent_correlations <- as.matrix(lavaan::lavInspect(fit, "cor.lv"))
    invalid_correlations <- length(latent_correlations) > 1L && any(abs(latent_correlations[row(latent_correlations) != col(latent_correlations)]) >= 1, na.rm = TRUE)
    shared_admissibility <- structural_canvas_fit_admissibility(fit)
    return(list(
      fit = fit, syntax = syntax, converged = converged, post_check = post_check,
      identified = is.finite(model_df) && model_df >= 0,
      df = model_df,
      admissible = isTRUE(shared_admissibility$admissible),
      admissibility_reasons = shared_admissibility$reasons,
      negative_residuals = negative_residuals, negative_latent_variances = negative_latent_variances,
      theta_min_eigenvalue = theta_min_eigenvalue, latent_min_eigenvalue = latent_min_eigenvalue,
      non_psd_theta = non_psd_theta, non_psd_latent_covariance = non_psd_latent_covariance,
      near_singular_theta = near_singular_theta, near_singular_latent_covariance = near_singular_latent_covariance,
      parameter_min_eigenvalue = parameter_min_eigenvalue,
      non_psd_parameter_covariance = non_psd_parameter_covariance, near_singular_parameter_covariance = near_singular_parameter_covariance,
      theta_condition_number = theta_condition_number, latent_condition_number = latent_condition_number,
      ill_conditioned_theta = ill_conditioned_theta, ill_conditioned_latent_covariance = ill_conditioned_latent_covariance,
      parameter_condition_number = parameter_condition_number, ill_conditioned_parameter_covariance = ill_conditioned_parameter_covariance,
      invalid_correlations = invalid_correlations
    ))
  }

  constructs <- lapply(latents, function(latent) {
    indicator_names <- vapply(Filter(function(edge) {
      from <- structural_canvas_node(snapshot, edge$from)
      to <- structural_canvas_node(snapshot, edge$to)
      (identical(from$id, latent$id) && identical(to$role, "indicator")) ||
        (identical(to$id, latent$id) && identical(from$role, "indicator"))
    }, edges), function(edge) {
      from <- structural_canvas_node(snapshot, edge$from)
      to <- structural_canvas_node(snapshot, edge$to)
      structural_canvas_name(if (identical(from$role, "indicator")) from else to)
    }, character(1))
    if (identical(latent$measurementMode %||% "reflective", "formative")) {
      seminr::composite(structural_canvas_name(latent), indicator_names)
    } else {
      seminr::reflective(structural_canvas_name(latent), indicator_names)
    }
  })
  path_specs <- lapply(Filter(function(edge) {
    if (identical(edge$kind, "covariance")) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    identical(from$role, "latent") && identical(to$role, "latent")
  }, edges), function(edge) {
    seminr::paths(
      from = structural_canvas_name(structural_canvas_node(snapshot, edge$from)),
      to = structural_canvas_name(structural_canvas_node(snapshot, edge$to))
    )
  })
  fit <- seminr::estimate_pls(
    data = data,
    measurement_model = do.call(seminr::constructs, constructs),
    structural_model = if (length(path_specs)) do.call(seminr::relationships, path_specs) else NULL
  )
  list(fit = fit, converged = TRUE)
}

structural_canvas_allowed_mi <- function(snapshot, fit, mode = "theory") {
  nodes <- snapshot$nodes %||% list()
  edges <- snapshot$edges %||% list()
  latents <- Filter(function(node) identical(node$role, "latent"), nodes)
  latent_ids <- vapply(latents, function(node) as.character(node$id), character(1))
  latent_names <- stats::setNames(vapply(latents, structural_canvas_name, character(1)), latent_ids)
  name_to_id <- stats::setNames(names(latent_names), unname(latent_names))
  structural_edges <- Filter(function(edge) {
    if (identical(edge$kind, "covariance")) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    identical(from$role, "latent") && identical(to$role, "latent")
  }, edges)
  incoming <- unique(vapply(structural_edges, function(edge) as.character(edge$to), character(1)))
  exogenous <- setdiff(latent_ids, incoming)
  endogenous <- intersect(latent_ids, incoming)
  indicator_parent <- character(0)
  for (edge in edges) {
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    if (identical(from$role, "latent") && identical(to$role, "indicator")) indicator_parent[[structural_canvas_name(to)]] <- as.character(from$id)
    if (identical(to$role, "latent") && identical(from$role, "indicator")) indicator_parent[[structural_canvas_name(from)]] <- as.character(to$id)
  }
  reachable <- function(from_id, to_id) {
    visit <- function(current, seen = character(0)) {
      if (current %in% seen) return(FALSE)
      targets <- vapply(Filter(function(edge) identical(as.character(edge$from), current), structural_edges), function(edge) as.character(edge$to), character(1))
      if (to_id %in% targets) return(TRUE)
      any(vapply(targets, visit, logical(1), seen = c(seen, current)))
    }
    visit(from_id)
  }
  mi <- lavaan::modindices(fit)
  if (!nrow(mi)) return(mi)
  valid_mi <- is.finite(mi$mi) & mi$mi >= 0
  mi$`MI p` <- NA_real_
  mi$`BH-adjusted p` <- NA_real_
  mi$`Multiplicity family size` <- sum(valid_mi)
  mi$`MI p`[valid_mi] <- stats::pchisq(mi$mi[valid_mi], df = 1L, lower.tail = FALSE)
  mi$`BH-adjusted p`[valid_mi] <- stats::p.adjust(mi$`MI p`[valid_mi], method = "BH")
  mi <- mi[valid_mi & mi$mi >= 4, , drop = FALSE]
  if (!nrow(mi)) return(mi)
  if (identical(mode, "conventional")) {
    mi$Allowed <- TRUE
    mi$Reason <- "Conventional modification-index output"
    return(mi[order(-mi$mi), , drop = FALSE])
  }
  mi$Allowed <- FALSE
  mi$Reason <- "Not allowed"
  for (index in seq_len(nrow(mi))) {
    if (!identical(mi$op[[index]], "~~") || identical(mi$lhs[[index]], mi$rhs[[index]])) next
    lhs <- mi$lhs[[index]]
    rhs <- mi$rhs[[index]]
    lhs_parent <- if (lhs %in% names(indicator_parent)) indicator_parent[[lhs]] else ""
    rhs_parent <- if (rhs %in% names(indicator_parent)) indicator_parent[[rhs]] else ""
    if (nzchar(lhs_parent) && identical(lhs_parent, rhs_parent)) {
      mi$Allowed[[index]] <- TRUE
      mi$Reason[[index]] <- "Measurement errors within the same latent variable"
      next
    }
    lhs_id <- if (lhs %in% names(name_to_id)) name_to_id[[lhs]] else ""
    rhs_id <- if (rhs %in% names(name_to_id)) name_to_id[[rhs]] else ""
    if (nzchar(lhs_id) && nzchar(rhs_id) && lhs_id %in% exogenous && rhs_id %in% exogenous) {
      mi$Allowed[[index]] <- TRUE
      mi$Reason[[index]] <- "Covariance between exogenous latent variables"
      next
    }
    if (nzchar(lhs_id) && nzchar(rhs_id) && lhs_id %in% endogenous && rhs_id %in% endogenous &&
        !reachable(lhs_id, rhs_id) && !reachable(rhs_id, lhs_id)) {
      mi$Allowed[[index]] <- TRUE
      mi$Reason[[index]] <- "Covariance between structurally unrelated disturbances"
    }
  }
  mi <- mi[mi$Allowed, , drop = FALSE]
  mi[order(-mi$mi), , drop = FALSE]
}

structural_canvas_fit_admissibility <- function(fit) {
  converged <- isTRUE(lavaan::lavInspect(fit, "converged"))
  post_check <- isTRUE(lavaan::lavInspect(fit, "post.check"))
  model_df <- suppressWarnings(as.numeric(lavaan::fitMeasures(fit, "df")[[1L]]))
  as_matrix_list <- function(value) {
    if (is.list(value) && !is.matrix(value)) lapply(value, as.matrix) else list(as.matrix(value))
  }
  theta <- as_matrix_list(lavaan::lavInspect(fit, "theta"))
  latent_covariance <- as_matrix_list(lavaan::lavInspect(fit, "cov.lv"))
  parameter_covariance <- tryCatch(as_matrix_list(lavaan::lavInspect(fit, "vcov")), error = function(error) list(matrix(numeric(0), 0L, 0L)))
  group_labels <- as.character(lavaan::lavInspect(fit, "group.label") %||% character(0))
  group_name <- function(index, total) if (total > 1L) {
    if (index <= length(group_labels) && nzchar(group_labels[[index]])) group_labels[[index]] else paste("group", index)
  } else "overall"
  matrix_status <- function(values, floor_scale = TRUE) {
    statuses <- lapply(values, function(value) {
      minimum <- structural_canvas_minimum_eigenvalue(value)
      scale <- if (length(value)) max(abs(diag(value)), na.rm = TRUE) else NA_real_
      tolerance <- if (is.finite(scale)) sqrt(.Machine$double.eps) * if (floor_scale) max(1, scale) else scale else NA_real_
      eigenvalues <- if (length(value) && nrow(value) == ncol(value) && all(is.finite(value))) {
        tryCatch(eigen((value + t(value)) / 2, symmetric = TRUE, only.values = TRUE)$values, error = function(error) numeric(0))
      } else numeric(0)
      list(
        minimum = minimum,
        non_psd = is.finite(minimum) && is.finite(tolerance) && minimum < -tolerance,
        boundary = is.finite(minimum) && is.finite(tolerance) && minimum >= -tolerance && minimum <= tolerance,
        boundary_count = if (length(eigenvalues) && is.finite(tolerance)) sum(abs(eigenvalues) <= tolerance) else 0L,
        condition_number = structural_canvas_symmetric_condition_number(value)
      )
    })
    list(
      minimum = vapply(statuses, `[[`, numeric(1), "minimum"),
      non_psd = any(vapply(statuses, `[[`, logical(1), "non_psd")),
      boundary = any(vapply(statuses, `[[`, logical(1), "boundary")),
      boundary_count = sum(vapply(statuses, `[[`, integer(1), "boundary_count")),
      condition_numbers = vapply(statuses, `[[`, numeric(1), "condition_number"),
      non_psd_indices = which(vapply(statuses, `[[`, logical(1), "non_psd")),
      boundary_indices = which(vapply(statuses, `[[`, logical(1), "boundary"))
    )
  }
  theta_status <- matrix_status(theta)
  latent_status <- matrix_status(latent_covariance)
  parameter_status <- matrix_status(parameter_covariance, floor_scale = FALSE)
  equality_constraint_count <- sum(lavaan::parameterTable(fit)$op == "==")
  unexplained_parameter_boundary <- parameter_status$boundary_count > equality_constraint_count
  negative_diagonal_names <- function(values) unlist(lapply(seq_along(values), function(index) {
    value <- values[[index]]
    if (!length(value)) return(character(0))
    names <- rownames(value)[diag(value) < 0]
    if (!length(names)) return(character(0))
    if (length(values) > 1L) paste0(group_name(index, length(values)), ":", names) else names
  }), use.names = FALSE)
  negative_residuals <- negative_diagonal_names(theta)
  negative_latent_variances <- negative_diagonal_names(latent_covariance)
  latent_correlations <- as_matrix_list(lavaan::lavInspect(fit, "cor.lv"))
  invalid_correlations <- any(vapply(latent_correlations, function(value) {
    length(value) > 1L && any(abs(value[row(value) != col(value)]) >= 1, na.rm = TRUE)
  }, logical(1)))
  reasons <- c(
    if (!converged) "nonconvergence",
    if (!post_check) "lavaan post.check failure",
    if (!is.finite(model_df) || model_df < 0) "invalid degrees of freedom",
    if (length(negative_residuals)) paste0("negative residual variance: ", paste(negative_residuals, collapse = ", ")),
    if (length(negative_latent_variances)) paste0("negative latent variance: ", paste(negative_latent_variances, collapse = ", ")),
    if (theta_status$non_psd || theta_status$boundary) paste0("non-positive-definite or boundary residual covariance matrix: ", paste(vapply(unique(c(theta_status$non_psd_indices, theta_status$boundary_indices)), group_name, character(1), total = length(theta)), collapse = ", ")),
    if (latent_status$non_psd || latent_status$boundary) paste0("non-positive-definite or boundary latent covariance matrix: ", paste(vapply(unique(c(latent_status$non_psd_indices, latent_status$boundary_indices)), group_name, character(1), total = length(latent_covariance)), collapse = ", ")),
    if (parameter_status$non_psd || unexplained_parameter_boundary) paste0("non-positive-definite or unexplained boundary parameter covariance matrix (boundary dimensions = ", parameter_status$boundary_count, "; explicit equality constraints = ", equality_constraint_count, ")"),
    if (invalid_correlations) "absolute latent correlation at least 1"
  )
  list(
    admissible = !length(reasons), reasons = reasons,
    parameter_boundary_dimensions = parameter_status$boundary_count,
    equality_constraint_count = equality_constraint_count,
    group_labels = group_labels,
    residual_min_eigenvalue = if (length(theta_status$minimum) && any(is.finite(theta_status$minimum))) min(theta_status$minimum, na.rm = TRUE) else NA_real_,
    latent_min_eigenvalue = if (length(latent_status$minimum) && any(is.finite(latent_status$minimum))) min(latent_status$minimum, na.rm = TRUE) else NA_real_,
    parameter_min_eigenvalue = if (length(parameter_status$minimum) && any(is.finite(parameter_status$minimum))) min(parameter_status$minimum, na.rm = TRUE) else NA_real_,
    residual_condition_number = if (length(theta_status$condition_numbers) && any(is.finite(theta_status$condition_numbers))) max(theta_status$condition_numbers, na.rm = TRUE) else Inf,
    latent_condition_number = if (length(latent_status$condition_numbers) && any(is.finite(latent_status$condition_numbers))) max(latent_status$condition_numbers, na.rm = TRUE) else Inf,
    parameter_condition_number = if (length(parameter_status$condition_numbers) && any(is.finite(parameter_status$condition_numbers))) max(parameter_status$condition_numbers, na.rm = TRUE) else Inf
  )
}

structural_canvas_mi_refits <- function(snapshot, result, data, analysis_type, estimator, missing, std_lv, mode = "theory", ordered = character(0)) {
  mi <- structural_canvas_allowed_mi(snapshot, result$fit, mode = mode)
  if (!nrow(mi)) return(mi)
  for (column in c("cfi_after", "tli_after", "rmsea_after", "srmr_after")) mi[[column]] <- NA_real_
  if (identical(mode, "conventional")) return(mi)

  current_fit <- result$fit
  cumulative_syntax <- result$syntax
  steps <- list()
  for (step in seq_len(5L)) {
    candidates <- structural_canvas_allowed_mi(snapshot, current_fit, mode = "theory")
    if (!nrow(candidates)) break
    candidate <- NULL
    candidate_row <- NULL
    candidate_syntax <- NULL
    skipped_candidates <- 0L
    skipped_details <- character(0)
    for (candidate_index in seq_len(nrow(candidates))) {
      trial_row <- candidates[candidate_index, , drop = FALSE]
      trial_syntax <- paste(
        cumulative_syntax,
        paste(trial_row$lhs[[1L]], trial_row$op[[1L]], trial_row$rhs[[1L]]),
        sep = "\n"
      )
      trial_error <- ""
      trial <- tryCatch({
        if (identical(analysis_type, "cfa")) {
          lavaan::cfa(trial_syntax, data = data, estimator = estimator, missing = missing, std.lv = std_lv, ordered = ordered, auto.cov.lv.x = FALSE)
        } else {
          lavaan::sem(trial_syntax, data = data, estimator = estimator, missing = missing, std.lv = std_lv, ordered = ordered, auto.cov.lv.x = FALSE)
        }
      }, error = function(error) {
        trial_error <<- conditionMessage(error)
        NULL
      })
      trial_admissibility <- if (!is.null(trial)) structural_canvas_fit_admissibility(trial) else list(admissible = FALSE)
      if (!is.null(trial) && isTRUE(trial_admissibility$admissible)) {
        candidate <- trial
        candidate_row <- trial_row
        candidate_syntax <- trial_syntax
        skipped_candidates <- candidate_index - 1L
        break
      }
      path_label <- paste(trial_row$lhs[[1L]], trial_row$op[[1L]], trial_row$rhs[[1L]])
      reason <- if (nzchar(trial_error)) paste0("fit error: ", trial_error) else paste(trial_admissibility$reasons %||% "inadmissible trial fit", collapse = "; ")
      skipped_details <- c(skipped_details, paste0(path_label, " [", reason, "]"))
    }
    if (is.null(candidate)) break

    cumulative_syntax <- candidate_syntax
    current_fit <- candidate
    indices <- structural_canvas_fit_measures(candidate, estimator, .90)$values
    candidate_row$step <- step
    candidate_row$skipped_inadmissible <- skipped_candidates
    candidate_row$skipped_details <- paste(skipped_details, collapse = " | ")
    candidate_row$cfi_after <- indices[[5L]]
    candidate_row$tli_after <- indices[[6L]]
    candidate_row$rmsea_after <- indices[[8L]]
    candidate_row$srmr_after <- indices[[7L]]
    steps[[length(steps) + 1L]] <- candidate_row
  }

  if (!length(steps)) return(mi[0L, , drop = FALSE])
  do.call(rbind, steps)
}

structural_canvas_fit_measures <- function(fit, estimator = "ML", ci_level = .90, preferred_keys = NULL) {
  measures <- suppressWarnings(lavaan::fitMeasures(fit, fm_args = list(rmsea.ci.level = ci_level)))
  value <- function(key) if (key %in% names(measures)) unname(measures[[key]]) else NA_real_
  choose <- function(keys) {
    for (key in keys) {
      candidate <- value(key)
      if (is.finite(candidate)) return(list(value = candidate, key = key))
    }
    list(value = NA_real_, key = keys[[length(keys)]])
  }
  robust <- toupper(as.character(estimator %||% "ML")) %in% c("MLR", "WLSMV", "DWLS")
  requested <- function(name, fallback) {
    key <- as.character(preferred_keys[[name]] %||% "")
    if (nzchar(key)) key else fallback
  }
  chisq_keys <- if (robust) c("chisq.scaled", "chisq") else "chisq"
  cfi_keys <- if (robust) c("cfi.robust", "cfi.scaled", "cfi") else "cfi"
  tli_keys <- if (robust) c("tli.robust", "tli.scaled", "tli") else "tli"
  rmsea_keys <- if (robust) c("rmsea.robust", "rmsea.scaled", "rmsea") else "rmsea"
  chisq_keys <- requested("chisq", chisq_keys)
  cfi_keys <- requested("cfi", cfi_keys)
  tli_keys <- requested("tli", tli_keys)
  rmsea_keys <- requested("rmsea", rmsea_keys)
  selected <- list(
    chisq = choose(chisq_keys),
    pvalue = choose(if (identical(chisq_keys[[1L]], "chisq.scaled")) "pvalue.scaled" else "pvalue"),
    cfi = choose(cfi_keys),
    tli = choose(tli_keys),
    rmsea = choose(rmsea_keys)
  )
  rmsea_suffix <- sub("^rmsea", "", selected$rmsea$key)
  lower <- choose(c(paste0("rmsea.ci.lower", rmsea_suffix), "rmsea.ci.lower.scaled", "rmsea.ci.lower"))
  upper <- choose(c(paste0("rmsea.ci.upper", rmsea_suffix), "rmsea.ci.upper.scaled", "rmsea.ci.upper"))
  prefix <- function(key, base) {
    if (grepl("\\.robust$", key)) paste("Robust", base)
    else if (grepl("\\.scaled$", key)) paste("Scaled", base)
    else base
  }
  list(
    values = c(selected$chisq$value, value("df"), selected$pvalue$value, if (is.finite(value("df")) && value("df") > 0) selected$chisq$value / value("df") else NA_real_, selected$cfi$value, selected$tli$value, value("srmr"), selected$rmsea$value, lower$value, upper$value),
    labels = c(prefix(selected$chisq$key, "chi-square"), "df", "p", "Q", prefix(selected$cfi$key, "CFI"), prefix(selected$tli$key, "TLI"), "SRMR", prefix(selected$rmsea$key, "RMSEA")),
    keys = c(chisq = selected$chisq$key, cfi = selected$cfi$key, tli = selected$tli$key, rmsea = selected$rmsea$key, rmsea.lower = lower$key, rmsea.upper = upper$key),
    adjusted = robust,
    measures = measures
  )
}

structural_canvas_common_fit_measures <- function(fits, estimator = "ML", ci_level = .90) {
  fits <- Filter(Negate(is.null), fits)
  selections <- lapply(fits, structural_canvas_fit_measures, estimator = estimator, ci_level = ci_level)
  if (length(selections) < 2L || all(vapply(selections[-1L], function(item) identical(item$keys, selections[[1L]]$keys), logical(1)))) return(selections)
  robust <- toupper(as.character(estimator %||% "ML")) %in% c("MLR", "WLSMV", "DWLS")
  common_key <- function(keys) {
    for (key in keys) {
      if (all(vapply(selections, function(item) key %in% names(item$measures) && is.finite(item$measures[[key]]), logical(1)))) return(key)
    }
    keys[[length(keys)]]
  }
  common_rmsea_key <- function(keys) {
    for (key in keys) {
      suffix <- sub("^rmsea", "", key)
      required <- c(key, paste0("rmsea.ci.lower", suffix), paste0("rmsea.ci.upper", suffix))
      if (all(vapply(selections, function(item) all(required %in% names(item$measures)) && all(is.finite(item$measures[required])), logical(1)))) return(key)
    }
    keys[[length(keys)]]
  }
  preferred <- c(
    chisq = common_key(if (robust) c("chisq.scaled", "chisq") else "chisq"),
    cfi = common_key(if (robust) c("cfi.robust", "cfi.scaled", "cfi") else "cfi"),
    tli = common_key(if (robust) c("tli.robust", "tli.scaled", "tli") else "tli"),
    rmsea = common_rmsea_key(if (robust) c("rmsea.robust", "rmsea.scaled", "rmsea") else "rmsea")
  )
  lapply(fits, structural_canvas_fit_measures, estimator = estimator, ci_level = ci_level, preferred_keys = preferred)
}

structural_canvas_nested_comparison_eligibility <- function(first_fit, second_fit) {
  metadata <- lapply(list(first_fit, second_fit), function(fit) {
    options <- lavaan::lavInspect(fit, "options")
    estimator <- toupper(as.character(options$estimator %||% ""))
    parameter_table <- lavaan::parameterTable(fit)
    lhs <- as.character(parameter_table$lhs)
    rhs <- as.character(parameter_table$rhs)
    covariance <- parameter_table$op == "~~"
    swap <- covariance & lhs > rhs
    canonical_lhs <- ifelse(swap, rhs, lhs)
    canonical_rhs <- ifelse(swap, lhs, rhs)
    keys <- paste(parameter_table$group, canonical_lhs, parameter_table$op, canonical_rhs, sep = "\r")
    analyzed_data <- as.matrix(lavaan::lavInspect(fit, "data"))
    if (!is.null(colnames(analyzed_data))) analyzed_data <- analyzed_data[, sort(colnames(analyzed_data)), drop = FALSE]
    list(
      n = as.numeric(lavaan::lavInspect(fit, "ntotal")),
      observed = sort(lavaan::lavNames(fit, "ov")),
      data = analyzed_data,
      groups = as.integer(lavaan::lavInspect(fit, "ngroups")),
      family = if (estimator %in% c("ML", "MLR")) "ML" else estimator,
      df = unname(lavaan::fitMeasures(fit, "df")),
      free = unique(keys[parameter_table$free > 0L]),
      admissibility = structural_canvas_fit_admissibility(fit)
    )
  })
  if (!isTRUE(metadata[[1L]]$admissibility$admissible) || !isTRUE(metadata[[2L]]$admissibility$admissible)) {
    details <- vapply(seq_along(metadata), function(index) {
      reasons <- metadata[[index]]$admissibility$reasons
      if (length(reasons)) paste0("model ", index, ": ", paste(reasons, collapse = "; ")) else ""
    }, character(1))
    details <- details[nzchar(details)]
    return(list(available = FALSE, reason = paste0("One or both models are inadmissible", if (length(details)) paste0(" (", paste(details, collapse = " | "), ")") else "", ".")))
  }
  if (metadata[[1L]]$n != metadata[[2L]]$n) return(list(available = FALSE, reason = "Models use different sample sizes."))
  if (!identical(metadata[[1L]]$observed, metadata[[2L]]$observed)) return(list(available = FALSE, reason = "Models use different observed variables."))
  if (!isTRUE(all.equal(metadata[[1L]]$data, metadata[[2L]]$data, check.attributes = FALSE))) return(list(available = FALSE, reason = "Models do not use the same analyzed observations and values."))
  if (metadata[[1L]]$groups != metadata[[2L]]$groups) return(list(available = FALSE, reason = "Models use different group structures."))
  if (!identical(metadata[[1L]]$family, metadata[[2L]]$family)) return(list(available = FALSE, reason = "Models use incompatible estimator families."))
  if (!all(is.finite(c(metadata[[1L]]$df, metadata[[2L]]$df))) || metadata[[1L]]$df == metadata[[2L]]$df) return(list(available = FALSE, reason = "Models do not have different finite degrees of freedom."))
  first_within_second <- all(metadata[[1L]]$free %in% metadata[[2L]]$free)
  second_within_first <- all(metadata[[2L]]$free %in% metadata[[1L]]$free)
  if (!xor(first_within_second, second_within_first)) return(list(available = FALSE, reason = "A strict free-parameter nesting relation was not verified."))
  list(available = TRUE, reason = "Compatible samples, variables, estimator family, degrees of freedom, and strict free-parameter nesting were verified.")
}

structural_canvas_model_difference <- function(original_fit, modified_fit, verify_nesting = TRUE) {
  if (isTRUE(verify_nesting)) {
    eligibility <- structural_canvas_nested_comparison_eligibility(original_fit, modified_fit)
    if (!isTRUE(eligibility$available)) return(NULL)
  }
  comparison <- tryCatch(
    suppressWarnings(lavaan::lavTestLRT(original_fit, modified_fit)),
    error = function(error) NULL
  )
  if (is.null(comparison) || nrow(comparison) < 2L) return(NULL)
  difference_row <- comparison[nrow(comparison), , drop = FALSE]
  column_value <- function(pattern) {
    column <- grep(pattern, names(difference_row), value = TRUE, ignore.case = TRUE)
    if (length(column)) as.numeric(difference_row[[column[[1L]]]][[1L]]) else NA_real_
  }
  list(
    chisq = column_value("Chisq diff|Chisq diff"),
    df = column_value("Df diff"),
    pvalue = column_value("Pr\\(>Chisq\\)"),
    method = as.character(attr(comparison, "heading") %||% "Likelihood-ratio difference test")
  )
}

structural_canvas_model_difference_report <- function(bundle) {
  if (is.null(bundle$baseline_fit) || is.null(bundle$fit)) return(data.frame())
  eligibility <- structural_canvas_nested_comparison_eligibility(bundle$baseline_fit, bundle$fit)
  difference <- if (isTRUE(eligibility$available)) structural_canvas_model_difference(bundle$baseline_fit, bundle$fit) else NULL
  data.frame(
    Available = !is.null(difference),
    Reason = if (!isTRUE(eligibility$available)) eligibility$reason else if (is.null(difference)) "Nesting was verified, but lavaan did not return a usable difference test." else eligibility$reason,
    `Delta chi-square` = if (!is.null(difference)) difference$chisq else NA_real_,
    `Delta df` = if (!is.null(difference)) difference$df else NA_real_,
    p = if (!is.null(difference)) difference$pvalue else NA_real_,
    Method = if (!is.null(difference)) paste(difference$method, collapse = " ") else NA_character_,
    Context = if (identical(bundle$comparison_type %||% "", "mi")) "Exploratory same-sample MI modification" else "Nested-model comparison",
    check.names = FALSE
  )
}

structural_canvas_invariance_group_diagnostics <- function(data, group, indicators, ordered = character(0)) {
  indicators <- intersect(unique(as.character(indicators)), names(data))
  groups <- unique(data[[group]][!is.na(data[[group]])])
  rows <- lapply(groups, function(group_value) {
    subset <- data[data[[group]] == group_value & !is.na(data[[group]]), indicators, drop = FALSE]
    missing_count <- sum(is.na(subset))
    missing_categories <- character(0)
    minimum_category_count <- NA_integer_
    for (indicator in intersect(ordered, indicators)) {
      global <- data[[indicator]]
      levels_value <- if (is.factor(global)) levels(global) else sort(unique(global[!is.na(global)]))
      counts <- table(factor(subset[[indicator]], levels = levels_value), useNA = "no")
      absent <- names(counts)[counts == 0L]
      if (length(absent)) missing_categories <- c(missing_categories, paste0(indicator, "={", paste(absent, collapse = ","), "}"))
      positive <- as.integer(counts[counts > 0L])
      if (length(positive)) minimum_category_count <- min(c(minimum_category_count, positive), na.rm = TRUE)
    }
    data.frame(
      Group = as.character(group_value), N = nrow(subset), `Complete indicator cases` = sum(stats::complete.cases(subset)),
      `Indicator missing %` = if (length(subset)) 100 * missing_count / length(as.matrix(subset)) else NA_real_,
      `Minimum category count` = minimum_category_count,
      `Absent ordered categories` = if (length(missing_categories)) paste(missing_categories, collapse = "; ") else "None",
      Status = if (length(missing_categories)) "Ordered category absent" else if (nrow(subset) < 100L) "Small group; review power/stability" else "No group-level flag",
      check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

structural_canvas_invariance_score_diagnostics <- function(fit, top_n = 20L) {
  score <- tryCatch(suppressWarnings(lavaan::lavTestScore(fit, epc = TRUE)), error = function(error) NULL)
  if (is.null(score) || is.null(score$uni) || !nrow(score$uni)) return(data.frame())
  tests <- as.data.frame(score$uni, check.names = FALSE)
  epc <- as.data.frame(score$epc %||% data.frame(), check.names = FALSE)
  group_labels <- as.character(lavaan::lavInspect(fit, "group.label") %||% character(0))
  scaled_x2 <- if ("X2.scaled" %in% names(tests)) tests[["X2.scaled"]] else tests[["X2"]]
  scaled_p <- if ("p.value.scaled" %in% names(tests)) tests[["p.value.scaled"]] else tests[["p.value"]]
  describe_label <- function(label) {
    rows <- epc[as.character(epc$plabel) == as.character(label), , drop = FALSE]
    if (!nrow(rows)) return(as.character(label))
    labels <- ifelse(rows$group >= 1L & rows$group <= length(group_labels), group_labels[rows$group], as.character(rows$group))
    paste0(rows$lhs, " ", rows$op, " ", rows$rhs, " [group ", labels, "]")
  }
  standardized_epc <- function(first_label, second_label) {
    rows <- epc[as.character(epc$plabel) %in% c(as.character(first_label), as.character(second_label)), , drop = FALSE]
    values <- if ("sepc.all" %in% names(rows)) abs(as.numeric(rows$sepc.all)) else numeric(0)
    values <- values[is.finite(values)]
    if (length(values)) max(values) else NA_real_
  }
  result <- data.frame(
    Constraint = paste0(vapply(tests$lhs, describe_label, character(1)), " = ", vapply(tests$rhs, describe_label, character(1))),
    `Score χ²` = as.numeric(scaled_x2), df = as.numeric(tests$df), p = as.numeric(scaled_p),
    `BH-adjusted p` = stats::p.adjust(as.numeric(scaled_p), method = "BH"),
    `Max |standardized EPC|` = mapply(standardized_epc, tests$lhs, tests$rhs),
    `Raw χ²` = as.numeric(tests$X2), `Raw p` = as.numeric(tests$p.value),
    `Raw BH-adjusted p` = stats::p.adjust(as.numeric(tests$p.value), method = "BH"), check.names = FALSE
  )
  result <- result[order(-result[["Score χ²"]]), , drop = FALSE]
  utils::head(result, as.integer(top_n))
}

structural_canvas_holdout_split <- function(data, validation_fraction = .30, seed = 13579L) {
  validation_fraction <- as.numeric(validation_fraction)
  if (!is.finite(validation_fraction) || validation_fraction <= 0 || validation_fraction >= 1) stop("Validation fraction must be between 0 and 1.")
  n <- nrow(data)
  if (n < 100L) stop("MI holdout validation requires at least 100 observations before missing-data handling.")
  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (old_seed_exists) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(as.integer(seed))
  validation_n <- max(30L, floor(n * validation_fraction))
  if (validation_n >= n - 30L) stop("The split must leave at least 30 observations in both exploration and validation samples.")
  validation_rows <- sort(sample.int(n, validation_n, replace = FALSE))
  exploration_rows <- setdiff(seq_len(n), validation_rows)
  list(
    exploration = data[exploration_rows, , drop = FALSE], validation = data[validation_rows, , drop = FALSE],
    exploration_rows = exploration_rows, validation_rows = validation_rows,
    seed = as.integer(seed), validation_fraction = validation_fraction
  )
}

structural_canvas_validate_holdout_options <- function(enabled, analysis_type = "cfa", estimator = "MLR", ordered = character(0), invariance_enabled = FALSE, residual_variance_fixes = numeric(0)) {
  if (!isTRUE(enabled)) return(invisible(TRUE))
  if (!identical(analysis_type, "cfa") || length(ordered) || !toupper(estimator) %in% c("ML", "MLR")) {
    stop("MI exploration/validation splitting is currently available only for continuous-indicator CFA estimated with ML or MLR.")
  }
  if (isTRUE(invariance_enabled)) {
    stop("Measurement invariance and MI exploration/validation splitting cannot be run simultaneously. Run invariance on the intended full/grouped sample, or disable invariance and use the split strictly for exploratory MI validation.")
  }
  if (length(residual_variance_fixes)) {
    stop("MI exploration/validation splitting cannot be combined with Heywood residual-variance sensitivity constraints. Complete and document the admissibility analysis separately before exploratory MI validation.")
  }
  invisible(TRUE)
}

structural_canvas_validate_holdout_reuse <- function(enabled, validation_revealed = FALSE) {
  if (isTRUE(enabled) && isTRUE(validation_revealed)) {
    stop("The reserved validation sample has already been evaluated. Additional MI changes would reuse and contaminate the holdout sample. Start a new analysis with a newly chosen split seed before testing another modified model.")
  }
  invisible(TRUE)
}

structural_canvas_holdout_model_comparison <- function(original_syntax, modified_syntax, validation_data, estimator = "MLR", missing = "fiml", std_lv = FALSE, ci_level = .90) {
  if (!toupper(estimator) %in% c("ML", "MLR")) stop("MI holdout validation currently supports continuous indicators estimated with ML or MLR.")
  fit_model <- function(syntax) lavaan::cfa(
    syntax, data = validation_data, estimator = estimator, missing = missing,
    std.lv = isTRUE(std_lv), auto.cov.lv.x = FALSE
  )
  original_fit <- fit_model(original_syntax)
  modified_fit <- fit_model(modified_syntax)
  fits <- list(`Original model` = original_fit, `Modified model` = modified_fit)
  used_n <- vapply(fits, function(fit) as.numeric(lavaan::lavInspect(fit, "ntotal")), numeric(1))
  if (any(!is.finite(used_n) | used_n < 30L)) {
    stop(paste0("The validation sample has fewer than 30 usable observations after missing-data handling (", paste(names(used_n), used_n, sep = " = ", collapse = "; "), ")."))
  }
  selections <- structural_canvas_common_fit_measures(fits, estimator, ci_level)
  values <- lapply(selections, `[[`, "values")
  admissibility <- lapply(fits, structural_canvas_fit_admissibility)
  table <- do.call(rbind, lapply(seq_along(values), function(index) data.frame(
    Model = names(fits)[[index]], `N used` = used_n[[index]], Chisq = values[[index]][[1L]], df = values[[index]][[2L]], p = values[[index]][[3L]],
    CFI = values[[index]][[5L]], TLI = values[[index]][[6L]], SRMR = values[[index]][[7L]], RMSEA = values[[index]][[8L]],
    Converged = isTRUE(lavaan::lavInspect(fits[[index]], "converged")),
    Admissible = isTRUE(admissibility[[index]]$admissible),
    `Admissibility reasons` = if (length(admissibility[[index]]$reasons)) paste(admissibility[[index]]$reasons, collapse = "; ") else "None",
    check.names = FALSE
  )))
  comparable <- all(table$Admissible)
  difference <- if (comparable) structural_canvas_model_difference(original_fit, modified_fit) else NULL
  changes <- data.frame(
    DeltaCFI = if (comparable) table$CFI[[2L]] - table$CFI[[1L]] else NA_real_, DeltaTLI = if (comparable) table$TLI[[2L]] - table$TLI[[1L]] else NA_real_,
    DeltaSRMR = if (comparable) table$SRMR[[2L]] - table$SRMR[[1L]] else NA_real_, DeltaRMSEA = if (comparable) table$RMSEA[[2L]] - table$RMSEA[[1L]] else NA_real_,
    DeltaChisq = as.numeric(difference$chisq %||% NA_real_), DeltaDf = as.numeric(difference$df %||% NA_real_),
    DeltaP = as.numeric(difference$pvalue %||% NA_real_),
    `Comparison status` = if (comparable) "Both validation models admissible" else "Suppressed because one or both validation models are inadmissible",
    check.names = FALSE
  )
  list(table = table, changes = changes, fits = fits, difference = difference, validation_n_raw = nrow(validation_data), validation_n_used = used_n, estimator = estimator)
}

structural_canvas_measurement_invariance <- function(syntax, data, group, estimator = "MLR", missing = "fiml", std_lv = FALSE, ci_level = .90, ordered = character(0)) {
  group <- as.character(group %||% "")
  if (!nzchar(group) || !group %in% names(data)) stop("A valid grouping variable is required for measurement invariance analysis.")
  ordinal <- length(ordered) > 0L
  if (!ordinal && !toupper(estimator) %in% c("ML", "MLR")) stop("Continuous-indicator measurement invariance requires ML or MLR.")
  if (ordinal && !toupper(estimator) %in% c("WLSMV", "DWLS")) stop("Ordered-indicator measurement invariance requires WLSMV or DWLS.")
  group_values <- data[[group]]
  observed_groups <- unique(group_values[!is.na(group_values)])
  if (length(observed_groups) < 2L) stop("Measurement invariance analysis requires at least two non-empty groups.")
  measurement_lines <- strsplit(as.character(syntax), "\n", fixed = TRUE)[[1L]]
  measurement_lines <- measurement_lines[grepl("=~", measurement_lines, fixed = TRUE)]
  indicator_tokens <- unlist(lapply(measurement_lines, function(line) {
    rhs <- strsplit(line, "=~", fixed = TRUE)[[1L]][[2L]]
    trimws(unlist(strsplit(rhs, "+", fixed = TRUE)))
  }), use.names = FALSE)
  indicators <- intersect(unique(sub("^[^*]*\\*", "", indicator_tokens)), names(data))
  group_diagnostics <- structural_canvas_invariance_group_diagnostics(data, group, indicators, ordered)
  if (ordinal && any(group_diagnostics[["Absent ordered categories"]] != "None")) {
    details <- paste0(group_diagnostics$Group[group_diagnostics[["Absent ordered categories"]] != "None"], ": ", group_diagnostics[["Absent ordered categories"]][group_diagnostics[["Absent ordered categories"]] != "None"])
    stop(paste0("Ordered measurement invariance cannot be estimated comparably because categories are absent within group(s): ", paste(details, collapse = "; "), "."))
  }
  stages <- if (ordinal) list(
    Configural = character(0),
    Thresholds = "thresholds",
    `Scalar (thresholds + loadings)` = c("thresholds", "loadings"),
    Strict = c("thresholds", "loadings", "residuals")
  ) else list(
    Configural = character(0), Metric = "loadings",
    Scalar = c("loadings", "intercepts"), Strict = c("loadings", "intercepts", "residuals")
  )
  fits <- lapply(stages, function(equal) {
    arguments <- list(
      model = syntax, data = data, group = group, group.equal = equal,
      estimator = estimator, missing = missing, std.lv = isTRUE(std_lv),
      ordered = ordered, auto.cov.lv.x = FALSE
    )
    if (ordinal) arguments$parameterization <- "theta"
    do.call(lavaan::cfa, arguments)
  })
  names(fits) <- names(stages)
  selections <- structural_canvas_common_fit_measures(fits, estimator, ci_level)
  admissibility <- lapply(fits, structural_canvas_fit_admissibility)
  rows <- lapply(seq_along(fits), function(index) {
    fit <- fits[[index]]
    selected <- selections[[index]]$values
    comparable <- index > 1L && isTRUE(admissibility[[index - 1L]]$admissible) && isTRUE(admissibility[[index]]$admissible)
    difference <- if (comparable) structural_canvas_model_difference(fits[[index - 1L]], fit, verify_nesting = FALSE) else NULL
    previous <- if (index > 1L) selections[[index - 1L]]$values else rep(NA_real_, length(selected))
    data.frame(
      Model = names(fits)[[index]],
      Chisq = selected[[1L]], df = selected[[2L]], p = selected[[3L]],
      CFI = selected[[5L]], RMSEA = selected[[8L]], SRMR = selected[[7L]],
      DeltaCFI = if (comparable) selected[[5L]] - previous[[5L]] else NA_real_,
      DeltaRMSEA = if (comparable) selected[[8L]] - previous[[8L]] else NA_real_,
      DeltaSRMR = if (comparable) selected[[7L]] - previous[[7L]] else NA_real_,
      DeltaChisq = as.numeric(difference$chisq %||% NA_real_),
      DeltaDf = as.numeric(difference$df %||% NA_real_),
      DeltaP = as.numeric(difference$pvalue %||% NA_real_),
      Converged = isTRUE(lavaan::lavInspect(fit, "converged")),
      Admissible = isTRUE(admissibility[[index]]$admissible),
      `Admissibility reasons` = if (length(admissibility[[index]]$reasons)) paste(admissibility[[index]]$reasons, collapse = "; ") else "None",
      `Parameter boundary dimensions` = admissibility[[index]]$parameter_boundary_dimensions,
      `Explicit equality constraints` = admissibility[[index]]$equality_constraint_count,
      `Residual min eigenvalue` = admissibility[[index]]$residual_min_eigenvalue,
      `Latent min eigenvalue` = admissibility[[index]]$latent_min_eigenvalue,
      `Parameter min eigenvalue` = admissibility[[index]]$parameter_min_eigenvalue,
      `Residual condition number` = admissibility[[index]]$residual_condition_number,
      `Latent condition number` = admissibility[[index]]$latent_condition_number,
      `Parameter condition number` = admissibility[[index]]$parameter_condition_number,
      `Ill-conditioned warning` = any(c(admissibility[[index]]$residual_condition_number, admissibility[[index]]$latent_condition_number, admissibility[[index]]$parameter_condition_number) > 1e8),
      check.names = FALSE
    )
  })
  score_diagnostics <- stats::setNames(lapply(seq_along(fits), function(index) {
    if (index == 1L || !isTRUE(admissibility[[index]]$admissible)) data.frame() else structural_canvas_invariance_score_diagnostics(fits[[index]])
  }), names(fits))
  configural_fit <- fits[[1L]]
  group_reliability <- structural_canvas_group_reliability_estimates(configural_fit)
  group_htmt <- structural_canvas_group_htmt(configural_fit)
  group_residuals <- structural_canvas_residual_diagnostics(configural_fit)
  list(
    table = do.call(rbind, rows), fits = fits, score_diagnostics = score_diagnostics,
    group = group, groups = observed_groups, group_diagnostics = group_diagnostics,
    group_reliability = group_reliability, group_htmt = group_htmt,
    group_residuals = group_residuals,
    estimator = estimator, ordered = ordered, ordinal = ordinal
  )
}

register_structural_equation_canvas_handlers <- function(input, output, session, dataset_fn, selected_names_fn, variable_table_fn, labels_fn, category_table_fn, mark_settings_dirty, app_language_fn = NULL) {
  lapply(c("cfa", "cbsem", "plssem"), function(analysis_type) {
    prefix <- structural_analysis_prefix(analysis_type)
    canvas_input <- paste0(prefix, "_canvas_state")
    canvas_output <- paste0(prefix, "_canvas_setup")
    run_input <- paste0(prefix, "_canvas_run_request")
    confirm_input <- paste0(prefix, "_canvas_run_confirm")
    advanced_input <- paste0(prefix, "_canvas_advanced_request")
    fit_result <- reactiveVal(NULL)
    pending_mi_rows <- reactiveVal(integer(0))
    pending_estimator_snapshot <- reactiveVal(NULL)
    if (identical(analysis_type, "cfa")) output[[paste0(prefix, "_download_reproducibility")]] <- downloadHandler(
      filename = function() paste0("cfa-analysis-record-", format(Sys.Date(), "%Y%m%d"), ".txt"),
      contentType = "text/plain; charset=utf-8",
      content = function(file) {
        bundle <- fit_result()
        shiny::req(!is.null(bundle))
        writeLines(structural_canvas_reproducibility_record(bundle), file, useBytes = TRUE)
      }
    )
    if (identical(analysis_type, "cfa")) output[[paste0(prefix, "_download_tables")]] <- downloadHandler(
      filename = function() paste0("cfa-result-tables-", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      content = function(file) {
        shiny::req(requireNamespace("openxlsx", quietly = TRUE))
        bundle <- fit_result()
        shiny::req(!is.null(bundle))
        sheets <- structural_canvas_result_workbook_sheets(bundle, result_table)
        structural_canvas_write_result_workbook(sheets, file)
      }
    )
    if (identical(analysis_type, "cfa")) observe({
      data <- dataset_fn()
      choices <- names(data %||% data.frame())
      current <- as.character(input[[paste0(prefix, "_invariance_group")]] %||% "")
      updateSelectInput(session, paste0(prefix, "_invariance_group"), choices = choices, selected = if (current %in% choices) current else "")
    })
    result_table <- function(kind) {
      bundle <- fit_result()
      shiny::req(!is.null(bundle))
      fit <- bundle$fit
      snapshot <- bundle$snapshot %||% list()
      labels <- labels_fn() %||% character(0)
      ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
      display_name <- function(name) {
        name <- as.character(name %||% "")
        node <- Filter(function(item) identical(structural_canvas_name(item), name), snapshot$nodes %||% list())
        label <- if (length(node)) as.character(node[[1]]$canvasLabel %||% "") else ""
        if (!nzchar(label) && !is.null(names(labels)) && name %in% names(labels)) label <- as.character(labels[[name]] %||% "")
        if (!nzchar(label) && length(node)) label <- as.character(node[[1]]$dataLabel %||% "")
        if (!ko && grepl("^잠재변수\\s*[0-9]+$", label)) {
          label <- sub("^잠재변수\\s*", "Latent variable ", label)
        }
        if (nzchar(label)) label else name
      }
      residual_name <- function(name) {
        target <- Filter(function(item) identical(structural_canvas_name(item), as.character(name)), snapshot$nodes %||% list())
        if (!length(target)) return(display_name(name))
        target_id <- as.character(target[[1]]$id %||% "")
        residual_edge <- Filter(function(edge) {
          if (identical(edge$kind, "covariance") || !identical(as.character(edge$to), target_id)) return(FALSE)
          source <- structural_canvas_node(snapshot, edge$from)
          !is.null(source) && source$role %in% c("error", "disturbance")
        }, snapshot$edges %||% list())
        if (!length(residual_edge)) return(display_name(name))
        residual <- structural_canvas_node(snapshot, residual_edge[[1]]$from)
        candidates <- as.character(c(residual$canvasLabel, residual$dataLabel, residual$name))
        candidates <- candidates[nzchar(candidates)]
        label <- if (length(candidates)) candidates[[1L]] else ""
        if (nzchar(label)) label else display_name(name)
      }
      fmt <- function(value) vapply(as.numeric(value), format_decimal3, character(1))
      if (analysis_type %in% c("cfa", "cbsem")) {
        if (identical(kind, "overview")) {
          overview_df <- data.frame(
            Item = if (ko) c("분석 방법", "추정 방법", "표본 크기(N)", "관측변수 수", "잠재변수 수", "자유 파라미터 수", "수렴 여부", "적합해 여부") else c("Analysis", "Estimator", "N", "Observed variables", "Latent variables", "Free parameters", "Converged", "Admissible solution"),
            Value = c(structural_analysis_title(analysis_type, "en"), lavaan::lavInspect(fit, "options")$estimator, lavaan::lavInspect(fit, "ntotal"), length(lavaan::lavNames(fit, "ov")), length(lavaan::lavNames(fit, "lv")), lavaan::lavInspect(fit, "npar"), if (isTRUE(lavaan::lavInspect(fit, "converged"))) "Yes" else "No", if (isTRUE(bundle$diagnostics$admissible %||% lavaan::lavInspect(fit, "post.check"))) "Yes" else "No"),
            check.names = FALSE
          )
          names(overview_df)[[1]] <- if (ko) "항목" else "Item"
          names(overview_df)[[2]] <- if (ko) "값" else "Value"
          overview_df <- rbind(
            overview_df[1L, , drop = FALSE],
            data.frame(overview_df[1L, , drop = FALSE], check.names = FALSE),
            overview_df[-1L, , drop = FALSE]
          )
          overview_df[2L, 1L] <- "Analysis context"
          overview_df[2L, 2L] <- structural_canvas_analysis_context(bundle)
          return(overview_df)
        }
        if (identical(kind, "fit")) {
          ci_level <- as.numeric(bundle$rmsea_ci %||% .90)
          comparison_fits <- if (isTRUE(bundle$modified_from_baseline) && !is.null(bundle$baseline_fit)) list(bundle$baseline_fit, fit) else list(fit)
          selections <- structural_canvas_common_fit_measures(comparison_fits, bundle$estimator %||% "ML", ci_level)
          model_labels <- if (ko) "모형" else "Model"
          row_labels <- if (ko) "기존 모형" else "Original model"
          if (length(selections) > 1L) {
            modified_label <- bundle$comparison_label %||% if (ko) "수정 모형" else "Modified model"
            row_labels <- c(if (ko) "기존 모형" else "Original model", modified_label)
          }
          values <- do.call(rbind, lapply(selections, function(item) item$values))
          ci_percent <- round(100 * ci_level)
          table <- data.frame(
            row_labels,
            `χ²` = fmt(values[, 1L]), df = fmt(values[, 2L]),
            p = vapply(values[, 3L], format_p, character(1)), Q = fmt(values[, 4L]),
            CFI = fmt(values[, 5L]), TLI = fmt(values[, 6L]), SRMR = fmt(values[, 7L]),
            RMSEA = fmt(values[, 8L]), fmt(values[, 9L]), fmt(values[, 10L]),
            check.names = FALSE
          )
          names(table)[[1L]] <- model_labels
          names(table)[10:11] <- paste0(ci_percent, "% CI ", c("LLCI", "ULCI"))
          return(table)
        }
        if (identical(kind, "validity")) {
          standardized <- lavaan::standardizedSolution(fit, ci = TRUE, level = .95)
          loadings <- standardized[standardized$op == "=~", c("lhs", "rhs", "est.std"), drop = FALSE]
          observed_names <- lavaan::lavNames(fit, "ov")
          loadings <- loadings[loadings$rhs %in% observed_names, , drop = FALSE]
          latent_names <- unique(loadings$lhs)
          indicator_counts <- stats::setNames(vapply(latent_names, function(name) sum(loadings$lhs == name), integer(1)), latent_names)
          formula_mode <- bundle$validity_formula %||% "standardized"
          if (identical(formula_mode, "model_implied")) {
            parameters <- lavaan::parameterEstimates(fit)
            latent_variance <- diag(lavaan::lavInspect(fit, "cov.lv"))
            residuals <- parameters[parameters$op == "~~" & parameters$lhs == parameters$rhs, c("lhs", "est"), drop = FALSE]
            theta_matrix <- as.matrix(lavaan::lavInspect(fit, "theta"))
            ave <- stats::setNames(vapply(latent_names, function(name) {
              factor_loadings <- parameters$est[parameters$op == "=~" & parameters$lhs == name]
              indicators <- parameters$rhs[parameters$op == "=~" & parameters$lhs == name]
              theta <- residuals$est[match(indicators, residuals$lhs)]
              common <- sum(factor_loadings^2 * latent_variance[[name]], na.rm = TRUE)
              common / (common + sum(theta, na.rm = TRUE))
            }, numeric(1)), latent_names)
            cr <- stats::setNames(vapply(latent_names, function(name) {
              factor_loadings <- parameters$est[parameters$op == "=~" & parameters$lhs == name]
              indicators <- parameters$rhs[parameters$op == "=~" & parameters$lhs == name]
              common <- sum(factor_loadings)^2 * latent_variance[[name]]
              theta <- theta_matrix[indicators, indicators, drop = FALSE]
              denominator <- common + sum(theta, na.rm = TRUE)
              if (is.finite(denominator) && denominator > 0) common / denominator else NA_real_
            }, numeric(1)), latent_names)
          } else {
            standardized_parameters <- lavaan::standardizedSolution(fit)
            theta_matrix <- matrix(0, nrow = length(observed_names), ncol = length(observed_names), dimnames = list(observed_names, observed_names))
            theta_rows <- standardized_parameters$op == "~~" & standardized_parameters$lhs %in% observed_names & standardized_parameters$rhs %in% observed_names
            for (index in which(theta_rows)) {
              lhs <- standardized_parameters$lhs[[index]]
              rhs <- standardized_parameters$rhs[[index]]
              theta_matrix[lhs, rhs] <- standardized_parameters$est.std[[index]]
              theta_matrix[rhs, lhs] <- standardized_parameters$est.std[[index]]
            }
            ave <- stats::setNames(vapply(latent_names, function(name) mean(loadings$est.std[loadings$lhs == name]^2, na.rm = TRUE), numeric(1)), latent_names)
            cr <- stats::setNames(vapply(latent_names, function(name) {
              lambda <- loadings$est.std[loadings$lhs == name]
              indicators <- loadings$rhs[loadings$lhs == name]
              common <- sum(lambda)^2
              theta <- theta_matrix[indicators, indicators, drop = FALSE]
              denominator <- common + sum(theta, na.rm = TRUE)
              if (is.finite(denominator) && denominator > 0) common / denominator else NA_real_
            }, numeric(1)), latent_names)
          }
          correlations <- as.matrix(lavaan::lavInspect(fit, "cor.lv"))
          if (!length(dim(correlations))) correlations <- matrix(correlations, nrow = 1L, ncol = 1L)
          if (is.null(rownames(correlations))) rownames(correlations) <- latent_names
          if (is.null(colnames(correlations))) colnames(correlations) <- latent_names
          missing_covariances <- structural_canvas_missing_exogenous_covariances(snapshot)
          fl <- structural_canvas_fornell_larcker(ave, correlations, indicator_counts, assessable = !length(missing_covariances))
          sample_statistics <- lavaan::lavInspect(fit, "sampstat")
          sample_covariance <- sample_statistics$cov %||% NULL
          alpha <- stats::setNames(vapply(latent_names, function(name) {
            if (is.null(sample_covariance)) return(NA_real_)
            structural_canvas_cronbach_alpha(sample_covariance, loadings$rhs[loadings$lhs == name])
          }, numeric(1)), latent_names)
          omega <- cr
          constrained_single_factors <- structural_canvas_constrained_single_indicators(snapshot)
          table <- matrix("", nrow = length(latent_names), ncol = length(latent_names) + 9L)
          colnames(table) <- c(if (ko) "잠재변수" else "Latent", vapply(latent_names, display_name, character(1)), "Max |r|", "FL criterion", "k", "AVE", "CR", "Cronbach's α", "ωtotal", "Guidance")
          for (row in seq_along(latent_names)) {
            latent_name <- latent_names[[row]]
            single_indicator <- indicator_counts[[latent_name]] < 2L
            constrained_single <- single_indicator && latent_name %in% constrained_single_factors
            ave_value <- ave[[latent_name]]
            table[row, 1] <- display_name(latent_name)
            for (column in seq_along(latent_names)) {
              if (row == column) table[row, column + 1L] <- if (single_indicator && !constrained_single) "(N/A‡)" else if (!is.finite(ave_value) || ave_value < 0) "(N/A†)" else paste0("(", format_decimal3(sqrt(ave_value)), if (ave_value > 1) "†" else if (constrained_single) "¶" else "", ")")
              if (row > column) table[row, column + 1L] <- format_decimal3(correlations[latent_name, latent_names[[column]]])
            }
            table[row, ncol(table) - 7L] <- if (is.finite(fl$max_correlation[[latent_name]])) format_decimal3(fl$max_correlation[[latent_name]]) else "—"
            table[row, ncol(table) - 6L] <- if (single_indicator) if (constrained_single) "Not assessed¶" else "Not assessed‡" else if (length(missing_covariances)) "Not assessed§" else fl$criterion[[latent_name]]
            table[row, ncol(table) - 5L] <- as.character(indicator_counts[[latent_name]])
            ave_marker <- if (!is.finite(ave_value) || ave_value < 0 || ave_value > 1) "†" else ""
            table[row, ncol(table) - 4L] <- if (single_indicator && !constrained_single) "N/A‡" else paste0(format_decimal3(ave_value), ave_marker, if (constrained_single && !nzchar(ave_marker)) "¶" else "")
            cr_value <- cr[[latent_name]]
            cr_marker <- if (!is.finite(cr_value) || cr_value < 0 || cr_value > 1) "†" else ""
            table[row, ncol(table) - 3L] <- if (single_indicator && !constrained_single) "N/A‡" else paste0(format_decimal3(cr_value), cr_marker, if (constrained_single && !nzchar(cr_marker)) "¶" else "")
            alpha_value <- alpha[[latent_name]]
            alpha_marker <- if (!is.finite(alpha_value) || alpha_value < 0 || alpha_value > 1) "†" else ""
            table[row, ncol(table) - 2L] <- if (single_indicator) if (constrained_single) "N/A¶" else "N/A‡" else paste0(format_decimal3(alpha_value), alpha_marker)
            omega_value <- omega[[latent_name]]
            omega_marker <- if (!is.finite(omega_value) || omega_value < 0 || omega_value > 1) "†" else ""
            table[row, ncol(table) - 1L] <- if (single_indicator && !constrained_single) "N/A‡" else paste0(format_decimal3(omega_value), omega_marker, if (constrained_single && !nzchar(omega_marker)) "¶" else "")
            table[row, ncol(table)] <- if (single_indicator) if (constrained_single) "Externally constrained¶" else "Not assessed‡" else structural_canvas_measurement_quality_guidance(ave_value, cr_value, alpha_value, omega_value)
          }
          return(as.data.frame(table, check.names = FALSE))
        }
        if (identical(kind, "measurement")) {
          raw <- lavaan::parameterEstimates(fit)
          parameter_table <- lavaan::parameterTable(fit)
          standardized <- lavaan::standardizedSolution(fit, ci = TRUE, level = .95)
          rows <- raw$op == "=~"
          raw <- raw[rows, c("lhs", "rhs", "est", "se", "z", "pvalue", "ci.lower", "ci.upper"), drop = FALSE]
          loading_parameters <- parameter_table[parameter_table$op == "=~", c("lhs", "rhs", "free"), drop = FALSE]
          standardized_loadings <- standardized[standardized$op == "=~", c("lhs", "rhs", "est.std", "ci.lower", "ci.upper"), drop = FALSE]
          raw_key <- paste(raw$lhs, raw$rhs, sep = "\r")
          standardized_key <- paste(standardized_loadings$lhs, standardized_loadings$rhs, sep = "\r")
          standardized_match <- match(raw_key, standardized_key)
          beta <- standardized_loadings$est.std[standardized_match]
          beta_ci_lower <- standardized_loadings$ci.lower[standardized_match]
          beta_ci_upper <- standardized_loadings$ci.upper[standardized_match]
          standardized_residuals <- standardized[
            standardized$op == "~~" & standardized$lhs == standardized$rhs & standardized$lhs %in% raw$rhs,
            c("lhs", "est.std", "ci.lower", "ci.upper"), drop = FALSE
          ]
          residual_match <- match(raw$rhs, standardized_residuals$lhs)
          residual_variance <- standardized_residuals$est.std[residual_match]
          r2_ci_lower <- 1 - standardized_residuals$ci.upper[residual_match]
          r2_ci_upper <- 1 - standardized_residuals$ci.lower[residual_match]
          r2_ci_abnormal <- !is.finite(r2_ci_lower) | !is.finite(r2_ci_upper) | r2_ci_lower < 0 | r2_ci_upper > 1
          r2_ci_lower_display <- paste0(fmt(r2_ci_lower), ifelse(r2_ci_abnormal, "†", ""))
          r2_ci_upper_display <- paste0(fmt(r2_ci_upper), ifelse(r2_ci_abnormal, "†", ""))
          residual_marker <- ifelse(!is.finite(residual_variance) | residual_variance < 0 | residual_variance > 1, "†", "")
          residual_display <- paste0(fmt(residual_variance), residual_marker)
          cross_loaded <- duplicated(raw$rhs) | duplicated(raw$rhs, fromLast = TRUE)
          loading_guidance <- mapply(
            structural_canvas_indicator_loading_guidance,
            beta, beta_ci_lower, beta_ci_upper, residual_variance, cross_loaded,
            USE.NAMES = FALSE
          )
          parameter_key <- paste(loading_parameters$lhs, loading_parameters$rhs, sep = "\r")
          parameter_match <- match(raw_key, parameter_key)
          fixed <- loading_parameters$free[parameter_match] == 0L
          fixed[is.na(fixed)] <- raw$se[is.na(fixed)] == 0 & is.na(raw$z[is.na(fixed)]) & is.na(raw$pvalue[is.na(fixed)])
          se <- fmt(raw$se)
          z <- fmt(raw$z)
          p <- vapply(raw$pvalue, format_p, character(1))
          r2_values <- lavaan::lavInspect(fit, "r2")
          r2 <- as.numeric(r2_values[raw$rhs])
          se[fixed] <- "Fixed*"
          z[fixed] <- "—"
          p[fixed] <- "—"
          table <- data.frame(
            vapply(raw$lhs, display_name, character(1)), vapply(raw$rhs, display_name, character(1)),
            B = fmt(raw$est), `B 95% CI lower` = fmt(raw$ci.lower), `B 95% CI upper` = fmt(raw$ci.upper),
            SE = se, beta = fmt(beta),
            `β 95% CI lower` = fmt(beta_ci_lower), `β 95% CI upper` = fmt(beta_ci_upper),
            R2 = fmt(r2), `R² 95% CI lower` = r2_ci_lower_display, `R² 95% CI upper` = r2_ci_upper_display,
            `Std. residual variance` = residual_display, `Cross-loading` = ifelse(cross_loaded, "Yes", "No"), Guidance = loading_guidance,
            z = z, p = p, check.names = FALSE
          )
          names(table)[1:2] <- if (ko) c("잠재변수", "측정변수") else c("Latent", "Indicator")
          names(table)[names(table) == "R2"] <- "R²"
          return(table)
        }
        mi <- bundle$mi %||% structural_canvas_allowed_mi(snapshot, fit)
        if (!nrow(mi)) return(data.frame())
        theory_mi <- identical(bundle$mi_mode %||% "theory", "theory")
        if (!theory_mi) {
          mi <- mi[mi$op == "~~" & mi$lhs != mi$rhs, , drop = FALSE]
          if (!nrow(mi)) return(data.frame())
        }
        relation <- vapply(seq_len(nrow(mi)), function(index) {
          lhs <- if (identical(mi$op[[index]], "~~")) residual_name(mi$lhs[[index]]) else display_name(mi$lhs[[index]])
          rhs <- if (identical(mi$op[[index]], "~~")) residual_name(mi$rhs[[index]]) else display_name(mi$rhs[[index]])
          if (identical(mi$op[[index]], "~~")) paste(lhs, "<-->", rhs) else if (identical(mi$op[[index]], "=~")) paste(lhs, "=~", rhs) else paste(rhs, "-->", lhs)
        }, character(1))
        if (!theory_mi) {
          epc <- if ("epc" %in% names(mi)) mi$epc else rep(NA_real_, nrow(mi))
          standardized_epc <- if ("sepc.all" %in% names(mi)) mi$sepc.all else rep(NA_real_, nrow(mi))
          return(data.frame(Covariance = relation, MI = fmt(mi$mi), `MI p` = vapply(mi$`MI p`, format_p, character(1)), `BH-adjusted p` = vapply(mi$`BH-adjusted p`, format_p, character(1)), `MI tests` = mi$`Multiplicity family size`, EPC = fmt(epc), `Std. EPC` = fmt(standardized_epc), check.names = FALSE))
        }
        epc <- if ("epc" %in% names(mi)) mi$epc else rep(NA_real_, nrow(mi))
        standardized_epc <- if ("sepc.all" %in% names(mi)) mi$sepc.all else rep(NA_real_, nrow(mi))
        step <- if ("step" %in% names(mi)) as.integer(mi$step) else seq_len(nrow(mi))
        skipped <- if ("skipped_inadmissible" %in% names(mi)) as.integer(mi$skipped_inadmissible) else rep(0L, nrow(mi))
        skipped_details <- if ("skipped_details" %in% names(mi)) as.character(mi$skipped_details) else rep("", nrow(mi))
        table <- data.frame(Step = step, `Skipped unsafe` = skipped, `Skipped details` = skipped_details, relation, MI = fmt(mi$mi), `MI p` = vapply(mi$`MI p`, format_p, character(1)), `BH-adjusted p` = vapply(mi$`BH-adjusted p`, format_p, character(1)), `MI tests` = mi$`Multiplicity family size`, EPC = fmt(epc), `Std. EPC` = fmt(standardized_epc), CFI = fmt(mi$cfi_after), TLI = fmt(mi$tli_after), RMSEA = fmt(mi$rmsea_after), SRMR = fmt(mi$srmr_after), check.names = FALSE)
        names(table)[[4]] <- if (ko) "추천 공분산" else "Covariance"
        return(table)
      }
      summary_fit <- summary(fit)
      matrix_value <- switch(kind, overview = summary_fit$paths, fit = summary_fit$paths, validity = summary_fit$reliability, measurement = summary_fit$loadings, mi = NULL)
      if (is.null(matrix_value)) return(data.frame())
      table <- as.data.frame(matrix_value, check.names = FALSE)
      row_labels <- rownames(table)
      row_labels <- vapply(row_labels, function(name) if (name %in% c("R^2", "AdjR^2")) name else display_name(name), character(1))
      names(table) <- vapply(names(table), display_name, character(1))
      result <- cbind(row_labels, table, row.names = NULL, check.names = FALSE)
      names(result)[[1]] <- if (ko) "항목" else "Item"
      result
    }
    output[[canvas_output]] <- renderUI({
      structural_equation_workspace(selected_names_fn(), variable_table_fn(), labels_fn(), analysis_type, statedu_current_language(app_language_fn))
    })
    output[[paste0(prefix, "_results")]] <- renderUI({
      shiny::req(!is.null(fit_result()))
      ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
      div(
        class = "structural-analysis-results regression-results",
        h3(if (ko) "분석 결과" else "Analysis Results"),
        if (analysis_type == "cfa") downloadButton(paste0(prefix, "_download_reproducibility"), if (ko) "분석 기록 다운로드" else "Download analysis record", class = "btn btn-default btn-sm"),
        if (analysis_type == "cfa") downloadButton(paste0(prefix, "_download_tables"), if (ko) "결과표 Excel 다운로드" else "Download result tables", class = "btn btn-default btn-sm"),
        div(class = "result-section regression-result-panel", h4(if (ko) "1. 모형 개요" else "1. Model overview"), div(class = "table-responsive", tableOutput(paste0(prefix, "_result_overview")))),
        uiOutput(paste0(prefix, "_result_identification")),
        uiOutput(paste0(prefix, "_result_normality")),
        uiOutput(paste0(prefix, "_result_missing_outliers")),
        uiOutput(paste0(prefix, "_result_risk_diagnostics")),
        uiOutput(paste0(prefix, "_result_heywood")),
        div(class = "result-section regression-result-panel", h4(if (ko) "2. 모형 적합도" else "2. Model fit"), div(class = "table-responsive", uiOutput(paste0(prefix, "_result_fit"))), uiOutput(paste0(prefix, "_result_fit_guidance")), uiOutput(paste0(prefix, "_result_rmsea_tests")), uiOutput(paste0(prefix, "_result_information_criteria")), uiOutput(paste0(prefix, "_result_bollen_stine")), div(class = "table-responsive", uiOutput(paste0(prefix, "_result_fit_difference")))),
        uiOutput(paste0(prefix, "_result_invariance")),
        div(class = "result-section regression-result-panel", h4(if (ko) "3. 잠재 구성개념 상관, 신뢰도 및 수렴·판별타당도" else "3. Latent construct correlations, reliability, and convergent/discriminant validity"), div(class = "table-responsive", tableOutput(paste0(prefix, "_result_validity"))), uiOutput(paste0(prefix, "_result_latent_correlation_ci")), uiOutput(paste0(prefix, "_result_validity_note")), uiOutput(paste0(prefix, "_result_reliability_bootstrap")), uiOutput(paste0(prefix, "_result_factor_scores")), uiOutput(paste0(prefix, "_result_htmt"))),
        div(
          class = "result-section regression-result-panel structural-measurement-result",
          h4(if (ko) "4. 측정모형" else "4. Measurement model"),
          div(class = "table-responsive", tableOutput(paste0(prefix, "_result_measurement"))),
          tags$p(class = "structural-result-note", "* Fixed reference loading; its unstandardized SE, z, and p are not estimated. The standardized loading remains a derived estimate and therefore has a confidence interval."),
          tags$p(class = "structural-result-note", "B confidence intervals are 95% intervals for unstandardized loadings. A fixed reference loading has a degenerate B interval at its fixed value."),
          tags$p(class = "structural-result-note", "Standardized-loading confidence intervals are 95% delta-method intervals from lavaan; robust fitted models use the fitted model's robust covariance information."),
          tags$p(class = "structural-result-note", "R² confidence intervals are obtained by complementing the standardized residual-variance interval (lower = 1 − residual upper; upper = 1 − residual lower)."),
          tags$p(class = "structural-result-note", "Std. residual variance is the standardized diagonal residual variance for each indicator. † marks an unavailable residual value, a residual outside [0, 1], or an R² interval extending beyond [0, 1], including a negative-residual Heywood case."),
          tags$p(class = "structural-result-note", "Guidance prioritizes inadmissible residual variance, then cross-loading, a standardized-loading CI containing 0, and |β| < .40. The .40 value is a descriptive review guideline rather than a universal item-retention rule."),
          tags$p(class = "structural-result-note", "Cross-loaded indicators require theory-based interpretation; simple-structure reliability and discriminant-validity summaries, especially HTMT, may be unavailable or require caution.")
        ),
        uiOutput(paste0(prefix, "_result_higher_order")),
        uiOutput(paste0(prefix, "_result_residuals")),
        uiOutput(paste0(prefix, "_result_mi_holdout")),
        uiOutput(paste0(prefix, "_result_mi_history")),
        if (analysis_type != "plssem") div(class = "result-section regression-result-panel structural-mi-result", h4(if (ko) "6. 수정지수(MI)" else "6. Modification indices (MI)"), uiOutput(paste0(prefix, "_result_mi")))
      )
    })
    output[[paste0(prefix, "_result_fit")]] <- renderUI({
      values <- result_table("fit")
      shiny::req(nrow(values) > 0)
      ci_percent <- round(100 * as.numeric(fit_result()$rmsea_ci %||% .90))
      comparison_fits <- if (isTRUE(fit_result()$modified_from_baseline) && !is.null(fit_result()$baseline_fit)) list(fit_result()$baseline_fit, fit_result()$fit) else list(fit_result()$fit)
      fit_selections <- structural_canvas_common_fit_measures(comparison_fits, fit_result()$estimator %||% "ML", fit_result()$rmsea_ci %||% .90)
      selection <- fit_selections[[length(fit_selections)]]
      fit_labels <- selection$labels
      baseline_selection <- if (length(fit_selections) > 1L) fit_selections[[1L]] else NULL
      same_measure_keys <- is.null(baseline_selection) || identical(selection$keys, baseline_selection$keys)
      if (!same_measure_keys) fit_labels[c(5L, 6L, 8L)] <- c("Adjusted CFI", "Adjusted TLI", "Adjusted RMSEA")
      tagList(tags$table(
        class = "table table-striped table-bordered structural-fit-table",
        tags$thead(
          tags$tr(
            tags$th(rowspan = "2", "Model"),
            tags$th(rowspan = "2", if (!same_measure_keys) HTML("Adjusted &chi;<sup>2</sup>*") else if (grepl("Scaled", fit_labels[[1L]], fixed = TRUE)) HTML("Scaled &chi;<sup>2</sup>*") else HTML("&chi;<sup>2</sup>")), tags$th(rowspan = "2", "df"), tags$th(rowspan = "2", "p"), tags$th(rowspan = "2", "Q"),
            tags$th(rowspan = "2", paste0(fit_labels[[5L]], if (selection$adjusted) "*" else "")), tags$th(rowspan = "2", paste0(fit_labels[[6L]], if (selection$adjusted) "*" else "")), tags$th(rowspan = "2", "SRMR"), tags$th(rowspan = "2", paste0(fit_labels[[8L]], if (selection$adjusted) "*" else "")),
            tags$th(colspan = "2", paste0(ci_percent, "% CI"))
          ),
          tags$tr(tags$th("LLCI"), tags$th("ULCI"))
        ),
        tags$tbody(lapply(seq_len(nrow(values)), function(index) tags$tr(lapply(as.character(values[index, , drop = TRUE]), tags$td))))
      ), if (selection$adjusted)
        tags$p(class = "structural-result-note", if (is.null(baseline_selection) || same_measure_keys) {
          paste0("* Reported lavaan measures: ", paste(unname(selection$keys), collapse = ", "), ". SRMR has no separate robust correction.")
        } else {
          paste0("* Original-model measures: ", paste(unname(baseline_selection$keys), collapse = ", "), "; modified-model measures: ", paste(unname(selection$keys), collapse = ", "), ". SRMR has no separate robust correction.")
        }),
        if ((!is.null(baseline_selection) && baseline_selection$values[[2L]] == 0) || selection$values[[2L]] == 0)
          tags$p(class = "structural-result-note", "Q (chi-square/df) and some fit indices are not interpretable for a saturated model with df = 0.")
      )
    })
    output[[paste0(prefix, "_result_identification")]] <- renderUI({
      bundle <- fit_result()
      issues <- bundle$identification %||% data.frame()
      if (!nrow(issues)) return(tags$p(class = "structural-result-note", "Pre-fit structural identification check: no rule-based issues detected."))
      tags$div(class = "structural-identification-result",
        tags$h5("Pre-fit identification diagnostics"),
        tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(issues), tags$th))),
          tags$tbody(lapply(seq_len(nrow(issues)), function(index) tags$tr(lapply(as.character(issues[index, ]), tags$td))))
        ),
        tags$p(class = "structural-result-note", "This rule-based screen does not prove mathematical identification; lavaan estimation, degrees of freedom, information-matrix checks, and solution admissibility remain decisive.")
      )
    })
    output[[paste0(prefix, "_result_normality")]] <- renderUI({
      bundle <- fit_result()
      if (analysis_type == "plssem" || length(bundle$ordered %||% character(0))) return(NULL)
      indicators <- lavaan::lavNames(bundle$fit, "ov")
      diagnosis <- structural_canvas_mardia(dataset_fn(), indicators)
      if (!isTRUE(diagnosis$available)) {
        return(div(class = "result-section regression-result-panel structural-normality-result",
          h4("Multivariate normality"), tags$p(class = "structural-result-note", diagnosis$reason)))
      }
      table <- data.frame(
        Test = c("Mardia skewness", "Mardia kurtosis"),
        Estimate = c(format_decimal3(diagnosis$skewness), format_decimal3(diagnosis$kurtosis)),
        Statistic = c(format_decimal3(diagnosis$skew_statistic), format_decimal3(diagnosis$kurtosis_z)),
        df = c(format_decimal3(diagnosis$skew_df), "—"),
        p = c(format_p(diagnosis$skew_p), format_p(diagnosis$kurtosis_p)),
        check.names = FALSE
      )
      div(class = "result-section regression-result-panel structural-normality-result",
        h4("Multivariate normality and estimator guidance"),
        tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(table), tags$th))),
          tags$tbody(lapply(seq_len(nrow(table)), function(index) tags$tr(lapply(as.character(table[index, ]), tags$td))))
        ),
        tags$p(class = "structural-result-note", paste0("Guidance: ", diagnosis$recommendation, ". This recommendation is diagnostic and does not automatically change the estimator.")),
        tags$p(class = "structural-result-note", paste0("Complete cases used: ", diagnosis$n, " of ", diagnosis$original_n, "; indicators: ", diagnosis$p, ".")),
        if (isTRUE(diagnosis$sampled)) tags$p(class = "structural-result-note", "For computational stability, Mardia statistics used an evenly spaced deterministic subsample of 2,000 complete cases."),
        tags$p(class = "structural-result-note", "Mardia tests are sample-size sensitive. Consider distribution shape, outliers, and substantive measurement assumptions alongside p-values.")
      )
    })
    output[[paste0(prefix, "_result_risk_diagnostics")]] <- renderUI({
      bundle <- fit_result()
      if (analysis_type == "plssem") return(NULL)
      factor_correlations <- structural_canvas_factor_correlation_diagnostics(bundle$fit)
      flagged_correlations <- factor_correlations[factor_correlations$Severity != "Acceptable", , drop = FALSE]
      if (nrow(flagged_correlations)) {
        flagged_correlations$Correlation <- vapply(flagged_correlations$Correlation, format_decimal3, character(1))
        flagged_correlations[["Absolute correlation"]] <- vapply(flagged_correlations[["Absolute correlation"]], format_decimal3, character(1))
      }
      categories <- structural_canvas_ordered_category_diagnostics(dataset_fn(), bundle$ordered %||% character(0))
      flagged_categories <- if (nrow(categories)) categories[categories$Status != "Adequate", , drop = FALSE] else categories
      if (nrow(flagged_categories)) flagged_categories$Percent <- paste0(vapply(flagged_categories$Percent, format_decimal3, character(1)), "%")
      ordered_pairs <- structural_canvas_ordered_pair_diagnostics(dataset_fn(), bundle$ordered %||% character(0))
      flagged_pairs <- if (nrow(ordered_pairs)) ordered_pairs[ordered_pairs$Status != "Adequate", , drop = FALSE] else ordered_pairs
      if (nrow(flagged_pairs)) flagged_pairs[["Empty %"]] <- paste0(vapply(flagged_pairs[["Empty %"]], format_decimal3, character(1)), "%")
      error_covariances <- structural_canvas_error_covariance_diagnostics(bundle$snapshot %||% list())
      if (!nrow(flagged_correlations) && !nrow(flagged_categories) && !nrow(flagged_pairs) && error_covariances$count == 0L) return(NULL)
      render_data_table <- function(table) tags$table(class = "table table-striped table-bordered",
        tags$thead(tags$tr(lapply(names(table), tags$th))),
        tags$tbody(lapply(seq_len(nrow(table)), function(index) tags$tr(lapply(as.character(table[index, ]), tags$td))))
      )
      div(class = "result-section regression-result-panel structural-risk-result",
        h4("Data and model risk diagnostics"),
        if (nrow(flagged_correlations)) tagList(tags$h5("High latent correlations"), render_data_table(flagged_correlations)),
        if (nrow(flagged_categories)) tagList(tags$h5("Sparse ordered categories"), render_data_table(flagged_categories)),
        if (nrow(flagged_pairs)) tagList(tags$h5("Sparse ordered-indicator cross-tabulations"), render_data_table(flagged_pairs)),
        if (error_covariances$count > 0L) tagList(
          tags$h5("Correlated measurement errors"),
          tags$p(paste0(error_covariances$count, " of ", error_covariances$possible, " possible indicator pairs (", format_decimal3(100 * error_covariances$ratio), "%): ", error_covariances$status, "."))
        ),
        tags$p(class = "structural-result-note", "Latent correlations of .85 or greater warrant discriminant-validity review; .90 or greater are high, and .95 or greater indicate severe construct overlap."),
        if (nrow(flagged_categories)) tags$p(class = "structural-result-note", "A category is flagged when empty, when its count is no greater than max(5, 1% of valid responses), or when it contains at least 95% of valid responses. Sparse or extremely dominant categories can destabilize thresholds and polychoric correlations."),
        if (nrow(flagged_pairs)) tags$p(class = "structural-result-note", "Each ordered-indicator pair is cross-tabulated over all observed or declared categories. Empty cells or nonempty cells with counts no greater than max(5, 1% of pairwise-valid responses) are flagged because they can destabilize polychoric correlations and the WLSMV weight matrix. Category collapsing requires substantive justification and must preserve order."),
        if (error_covariances$count > 0L) tags$p(class = "structural-result-note", "Several correlated errors can indicate item redundancy or data-driven overfitting. Each covariance requires substantive justification.")
      )
    })
    output[[paste0(prefix, "_result_missing_outliers")]] <- renderUI({
      bundle <- fit_result()
      if (analysis_type == "plssem") return(NULL)
      indicators <- lavaan::lavNames(bundle$fit, "ov")
      missing <- structural_canvas_missing_diagnostics(dataset_fn(), indicators)
      outliers <- if (!length(bundle$ordered %||% character(0))) structural_canvas_mahalanobis_diagnostics(dataset_fn(), indicators) else list(available = FALSE, reason = "Mahalanobis diagnostics are not reported for ordered indicators.")
      if (isTRUE(missing$available)) {
        missing_variables <- missing$variables[missing$variables$Missing > 0L, , drop = FALSE]
        if (nrow(missing_variables)) missing_variables$Percent <- paste0(vapply(missing_variables$Percent, format_decimal3, character(1)), "%")
      } else missing_variables <- data.frame()
      render_table <- function(table) tags$table(class = "table table-striped table-bordered",
        tags$thead(tags$tr(lapply(names(table), tags$th))),
        tags$tbody(lapply(seq_len(nrow(table)), function(index) tags$tr(lapply(as.character(table[index, ]), tags$td))))
      )
      div(class = "result-section regression-result-panel structural-missing-outlier-result",
        h4("Missing data and multivariate outliers"),
        if (nrow(missing_variables)) tagList(tags$h5("Variable-level missingness"), render_table(missing_variables)) else tags$p("No missing indicator values were detected."),
        if (isTRUE(missing$available)) tags$p(paste0("Complete cases: ", missing$complete_n, " of ", missing$n, "; incomplete cases: ", missing$incomplete_n, "; distinct missingness patterns: ", missing$pattern_count, ".")),
        if (isTRUE(missing$available) && missing$pattern_count > 1L) tagList(tags$h5("Missingness patterns"), render_table(utils::head(missing$patterns, 20L))),
        tags$p(class = "structural-result-note", if (length(bundle$ordered %||% character(0))) {
          paste0("Ordered-indicator estimation uses ", bundle$missing %||% "pairwise", " missing-data handling. Review sparse pairwise coverage alongside category frequencies.")
        } else if (identical(bundle$missing %||% "", "fiml")) {
          "FIML uses all available observations under the missing-at-random assumption; the complete-case count above applies only to diagnostics such as Mardia and Mahalanobis distance."
        } else {
          paste0("The fitted model used missing-data option: ", bundle$missing %||% "unspecified", ".")
        }),
        tags$h5("Mahalanobis outlier candidates"),
        if (!isTRUE(outliers$available)) tags$p(class = "structural-result-note", outliers$reason) else if (!nrow(outliers$table)) tags$p(paste0("No complete cases were flagged at p < ", outliers$alpha, ".")) else {
          outlier_table <- outliers$table
          outlier_table$Mahalanobis <- vapply(outlier_table$Mahalanobis, format_decimal3, character(1))
          outlier_table$p <- vapply(outlier_table$p, format_p, character(1))
          tagList(render_table(outlier_table), tags$p(paste0(outliers$flagged_n, " of ", outliers$n, " complete cases flagged at p < ", outliers$alpha, ".")))
        },
        tags$p(class = "structural-result-note", "Mahalanobis candidates should be investigated for data errors, unusual but valid cases, and influence. They are not removed automatically; use robust estimation or a documented sensitivity analysis when appropriate.")
      )
    })
    output[[paste0(prefix, "_result_fit_difference")]] <- renderUI({
      bundle <- fit_result()
      if (!identical(bundle$comparison_type %||% "", "mi") || is.null(bundle$baseline_fit)) return(NULL)
      report <- structural_canvas_model_difference_report(bundle)
      if (!nrow(report) || !isTRUE(report$Available[[1L]])) {
        reason <- if (nrow(report)) as.character(report$Reason[[1L]]) else "eligibility was not established"
        return(tags$p(class = "structural-result-note", paste0("A formal model-difference test was suppressed: ", reason)))
      }
      tags$div(
        class = "structural-fit-difference",
        tags$h5("Original vs modified model"),
        tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(tags$th("Δχ²"), tags$th("Δdf"), tags$th("p"))),
          tags$tbody(tags$tr(
            tags$td(format_decimal3(report$`Delta chi-square`[[1L]])),
            tags$td(format_decimal3(report$`Delta df`[[1L]])),
            tags$td(format_p(report$p[[1L]]))
          ))
        ),
        tags$p(class = "structural-result-note", report$Method[[1L]]),
        tags$p(class = "structural-result-note", "Because the modification was selected using MI from the same data, this difference test is exploratory and should not be treated as confirmatory evidence.")
      )
    })
    output[[paste0(prefix, "_result_invariance")]] <- renderUI({
      bundle <- fit_result()
      result <- bundle$invariance_result %||% NULL
      if (is.null(result)) return(NULL)
      table <- result$table
      group_table <- result$group_diagnostics
      group_reliability <- result$group_reliability %||% data.frame()
      group_htmt <- result$group_htmt %||% data.frame()
      group_residuals <- result$group_residuals %||% list()
      residual_summary <- if (isTRUE(group_residuals$available)) group_residuals$group_summary %||% data.frame() else data.frame()
      residual_largest <- if (isTRUE(group_residuals$available)) group_residuals$group_largest %||% data.frame() else data.frame()
      group_table[["Indicator missing %"]] <- paste0(vapply(group_table[["Indicator missing %"]], format_decimal3, character(1)), "%")
      if (nrow(group_reliability)) {
        for (name in intersect(c("AVE", "CR", "Cronbach's alpha", "Omega total"), names(group_reliability))) {
          group_reliability[[name]] <- vapply(group_reliability[[name]], format_decimal3, character(1))
        }
      }
      if (nrow(group_htmt)) {
        for (name in intersect(c("HTMT"), names(group_htmt))) {
          group_htmt[[name]] <- vapply(group_htmt[[name]], format_decimal3, character(1))
        }
      }
      if (nrow(residual_summary)) {
        residual_summary[["Max |standardized residual|"]] <- vapply(residual_summary[["Max |standardized residual|"]], format_decimal3, character(1))
      }
      if (nrow(residual_largest)) {
        residual_largest[["Standardized residual"]] <- vapply(residual_largest[["Standardized residual"]], format_decimal3, character(1))
        residual_largest[["Correlation residual"]] <- vapply(residual_largest[["Correlation residual"]], format_decimal3, character(1))
      }
      srmr_limit <- ifelse(table$Model == "Metric", .030, .010)
      table$Decision <- ifelse(
        !table$Admissible, "Inadmissible stage",
        ifelse(table$Model == "Configural", "Baseline stage",
        ifelse(table$Admissible & table$DeltaCFI >= -.010 & table$DeltaRMSEA <= .015 & table$DeltaSRMR <= srmr_limit, "Change criteria met", "Review noninvariance")
      ))
      reviewed_stages <- table$Model[table$Decision == "Review noninvariance"]
      score_tables <- (result$score_diagnostics %||% list())[intersect(reviewed_stages, names(result$score_diagnostics %||% list()))]
      score_tables <- Filter(function(value) nrow(value), score_tables)
      numeric_columns <- c("Chisq", "df", "CFI", "RMSEA", "SRMR", "DeltaCFI", "DeltaRMSEA", "DeltaSRMR", "DeltaChisq", "DeltaDf", "Residual min eigenvalue", "Latent min eigenvalue", "Parameter min eigenvalue")
      for (name in numeric_columns) table[[name]] <- vapply(table[[name]], format_decimal3, character(1))
      for (name in c("Residual condition number", "Latent condition number", "Parameter condition number")) table[[name]] <- vapply(table[[name]], function(value) if (is.finite(value)) format(value, scientific = TRUE, digits = 3) else "Inf", character(1))
      table$p <- vapply(table$p, format_p, character(1))
      table$DeltaP <- vapply(table$DeltaP, format_p, character(1))
      names(table)[names(table) == "DeltaCFI"] <- "ΔCFI"
      names(table)[names(table) == "DeltaRMSEA"] <- "ΔRMSEA"
      names(table)[names(table) == "DeltaSRMR"] <- "ΔSRMR"
      names(table)[names(table) == "DeltaChisq"] <- "Δχ²"
      names(table)[names(table) == "DeltaDf"] <- "Δdf"
      names(table)[names(table) == "DeltaP"] <- "Δp"
      div(class = "result-section regression-result-panel structural-invariance-result",
        h4(paste0("Measurement invariance by ", result$group)),
        tags$h5("Group-level data diagnostics"),
        tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(group_table), tags$th))),
          tags$tbody(lapply(seq_len(nrow(group_table)), function(index) tags$tr(lapply(as.character(group_table[index, ]), tags$td))))
        )),
        tags$p(class = "structural-result-note", "Group N below 100 is flagged as a descriptive small-group warning, not a universal minimum. Adequacy depends on model complexity, indicator quality, estimator, missingness, category distribution, and effect size."),
        if (nrow(residual_summary)) tagList(
          tags$h5("Group-specific configural residual diagnostics"),
          tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
            tags$thead(tags$tr(lapply(names(residual_summary), tags$th))),
            tags$tbody(lapply(seq_len(nrow(residual_summary)), function(index) tags$tr(lapply(as.character(residual_summary[index, ]), tags$td))))
          )),
          if (nrow(residual_largest)) tagList(
            tags$h5(paste0("Large group-specific residuals (|z| >= ", group_residuals$cutoff %||% 1.96, ")")),
            tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
              tags$thead(tags$tr(lapply(names(residual_largest), tags$th))),
              tags$tbody(lapply(seq_len(nrow(residual_largest)), function(index) tags$tr(lapply(as.character(residual_largest[index, ]), tags$td))))
            ))
          ) else tags$p(class = "structural-result-note", paste0("No group-specific configural residuals exceeded |z| >= ", group_residuals$cutoff %||% 1.96, ".")),
          tags$p(class = "structural-result-note", "These residual diagnostics are computed separately within each group from the configural model. For WLSMV ordered indicators they are approximate local-fit diagnostics on the fitted latent-response/polychoric scale; use them to locate candidate areas for review, not as automatic modification instructions.")
        ),
        if (nrow(group_reliability)) tagList(
          tags$h5("Group-specific reliability and convergent validity"),
          tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
            tags$thead(tags$tr(lapply(names(group_reliability), tags$th))),
            tags$tbody(lapply(seq_len(nrow(group_reliability)), function(index) tags$tr(lapply(as.character(group_reliability[index, ]), tags$td))))
          )),
          tags$p(class = "structural-result-note", "Group-specific AVE, CR, alpha, and omega are computed from the configural model, before imposing equality constraints. Use them as descriptive group diagnostics, not as formal invariance tests.")
        ),
        if (nrow(group_htmt)) tagList(
          tags$h5("Group-specific HTMT"),
          tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
            tags$thead(tags$tr(lapply(names(group_htmt), tags$th))),
            tags$tbody(lapply(seq_len(nrow(group_htmt)), function(index) tags$tr(lapply(as.character(group_htmt[index, ]), tags$td))))
          )),
          tags$p(class = "structural-result-note", "Group-specific HTMT uses each group's configural-model sample correlation matrix. Bootstrap intervals remain single-group in this release.")
        ),
        tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(table), tags$th))),
          tags$tbody(lapply(seq_len(nrow(table)), function(index) tags$tr(lapply(as.character(table[index, ]), tags$td))))
        )),
        if (any(!result$table$Admissible)) tags$p(class = "structural-result-note", "An inadmissible invariance stage failed the same full checks as the main CFA. Its change indices, formal difference test, and equality-constraint score diagnostics are suppressed; resolve the stage-specific variance, covariance-matrix, df, or latent-correlation problem before judging invariance."),
        tags$p(class = "structural-result-note", "Parameter boundary dimensions counts near-zero eigenvalues in the parameter covariance matrix. Boundary dimensions up to the number of explicit equality constraints are treated as constraint-induced; any excess is flagged as unexplained empirical underidentification."),
        tags$p(class = "structural-result-note", "Minimum eigenvalues report the worst group-specific residual and latent covariance eigenvalues and the parameter covariance minimum. Negative values beyond numerical tolerance indicate non-positive-definiteness; values near zero indicate a boundary or singular direction."),
        if (any(result$table$`Ill-conditioned warning`)) tags$p(class = "structural-result-note", "Ill-conditioned warning marks a residual, latent, or parameter covariance condition number above 1e8 (or an infinite value). This indicates numerical sensitivity but is not by itself proof of inadmissibility."),
        if (length(score_tables)) tagList(
          tags$h5("Largest equality-constraint score tests"),
          lapply(names(score_tables), function(stage) {
            score_table <- utils::head(score_tables[[stage]], 10L)
            score_table[["Score χ²"]] <- vapply(score_table[["Score χ²"]], format_decimal3, character(1))
            score_table[["Raw χ²"]] <- vapply(score_table[["Raw χ²"]], format_decimal3, character(1))
            score_table$p <- vapply(score_table$p, format_p, character(1))
            score_table[["BH-adjusted p"]] <- vapply(score_table[["BH-adjusted p"]], format_p, character(1))
            score_table[["Max |standardized EPC|"]] <- vapply(score_table[["Max |standardized EPC|"]], format_decimal3, character(1))
            score_table[["Raw p"]] <- vapply(score_table[["Raw p"]], format_p, character(1))
            score_table[["Raw BH-adjusted p"]] <- vapply(score_table[["Raw BH-adjusted p"]], format_p, character(1))
            tagList(tags$h6(stage), tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
              tags$thead(tags$tr(lapply(names(score_table), tags$th))),
              tags$tbody(lapply(seq_len(nrow(score_table)), function(index) tags$tr(lapply(as.character(score_table[index, ]), tags$td))))
            )))
          }),
          tags$p(class = "structural-result-note", "Score tests rank equality constraints that contribute to stage misfit. Group labels are the observed grouping-variable categories. Max |standardized EPC| is the largest absolute fully standardized expected parameter change among the parameters in that equality constraint and provides an effect-size-oriented diagnostic. BH-adjusted p values control the false-discovery rate within each invariance stage. These remain exploratory diagnostics and do not automatically establish partial invariance or justify freeing a constraint.")
        ),
        tags$p(class = "structural-result-note", if (isTRUE(result$ordinal)) "For ordered indicators, nested theta-parameterized stages constrain thresholds, then thresholds plus loadings (scalar/strong invariance), then residual variances (strict). A separate conventional metric-only stage is not reported because categorical identification depends jointly on thresholds, loadings, scales, and intercepts." else "Nested stages constrain loadings (metric), then intercepts (scalar), then residual variances (strict). Δ values compare each row with the immediately preceding stage; robust/scaled fit indices and difference tests are used when available."),
        if (isTRUE(result$ordinal)) tags$p(class = "structural-result-note", "Ordered-indicator models use WLSMV/DWLS, category thresholds, and theta parameterization. Scalar invariance therefore means invariant thresholds and loadings, not equality of observed-variable intercepts as in continuous CFA."),
        tags$p(class = "structural-result-note", "Descriptive change guidance flags ΔCFI < −.010, ΔRMSEA > .015, or ΔSRMR > .030 for metric and > .010 for scalar/strict invariance. These guidelines should be considered jointly with parameter changes, group sizes, theory, and model admissibility; Δχ² is sample-size sensitive."),
        tags$p(class = "structural-result-note", "Failure at a stage does not justify automatically freeing parameters. Partial invariance requires substantively defensible constraints and transparent reporting.")
      )
    })
    output[[paste0(prefix, "_result_fit_guidance")]] <- renderUI({
      bundle <- fit_result()
      comparison_fits <- if (isTRUE(bundle$modified_from_baseline) && !is.null(bundle$baseline_fit)) list(bundle$baseline_fit, bundle$fit) else list(bundle$fit)
      selections <- structural_canvas_common_fit_measures(comparison_fits, bundle$estimator %||% "ML", bundle$rmsea_ci %||% .90)
      labels <- if (length(selections) > 1L) c("Original model", bundle$comparison_label %||% "Modified model") else "Original model"
      tables <- lapply(seq_along(selections), function(index) {
        guidance <- structural_canvas_fit_guidance(selections[[index]]$values)
        guidance$Model <- labels[[index]]
        guidance[, c("Model", "Metric", "Value", "Guidance", "Reference"), drop = FALSE]
      })
      table <- do.call(rbind, tables)
      table$Value <- vapply(table$Value, format_decimal3, character(1))
      severity <- c("Good" = 1L, "Marginal" = 2L, "Review" = 3L, "Not assessed" = 4L)
      summaries <- vapply(split(table$Guidance, table$Model), function(values) {
        if (all(values == "Not assessed")) return("Not assessed")
        score <- max(vapply(values[values != "Not assessed"], function(value) severity[[value]], integer(1)))
        names(severity)[match(score, severity)]
      }, character(1))
      div(class = "structural-fit-guidance-result",
        tags$h5("Reference-based fit guidance"),
        tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(table), tags$th))),
          tags$tbody(lapply(seq_len(nrow(table)), function(index) tags$tr(lapply(as.character(table[index, ]), tags$td))))
        ),
        tags$p(paste(vapply(names(summaries), function(name) paste0(name, ": ", summaries[[name]]), character(1)), collapse = " | ")),
        tags$p(class = "structural-result-note", "Good/Marginal/Review labels are descriptive reference guidance based on commonly used approximate cutoffs. They are not universal acceptance rules and do not replace model identification, residual diagnostics, parameter plausibility, theory, sample characteristics, or comparison with plausible alternatives."),
        tags$p(class = "structural-result-note", "Incremental-fit guidance: CFI/TLI >= .95 Good, >= .90 Marginal. Absolute-fit guidance: RMSEA <= .06 Good, <= .08 Marginal; SRMR <= .08 Good, <= .10 Marginal. Values outside these ranges are marked Review."),
        if (any(table$Guidance == "Not assessed")) tags$p(class = "structural-result-note", "Fit guidance is not assessed for saturated models (df = 0) or unavailable fit indices.")
      )
    })
    output[[paste0(prefix, "_result_rmsea_tests")]] <- renderUI({
      bundle <- fit_result()
      table <- structural_canvas_rmsea_hypothesis_tests(bundle)
      display <- table
      for (column in c("RMSEA", "Close-fit H0", "Not-close H0")) display[[column]] <- vapply(display[[column]], format_decimal3, character(1))
      for (column in c("Close-fit p", "Not-close p")) display[[column]] <- vapply(display[[column]], format_p, character(1))
      tagList(
        tags$h5("RMSEA hypothesis tests"),
        tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(display), tags$th))),
          tags$tbody(lapply(seq_len(nrow(display)), function(index) tags$tr(lapply(as.character(display[index, ]), tags$td))))
        )),
        tags$p(class = "structural-result-note", "Close-fit tests H0: population RMSEA <= .05; a small p value rejects close fit. Not-close tests H0: population RMSEA >= .08; a small p value rejects poor approximate fit. Interpret both with the RMSEA estimate and confidence interval."),
        tags$p(class = "structural-result-note", "Robust or scaled p values are selected to match the reported RMSEA when available. These hypothesis tests are sample-size sensitive and are not standalone model-acceptance rules.")
      )
    })
    output[[paste0(prefix, "_result_information_criteria")]] <- renderUI({
      bundle <- fit_result()
      table <- structural_canvas_information_criteria(bundle)
      if (!any(is.finite(table$AIC)) && !any(is.finite(table$BIC))) return(NULL)
      display <- table
      numeric_columns <- names(display)[vapply(display, is.numeric, logical(1))]
      for (column in numeric_columns) display[[column]] <- vapply(display[[column]], format_decimal3, character(1))
      tagList(
        tags$h5("Likelihood-based information criteria"),
        tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(display), tags$th))),
          tags$tbody(lapply(seq_len(nrow(display)), function(index) tags$tr(lapply(as.character(display[index, ]), tags$td))))
        )),
        tags$p(class = "structural-result-note", "Lower AIC, BIC, and adjusted BIC indicate better relative expected fit after penalizing complexity; delta values are relative to the smallest criterion in this displayed set."),
        if (any(grepl("^Not comparable", table$`Comparison status`))) tags$p(class = "structural-result-note", "Delta values were suppressed because analyzed observations, observed variables, estimator family, or model admissibility differed across models."),
        tags$p(class = "structural-result-note", "Compare information criteria only for models fitted to the same observations and observed variables with the same likelihood and estimator family. They are not absolute goodness-of-fit tests and are not reported for WLSMV models without a comparable likelihood.")
      )
    })
    output[[paste0(prefix, "_result_bollen_stine")]] <- renderUI({
      bundle <- fit_result()
      result <- bundle$bollen_stine_result %||% NULL
      if (is.null(result) || !nrow(result)) return(NULL)
      display <- result
      display[["Observed chi-square"]] <- vapply(display[["Observed chi-square"]], format_decimal3, character(1))
      display[["Bootstrap p"]] <- vapply(display[["Bootstrap p"]], format_p, character(1))
      for (column in c("Monte Carlo SE", "Monte Carlo 95% lower", "Monte Carlo 95% upper")) {
        display[[column]] <- vapply(display[[column]], format_decimal3, character(1))
      }
      display[["Valid %"]] <- paste0(vapply(display[["Valid %"]], format_decimal3, character(1)), "%")
      tagList(
        tags$h5("Bollen-Stine bootstrap global-fit test"),
        tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(display), tags$th))),
          tags$tbody(lapply(seq_len(nrow(display)), function(index) tags$tr(lapply(as.character(display[index, ]), tags$td))))
        )),
        tags$p(class = "structural-result-note", "The bootstrap p value uses the plus-one correction: (1 + bootstrap chi-square values at least as large as observed) / (1 + valid replicates). Valid replicates pass the same convergence, variance, covariance-matrix, df, and latent-correlation admissibility checks as the main CFA. A small p value indicates global model misfit under the exact-fit null."),
        tags$p(class = "structural-result-note", "Monte Carlo SE and the 95% Wilson interval quantify simulation error from the finite number of valid bootstrap replicates; they are not a confidence interval for a population model parameter."),
        if (any(result$Status != "Adequate")) tags$p(class = "structural-result-note", "Fewer than 80% of requested resamples produced a converged admissible statistic. Treat the bootstrap p value and Monte Carlo interval as unstable; resolve convergence or admissibility problems before reporting."),
        if (isTRUE(bundle$modified_from_baseline)) tags$p(class = "structural-result-note", "This model was modified using the analyzed data. Its Bollen-Stine result is exploratory and does not provide confirmatory evidence for the data-driven modification."),
        tags$p(class = "structural-result-note", "This transformed-data test is available only for complete continuous single-group ML CFA and does not replace approximate fit indices, residual diagnostics, or substantive model evaluation.")
      )
    })
    output[[paste0(prefix, "_result_heywood")]] <- renderUI({
      bundle <- fit_result()
      diagnostics <- bundle$baseline_diagnostics %||% bundle$diagnostics %||% list()
      variables <- as.character(diagnostics$negative_residuals %||% character(0))
      latent_variables <- as.character(diagnostics$negative_latent_variances %||% character(0))
      theta_matrix_issue <- isTRUE(diagnostics$non_psd_theta) || isTRUE(diagnostics$near_singular_theta) || isTRUE(diagnostics$ill_conditioned_theta)
      latent_matrix_issue <- isTRUE(diagnostics$non_psd_latent_covariance) || isTRUE(diagnostics$near_singular_latent_covariance) || isTRUE(diagnostics$ill_conditioned_latent_covariance)
      parameter_matrix_issue <- isTRUE(diagnostics$non_psd_parameter_covariance) || isTRUE(diagnostics$near_singular_parameter_covariance) || isTRUE(diagnostics$ill_conditioned_parameter_covariance)
      matrix_issue <- theta_matrix_issue || latent_matrix_issue || parameter_matrix_issue
      if ((!length(variables) && !length(latent_variables) && !matrix_issue) || analysis_type == "plssem") return(NULL)
      diagnostic_fit <- bundle$baseline_fit %||% bundle$fit
      theta <- as.matrix(lavaan::lavInspect(diagnostic_fit, "theta"))
      standardized <- lavaan::standardizedSolution(diagnostic_fit)
      r2 <- lavaan::lavInspect(diagnostic_fit, "r2")
      loadings <- standardized[standardized$op == "=~", c("lhs", "rhs", "est.std"), drop = FALSE]
      residual_rows <- standardized$op == "~~" & standardized$lhs == standardized$rhs
      standardized_residuals <- stats::setNames(standardized$est.std[residual_rows], standardized$lhs[residual_rows])
      data <- dataset_fn()
      observed_variances <- vapply(variables, function(name) stats::var(data[[name]], na.rm = TRUE), numeric(1))
      fixed_values <- as.numeric((bundle$residual_variance_fixes %||% numeric(0))[variables])
      applied_percent <- 100 * fixed_values / observed_variances
      diagnostic_table <- data.frame(
        Variable = variables,
        Factor = vapply(variables, function(name) paste(unique(loadings$lhs[loadings$rhs == name]), collapse = ", "), character(1)),
        `Residual variance` = vapply(variables, function(name) theta[name, name], numeric(1)),
        `Standardized residual` = as.numeric(standardized_residuals[variables]),
        `R²` = as.numeric(r2[variables]),
        `Observed variance` = observed_variances,
        `Applied %` = applied_percent,
        `Fixed value` = fixed_values,
        Status = "Heywood",
        check.names = FALSE
      )
      latent_covariance <- as.matrix(lavaan::lavInspect(diagnostic_fit, "cov.lv"))
      latent_table <- if (length(latent_variables)) data.frame(
        `Latent factor` = latent_variables,
        Variance = vapply(latent_variables, function(name) latent_covariance[name, name], numeric(1)),
        Status = "Latent Heywood", check.names = FALSE
      ) else data.frame()
      matrix_table <- data.frame(
        Matrix = c(if (theta_matrix_issue) "Residual covariance (theta)", if (latent_matrix_issue) "Latent covariance", if (parameter_matrix_issue) "Parameter-estimate covariance (vcov)"),
        `Minimum eigenvalue` = c(if (theta_matrix_issue) diagnostics$theta_min_eigenvalue, if (latent_matrix_issue) diagnostics$latent_min_eigenvalue, if (parameter_matrix_issue) diagnostics$parameter_min_eigenvalue),
        `Condition number` = c(if (theta_matrix_issue) diagnostics$theta_condition_number, if (latent_matrix_issue) diagnostics$latent_condition_number, if (parameter_matrix_issue) diagnostics$parameter_condition_number),
        Status = c(
          if (theta_matrix_issue) if (isTRUE(diagnostics$non_psd_theta)) "Not positive semidefinite" else if (isTRUE(diagnostics$near_singular_theta)) "Near singular / boundary" else "Ill-conditioned",
          if (latent_matrix_issue) if (isTRUE(diagnostics$non_psd_latent_covariance)) "Not positive semidefinite" else if (isTRUE(diagnostics$near_singular_latent_covariance)) "Near singular / boundary" else "Ill-conditioned",
          if (parameter_matrix_issue) if (isTRUE(diagnostics$non_psd_parameter_covariance)) "Unreliable standard errors" else if (isTRUE(diagnostics$near_singular_parameter_covariance)) "Empirical identification boundary" else "Ill-conditioned standard errors"
        ), check.names = FALSE
      )
      can_refit <- length(variables) > 0L && toupper(as.character(bundle$estimator %||% "ML")) %in% c("ML", "MLR") && !length(bundle$ordered %||% character(0))
      tagList(
        div(class = "result-section regression-result-panel structural-heywood-result",
          h4("Heywood case diagnostics"),
          if (nrow(diagnostic_table)) tags$table(class = "table table-striped table-bordered",
            tags$thead(tags$tr(lapply(names(diagnostic_table), tags$th))),
            tags$tbody(lapply(seq_len(nrow(diagnostic_table)), function(index) tags$tr(lapply(c(
              diagnostic_table$Variable[[index]], diagnostic_table$Factor[[index]],
              format_decimal3(diagnostic_table[["Residual variance"]][[index]]),
              format_decimal3(diagnostic_table[["Standardized residual"]][[index]]),
              format_decimal3(diagnostic_table[["R²"]][[index]]),
              format_decimal3(diagnostic_table[["Observed variance"]][[index]]),
              if (is.finite(diagnostic_table[["Applied %"]][[index]])) paste0(format_decimal3(diagnostic_table[["Applied %"]][[index]]), "%") else "—",
              if (is.finite(diagnostic_table[["Fixed value"]][[index]])) format_decimal3(diagnostic_table[["Fixed value"]][[index]]) else "—", diagnostic_table$Status[[index]]
            ), tags$td))))
          ),
          if (nrow(latent_table)) tagList(
            tags$h5("Negative latent variances"),
            tags$table(class = "table table-striped table-bordered",
              tags$thead(tags$tr(lapply(names(latent_table), tags$th))),
              tags$tbody(lapply(seq_len(nrow(latent_table)), function(index) tags$tr(
                tags$td(latent_table[["Latent factor"]][[index]]),
                tags$td(format_decimal3(latent_table$Variance[[index]])),
                tags$td(latent_table$Status[[index]])
              )))
            ),
            tags$p(class = "structural-result-note", "A negative latent variance is a latent-variable Heywood case. The indicator residual-variance sensitivity button does not correct it; review factor specification, scaling, higher-order structure, correlations, and identification constraints.")
          ),
          if (nrow(matrix_table)) tagList(
            tags$h5("Covariance-matrix definiteness diagnostics"),
            tags$table(class = "table table-striped table-bordered",
              tags$thead(tags$tr(lapply(names(matrix_table), tags$th))),
              tags$tbody(lapply(seq_len(nrow(matrix_table)), function(index) tags$tr(
                tags$td(matrix_table$Matrix[[index]]),
                tags$td(format_decimal3(matrix_table[["Minimum eigenvalue"]][[index]])),
                tags$td(if (is.finite(matrix_table[["Condition number"]][[index]])) format(matrix_table[["Condition number"]][[index]], scientific = TRUE, digits = 3) else "Inf"),
                tags$td(matrix_table$Status[[index]])
              )))
            ),
            tags$p(class = "structural-result-note", "A negative minimum eigenvalue means the covariance matrix is not positive semidefinite. A value near zero indicates a singular boundary. For vcov, these findings mean standard errors, confidence intervals, z tests, and p values may be unreliable and can indicate empirical underidentification. A condition number above 1e8 flags severe numerical sensitivity; it is a warning rather than, by itself, proof of inadmissibility. Review excessive covariance paths, near-collinear factors, correlations, constraints, and identification.")
          ),
          if (can_refit) actionButton(paste0(prefix, "_heywood_refit"), "Constrained reanalysis", class = "btn-warning btn-sm"),
          if (length(variables) && !can_refit) tags$p(class = "structural-result-note", "Constrained residual-variance reanalysis is available only for continuous indicators estimated with ML or MLR."),
          tags$p(class = "structural-result-note", "A constrained reanalysis is a sensitivity analysis and does not resolve the source of the Heywood case.")
        )
      )
    })
    output[[paste0(prefix, "_result_latent_correlation_ci")]] <- renderUI({
      bundle <- fit_result()
      values <- structural_canvas_latent_correlation_intervals(bundle$fit, level = .95)
      if (!nrow(values)) return(NULL)
      values$r <- vapply(values$r, format_decimal3, character(1))
      values[["CI lower"]] <- vapply(values[["CI lower"]], format_decimal3, character(1))
      values[["CI upper"]] <- vapply(values[["CI upper"]], format_decimal3, character(1))
      values$p <- vapply(values$p, format_p, character(1))
      values$p[values$Type == "Fixed"] <- "—"
      tags$div(
        class = "structural-latent-correlation-ci",
        tags$h5("Latent correlation confidence intervals"),
        tags$div(class = "table-responsive", tags$table(
          class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(values), tags$th))),
          tags$tbody(lapply(seq_len(nrow(values)), function(index) tags$tr(lapply(as.character(values[index, ]), tags$td))))
        )),
        tags$p(class = "structural-result-note", "Intervals are 95% delta-method intervals for explicitly estimated or fixed latent covariance paths. 'CI reaches |1|' flags an interval touching an inadmissible correlation boundary; implied correlations without an explicit covariance parameter are not assigned a delta-method interval here.")
      )
    })
    output[[paste0(prefix, "_result_validity_note")]] <- renderUI({
      bundle <- fit_result()
      validity_values <- result_table("validity")
      abnormal_reliability <- any(grepl("†", as.character(unlist(validity_values, use.names = FALSE)), fixed = TRUE))
      single_indicator <- any(grepl("‡", as.character(unlist(validity_values, use.names = FALSE)), fixed = TRUE))
      constrained_single_indicator <- any(grepl("¶", as.character(unlist(validity_values, use.names = FALSE)), fixed = TRUE))
      orthogonal_not_assessed <- any(grepl("§", as.character(unlist(validity_values, use.names = FALSE)), fixed = TRUE))
      theta <- as.matrix(lavaan::lavInspect(bundle$fit, "theta"))
      correlated_errors <- length(theta) > 1L && any(abs(theta[row(theta) != col(theta)]) > sqrt(.Machine$double.eps), na.rm = TRUE)
      snapshot <- bundle$snapshot %||% list()
      missing_covariances <- structural_canvas_missing_exogenous_covariances(snapshot)
      has_higher_order <- any(vapply(snapshot$edges %||% list(), function(edge) identical(as.character(edge$pathType %||% ""), "higherOrder"), logical(1)))
      validity_loadings <- lavaan::standardizedSolution(bundle$fit)
      validity_loadings <- validity_loadings[validity_loadings$op == "=~" & validity_loadings$rhs %in% lavaan::lavNames(bundle$fit, "ov"), c("lhs", "rhs"), drop = FALSE]
      cross_loaded_indicators <- unique(validity_loadings$rhs[duplicated(validity_loadings$rhs) | duplicated(validity_loadings$rhs, fromLast = TRUE)])
      tagList(
        tags$p(class = "structural-result-note", "Diagonal values in parentheses are sqrt(AVE); lower-triangle values are latent correlations. Max |r| is compared with sqrt(AVE). The remaining columns report the number of indicators (k), AVE, CR, Cronbach's alpha, and McDonald's omega total."),
        tags$p(class = "structural-result-note", "Fornell-Larcker is marked 'Criterion met' when sqrt(AVE) is greater than the factor's largest absolute correlation; otherwise it is marked 'Review needed'."),
        tags$p(class = "structural-result-note", "Guidance uses commonly cited descriptive cutoffs (AVE ≥ .50; CR, Cronbach's alpha, and omega total ≥ .70). These are heuristics rather than universal pass/fail rules and should be interpreted with construct breadth, item count, model admissibility, and study purpose."),
        if (length(missing_covariances)) tags$p(class = "structural-result-note", paste0("Caution: missing exogenous latent covariance paths (", paste(missing_covariances, collapse = ", "), ") are fixed to zero.")),
        if (correlated_errors) tags$p(class = "structural-result-note", "CR incorporates the estimated measurement-error covariances in the residual covariance matrix."),
        if (abnormal_reliability) tags$p(class = "structural-result-note", "† AVE or a reliability coefficient is unavailable or outside [0, 1], indicating unusual item covariances, an inadmissible solution, or an unidentified calculation."),
        if (single_indicator) tags$p(class = "structural-result-note", "‡ AVE and CR are not reported for a single-indicator factor without externally justified reliability constraints."),
        if (constrained_single_indicator) tags$p(class = "structural-result-note", "¶ The single-indicator factor uses an externally justified fixed residual variance. AVE, CR, and omega total reflect that imposed reliability constraint rather than independently estimated internal consistency; Cronbach's alpha and Fornell-Larcker are not assessed."),
        if (orthogonal_not_assessed) tags$p(class = "structural-result-note", "§ Fornell-Larcker was not assessed because one or more exogenous latent covariances were fixed to zero by omitted covariance paths."),
        if (length(bundle$ordered %||% character(0))) tags$p(class = "structural-result-note", "For ordered indicators, AVE, CR, and omega use the standardized latent-response solution; Cronbach's alpha uses lavaan's polychoric correlation matrix (ordinal alpha)."),
        if (!length(bundle$ordered %||% character(0))) tags$p(class = "structural-result-note", "Cronbach's alpha uses the analyzed indicators' sample covariance matrix. Omega is model-based and incorporates estimated residual covariances."),
        tags$p(class = "structural-result-note", "In this table, omega total uses the same model-implied loadings and residual covariance matrix as CR; under the current congeneric scoring specification it therefore equals CR. Both labels are retained for reporting clarity."),
        if (has_higher_order) tags$p(class = "structural-result-note", "AVE, CR, Fornell-Larcker, and HTMT are reported for first-order factors with observed indicators; higher-order factors are excluded from these reliability and discriminant-validity calculations."),
        if (length(cross_loaded_indicators)) tags$p(class = "structural-result-note", paste0("Caution: cross-loaded indicators (", paste(cross_loaded_indicators, collapse = ", "), ") appear in more than one factor. Factor-specific AVE/CR remain descriptive, while simple-structure discriminant-validity interpretations require particular caution.")),
        if (!isTRUE(bundle$diagnostics$admissible %||% TRUE)) tags$p(class = "structural-result-note", "Caution: the fitted solution failed one or more admissibility checks; AVE, CR, and discriminant-validity results should not be interpreted as final.")
      )
    })
    output[[paste0(prefix, "_result_factor_scores")]] <- renderUI({
      bundle <- fit_result()
      values <- structural_canvas_factor_score_quality(bundle$fit)
      if (!nrow(values)) return(NULL)
      values$Determinacy <- vapply(values$Determinacy, format_decimal3, character(1))
      values[["Score reliability"]] <- vapply(values[["Score reliability"]], format_decimal3, character(1))
      tagList(
        tags$h5("Factor-score quality"),
        tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(values), tags$th))),
          tags$tbody(lapply(seq_len(nrow(values)), function(index) tags$tr(lapply(as.character(values[index, ]), tags$td))))
        )),
        tags$p(class = "structural-result-note", "Determinacy is the correlation between regression factor scores and the latent factor; score reliability is its square. Descriptive guidance uses .90 as strong and .80 as a cautious-use threshold. These indices concern estimated factor scores for downstream or individual-level use and are not substitutes for CR, omega, validity evidence, or model admissibility."),
        if (length(bundle$ordered %||% character(0))) tags$p(class = "structural-result-note", "For ordered indicators, factor-score quality is conditional on the fitted latent-response WLSMV model and category thresholds.")
      )
    })
    output[[paste0(prefix, "_result_reliability_bootstrap")]] <- renderUI({
      bundle <- fit_result()
      values <- bundle$reliability_bootstrap_result %||% NULL
      requested <- as.integer(bundle$reliability_bootstrap %||% 0L)
      if (requested <= 0L) return(tags$p(class = "structural-result-note", "AVE, CR, Cronbach's alpha, and omega are point estimates. Select AVE/reliability bootstrap CI in the analysis options when interval estimates are required."))
      if (is.null(values) || !nrow(values)) return(tags$p(class = "structural-result-note", "AVE/reliability bootstrap intervals could not be estimated because no resample produced usable estimates."))
      reliability_ci_method <- structural_canvas_bootstrap_ci_method(bundle$reliability_ci_method %||% "percentile")
      reliability_ci_label <- if (identical(reliability_ci_method, "bca")) "BCa" else "percentile"
      incomplete <- values[["Valid replicates"]] < values[["Requested replicates"]]
      caution_intervals <- values$Status == "Caution"
      unreliable_intervals <- values$Status == "Unreliable"
      boundary_interval <- !is.finite(values$Lower) | !is.finite(values$Upper) | values$Lower < 0 | values$Upper > 1
      if (!"CI method" %in% names(values)) values[["CI method"]] <- "Percentile"
      bca_unavailable <- "CI method" %in% names(values) && any(values[["CI method"]] == "BCa unavailable")
      values$Statistic[values$Statistic == "Alpha"] <- "Cronbach's α"
      values$Statistic[values$Statistic == "Omega"] <- "McDonald's ωtotal"
      values$Estimate <- paste0(vapply(values$Estimate, format_decimal3, character(1)), ifelse(!is.finite(values$Estimate) | values$Estimate < 0 | values$Estimate > 1, "†", ""))
      values$Lower <- paste0(vapply(values$Lower, format_decimal3, character(1)), ifelse(boundary_interval, "†", ""))
      values$Upper <- paste0(vapply(values$Upper, format_decimal3, character(1)), ifelse(boundary_interval, "†", ""))
      values[["Valid %"]] <- paste0(vapply(values[["Valid %"]], format_decimal3, character(1)), "%")
      names(values)[names(values) == "Lower"] <- "95% CI lower"
      names(values)[names(values) == "Upper"] <- "95% CI upper"
      tagList(
        tags$h5(paste0("AVE and reliability ", reliability_ci_label, " bootstrap intervals (", requested, " resamples; seed = ", bundle$reliability_seed, ")")),
        tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(values), tags$th))),
          tags$tbody(lapply(seq_len(nrow(values)), function(index) tags$tr(lapply(as.character(values[index, ]), tags$td))))
        )),
        tags$p(class = "structural-result-note", paste0("Intervals use case resampling", if (identical(reliability_ci_method, "bca")) " with leave-one-out jackknife acceleration" else " percentile bootstrap", " and the selected ", if (identical(bundle$validity_formula, "model_implied")) "model-implied parameter" else "standardized-loading", " AVE/CR formula. McDonald's omega total follows the same fitted congeneric scoring formula as CR in this output.")),
        if (bca_unavailable) tags$p(class = "structural-result-note", "BCa unavailable means the bias-correction or jackknife acceleration could not be computed for that statistic; increase valid replicates or use percentile CI for reporting."),
        if (any(boundary_interval)) tags$p(class = "structural-result-note", "† marks an estimate or interval extending outside the admissible [0, 1] coefficient range. Do not truncate the interval for reporting; investigate model admissibility, sample instability, item covariance structure, and failed resamples."),
        if (any(incomplete)) tags$p(class = "structural-result-note", "Some resamples failed the same convergence, variance, covariance-matrix, df, and latent-correlation admissibility checks as the main CFA, or yielded unavailable statistics. Interpret intervals cautiously when the valid-replicate count is materially below the requested count."),
        if (any(caution_intervals)) tags$p(class = "structural-result-note", "Caution indicates that 50% to less than 80% of requested resamples yielded the statistic. Treat the percentile limits as unstable and report the valid-replicate count."),
        if (any(unreliable_intervals)) tags$p(class = "structural-result-note", "Unreliable indicates that fewer than 50% of requested resamples yielded the statistic. The displayed quantiles are diagnostic only and should not be reported as a defensible confidence interval; resolve convergence, admissibility, sparse-category, or specification problems first.")
      )
    })
    output[[paste0(prefix, "_result_htmt")]] <- renderUI({
      bundle <- fit_result()
      fit <- bundle$fit
      standardized <- lavaan::standardizedSolution(fit)
      loadings <- standardized[standardized$op == "=~", c("lhs", "rhs"), drop = FALSE]
      loadings <- loadings[loadings$rhs %in% lavaan::lavNames(fit, "ov"), , drop = FALSE]
      factor_names <- unique(loadings$lhs)
      if (length(factor_names) < 2L) return(NULL)
      indicators_by_factor <- stats::setNames(lapply(factor_names, function(name) unique(loadings$rhs[loadings$lhs == name])), factor_names)
      sample_statistics <- lavaan::lavInspect(fit, "sampstat")
      sample_covariance <- sample_statistics$cov %||% NULL
      if (is.null(sample_covariance)) return(tags$p(class = "structural-result-note", "HTMT is not currently displayed for multigroup sample statistics."))
      sample_correlations <- stats::cov2cor(as.matrix(sample_covariance))
      threshold <- as.numeric(bundle$htmt_threshold %||% .85)
      htmt <- structural_canvas_htmt(sample_correlations, indicators_by_factor, threshold)
      matrix_values <- matrix("", nrow = length(factor_names), ncol = length(factor_names) + 1L)
      colnames(matrix_values) <- c("Factor", factor_names)
      for (row in seq_along(factor_names)) {
        matrix_values[row, 1L] <- factor_names[[row]]
        for (column in seq_along(factor_names)) {
          if (row == column) matrix_values[row, column + 1L] <- "—"
          else if (row > column) matrix_values[row, column + 1L] <- format_decimal3(htmt$matrix[row, column])
        }
      }
      pair_table <- htmt$pairs
      pair_table$HTMT <- vapply(pair_table$HTMT, format_decimal3, character(1))
      pair_table$Reason[!nzchar(pair_table$Reason)] <- "—"
      bootstrap_reps <- as.integer(bundle$htmt_bootstrap %||% 0L)
      bootstrap_seed <- as.integer(bundle$htmt_seed %||% 12345L)
      htmt_ci_method <- structural_canvas_bootstrap_ci_method(bundle$htmt_ci_method %||% "percentile")
      htmt_ci_label <- if (identical(htmt_ci_method, "bca")) "BCa" else "percentile"
      bootstrap_table <- NULL
      bootstrap_incomplete <- FALSE
      bootstrap_caution <- FALSE
      bootstrap_unreliable <- FALSE
      bootstrap_bca_unavailable <- FALSE
      if (bootstrap_reps > 0L) {
        bootstrap_table <- bundle$htmt_bootstrap_result %||% NULL
        if (!is.null(bootstrap_table)) {
          bootstrap_incomplete <- any(bootstrap_table[["Valid replicates"]] < bootstrap_reps)
          bootstrap_caution <- any(bootstrap_table$Status == "Caution")
          bootstrap_unreliable <- any(bootstrap_table$Status == "Unreliable")
          bootstrap_bca_unavailable <- "CI method" %in% names(bootstrap_table) && any(bootstrap_table[["CI method"]] == "BCa unavailable")
          names(bootstrap_table)[names(bootstrap_table) == "Lower"] <- "95% CI lower"
          names(bootstrap_table)[names(bootstrap_table) == "Upper"] <- "95% CI upper"
          names(bootstrap_table)[names(bootstrap_table) == "One-sided upper"] <- "One-sided 95% upper"
          bootstrap_table[["95% CI lower"]] <- vapply(bootstrap_table[["95% CI lower"]], format_decimal3, character(1))
          bootstrap_table[["95% CI upper"]] <- vapply(bootstrap_table[["95% CI upper"]], format_decimal3, character(1))
          bootstrap_table[["One-sided 95% upper"]] <- vapply(bootstrap_table[["One-sided 95% upper"]], format_decimal3, character(1))
          bootstrap_table[["Valid %"]] <- paste0(vapply(bootstrap_table[["Valid %"]], format_decimal3, character(1)), "%")
        }
      }
      tagList(
        tags$h5(paste0("HTMT (threshold = ", format(threshold, nsmall = 2L), ")")),
        tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered structural-htmt-matrix",
          tags$thead(tags$tr(lapply(colnames(matrix_values), tags$th))),
          tags$tbody(lapply(seq_len(nrow(matrix_values)), function(index) tags$tr(lapply(as.character(matrix_values[index, ]), tags$td))))
        )),
        tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered structural-htmt-criterion",
          tags$thead(tags$tr(lapply(c("Factor 1", "Factor 2", "HTMT", "Criterion", "Reason"), tags$th))),
          tags$tbody(lapply(seq_len(nrow(pair_table)), function(index) tags$tr(lapply(as.character(pair_table[index, ]), tags$td))))
        )),
        if (!is.null(bootstrap_table)) tagList(
          tags$h5(paste0("HTMT ", htmt_ci_label, " bootstrap confidence intervals (", bootstrap_reps, " resamples; seed = ", bootstrap_seed, ")")),
          tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered structural-htmt-bootstrap",
            tags$thead(tags$tr(lapply(names(bootstrap_table), tags$th))),
            tags$tbody(lapply(seq_len(nrow(bootstrap_table)), function(index) tags$tr(lapply(as.character(bootstrap_table[index, ]), tags$td))))
          ))
        ),
        tags$p(class = "structural-result-note", "HTMT uses absolute item correlations. Values below the selected threshold are marked 'Criterion met'."),
        if (length(bundle$ordered %||% character(0))) tags$p(class = "structural-result-note", "For ordered indicators, HTMT uses lavaan's polychoric latent-response correlations."),
        if (bootstrap_reps <= 0L) tags$p(class = "structural-result-note", "HTMT point estimates are descriptive. Select HTMT bootstrap CI in the analysis options when interval estimates are required."),
        if (bootstrap_reps > 0L && is.null(bootstrap_table)) tags$p(class = "structural-result-note", "HTMT bootstrap confidence intervals could not be estimated from the selected indicators."),
        if (bootstrap_incomplete) tags$p(class = "structural-result-note", "Some bootstrap resamples could not produce an admissible correlation matrix, commonly because an ordered category was absent or sparse. Interpret intervals with reduced valid-replicate counts cautiously."),
        if (bootstrap_bca_unavailable) tags$p(class = "structural-result-note", "BCa unavailable means the bias-correction or jackknife acceleration could not be computed for that pair; increase valid replicates or use percentile CI for reporting."),
        if (bootstrap_caution) tags$p(class = "structural-result-note", "HTMT bootstrap status Caution means that 50% to less than 80% of requested resamples were valid. Treat the interval as unstable and report the valid-replicate count."),
        if (bootstrap_unreliable) tags$p(class = "structural-result-note", "HTMT bootstrap status Unreliable means that fewer than 50% of requested resamples were valid. Confidence limits and threshold decisions are not assessed and should not be used as discriminant-validity evidence."),
        if (!is.null(bootstrap_table)) tags$p(class = "structural-result-note", paste0(
          "The ", htmt_ci_label, " interval is based on case resampling", if (identical(htmt_ci_method, "bca")) " with leave-one-out jackknife acceleration" else "", if (length(bundle$ordered %||% character(0))) " with polychoric correlations re-estimated in each resample" else "",
          ". 'Upper < threshold' uses the one-sided 95% upper confidence limit for the selected .85/.90 criterion. 'Upper < 1' indicates whether the two-sided 95% interval excludes 1. These are intentionally reported separately from the point-estimate criterion."
        ))
      )
    })
    output[[paste0(prefix, "_result_residuals")]] <- renderUI({
      bundle <- fit_result()
      diagnostics <- structural_canvas_residual_diagnostics(bundle$fit)
      if (!isTRUE(diagnostics$available)) return(NULL)
      matrix_table <- function(matrix_value, title) {
        values <- matrix("", nrow(matrix_value), ncol(matrix_value) + 1L)
        colnames(values) <- c("Indicator", colnames(matrix_value))
        for (row_index in seq_len(nrow(matrix_value))) {
          values[row_index, 1L] <- rownames(matrix_value)[[row_index]]
          for (column_index in seq_len(ncol(matrix_value))) {
            value <- matrix_value[row_index, column_index]
            if (is.finite(value)) values[row_index, column_index + 1L] <- format_decimal3(value)
          }
        }
        tagList(tags$h5(title), tags$table(class = "table table-striped table-bordered structural-residual-matrix",
          tags$thead(tags$tr(lapply(colnames(values), tags$th))),
          tags$tbody(lapply(seq_len(nrow(values)), function(index) tags$tr(lapply(as.character(values[index, ]), tags$td))))
        ))
      }
      largest <- diagnostics$largest
      if (nrow(largest)) {
        largest[["Standardized residual"]] <- vapply(largest[["Standardized residual"]], format_decimal3, character(1))
        largest[["Correlation residual"]] <- vapply(largest[["Correlation residual"]], format_decimal3, character(1))
      }
      div(class = "result-section regression-result-panel structural-residual-result",
        h4("5. Local fit diagnostics"),
        matrix_table(diagnostics$standardized, "Standardized residual matrix"),
        matrix_table(diagnostics$correlation, "Correlation residual matrix"),
        tags$h5(paste0("Large standardized residuals (|z| >= ", diagnostics$cutoff, ")")),
        if (!nrow(largest)) tags$p("No residuals exceeded the cutoff.") else tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(largest), tags$th))),
          tags$tbody(lapply(seq_len(nrow(largest)), function(index) tags$tr(lapply(as.character(largest[index, ]), tags$td))))
        ),
        tags$p(class = "structural-result-note", "Large standardized residuals identify local areas of model misfit and should be interpreted with theory rather than used as automatic modification instructions.")
      )
    })
    output[[paste0(prefix, "_result_higher_order")]] <- renderUI({
      bundle <- fit_result()
      higher <- structural_canvas_higher_order_results(bundle$snapshot %||% list(), bundle$fit)
      if (!isTRUE(higher$available)) return(NULL)
      table <- higher$table
      fixed <- !is.na(table$SE) & table$SE == 0 & is.na(table$z) & is.na(table$p)
      residual_abnormal <- !is.finite(table$ResidualVariance) | table$ResidualVariance < 0 | table$ResidualVariance > 1
      residual_display <- paste0(vapply(table$ResidualVariance, format_decimal3, character(1)), ifelse(residual_abnormal, "†", ""))
      r2_interval_abnormal <- !is.finite(table$R2CILower) | !is.finite(table$R2CIUpper) | table$R2CILower < 0 | table$R2CIUpper > 1
      loading_guidance <- vapply(table$Beta, structural_canvas_higher_order_loading_guidance, character(1))
      display <- data.frame(
        `Higher-order factor` = table$HigherOrderFactor,
        `Lower-order factor` = table$LowerOrderFactor,
        B = vapply(table$B, format_decimal3, character(1)),
        `B 95% CI lower` = vapply(table$BCILower, format_decimal3, character(1)),
        `B 95% CI upper` = vapply(table$BCIUpper, format_decimal3, character(1)),
        SE = vapply(table$SE, format_decimal3, character(1)),
        Beta = vapply(table$Beta, format_decimal3, character(1)),
        `β 95% CI lower` = vapply(table$BetaCILower, format_decimal3, character(1)),
        `β 95% CI upper` = vapply(table$BetaCIUpper, format_decimal3, character(1)),
        `R²` = vapply(table$R2, format_decimal3, character(1)),
        `R² 95% CI lower` = paste0(vapply(table$R2CILower, format_decimal3, character(1)), ifelse(r2_interval_abnormal, "†", "")),
        `R² 95% CI upper` = paste0(vapply(table$R2CIUpper, format_decimal3, character(1)), ifelse(r2_interval_abnormal, "†", "")),
        `Residual variance` = residual_display,
        Guidance = ifelse(residual_abnormal | r2_interval_abnormal, "Review residual/R² interval", loading_guidance),
        z = vapply(table$z, format_decimal3, character(1)),
        p = vapply(table$p, format_p, character(1)),
        check.names = FALSE
      )
      display$SE[fixed] <- "Fixed*"
      display$z[fixed] <- "—"
      display$p[fixed] <- "—"
      omega_h <- structural_canvas_omega_h(bundle$snapshot %||% list(), bundle$fit)
      div(class = "result-section regression-result-panel structural-higher-order-result",
        h4("Higher-order CFA results"),
        tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(display), tags$th))),
          tags$tbody(lapply(seq_len(nrow(display)), function(index) tags$tr(lapply(as.character(display[index, ]), tags$td))))
        )),
        if (isTRUE(omega_h$available)) tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(tags$th("Higher-order factor"), tags$th("Indicators"), tags$th("Hierarchical omega (ωh)"), tags$th("Guidance"))),
          tags$tbody(tags$tr(
            tags$td(omega_h$higher_order_factor), tags$td(omega_h$indicators),
            tags$td(paste0(format_decimal3(omega_h$omega_h), if (!is.finite(omega_h$omega_h) || omega_h$omega_h < 0 || omega_h$omega_h > 1) "†" else "")),
            tags$td(structural_canvas_omega_h_guidance(omega_h$omega_h))
          ))
        )) else tags$p(class = "structural-result-note", paste0("Hierarchical omega was not reported: ", omega_h$reason)),
        tags$p(class = "structural-result-note", "Lower-order R² is the variance explained by the higher-order factor. Residual variance is reported on the standardized latent-variable scale."),
        tags$p(class = "structural-result-note", "Lower-order R² intervals complement the standardized residual-variance intervals. † also marks an R² interval extending beyond [0, 1]."),
        tags$p(class = "structural-result-note", "Higher-order standardized-loading confidence intervals are 95% delta-method intervals from lavaan."),
        tags$p(class = "structural-result-note", "B confidence intervals are 95% intervals for unstandardized higher-order loadings; a fixed reference loading has a degenerate interval at its fixed value."),
        tags$p(class = "structural-result-note", "ωh estimates the proportion of unit-weighted total-score variance attributable to one higher-order general factor under the fitted higher-order CFA model."),
        tags$p(class = "structural-result-note", "The .40 loading and .70 ωh values are descriptive review guidelines, not universal pass/fail rules. † marks an unavailable value or a coefficient/residual variance outside [0, 1]."),
        tags$p(class = "structural-result-note", "* Fixed reference loading; SE, z, and p are not estimated.")
      )
    })
    for (kind in c("overview", "validity", "measurement")) local({
      result_kind <- kind
      output[[paste0(prefix, "_result_", result_kind)]] <- renderTable(result_table(result_kind), striped = TRUE, bordered = TRUE, sanitize.text.function = identity)
    })
    output[[paste0(prefix, "_result_mi")]] <- renderUI({
      table <- result_table("mi")
      if (!nrow(table)) return(NULL)
      theory_mi <- identical(fit_result()$mi_mode %||% "theory", "theory")
      tagList(tags$table(
        class = "table table-striped table-bordered structural-mi-table",
        tags$thead(tags$tr(
          lapply(names(table), tags$th),
          if (theory_mi) tags$th("Select")
        )),
        tags$tbody(lapply(seq_len(nrow(table)), function(index) {
          tags$tr(
            lapply(as.character(table[index, , drop = TRUE]), tags$td),
            if (theory_mi) tags$td(actionButton(
              paste0(prefix, "_mi_select_", index),
              "Select",
              class = "btn-sm structural-mi-select-button"
            ))
          )
        }))
      ),
      tags$p(class = "structural-result-note", "MI p treats each modification index as an unscaled asymptotic 1-df chi-square test. BH-adjusted p controls the false-discovery rate across all finite lavaan candidate modifications before the displayed MI and theory filters; MI tests reports that multiplicity-family size."),
      tags$p(class = "structural-result-note", "For MLR or WLSMV, these derived p values are not a separate robust/scaled score-test correction and should be treated as exploratory reference values."),
      tags$p(class = "structural-result-note", "EPC is the expected unstandardized parameter change if the fixed parameter is freed; Std. EPC is lavaan's fully standardized expected change (sepc.all). Consider direction and magnitude rather than MI rank alone."),
      if (theory_mi) tags$p(class = "structural-result-note", "Each Step is sequential: the displayed path is added, the model is refitted, and MI, multiplicity family, EPC, and cumulative fit for the next row are recomputed from that updated model. Rows are not simultaneous candidates from one unchanged model."),
      if (theory_mi) tags$p(class = "structural-result-note", "Skipped unsafe counts higher-ranked candidates rejected for nonconvergence, post.check failure, negative variance, a non-positive-definite or boundary residual/latent/parameter covariance matrix, invalid df, or |latent correlation| >= 1. Skipped details records each rejected path and diagnostic reason; a skipped candidate is not offered for automatic application."),
      tags$p(class = "structural-result-note", "Neither an unadjusted nor adjusted p value justifies a modification. Use effect size (EPC/standardized EPC), residual diagnostics, admissibility, theory, and preferably independent validation."))
    })
    output[[paste0(prefix, "_result_mi_history")]] <- renderUI({
      bundle <- fit_result()
      history <- bundle$mi_history %||% data.frame()
      if (!nrow(history)) return(NULL)
      display <- history[, setdiff(names(history), "Signature"), drop = FALSE]
      for (name in intersect(c("MI", "EPC", "CFI", "TLI", "RMSEA", "SRMR"), names(display))) {
        display[[name]] <- vapply(display[[name]], format_decimal3, character(1))
      }
      display$Justification[!nzchar(display$Justification)] <- "Not provided"
      div(class = "result-section regression-result-panel structural-mi-history-result",
        h4("MI modification history"),
        tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(display), tags$th))),
          tags$tbody(lapply(seq_len(nrow(display)), function(index) tags$tr(lapply(as.character(display[index, ]), tags$td))))
        ),
        tags$p(class = "structural-result-note", "MI, EPC, and cumulative fit values are those available when the path was selected. The justification should document the substantive reason for freeing each parameter."),
        tags$p(class = "structural-result-note", "MI-driven modifications are exploratory and should be cross-validated in an independent sample.")
      )
    })
    output[[paste0(prefix, "_result_mi_holdout")]] <- renderUI({
      bundle <- fit_result()
      if (!isTRUE(bundle$mi_holdout_enabled)) return(NULL)
      comparison <- bundle$holdout_comparison %||% NULL
      if (is.null(comparison)) return(tags$div(class = "result-section regression-result-panel",
        tags$h4("MI holdout validation"),
        tags$p(paste0("Exploration N = ", nrow(bundle$analysis_data), "; reserved validation N = ", nrow(bundle$validation_data), ".")),
        tags$p(class = "structural-result-note", "MI candidates and all currently displayed CFA estimates are based only on the exploration sample. Validation results will appear after an MI path is applied.")
      ))
      table <- comparison$table
      for (name in c("Chisq", "df", "CFI", "TLI", "SRMR", "RMSEA")) table[[name]] <- vapply(table[[name]], format_decimal3, character(1))
      table$p <- vapply(table$p, format_p, character(1))
      changes <- comparison$changes
      for (name in names(changes)[vapply(changes, is.numeric, logical(1)) & names(changes) != "DeltaP"]) changes[[name]] <- vapply(changes[[name]], format_decimal3, character(1))
      changes$DeltaP <- vapply(changes$DeltaP, format_p, character(1))
      div(class = "result-section regression-result-panel structural-mi-holdout-result",
        h4("MI holdout validation"),
        tags$p(paste0("Exploration rows = ", nrow(bundle$analysis_data), "; reserved validation rows = ", comparison$validation_n_raw, "; validation N used = ", paste(unique(comparison$validation_n_used), collapse = ", "), "; split seed = ", bundle$mi_holdout_seed, ".")),
        tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(table), tags$th))),
          tags$tbody(lapply(seq_len(nrow(table)), function(index) tags$tr(lapply(as.character(table[index, ]), tags$td))))
        )),
        tags$h5("Validation-sample change: modified minus original"),
        tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(changes), tags$th))),
          tags$tbody(tags$tr(lapply(as.character(changes[1L, ]), tags$td)))
        )),
        if (any(!comparison$table$Admissible)) tags$p(class = "structural-result-note", "One or both validation-sample models failed the same full admissibility checks as the main CFA. Validation-sample change statistics and the formal difference test are suppressed; the modification must not be treated as replicated."),
        tags$p(class = "structural-result-note", "The MI path was selected only in the exploration sample. The table above refits both models independently in the reserved validation sample. Replication of improved fit supports stability but does not replace substantive justification; failure to replicate indicates likely sample-specific modification."),
        tags$p(class = "structural-result-note", "The validation sample is now unblinded and locked. Further MI changes are disabled for this split; start a new analysis with a newly chosen split seed to evaluate a different modified model.")
      )
    })
    execute_analysis <- function(snapshot, settings = NULL) {
      is_mi_refit <- !is.null(settings) && !is.null(settings$fit)
      settings <- settings %||% list()
      identification <- structural_canvas_identification_diagnostics(snapshot)
      identification_errors <- identification[identification$Severity == "Error", , drop = FALSE]
      if (nrow(identification_errors)) {
        stop(paste0("Model identification check failed: ", paste(paste0(identification_errors$Element, " — ", identification_errors$Message), collapse = "; ")))
      }
      identification_warnings <- identification[identification$Severity == "Warning", , drop = FALSE]
      if (nrow(identification_warnings)) {
        showNotification(paste0("Identification warning: ", paste(paste0(identification_warnings$Element, " — ", identification_warnings$Message), collapse = "; ")), type = "warning", duration = 12)
      }
      estimator <- settings$estimator %||% input[[paste0(prefix, "_estimator")]] %||% "ML"
      missing <- settings$missing %||% input[[paste0(prefix, "_missing")]] %||% "fiml"
      std_lv <- settings$std_lv %||% identical(input[[paste0(prefix, "_scale")]], "variance")
      mi_mode <- settings$mi_mode %||% input[[paste0(prefix, "_mi_mode")]] %||% "theory"
      rmsea_ci <- settings$rmsea_ci %||% as.numeric(input[[paste0(prefix, "_rmsea_ci")]] %||% .90)
      validity_formula <- settings$validity_formula %||% input[[paste0(prefix, "_validity_formula")]] %||% "standardized"
      reliability_bootstrap <- suppressWarnings(as.integer(settings$reliability_bootstrap %||% input[[paste0(prefix, "_reliability_bootstrap")]] %||% 0L))
      if (is.na(reliability_bootstrap) || !reliability_bootstrap %in% c(0L, 500L, 1000L, 2000L)) reliability_bootstrap <- 0L
      reliability_seed <- suppressWarnings(as.integer(settings$reliability_seed %||% input[[paste0(prefix, "_reliability_seed")]] %||% 24680L))
      if (is.na(reliability_seed) || reliability_seed < 1L) reliability_seed <- 24680L
      reliability_ci_method <- structural_canvas_bootstrap_ci_method(settings$reliability_ci_method %||% input[[paste0(prefix, "_reliability_ci_method")]] %||% "percentile")
      bollen_stine_bootstrap <- suppressWarnings(as.integer(settings$bollen_stine_bootstrap %||% input[[paste0(prefix, "_bollen_stine_bootstrap")]] %||% 0L))
      if (is.na(bollen_stine_bootstrap) || !bollen_stine_bootstrap %in% c(0L, 500L, 1000L, 2000L)) bollen_stine_bootstrap <- 0L
      bollen_stine_seed <- suppressWarnings(as.integer(settings$bollen_stine_seed %||% input[[paste0(prefix, "_bollen_stine_seed")]] %||% 97531L))
      if (is.na(bollen_stine_seed) || bollen_stine_seed < 1L) bollen_stine_seed <- 97531L
      htmt_threshold <- as.numeric(settings$htmt_threshold %||% input[[paste0(prefix, "_htmt_threshold")]] %||% .85)
      if (!is.finite(htmt_threshold) || !htmt_threshold %in% c(.85, .90)) htmt_threshold <- .85
      htmt_bootstrap <- suppressWarnings(as.integer(settings$htmt_bootstrap %||% input[[paste0(prefix, "_htmt_bootstrap")]] %||% 0L))
      if (is.na(htmt_bootstrap) || !htmt_bootstrap %in% c(0L, 500L, 1000L, 2000L)) htmt_bootstrap <- 0L
      htmt_seed <- suppressWarnings(as.integer(settings$htmt_seed %||% input[[paste0(prefix, "_htmt_seed")]] %||% 12345L))
      if (is.na(htmt_seed) || htmt_seed < 1L) htmt_seed <- 12345L
      htmt_ci_method <- structural_canvas_bootstrap_ci_method(settings$htmt_ci_method %||% input[[paste0(prefix, "_htmt_ci_method")]] %||% "percentile")
      invariance_enabled <- isTRUE(settings$invariance_enabled %||% input[[paste0(prefix, "_invariance_enabled")]] %||% FALSE)
      invariance_group <- as.character(settings$invariance_group %||% input[[paste0(prefix, "_invariance_group")]] %||% "")
      mi_holdout_enabled <- isTRUE(settings$mi_holdout_enabled %||% input[[paste0(prefix, "_mi_holdout_enabled")]] %||% FALSE)
      mi_holdout_fraction <- as.numeric(settings$mi_holdout_fraction %||% input[[paste0(prefix, "_mi_holdout_fraction")]] %||% .30)
      mi_holdout_seed <- suppressWarnings(as.integer(settings$mi_holdout_seed %||% input[[paste0(prefix, "_mi_holdout_seed")]] %||% 13579L))
      if (is.na(mi_holdout_seed) || mi_holdout_seed < 1L) mi_holdout_seed <- 13579L
      result_coefficient <- settings$result_coefficient %||% input[[paste0(prefix, "_result_coefficient")]] %||% "beta"
      residual_variance_fixes <- settings$residual_variance_fixes %||% numeric(0)
      full_data <- dataset_fn()
      variable_table <- variable_table_fn()
      nominal <- structural_canvas_nominal_indicators(snapshot, variable_table)
      if (length(nominal)) {
        stop(sprintf(
          "Nominal indicators are not supported by standard CFA/SEM: %s. Reclassify them as ordered only when their categories have a meaningful order.",
          paste(nominal, collapse = ", ")
        ))
      }
      ordered <- structural_canvas_ordered_indicators(snapshot, variable_table)
      if (length(ordered) > 0 || identical(toupper(estimator), "WLSMV")) {
        if (toupper(estimator) %in% c("ML", "MLR")) estimator <- "WLSMV"
        if (identical(missing, "fiml")) missing <- "pairwise"
      }
      structural_canvas_validate_holdout_options(
        mi_holdout_enabled, analysis_type, estimator, ordered,
        invariance_enabled, residual_variance_fixes
      )
      if (mi_holdout_enabled) {
        if (!is.null(settings$analysis_data) && !is.null(settings$validation_data)) {
          data <- settings$analysis_data
          validation_data <- settings$validation_data
          holdout_rows <- settings$holdout_rows %||% list()
        } else {
          split <- structural_canvas_holdout_split(full_data, mi_holdout_fraction, mi_holdout_seed)
          data <- split$exploration
          validation_data <- split$validation
          holdout_rows <- list(exploration = split$exploration_rows, validation = split$validation_rows)
        }
      } else {
        data <- full_data
        validation_data <- NULL
        holdout_rows <- list()
      }
      missing_covariances <- structural_canvas_missing_exogenous_covariances(snapshot)
      if (analysis_type %in% c("cfa", "cbsem") && length(missing_covariances)) {
        showNotification(paste0("Missing covariance paths between exogenous latent variables: ", paste(missing_covariances, collapse = ", "), ". These covariances will be fixed to zero."), type = "warning", duration = 10)
      }
      result <- run_structural_canvas_analysis(snapshot, data, analysis_type, estimator = estimator, missing = missing, std_lv = std_lv, ordered = ordered, nominal = nominal, residual_variance_fixes = residual_variance_fixes)
      if (!isTRUE(result$admissible)) {
        details <- c(
          if (!isTRUE(result$converged)) "the model did not converge",
          if (!isTRUE(result$post_check)) "lavaan post-estimation checks failed",
          if (!isTRUE(result$identified)) paste0("model degrees of freedom are invalid (df = ", format_decimal3(result$df), ")"),
          if (length(result$negative_residuals)) paste0("negative residual variances: ", paste(result$negative_residuals, collapse = ", ")),
          if (length(result$negative_latent_variances)) paste0("negative latent variances: ", paste(result$negative_latent_variances, collapse = ", ")),
          if (isTRUE(result$non_psd_theta)) paste0("residual covariance matrix is not positive semidefinite (minimum eigenvalue = ", format_decimal3(result$theta_min_eigenvalue), ")"),
          if (isTRUE(result$non_psd_latent_covariance)) paste0("latent covariance matrix is not positive semidefinite (minimum eigenvalue = ", format_decimal3(result$latent_min_eigenvalue), ")"),
          if (isTRUE(result$near_singular_theta)) paste0("residual covariance matrix is near singular or on the boundary (minimum eigenvalue = ", format_decimal3(result$theta_min_eigenvalue), ")"),
          if (isTRUE(result$near_singular_latent_covariance)) paste0("latent covariance matrix is near singular or on the boundary (minimum eigenvalue = ", format_decimal3(result$latent_min_eigenvalue), ")"),
          if (isTRUE(result$non_psd_parameter_covariance)) paste0("parameter-estimate covariance matrix is not positive semidefinite (minimum eigenvalue = ", format(result$parameter_min_eigenvalue, scientific = TRUE, digits = 3), ")"),
          if (isTRUE(result$near_singular_parameter_covariance)) paste0("parameter-estimate covariance matrix is near singular (minimum eigenvalue = ", format(result$parameter_min_eigenvalue, scientific = TRUE, digits = 3), ")"),
          if (isTRUE(result$invalid_correlations)) "one or more absolute latent correlations are at least 1"
        )
        showNotification(paste0("Potentially inadmissible solution: ", paste(details, collapse = "; "), ". Interpret fit, AVE, CR, and validity results with caution."), type = "error", duration = NULL)
      }
      conditioning_details <- c(
        if (isTRUE(result$ill_conditioned_theta)) paste0("residual covariance condition number = ", format(result$theta_condition_number, scientific = TRUE, digits = 3)),
        if (isTRUE(result$ill_conditioned_latent_covariance)) paste0("latent covariance condition number = ", format(result$latent_condition_number, scientific = TRUE, digits = 3)),
        if (isTRUE(result$ill_conditioned_parameter_covariance)) paste0("parameter-estimate covariance condition number = ", format(result$parameter_condition_number, scientific = TRUE, digits = 3))
      )
      if (length(conditioning_details)) {
        showNotification(paste0("Numerically ill-conditioned solution: ", paste(conditioning_details, collapse = "; "), ". Small data or specification changes may produce unstable estimates."), type = "warning", duration = 12)
      }
      if (identical(analysis_type, "cfa") && bollen_stine_bootstrap > 0L) {
        if (invariance_enabled) stop("Bollen-Stine bootstrap cannot be combined with measurement-invariance analysis; assess global fit within the appropriate group model instead of the pooled CFA.")
        eligibility <- structural_canvas_bollen_stine_eligibility(result$fit)
        if (!isTRUE(eligibility$available)) stop(eligibility$reason)
      }
      invariance_result <- NULL
      if (identical(analysis_type, "cfa") && invariance_enabled) {
        if (!length(ordered) && !toupper(estimator) %in% c("ML", "MLR")) stop("Continuous-indicator measurement invariance requires ML or MLR.")
        if (length(ordered) && !toupper(estimator) %in% c("WLSMV", "DWLS")) stop("Ordered-indicator measurement invariance requires WLSMV or DWLS.")
        if (!nzchar(invariance_group) || !invariance_group %in% names(data)) stop("Select a valid grouping variable for measurement invariance analysis.")
        if (invariance_group %in% lavaan::lavNames(result$fit, "ov")) stop("The grouping variable cannot also be an indicator in the CFA model.")
        group_count <- length(unique(data[[invariance_group]][!is.na(data[[invariance_group]])]))
        if (group_count < 2L || group_count > 20L) stop("The grouping variable must contain between 2 and 20 non-empty groups.")
        invariance_result <- shiny::withProgress(message = "Estimating measurement-invariance models", value = 0, {
          shiny::incProgress(.15, detail = "Configural, metric, scalar, and strict models")
          value <- structural_canvas_measurement_invariance(result$syntax, data, invariance_group, estimator, missing, std_lv, rmsea_ci, ordered)
          shiny::incProgress(.85, detail = "Preparing robust comparisons")
          value
        })
      }
      reliability_bootstrap_result <- NULL
      if (identical(analysis_type, "cfa") && reliability_bootstrap > 0L) {
        structural_canvas_validate_model_based_bootstrap(result$fit, "AVE/reliability bootstrap")
        reliability_bootstrap_result <- shiny::withProgress(message = "Estimating AVE and reliability confidence intervals", value = 0, {
          shiny::incProgress(.05, detail = paste0(reliability_bootstrap, " case-resampling replicates"))
          value <- structural_canvas_reliability_bootstrap(
            result$syntax, data, reps = reliability_bootstrap, confidence = .95, seed = reliability_seed,
            estimator = estimator, missing = missing, std_lv = std_lv, ordered = ordered, formula_mode = validity_formula,
            original_fit = result$fit,
            ci_method = reliability_ci_method,
            progress = function(done, total, valid) {
              shiny::setProgress(
                value = .05 + .90 * (as.numeric(done) / max(1, as.numeric(total))),
                detail = sprintf("Reliability bootstrap %s/%s; valid replicates %s", done, total, valid)
              )
            }
          )
          if (nrow(value)) {
            point <- structural_canvas_reliability_estimates(result$fit, validity_formula)
            statistic_column <- c(AVE = "AVE", CR = "CR", Alpha = "Alpha", Omega = "Omega")
            value$Estimate <- vapply(seq_len(nrow(value)), function(index) {
              row <- point[point$Factor == value$Factor[[index]], , drop = FALSE]
              column <- statistic_column[[value$Statistic[[index]]]]
              if (nrow(row) && column %in% names(row)) as.numeric(row[[column]][[1L]]) else NA_real_
            }, numeric(1))
            value$`Valid %` <- 100 * value[["Valid replicates"]] / value[["Requested replicates"]]
            value <- value[, c("Factor", "Statistic", "Estimate", "Lower", "Upper", "CI method", "Valid replicates", "Requested replicates", "Valid %", "Status"), drop = FALSE]
          }
          shiny::incProgress(.95, detail = paste0("Preparing ", if (identical(reliability_ci_method, "bca")) "BCa" else "percentile", " intervals"))
          value
        })
      }
      bollen_stine_result <- NULL
      if (identical(analysis_type, "cfa") && bollen_stine_bootstrap > 0L) {
        bollen_stine_result <- shiny::withProgress(message = "Estimating Bollen-Stine global-fit p value", value = 0, {
          shiny::incProgress(.05, detail = paste0(bollen_stine_bootstrap, " transformed-data bootstrap replicates"))
          value <- structural_canvas_bollen_stine(result$fit, bollen_stine_bootstrap, bollen_stine_seed)
          shiny::incProgress(.95, detail = "Preparing bootstrap goodness-of-fit result")
          value
        })
      }
      mi <- if (analysis_type %in% c("cfa", "cbsem")) structural_canvas_mi_refits(snapshot, result, data, analysis_type, estimator, missing, std_lv, mode = mi_mode, ordered = ordered) else NULL
      htmt_bootstrap_result <- NULL
      if (analysis_type %in% c("cfa", "cbsem") && htmt_bootstrap > 0L) {
        standardized_for_htmt <- lavaan::standardizedSolution(result$fit)
        observed_for_htmt <- lavaan::lavNames(result$fit, "ov")
        loadings_for_htmt <- standardized_for_htmt[
          standardized_for_htmt$op == "=~" & standardized_for_htmt$rhs %in% observed_for_htmt,
          c("lhs", "rhs"), drop = FALSE
        ]
        factor_names_for_htmt <- unique(loadings_for_htmt$lhs)
        if (length(factor_names_for_htmt) >= 2L) {
          indicators_for_htmt <- stats::setNames(lapply(factor_names_for_htmt, function(name) {
            unique(loadings_for_htmt$rhs[loadings_for_htmt$lhs == name])
          }), factor_names_for_htmt)
          htmt_bootstrap_result <- shiny::withProgress(
            message = "Estimating HTMT bootstrap confidence intervals",
            value = 0,
            {
              shiny::incProgress(.1, detail = paste0(htmt_bootstrap, " case-resampling replicates"))
              value <- structural_canvas_htmt_bootstrap(
                data, indicators_for_htmt, reps = htmt_bootstrap, confidence = .95,
                seed = htmt_seed, ordered = ordered, threshold = htmt_threshold,
                ci_method = htmt_ci_method,
                progress = function(done, total, valid) {
                  shiny::setProgress(
                    value = .10 + .80 * (as.numeric(done) / max(1, as.numeric(total))),
                    detail = sprintf("HTMT bootstrap %s/%s; valid replicates %s", done, total, valid)
                  )
                }
              )
              shiny::incProgress(.9, detail = "Preparing interval estimates")
              value
            }
          )
        }
      }
      baseline_fit <- if (is_mi_refit) settings$baseline_fit %||% settings$fit else result$fit
      baseline_diagnostics <- if (is_mi_refit) settings$baseline_diagnostics %||% settings$diagnostics else result
      baseline_syntax <- if (is_mi_refit) settings$baseline_syntax %||% settings$syntax else result$syntax
      holdout_comparison <- NULL
      if (mi_holdout_enabled && identical(settings$comparison_type %||% "", "mi") && !is.null(validation_data)) {
        holdout_comparison <- structural_canvas_holdout_model_comparison(
          baseline_syntax, result$syntax, validation_data,
          estimator = estimator, missing = missing, std_lv = std_lv, ci_level = rmsea_ci
        )
      }
      fit_result(list(
        fit = result$fit, syntax = result$syntax, snapshot = snapshot, mi = mi, mi_mode = mi_mode,
        rmsea_ci = rmsea_ci, validity_formula = validity_formula,
        reliability_bootstrap = reliability_bootstrap, reliability_seed = reliability_seed,
        reliability_ci_method = reliability_ci_method,
        reliability_bootstrap_result = reliability_bootstrap_result,
        bollen_stine_bootstrap = bollen_stine_bootstrap, bollen_stine_seed = bollen_stine_seed,
        bollen_stine_result = bollen_stine_result,
        htmt_threshold = htmt_threshold, htmt_bootstrap = htmt_bootstrap, htmt_seed = htmt_seed,
        htmt_ci_method = htmt_ci_method,
        htmt_bootstrap_result = htmt_bootstrap_result,
        invariance_enabled = invariance_enabled, invariance_group = invariance_group, invariance_result = invariance_result,
        mi_holdout_enabled = mi_holdout_enabled, mi_holdout_fraction = mi_holdout_fraction, mi_holdout_seed = mi_holdout_seed,
        analysis_data = data, validation_data = validation_data, holdout_rows = holdout_rows, holdout_comparison = holdout_comparison,
        estimator = estimator, missing = missing, std_lv = std_lv, ordered = ordered,
        result_coefficient = result_coefficient, diagnostics = result,
        baseline_fit = baseline_fit, modified_from_baseline = is_mi_refit || isTRUE(settings$modified_from_baseline),
        baseline_syntax = baseline_syntax,
        baseline_diagnostics = baseline_diagnostics,
        comparison_label = settings$comparison_label %||% NULL,
        comparison_type = settings$comparison_type %||% NULL,
        residual_variance_fixes = residual_variance_fixes,
        identification = identification,
        mi_history = settings$mi_history %||% data.frame()
      ))
      session$sendCustomMessage(
        "custom-model-canvas-result",
        list(
          rootId = paste0(prefix, "-canvas-root"),
          source = snapshot,
          result = structural_canvas_result_snapshot(snapshot, result$fit, result_coefficient),
          show = TRUE
        )
      )
      result
    }
    run_confirmed_analysis <- function(snapshot, settings = list()) {
      result <- execute_analysis(snapshot, settings)
      showNotification(
        if (identical(statedu_current_language(app_language_fn), "ko")) paste0(structural_analysis_title(analysis_type, statedu_current_language(app_language_fn)), " 遺꾩꽍???꾨즺?섏뿀?듬땲??") else paste0(structural_analysis_title(analysis_type, statedu_current_language(app_language_fn)), " analysis completed."),
        type = if (isTRUE(result$converged)) "message" else "warning"
      )
      result
    }
    observeEvent(input[[canvas_input]], mark_settings_dirty(), ignoreInit = TRUE)
    observeEvent(input[[confirm_input]], {
      package <- structural_analysis_package(analysis_type)
      if (!requireNamespace(package, quietly = TRUE)) {
        showNotification(sprintf("%s package is required.", package), type = "error")
      } else {
        tryCatch({
          snapshot <- input[[confirm_input]]
          recommendation <- structural_canvas_estimator_recommendation(
            snapshot, dataset_fn(), variable_table_fn(), analysis_type,
            input[[paste0(prefix, "_estimator")]] %||% "ML"
          )
          if (isTRUE(recommendation$recommend)) {
            pending_estimator_snapshot(snapshot)
            diagnosis <- recommendation$diagnosis
            bollen_requested <- identical(analysis_type, "cfa") && as.integer(input[[paste0(prefix, "_bollen_stine_bootstrap")]] %||% 0L) > 0L
            showModal(modalDialog(
              title = "Estimator recommendation",
              tags$p("Mardia diagnostics flagged nonnormal continuous indicators. Robust MLR is recommended before fitting this model."),
              tags$p(paste0(
                "Mardia skewness p = ", format_p(diagnosis$skew_p),
                "; kurtosis p = ", format_p(diagnosis$kurtosis_p),
                "; complete cases = ", diagnosis$n, " of ", diagnosis$original_n, "."
              )),
              if (bollen_requested) tags$p(class = "structural-result-note", "Bollen-Stine bootstrap is available only for ML; choosing MLR will run the model without Bollen-Stine bootstrap."),
              footer = tagList(
                modalButton("Cancel"),
                actionButton(paste0(prefix, "_run_with_ml"), "Run with ML", class = "btn-default"),
                actionButton(paste0(prefix, "_run_with_mlr"), "Run with MLR", class = "btn-primary")
              ),
              easyClose = TRUE
            ))
            return()
          }
          run_confirmed_analysis(snapshot)
          return()
          result <- execute_analysis(snapshot)
          showNotification(
            if (identical(statedu_current_language(app_language_fn), "ko")) paste0(structural_analysis_title(analysis_type, statedu_current_language(app_language_fn)), " 분석이 완료되었습니다.") else paste0(structural_analysis_title(analysis_type, statedu_current_language(app_language_fn)), " analysis completed."),
            type = if (isTRUE(result$converged)) "message" else "warning"
          )
        }, error = function(error) {
          showNotification(conditionMessage(error), type = "error", duration = 8)
        })
      }
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0(prefix, "_run_with_ml")]], {
      snapshot <- pending_estimator_snapshot()
      removeModal()
      shiny::req(!is.null(snapshot))
      tryCatch({
        run_confirmed_analysis(snapshot, list(estimator = "ML"))
      }, error = function(error) {
        showNotification(conditionMessage(error), type = "error", duration = 8)
      })
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0(prefix, "_run_with_mlr")]], {
      snapshot <- pending_estimator_snapshot()
      removeModal()
      shiny::req(!is.null(snapshot))
      tryCatch({
        settings <- list(estimator = "MLR")
        if (identical(analysis_type, "cfa")) settings$bollen_stine_bootstrap <- 0L
        run_confirmed_analysis(snapshot, settings)
      }, error = function(error) {
        showNotification(conditionMessage(error), type = "error", duration = 8)
      })
    }, ignoreInit = TRUE)
    lapply(seq_len(100L), function(index) local({
      row_index <- index
      observeEvent(input[[paste0(prefix, "_mi_select_", row_index)]], {
        bundle <- fit_result()
        shiny::req(!is.null(bundle), !is.null(bundle$mi), nrow(bundle$mi) >= row_index)
        reuse_error <- tryCatch({
          structural_canvas_validate_holdout_reuse(bundle$mi_holdout_enabled, !is.null(bundle$holdout_comparison))
          NULL
        }, error = identity)
        if (!is.null(reuse_error)) {
          showNotification(conditionMessage(reuse_error), type = "error", duration = 12)
          return()
        }
        selected_rows <- if (identical(bundle$mi_mode %||% "theory", "theory")) seq_len(row_index) else row_index
        existing <- bundle$mi_history %||% data.frame()
        existing_signatures <- if (nrow(existing)) existing$Signature else character(0)
        selected_rows <- selected_rows[!vapply(selected_rows, function(index) structural_canvas_mi_signature(bundle$mi$lhs[[index]], bundle$mi$op[[index]], bundle$mi$rhs[[index]]) %in% existing_signatures, logical(1))]
        if (!length(selected_rows)) {
          showNotification("All selected MI paths have already been applied.", type = "warning")
          return()
        }
        pending_mi_rows(selected_rows)
        parameters <- vapply(selected_rows, function(index) paste(bundle$mi$lhs[[index]], bundle$mi$op[[index]], bundle$mi$rhs[[index]]), character(1))
        showModal(modalDialog(
          title = "Document MI modification",
          tags$p(paste0("Paths to add: ", paste(parameters, collapse = ", "))),
          textAreaInput(paste0(prefix, "_mi_justification"), "Substantive justification", rows = 4, placeholder = "Explain why freeing these parameters is theoretically defensible."),
          footer = tagList(modalButton("Cancel"), actionButton(paste0(prefix, "_mi_confirm_apply"), "Apply and reanalyze", class = "btn-primary")),
          easyClose = TRUE
        ))
      }, ignoreInit = TRUE)
    }))
    observeEvent(input[[paste0(prefix, "_mi_confirm_apply")]], {
      bundle <- fit_result()
      selected_rows <- pending_mi_rows()
      shiny::req(!is.null(bundle), length(selected_rows))
      tryCatch({
        structural_canvas_validate_holdout_reuse(bundle$mi_holdout_enabled, !is.null(bundle$holdout_comparison))
        snapshot <- bundle$snapshot
        for (selected_row in selected_rows) snapshot <- structural_canvas_apply_mi(snapshot, bundle$mi[selected_row, , drop = FALSE])
        settings <- bundle
        settings$comparison_type <- "mi"
        settings$comparison_label <- "Modified model"
        settings$mi_history <- structural_canvas_mi_history_rows(bundle$mi, selected_rows, bundle$mi_history %||% data.frame(), input[[paste0(prefix, "_mi_justification")]] %||% "")
        removeModal()
        pending_mi_rows(integer(0))
        result <- execute_analysis(snapshot, settings)
        showNotification("The selected MI paths were added, documented, and reanalyzed.", type = if (isTRUE(result$converged)) "message" else "warning")
      }, error = function(error) showNotification(conditionMessage(error), type = "error", duration = 8))
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0(prefix, "_heywood_refit")]], {
      showModal(modalDialog(
        title = "Heywood-constrained reanalysis",
        tags$p("Fix each negative residual variance to a small positive percentage of that variable's observed variance."),
        numericInput(paste0(prefix, "_heywood_percent"), "Observed-variance percentage", value = 0.1, min = 0.01, max = 5, step = 0.01),
        tags$p(class = "structural-result-note", "Recommended starting value: 0.1%. This is a sensitivity analysis, not an automatic correction of model misspecification."),
        footer = tagList(modalButton("Cancel"), actionButton(paste0(prefix, "_heywood_confirm"), "Run constrained model", class = "btn-warning")),
        easyClose = TRUE
      ))
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0(prefix, "_heywood_confirm")]], {
      bundle <- fit_result()
      shiny::req(!is.null(bundle))
      tryCatch({
        if (!toupper(as.character(bundle$estimator %||% "ML")) %in% c("ML", "MLR") || length(bundle$ordered %||% character(0))) {
          stop("Heywood-constrained reanalysis is available only for continuous indicators estimated with ML or MLR.")
        }
        variables <- as.character((bundle$baseline_diagnostics %||% bundle$diagnostics)$negative_residuals %||% character(0))
        if (!length(variables)) stop("No negative residual variances were found in the original model.")
        percent <- as.numeric(input[[paste0(prefix, "_heywood_percent")]] %||% 0.1)
        if (!is.finite(percent) || percent < 0.01 || percent > 5) stop("Enter a percentage between 0.01 and 5.")
        data <- dataset_fn()
        observed_variances <- vapply(variables, function(name) stats::var(data[[name]], na.rm = TRUE), numeric(1))
        if (any(!is.finite(observed_variances) | observed_variances <= 0)) stop("A positive observed variance is required for every Heywood indicator.")
        fixes <- observed_variances * percent / 100
        names(fixes) <- variables
        settings <- bundle
        settings$residual_variance_fixes <- fixes
        settings$comparison_label <- "Heywood-constrained model"
        settings$comparison_type <- "heywood"
        removeModal()
        result <- execute_analysis(bundle$snapshot, settings)
        showNotification(paste0("The constrained model fixed ", paste(variables, collapse = ", "), " to ", format(percent, trim = TRUE), "% of observed variance."), type = if (isTRUE(result$admissible)) "message" else "warning", duration = 10)
      }, error = function(error) {
        showNotification(conditionMessage(error), type = "error", duration = 10)
      })
    }, ignoreInit = TRUE)
    observeEvent(input[[advanced_input]], {
      request <- input[[advanced_input]] %||% list()
      action <- as.character(request$action %||% "")
      candidates <- as.character(selected_names_fn() %||% character(0))
      ko <- identical(statedu_current_language(app_language_fn), "ko")
      if (identical(action, "multiGroup")) {
        showModal(modalDialog(
          title = if (ko) "다집단 분석 설정" else "Multigroup Analysis",
          selectInput(paste0(prefix, "_group_variable"), if (ko) "집단변수" else "Grouping variable", choices = candidates),
          helpText(if (ko) "집단 간 측정모형과 구조경로의 차이를 검정합니다." else "Compare measurement models and structural paths across groups."),
          footer = modalButton(if (ko) "닫기" else "Close"),
          easyClose = TRUE
        ))
      } else if (identical(action, "moderator")) {
        showModal(modalDialog(
          title = if (ko) "조절효과 설정" else "Moderation Settings",
          selectInput(paste0(prefix, "_moderator_variable"), if (ko) "조절변수" else "Moderator", choices = candidates),
          selectInput(paste0(prefix, "_moderated_predictor"), if (ko) "독립변수" else "Predictor", choices = candidates),
          selectInput(paste0(prefix, "_moderated_outcome"), if (ko) "종속변수" else "Outcome", choices = candidates),
          helpText(if (ko) "선택한 경로에 조절효과를 지정합니다." else "Assign a moderation effect to the selected path."),
          footer = modalButton(if (ko) "닫기" else "Close"),
          easyClose = TRUE
        ))
      }
    }, ignoreInit = TRUE)
    register_analysis_data_viewer_handlers(
      input = input,
      output = output,
      prefix = prefix,
      title = paste(structural_analysis_title(analysis_type, statedu_current_language(app_language_fn)), if (identical(statedu_current_language(app_language_fn), "ko")) "데이터 보기" else "Data Viewer"),
      dataset_fn = dataset_fn,
      selected_names_fn = selected_names_fn,
      variables_fn = local({
        state_input <- canvas_input
        function() custom_model_canvas_viewer_variables(input[[state_input]] %||% list())
      }),
      variable_table_fn = variable_table_fn,
      labels_fn = labels_fn,
      category_table_fn = category_table_fn,
      language_fn = app_language_fn
    )
  })
  invisible(TRUE)
}

custom_model_canvas_records <- function(value) {
  if (is.null(value)) {
    return(list())
  }
  if (is.data.frame(value)) {
    return(lapply(seq_len(nrow(value)), function(index) as.list(value[index, , drop = FALSE])))
  }
  if (is.list(value) && length(value) > 0L) {
    return(Filter(is.list, value))
  }
  list()
}

custom_model_canvas_record_value <- function(record, key, default = "") {
  value <- record[[key]] %||% default
  if (length(value) == 0L || is.null(value) || is.na(value[[1]])) {
    return(default)
  }
  as.character(value[[1]])
}

custom_model_canvas_numeric_value <- function(value, default = 0) {
  value <- value %||% default
  if (is.list(value)) {
    value <- unlist(value, recursive = TRUE, use.names = FALSE)
  }
  value <- suppressWarnings(as.numeric(value))
  if (length(value) == 0L || is.na(value[[1]])) {
    return(default)
  }
  value[[1]]
}

custom_model_canvas_record_number <- function(record, key, default = 0) {
  custom_model_canvas_numeric_value(record[[key]], default)
}

custom_model_canvas_node_variable <- function(node) {
  custom_model_canvas_record_value(node, "variableId", custom_model_canvas_record_value(node, "name", ""))
}

custom_model_canvas_viewer_variables <- function(snapshot) {
  nodes <- custom_model_canvas_records(snapshot$nodes)
  node_variables <- vapply(nodes, custom_model_canvas_node_variable, character(1))
  covariates <- as.character(snapshot$covariates %||% character(0))
  unique(c(node_variables[nzchar(node_variables)], covariates[nzchar(covariates)]))
}

custom_model_canvas_order_nodes <- function(nodes, primary = "y") {
  if (length(nodes) == 0L) {
    return(nodes)
  }
  x <- vapply(nodes, custom_model_canvas_record_number, numeric(1), key = "x")
  y <- vapply(nodes, custom_model_canvas_record_number, numeric(1), key = "y")
  order_index <- if (identical(primary, "x")) order(x, y) else order(y, x)
  nodes[order_index]
}

custom_model_canvas_order_variables <- function(values, selected_names = character(0)) {
  values <- as.character(values %||% character(0))
  values <- values[nzchar(values)]
  if (length(values) == 0L) {
    return(values)
  }
  selected_names <- as.character(selected_names %||% character(0))
  rank <- match(values, selected_names)
  rank[is.na(rank)] <- length(selected_names) + seq_len(sum(is.na(rank)))
  values[order(rank, seq_along(values))]
}

custom_model_canvas_snapshot_spec <- function(snapshot, selected_names, language = statedu_initial_language(), two_moderator_model = "3") {
  selected_names <- as.character(selected_names %||% character(0))
  nodes <- custom_model_canvas_records(snapshot$nodes)
  edges <- custom_model_canvas_records(snapshot$edges)
  moderations <- custom_model_canvas_records(snapshot$moderations)
  covariates <- unique(as.character(snapshot$covariates %||% character(0)))
  covariates <- covariates[nzchar(covariates)]
  covariates <- custom_model_canvas_order_variables(covariates, selected_names)

  node_id <- vapply(nodes, custom_model_canvas_record_value, character(1), key = "id")
  names(nodes) <- node_id
  node_role <- function(node) custom_model_canvas_record_value(node, "role", "independent")
  nodes_by_role <- function(role) {
    Filter(function(node) identical(node_role(node), role), nodes)
  }
  edge_node <- function(edge, endpoint) {
    id <- custom_model_canvas_record_value(edge, endpoint)
    nodes[[id]] %||% NULL
  }
  edge_moderated_path <- function(edge) {
    from <- edge_node(edge, "from")
    to <- edge_node(edge, "to")
    if (is.null(from) || is.null(to)) {
      return(NA_character_)
    }
    from_role <- node_role(from)
    to_role <- node_role(to)
    if (identical(from_role, "independent") && identical(to_role, "mediator")) {
      return("xm")
    }
    if (identical(from_role, "mediator") && identical(to_role, "dependent")) {
      return("my")
    }
    if (identical(from_role, "independent") && identical(to_role, "dependent")) {
      return("xy")
    }
    NA_character_
  }
  directed_x_to_y_pairs <- lapply(edges, function(edge) {
    from <- edge_node(edge, "from")
    to <- edge_node(edge, "to")
    if (is.null(from) || is.null(to)) {
      return(NULL)
    }
    if (identical(node_role(from), "independent") && identical(node_role(to), "dependent")) {
      return(c(
        y = custom_model_canvas_node_variable(to),
        x = custom_model_canvas_node_variable(from)
      ))
    }
    NULL
  })
  directed_x_to_y_pairs <- Filter(Negate(is.null), directed_x_to_y_pairs)
  directed_x_to_m_pairs <- lapply(edges, function(edge) {
    from <- edge_node(edge, "from")
    to <- edge_node(edge, "to")
    if (is.null(from) || is.null(to)) {
      return(NULL)
    }
    if (identical(node_role(from), "independent") && identical(node_role(to), "mediator")) {
      return(c(
        x = custom_model_canvas_node_variable(from),
        mediator = custom_model_canvas_node_variable(to)
      ))
    }
    NULL
  })
  directed_x_to_m_pairs <- Filter(Negate(is.null), directed_x_to_m_pairs)
  directed_m_to_y_pairs <- lapply(edges, function(edge) {
    from <- edge_node(edge, "from")
    to <- edge_node(edge, "to")
    if (is.null(from) || is.null(to)) {
      return(NULL)
    }
    if (identical(node_role(from), "mediator") && identical(node_role(to), "dependent")) {
      return(c(
        y = custom_model_canvas_node_variable(to),
        mediator = custom_model_canvas_node_variable(from)
      ))
    }
    NULL
  })
  directed_m_to_y_pairs <- Filter(Negate(is.null), directed_m_to_y_pairs)
  directed_m_to_m_pairs <- lapply(edges, function(edge) {
    from <- edge_node(edge, "from")
    to <- edge_node(edge, "to")
    if (is.null(from) || is.null(to)) {
      return(NULL)
    }
    if (identical(node_role(from), "mediator") && identical(node_role(to), "mediator")) {
      return(c(
        from = custom_model_canvas_node_variable(from),
        to = custom_model_canvas_node_variable(to)
      ))
    }
    NULL
  })
  directed_m_to_m_pairs <- Filter(Negate(is.null), directed_m_to_m_pairs)

  mediator_nodes <- nodes_by_role("mediator")
  has_mediator_chain <- any(vapply(edges, function(edge) {
    from <- edge_node(edge, "from")
    to <- edge_node(edge, "to")
    !is.null(from) && !is.null(to) &&
      identical(node_role(from), "mediator") &&
      identical(node_role(to), "mediator")
  }, logical(1)))
  mediator_arrangement <- if (length(mediator_nodes) >= 2L && isTRUE(has_mediator_chain)) "serial" else "parallel"

  roles <- list(
    y = vapply(custom_model_canvas_order_nodes(nodes_by_role("dependent"), "y"), custom_model_canvas_node_variable, character(1)),
    x = vapply(custom_model_canvas_order_nodes(nodes_by_role("independent"), "y"), custom_model_canvas_node_variable, character(1)),
    mediators = vapply(
      custom_model_canvas_order_nodes(mediator_nodes, if (identical(mediator_arrangement, "serial")) "x" else "y"),
      custom_model_canvas_node_variable,
      character(1)
    ),
    w = vapply(custom_model_canvas_order_nodes(nodes_by_role("moderator"), "y"), custom_model_canvas_node_variable, character(1)),
    covariates = covariates
  )
  roles$x <- custom_model_canvas_order_variables(roles$x, selected_names)
  roles$covariates <- custom_model_canvas_order_variables(roles$covariates, selected_names)
  roles <- mediation_moderation_role_values(
    y = roles$y,
    x = roles$x,
    mediators = roles$mediators,
    w = roles$w,
    covariates = roles$covariates,
    selected_names = selected_names
  )
  x_to_m <- stats::setNames(lapply(roles$mediators, function(mediator) character(0)), roles$mediators)
  for (pair in directed_x_to_m_pairs) {
    mediator <- as.character(pair[["mediator"]] %||% "")
    x_value <- as.character(pair[["x"]] %||% "")
    if (nzchar(mediator) && nzchar(x_value) && mediator %in% names(x_to_m) && x_value %in% roles$x) {
      x_to_m[[mediator]] <- unique(c(x_to_m[[mediator]], x_value))
    }
  }

  edge_by_id <- stats::setNames(edges, vapply(edges, custom_model_canvas_record_value, character(1), key = "id"))
  moderated_paths <- unique(vapply(moderations, function(moderation) {
    source <- nodes[[custom_model_canvas_record_value(moderation, "from")]] %||% NULL
    if (is.null(source) || !identical(node_role(source), "moderator")) {
      return(NA_character_)
    }
    target_edge <- edge_by_id[[custom_model_canvas_record_value(moderation, "toEdge")]] %||% NULL
    if (is.null(target_edge)) {
      return(NA_character_)
    }
    edge_moderated_path(target_edge)
  }, character(1)))
  moderated_paths <- intersect(moderated_paths[!is.na(moderated_paths)], c("xm", "my", "xy"))
  linked_moderators <- unique(vapply(moderations, function(moderation) {
    source <- nodes[[custom_model_canvas_record_value(moderation, "from")]] %||% NULL
    if (is.null(source) || !identical(node_role(source), "moderator")) {
      return(NA_character_)
    }
    target_edge <- edge_by_id[[custom_model_canvas_record_value(moderation, "toEdge")]] %||% NULL
    if (is.null(target_edge) || is.na(edge_moderated_path(target_edge))) {
      return(NA_character_)
    }
    custom_model_canvas_node_variable(source)
  }, character(1)))
  linked_moderators <- linked_moderators[!is.na(linked_moderators) & nzchar(linked_moderators)]
  roles$w <- intersect(roles$w, linked_moderators)
  moderation_map_rows <- list()
  moderated_x_to_m <- stats::setNames(lapply(roles$mediators, function(mediator) character(0)), roles$mediators)
  moderated_m_to_y <- character(0)
  for (moderation in moderations) {
    source <- nodes[[custom_model_canvas_record_value(moderation, "from")]] %||% NULL
    if (is.null(source) || !identical(node_role(source), "moderator")) next
    target_edge <- edge_by_id[[custom_model_canvas_record_value(moderation, "toEdge")]] %||% NULL
    if (is.null(target_edge)) next
    moderated_path <- edge_moderated_path(target_edge)
    from <- edge_node(target_edge, "from")
    to <- edge_node(target_edge, "to")
    if (is.null(from) || is.null(to)) next
    moderator <- custom_model_canvas_node_variable(source)
    if (identical(moderated_path, "xm")) {
      x_value <- custom_model_canvas_node_variable(from)
      mediator <- custom_model_canvas_node_variable(to)
      if (nzchar(x_value) && nzchar(mediator) && mediator %in% names(moderated_x_to_m) && x_value %in% roles$x) {
        moderated_x_to_m[[mediator]] <- unique(c(moderated_x_to_m[[mediator]], x_value))
        moderation_map_rows[[length(moderation_map_rows) + 1L]] <- data.frame(
          path_type = "xm",
          moderator = moderator,
          x = x_value,
          mediator = mediator,
          y = "",
          stringsAsFactors = FALSE
        )
      }
    } else if (identical(moderated_path, "my")) {
      mediator <- custom_model_canvas_node_variable(from)
      outcome <- custom_model_canvas_node_variable(to)
      if (nzchar(mediator) && mediator %in% roles$mediators) {
        moderated_m_to_y <- unique(c(moderated_m_to_y, mediator))
        moderation_map_rows[[length(moderation_map_rows) + 1L]] <- data.frame(
          path_type = "my",
          moderator = moderator,
          x = "",
          mediator = mediator,
          y = outcome,
          stringsAsFactors = FALSE
        )
      }
    } else if (identical(moderated_path, "xy")) {
      x_value <- custom_model_canvas_node_variable(from)
      outcome <- custom_model_canvas_node_variable(to)
      if (nzchar(x_value) && x_value %in% roles$x) {
        moderation_map_rows[[length(moderation_map_rows) + 1L]] <- data.frame(
          path_type = "xy",
          moderator = moderator,
          x = x_value,
          mediator = "",
          y = outcome,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  direct_x_to_y <- mediation_moderation_normalize_outcome_map(
    analysis_bind_rows(lapply(directed_x_to_y_pairs, function(pair) {
      data.frame(
        y = as.character(pair[["y"]] %||% ""),
        x = as.character(pair[["x"]] %||% ""),
        stringsAsFactors = FALSE
      )
    })),
    outcomes = roles$y,
    allowed = roles$x,
    default = character(0)
  )
  m_to_y <- mediation_moderation_normalize_outcome_map(
    analysis_bind_rows(lapply(directed_m_to_y_pairs, function(pair) {
      data.frame(
        y = as.character(pair[["y"]] %||% ""),
        mediator = as.character(pair[["mediator"]] %||% ""),
        stringsAsFactors = FALSE
      )
    })),
    outcomes = roles$y,
    allowed = roles$mediators,
    default = character(0)
  )
  m_to_m <- mediation_moderation_normalize_mediator_map(
    analysis_bind_rows(lapply(directed_m_to_m_pairs, function(pair) {
      data.frame(
        from = as.character(pair[["from"]] %||% ""),
        to = as.character(pair[["to"]] %||% ""),
        stringsAsFactors = FALSE
      )
    })),
    mediators = roles$mediators,
    default = character(0)
  )
  moderation_map <- mediation_moderation_normalize_moderation_map(
    analysis_bind_rows(moderation_map_rows),
    roles
  )

  structure <- mediation_moderation_structure_from_mediators(roles$mediators, mediator_arrangement)
  model <- mediation_moderation_infer_model(
    structure,
    moderated_paths,
    moderator_count = length(roles$w),
    two_moderator_model = two_moderator_model
  )
  if (is.na(model) || !model %in% mediation_moderation_models()) {
    model <- "custom"
  }
  list(
    roles = roles,
    mediator_arrangement = mediator_arrangement,
    moderated_paths = moderated_paths,
    direct_x = direct_x_to_y,
    direct_x_to_y = direct_x_to_y,
    x_to_m = x_to_m,
    m_to_y = m_to_y,
    m_to_m = m_to_m,
    moderated_x_to_m = moderated_x_to_m,
    moderated_m_to_y = moderated_m_to_y,
    moderation_map = moderation_map,
    model = model
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

register_custom_model_canvas_handlers <- function(
  input,
  output,
  session,
  dataset_fn,
  selected_names_fn,
  variable_table_fn,
  labels_fn,
  category_table_fn = function() NULL,
  mark_settings_dirty,
  app_language_fn = NULL
) {
  custom_model_canvas_snapshot <- reactiveVal(NULL)
  custom_model_canvas_pending_snapshot <- reactiveVal(NULL)

  output$custom_model_canvas_setup <- renderUI({
    custom_model_canvas_workspace(
      selected_names = selected_names_fn(),
      variable_table = variable_table_fn(),
      labels = labels_fn(),
      input = input,
      language = statedu_current_language(app_language_fn)
    )
  })

  observeEvent(input$custom_model_canvas_state, {
    custom_model_canvas_snapshot(input$custom_model_canvas_state)
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  lapply(c("custom_mm_analysis_method", "custom_mm_residual_diagnostics", "custom_mm_auto_method", "custom_mm_effect_size_y", "custom_mm_effect_size_m", "custom_mm_covariate_control_y", "custom_mm_covariate_control_m", "custom_mm_boot_r", "custom_mm_seed", "custom_mm_ci_method", "custom_mm_options_tab", "custom_mm_output_table_style"), function(input_id) {
    observeEvent(input[[input_id]], {
      mark_settings_dirty()
    }, ignoreInit = TRUE)
  })
  observeEvent(input$custom_mm_residual_diagnostics, {
    if (!isTRUE(input$custom_mm_residual_diagnostics)) {
      updateCheckboxInput(session, "custom_mm_auto_method", value = FALSE)
    }
  }, ignoreInit = TRUE)

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = "custom_model_canvas",
    title = custom_model_canvas_text(statedu_current_language(app_language_fn), "Custom Model Canvas Data Viewer", "\uc0ac\uc6a9\uc790 \ubaa8\ub378 \ub370\uc774\ud130 \ubcf4\uae30"),
    dataset_fn = dataset_fn,
    selected_names_fn = selected_names_fn,
    variables_fn = function() custom_model_canvas_viewer_variables(custom_model_canvas_snapshot() %||% list()),
    variable_table_fn = variable_table_fn,
    labels_fn = labels_fn,
    category_table_fn = category_table_fn,
    language_fn = app_language_fn
  )

  custom_model_canvas_result <- reactiveVal(NULL)
  output$custom_model_canvas_results <- renderUI({
    mediation_moderation_result_ui(
      custom_model_canvas_result(),
      statedu_current_language(app_language_fn),
      dash_nonsignificant = TRUE,
      output_table_style = analysis_output_table_style(input$custom_mm_output_table_style)
    )
  })
  output$custom_model_canvas_save_control <- renderUI({
    if (is.null(custom_model_canvas_result())) {
      return(NULL)
    }
    div(
      class = "mm-save-control",
      analysis_save_buttons(
        html_button_id = "save_custom_model_canvas_html_dialog",
        pdf_button_id = "save_custom_model_canvas_pdf_dialog",
        figure_button_id = "save_custom_model_canvas_figures_dialog",
        excel_button_id = "save_custom_model_canvas_excel_dialog",
        add_result_button_id = "add_custom_model_canvas_result",
        language = statedu_current_language(app_language_fn)
      )
    )
  })

  run_custom_model_canvas_analysis <- function(snapshot) {
    language <- statedu_current_language(app_language_fn)
    spec <- custom_model_canvas_snapshot_spec(
      snapshot,
      selected_names_fn(),
      language,
      two_moderator_model = "3"
    )
    progress_message <- custom_model_canvas_text(
      language,
      "Running custom mediation / moderation model",
      "\uc0ac\uc6a9\uc790 \ub9e4\uac1c\u00b7\uc870\uc808 \ubaa8\ud615 \uc2e4\ud589 \uc911"
    )
    result <- tryCatch(
      shiny::withProgress(
        message = progress_message,
        value = 0,
        {
          run_mediation_moderation_analysis(
            data = dataset_fn(),
            roles = spec$roles,
            mediator_arrangement = spec$mediator_arrangement,
            moderated_paths = spec$moderated_paths,
            boot_r = as.integer(input$custom_mm_boot_r %||% 5000L),
            seed = as.integer(input$custom_mm_seed %||% default_seed()),
            mean_center = FALSE,
            simple_slopes = TRUE,
            johnson_neyman = TRUE,
            analysis_method = input$custom_mm_analysis_method %||% "statedu",
            ci_method = input$custom_mm_ci_method %||% "bias_corrected",
            residual_diagnostics = input$custom_mm_residual_diagnostics %||% TRUE,
            auto_method = isTRUE(input$custom_mm_residual_diagnostics %||% TRUE) && isTRUE(input$custom_mm_auto_method %||% TRUE),
            direct_x = spec$direct_x,
            x_to_m = spec$x_to_m,
            m_to_y = spec$m_to_y,
            m_to_m = spec$m_to_m,
            moderated_x_to_m = spec$moderated_x_to_m,
            moderated_m_to_y = spec$moderated_m_to_y,
            moderation_map = spec$moderation_map,
            two_moderator_model = "3",
            custom_path_model = TRUE,
            effect_size_models = c(
              if (isTRUE(input$custom_mm_effect_size_y %||% TRUE)) "y" else character(0),
              if (isTRUE(input$custom_mm_effect_size_m %||% FALSE)) "m" else character(0)
            ),
            covariate_control = c(
              if (isTRUE(input$custom_mm_covariate_control_y %||% TRUE)) "y" else character(0),
              if (isTRUE(input$custom_mm_covariate_control_m %||% TRUE)) "m" else character(0)
            ),
            language = language,
            variable_info = variable_table_fn(),
            labels = labels_fn(),
            category_table = category_table_fn(),
            progress = function(done, total, focal) {
              counts <- mediation_moderation_bootstrap_progress_counts(done, total, input$custom_mm_boot_r %||% 5000L)
              shiny::setProgress(
                value = counts$done / counts$total,
                message = progress_message,
                detail = mediation_moderation_bootstrap_progress_detail(
                  counts$done,
                  counts$total,
                  focal,
                  input$custom_mm_boot_r %||% 5000L,
                  language
                )
              )
            }
          )
        }
      ),
      error = function(e) {
        showNotification(conditionMessage(e), type = "warning", duration = 7)
        NULL
      }
    )
    if (!is.null(result)) {
      if (is.data.frame(result$overview) && all(c("Item", "Value") %in% names(result$overview))) {
        result$overview$Value[result$overview$Item == "Model"] <- custom_model_canvas_text(
          language,
          "User-defined mediation / moderation model",
          "\uc0ac\uc6a9\uc790\uc815\uc758 \ub9e4\uac1c\u00b7\uc870\uc808 \ubaa8\ud615"
        )
      }
      source_snapshot <- snapshot
      source_snapshot$nonce <- NULL
      result_snapshot <- custom_model_canvas_result_snapshot(source_snapshot, result)
      result$custom_model_canvas <- TRUE
      result$custom_model_canvas_snapshot <- source_snapshot
      result$custom_model_canvas_result_snapshot <- result_snapshot
      custom_model_canvas_result(result)
      session$sendCustomMessage(
        "custom-model-canvas-result",
        list(
          source = source_snapshot,
          result = result_snapshot,
          show = TRUE
        )
      )
      showNotification(custom_model_canvas_text(language, "Custom model analysis finished.", "\uc0ac\uc6a9\uc790 \ubaa8\ud615 \ubd84\uc11d\uc774 \uc644\ub8cc\ub418\uc5c8\uc2b5\ub2c8\ub2e4."), type = "message", duration = 4)
    }
  }

  observeEvent(input$custom_model_canvas_run_request, {
    snapshot <- input$custom_model_canvas_run_request
    custom_model_canvas_pending_snapshot(snapshot)
    custom_model_canvas_snapshot(snapshot)
  }, ignoreInit = TRUE)

  observeEvent(input$custom_model_canvas_run_confirm, {
    snapshot <- input$custom_model_canvas_run_confirm %||% custom_model_canvas_pending_snapshot()
    custom_model_canvas_pending_snapshot(snapshot)
    custom_model_canvas_snapshot(snapshot)
    shiny::req(!is.null(snapshot))
    run_custom_model_canvas_analysis(snapshot)
  }, ignoreInit = TRUE)

  observeEvent(input$save_custom_model_canvas_html_dialog, {
    shiny::req(!is.null(custom_model_canvas_result()))
    path <- choose_html_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) {
      showNotification(statedu_t("result.save_dialog_canceled", statedu_current_language(app_language_fn)), type = "warning", duration = 5)
      return(invisible(NULL))
    }
    if (!grepl("\\.html?$", path, ignore.case = TRUE)) path <- paste0(path, ".html")
    tryCatch(
      {
        write_mediation_moderation_results_html(custom_model_canvas_result(), path, statedu_current_language(app_language_fn), dash_nonsignificant = TRUE, output_table_style = analysis_output_table_style(input$custom_mm_output_table_style))
        showNotification(sprintf(statedu_t("result.html_saved", statedu_current_language(app_language_fn)), path), type = "message")
      },
      error = function(e) showNotification(paste(statedu_t("result.html_save_failed", statedu_current_language(app_language_fn)), conditionMessage(e)), type = "error", duration = 8)
    )
  }, ignoreInit = TRUE)

  observeEvent(input$save_custom_model_canvas_pdf_dialog, {
    shiny::req(!is.null(custom_model_canvas_result()))
    path <- choose_pdf_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) {
      showNotification(statedu_t("result.save_dialog_canceled", statedu_current_language(app_language_fn)), type = "warning", duration = 5)
      return(invisible(NULL))
    }
    if (!grepl("\\.pdf$", path, ignore.case = TRUE)) path <- paste0(path, ".pdf")
    tryCatch(
      {
        write_mediation_moderation_results_pdf(custom_model_canvas_result(), path, statedu_current_language(app_language_fn), dash_nonsignificant = TRUE, output_table_style = analysis_output_table_style(input$custom_mm_output_table_style))
        showNotification(sprintf(statedu_t("result.pdf_saved", statedu_current_language(app_language_fn)), path), type = "message")
      },
      error = function(e) showNotification(paste(statedu_t("result.pdf_save_failed", statedu_current_language(app_language_fn)), conditionMessage(e)), type = "error", duration = 8)
    )
  }, ignoreInit = TRUE)

  observeEvent(input$save_custom_model_canvas_excel_dialog, {
    shiny::req(!is.null(custom_model_canvas_result()))
    path <- choose_excel_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) {
      showNotification(statedu_t("result.save_dialog_canceled", statedu_current_language(app_language_fn)), type = "warning", duration = 5)
      return(invisible(NULL))
    }
    if (!grepl("\\.xlsx$", path, ignore.case = TRUE)) path <- paste0(path, ".xlsx")
    tryCatch(
      {
        save_mediation_moderation_excel_file(custom_model_canvas_result(), path)
        showNotification(sprintf(statedu_t("result.excel_saved", statedu_current_language(app_language_fn)), path), type = "message")
      },
      error = function(e) showNotification(paste(statedu_t("result.excel_save_failed", statedu_current_language(app_language_fn)), conditionMessage(e)), type = "error", duration = 8)
    )
  }, ignoreInit = TRUE)

  observeEvent(input$save_custom_model_canvas_figures_dialog, {
    shiny::req(!is.null(custom_model_canvas_result()))
    directory <- choose_figure_save_dir()
    if (length(directory) == 0 || !nzchar(directory[[1]])) {
      showNotification(statedu_t("result.folder_dialog_canceled", statedu_current_language(app_language_fn)), type = "warning", duration = 5)
      return(invisible(NULL))
    }
    tryCatch(
      {
        saved <- save_mediation_moderation_figures_to_dir(custom_model_canvas_result(), directory, statedu_current_language(app_language_fn), dash_nonsignificant = TRUE)
        showNotification(sprintf(statedu_t("result.figures_saved", statedu_current_language(app_language_fn)), length(saved), directory), type = "message")
      },
      error = function(e) showNotification(paste(statedu_t("result.figures_save_failed", statedu_current_language(app_language_fn)), conditionMessage(e)), type = "error", duration = 8)
    )
  }, ignoreInit = TRUE)

  register_add_result_snapshot(
    input,
    session,
    "add_custom_model_canvas_result",
    "Custom mediation / moderation model",
    html_fn = function() {
      mediation_moderation_saved_results_html(
        custom_model_canvas_result(),
        statedu_current_language(app_language_fn),
        dash_nonsignificant = TRUE,
        output_table_style = analysis_output_table_style(input$custom_mm_output_table_style)
      )
    }
  )

  invisible(TRUE)
}
