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
          class = "custom-model-analysis-options",
          selectInput(paste0(structural_analysis_prefix(analysis_type), "_estimator"), if (ko) "추정 방법" else "Estimator", choices = if (analysis_type == "plssem") c("PLS" = "PLS") else c("ML" = "ML", "MLR" = "MLR", "WLSMV" = "WLSMV")),
          if (analysis_type != "plssem") selectInput(paste0(structural_analysis_prefix(analysis_type), "_missing"), if (ko) "결측치 처리" else "Missing data", choices = stats::setNames(c("fiml", "listwise"), c("FIML", if (ko) "목록 삭제" else "Listwise deletion"))),
          if (analysis_type != "plssem") selectInput(paste0(structural_analysis_prefix(analysis_type), "_scale"), if (ko) "잠재변수 스케일" else "Latent scale", choices = stats::setNames(c("marker", "variance"), c(if (ko) "첫 지표 부하량 = 1" else "Marker loading = 1", if (ko) "잠재변수 분산 = 1" else "Latent variance = 1"))),
          if (analysis_type != "plssem") selectInput(paste0(structural_analysis_prefix(analysis_type), "_rmsea_ci"), if (ko) "RMSEA 신뢰수준" else "RMSEA confidence level", choices = c("90% CI" = "0.90", "95% CI" = "0.95", "99% CI" = "0.99"), selected = "0.90"),
          if (analysis_type != "plssem") selectInput(
            paste0(structural_analysis_prefix(analysis_type), "_validity_formula"),
            if (ko) "AVE·CR 계산 방식" else "AVE/CR formula",
            choices = stats::setNames(c("standardized", "model_implied"), c(if (ko) "표준화 부하량(Fornell-Larcker)" else "Standardized loadings (Fornell-Larcker)", if (ko) "모형모수 방식(Raykov 계열)" else "Model-implied parameters (Raykov)")),
            selected = "standardized"
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
  tagList(
    div(
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
    ),
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
      identical(from_role, "latent") && identical(to_role, "latent")
    info <- list(label = "", p = NA_real_, matched = FALSE)
    if (identical(edge$kind, "covariance")) {
      info <- result_info(target_name(from), "~~", target_name(to))
    } else if (!is.null(from) && !is.null(to) && identical(from$role, "latent") && identical(to$role, "indicator")) {
      info <- result_info(structural_canvas_name(from), "=~", structural_canvas_name(to))
    } else if (!is.null(from) && !is.null(to) && identical(from$role, "indicator") && identical(to$role, "latent")) {
      info <- result_info(structural_canvas_name(to), "=~", structural_canvas_name(from))
    } else if (!is.null(from) && !is.null(to) && identical(from$role, "latent") && identical(to$role, "latent")) {
      info <- result_info(structural_canvas_name(to), "~", structural_canvas_name(from))
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

structural_canvas_ordered_indicators <- function(snapshot, variable_table = NULL) {
  if (is.null(variable_table) || !all(c("name", "measurement") %in% names(variable_table))) return(character(0))
  indicators <- unique(vapply(Filter(function(node) identical(node$role, "indicator"), snapshot$nodes %||% list()), structural_canvas_name, character(1)))
  measurements <- tolower(as.character(variable_table$measurement))
  names(measurements) <- as.character(variable_table$name)
  ordered_levels <- c("binary", "category", "categorical", "factor", "nominal", "ordinal", "ordered")
  is_ordered <- indicators %in% names(measurements) & measurements[indicators] %in% ordered_levels
  indicators[!is.na(is_ordered) & is_ordered]
}

run_structural_canvas_analysis <- function(snapshot, data, analysis_type, estimator = "ML", missing = "fiml", std_lv = FALSE, ordered = character(0)) {
  nodes <- snapshot$nodes %||% list()
  edges <- snapshot$edges %||% list()
  latents <- Filter(function(node) identical(node$role, "latent"), nodes)
  measurement_lines <- vapply(latents, function(latent) {
    indicators <- vapply(Filter(function(edge) {
      from <- structural_canvas_node(snapshot, edge$from)
      to <- structural_canvas_node(snapshot, edge$to)
      (identical(from$id, latent$id) && identical(to$role, "indicator")) ||
        (identical(to$id, latent$id) && identical(from$role, "indicator"))
    }, edges), function(edge) {
      from <- structural_canvas_node(snapshot, edge$from)
      to <- structural_canvas_node(snapshot, edge$to)
      structural_canvas_name(if (identical(from$role, "indicator")) from else to)
    }, character(1))
    paste(structural_canvas_name(latent), "=~", paste(indicators, collapse = " + "))
  }, character(1))
  measurement_lines <- measurement_lines[grepl("\\S+\\s*=~\\s*\\S+", measurement_lines)]
  structural_lines <- vapply(Filter(function(edge) {
    if (identical(edge$kind, "covariance")) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    identical(from$role, "latent") && identical(to$role, "latent")
  }, edges), function(edge) {
    paste(structural_canvas_name(structural_canvas_node(snapshot, edge$to)), "~", structural_canvas_name(structural_canvas_node(snapshot, edge$from)))
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
    if (!nzchar(from_name) || !nzchar(to_name)) "" else paste(from_name, "~~", to_name)
  }, character(1))
  covariance_lines <- covariance_lines[nzchar(covariance_lines)]

  if (analysis_type %in% c("cfa", "cbsem")) {
    syntax <- paste(c(measurement_lines, structural_lines, covariance_lines), collapse = "\n")
    if (!nzchar(syntax)) stop("The model does not contain estimable paths.")
    estimator <- toupper(as.character(estimator %||% "ML"))
    missing <- as.character(missing %||% "fiml")
    if (identical(estimator, "WLSMV") && identical(missing, "fiml")) missing <- "pairwise"
    ordered <- intersect(unique(as.character(ordered %||% character(0))), names(data))
    if (identical(estimator, "WLSMV") && !length(ordered)) {
      stop("WLSMV requires at least one binary, categorical, or ordinal indicator.")
    }
    fit <- if (identical(analysis_type, "cfa")) {
      lavaan::cfa(syntax, data = data, estimator = estimator, missing = missing, std.lv = isTRUE(std_lv), ordered = ordered, auto.cov.lv.x = FALSE)
    } else {
      lavaan::sem(syntax, data = data, estimator = estimator, missing = missing, std.lv = isTRUE(std_lv), ordered = ordered, auto.cov.lv.x = FALSE)
    }
    return(list(fit = fit, syntax = syntax, converged = isTRUE(lavaan::lavInspect(fit, "converged"))))
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
  mi <- mi[is.finite(mi$mi) & mi$mi >= 4, , drop = FALSE]
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

    candidate_row <- candidates[1L, , drop = FALSE]
    candidate_syntax <- paste(
      cumulative_syntax,
      paste(candidate_row$lhs[[1L]], candidate_row$op[[1L]], candidate_row$rhs[[1L]]),
      sep = "\n"
    )
    candidate <- tryCatch({
      if (identical(analysis_type, "cfa")) {
        lavaan::cfa(candidate_syntax, data = data, estimator = estimator, missing = missing, std.lv = std_lv, ordered = ordered, auto.cov.lv.x = FALSE)
      } else {
        lavaan::sem(candidate_syntax, data = data, estimator = estimator, missing = missing, std.lv = std_lv, ordered = ordered, auto.cov.lv.x = FALSE)
      }
    }, error = function(error) NULL)
    if (is.null(candidate) || !isTRUE(lavaan::lavInspect(candidate, "converged"))) break

    cumulative_syntax <- candidate_syntax
    current_fit <- candidate
    indices <- lavaan::fitMeasures(candidate, c("cfi", "tli", "rmsea", "srmr"))
    candidate_row$step <- step
    candidate_row$cfi_after <- unname(indices[["cfi"]])
    candidate_row$tli_after <- unname(indices[["tli"]])
    candidate_row$rmsea_after <- unname(indices[["rmsea"]])
    candidate_row$srmr_after <- unname(indices[["srmr"]])
    steps[[length(steps) + 1L]] <- candidate_row
  }

  if (!length(steps)) return(mi[0L, , drop = FALSE])
  do.call(rbind, steps)
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
    result_table <- function(kind) {
      bundle <- fit_result()
      shiny::req(!is.null(bundle))
      fit <- bundle$fit
      snapshot <- bundle$snapshot %||% list()
      labels <- labels_fn() %||% character(0)
      ko <- FALSE
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
            Item = if (ko) c("분석 방법", "추정 방법", "표본 크기(N)", "관측변수 수", "잠재변수 수", "자유 파라미터 수", "수렴 여부") else c("Analysis", "Estimator", "N", "Observed variables", "Latent variables", "Free parameters", "Converged"),
            Value = c(structural_analysis_title(analysis_type, "en"), lavaan::lavInspect(fit, "options")$estimator, lavaan::lavInspect(fit, "ntotal"), length(lavaan::lavNames(fit, "ov")), length(lavaan::lavNames(fit, "lv")), lavaan::lavInspect(fit, "npar"), if (isTRUE(lavaan::lavInspect(fit, "converged"))) "Yes" else "No"),
            check.names = FALSE
          )
          names(overview_df)[[1]] <- if (ko) "항목" else "Item"
          names(overview_df)[[2]] <- if (ko) "값" else "Value"
          return(overview_df)
        }
        if (identical(kind, "fit")) {
          ci_level <- as.numeric(bundle$rmsea_ci %||% .90)
          measures <- lavaan::fitMeasures(fit, fm_args = list(rmsea.ci.level = ci_level))
          value <- function(key) if (key %in% names(measures)) unname(measures[[key]]) else NA_real_
          values <- c(value("chisq"), value("df"), value("pvalue"), value("chisq") / value("df"), value("cfi"), value("tli"), value("srmr"), value("rmsea"), value("rmsea.ci.lower"), value("rmsea.ci.upper"))
          ci_percent <- round(100 * ci_level)
          table <- data.frame(`χ²` = fmt(values[[1]]), df = fmt(values[[2]]), p = format_p(values[[3]]), Q = fmt(values[[4]]), CFI = fmt(values[[5]]), TLI = fmt(values[[6]]), SRMR = fmt(values[[7]]), RMSEA = fmt(values[[8]]), fmt(values[[9]]), fmt(values[[10]]), check.names = FALSE)
          names(table)[9:10] <- paste0(ci_percent, "% CI ", c("LLCI", "ULCI"))
          return(table)
        }
        if (identical(kind, "validity")) {
          standardized <- lavaan::standardizedSolution(fit)
          loadings <- standardized[standardized$op == "=~", c("lhs", "rhs", "est.std"), drop = FALSE]
          latent_names <- unique(loadings$lhs)
          formula_mode <- bundle$validity_formula %||% "standardized"
          if (identical(formula_mode, "model_implied")) {
            parameters <- lavaan::parameterEstimates(fit)
            latent_variance <- diag(lavaan::lavInspect(fit, "cov.lv"))
            residuals <- parameters[parameters$op == "~~" & parameters$lhs == parameters$rhs, c("lhs", "est"), drop = FALSE]
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
              theta <- residuals$est[match(indicators, residuals$lhs)]
              common <- sum(factor_loadings)^2 * latent_variance[[name]]
              common / (common + sum(theta, na.rm = TRUE))
            }, numeric(1)), latent_names)
          } else {
            ave <- stats::setNames(vapply(latent_names, function(name) mean(loadings$est.std[loadings$lhs == name]^2, na.rm = TRUE), numeric(1)), latent_names)
            cr <- stats::setNames(vapply(latent_names, function(name) {
              lambda <- loadings$est.std[loadings$lhs == name]
              sum(lambda)^2 / (sum(lambda)^2 + sum(1 - lambda^2))
            }, numeric(1)), latent_names)
          }
          correlations <- as.matrix(lavaan::lavInspect(fit, "cor.lv"))
          if (!length(dim(correlations))) correlations <- matrix(correlations, nrow = 1L, ncol = 1L)
          if (is.null(rownames(correlations))) rownames(correlations) <- latent_names
          if (is.null(colnames(correlations))) colnames(correlations) <- latent_names
          table <- matrix("", nrow = length(latent_names), ncol = length(latent_names) + 2L)
          colnames(table) <- c(if (ko) "잠재변수" else "Latent", vapply(latent_names, display_name, character(1)), "CR")
          for (row in seq_along(latent_names)) {
            table[row, 1] <- display_name(latent_names[[row]])
            for (column in seq_along(latent_names)) {
              if (row == column) table[row, column + 1L] <- paste0("(", format_decimal3(ave[[latent_names[[row]]]]), ")")
              if (row > column) table[row, column + 1L] <- format_decimal3(correlations[latent_names[[row]], latent_names[[column]]])
            }
            table[row, ncol(table)] <- format_decimal3(cr[[latent_names[[row]]]])
          }
          return(as.data.frame(table, check.names = FALSE))
        }
        if (identical(kind, "measurement")) {
          raw <- lavaan::parameterEstimates(fit)
          standardized <- lavaan::standardizedSolution(fit)
          rows <- raw$op == "=~"
          raw <- raw[rows, c("lhs", "rhs", "est", "se", "z", "pvalue"), drop = FALSE]
          beta <- standardized$est.std[standardized$op == "=~"]
          table <- data.frame(vapply(raw$lhs, display_name, character(1)), vapply(raw$rhs, display_name, character(1)), B = fmt(raw$est), SE = fmt(raw$se), beta = fmt(beta), z = fmt(raw$z), p = vapply(raw$pvalue, format_p, character(1)), check.names = FALSE)
          names(table)[1:2] <- if (ko) c("잠재변수", "측정변수") else c("Latent", "Indicator")
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
          return(data.frame(Covariance = relation, MI = fmt(mi$mi), EPC = fmt(epc), check.names = FALSE))
        }
        table <- data.frame(relation, MI = fmt(mi$mi), CFI = fmt(mi$cfi_after), TLI = fmt(mi$tli_after), RMSEA = fmt(mi$rmsea_after), SRMR = fmt(mi$srmr_after), check.names = FALSE)
        names(table)[[1]] <- if (ko) "추천 공분산" else "Covariance"
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
      ko <- FALSE
      div(
        class = "structural-analysis-results regression-results",
        h3(if (ko) "분석 결과" else "Analysis Results"),
        div(class = "result-section regression-result-panel", h4(if (ko) "1. 모형 개요" else "1. Model overview"), tableOutput(paste0(prefix, "_result_overview"))),
        div(class = "result-section regression-result-panel", h4(if (ko) "2. 모형 적합도" else "2. Model fit"), uiOutput(paste0(prefix, "_result_fit"))),
        div(class = "result-section regression-result-panel", h4(if (ko) "3. 상관분석, 집중타당도 & 판별타당도" else "3. Correlations, convergent & discriminant validity"), tableOutput(paste0(prefix, "_result_validity"))),
        div(class = "result-section regression-result-panel structural-measurement-result", h4(if (ko) "4. 측정모형" else "4. Measurement model"), tableOutput(paste0(prefix, "_result_measurement"))),
        if (analysis_type != "plssem") div(class = "result-section regression-result-panel structural-mi-result", h4(if (ko) "5. 수정지수(MI)" else "5. Modification indices (MI)"), uiOutput(paste0(prefix, "_result_mi")))
      )
    })
    output[[paste0(prefix, "_result_fit")]] <- renderUI({
      values <- result_table("fit")
      shiny::req(nrow(values) > 0)
      ci_percent <- round(100 * as.numeric(fit_result()$rmsea_ci %||% .90))
      tags$table(
        class = "table table-striped table-bordered structural-fit-table",
        tags$thead(
          tags$tr(
            tags$th(rowspan = "2", HTML("&chi;<sup>2</sup>")), tags$th(rowspan = "2", "df"), tags$th(rowspan = "2", "p"), tags$th(rowspan = "2", "Q"),
            tags$th(rowspan = "2", "CFI"), tags$th(rowspan = "2", "TLI"), tags$th(rowspan = "2", "SRMR"), tags$th(rowspan = "2", "RMSEA"),
            tags$th(colspan = "2", paste0(ci_percent, "% CI"))
          ),
          tags$tr(tags$th("LLCI"), tags$th("ULCI"))
        ),
        tags$tbody(tags$tr(lapply(as.character(values[1, , drop = TRUE]), tags$td)))
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
      tags$table(
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
      )
    })
    execute_analysis <- function(snapshot, settings = NULL) {
      settings <- settings %||% list()
      estimator <- settings$estimator %||% input[[paste0(prefix, "_estimator")]] %||% "ML"
      missing <- settings$missing %||% input[[paste0(prefix, "_missing")]] %||% "fiml"
      std_lv <- settings$std_lv %||% identical(input[[paste0(prefix, "_scale")]], "variance")
      mi_mode <- settings$mi_mode %||% input[[paste0(prefix, "_mi_mode")]] %||% "theory"
      rmsea_ci <- settings$rmsea_ci %||% as.numeric(input[[paste0(prefix, "_rmsea_ci")]] %||% .90)
      validity_formula <- settings$validity_formula %||% input[[paste0(prefix, "_validity_formula")]] %||% "standardized"
      result_coefficient <- settings$result_coefficient %||% input[[paste0(prefix, "_result_coefficient")]] %||% "beta"
      data <- dataset_fn()
      ordered <- structural_canvas_ordered_indicators(snapshot, variable_table_fn())
      if (length(ordered) > 0 || identical(toupper(estimator), "WLSMV")) {
        if (identical(toupper(estimator), "ML")) estimator <- "WLSMV"
        if (identical(missing, "fiml")) missing <- "pairwise"
      }
      result <- run_structural_canvas_analysis(snapshot, data, analysis_type, estimator = estimator, missing = missing, std_lv = std_lv, ordered = ordered)
      mi <- if (analysis_type %in% c("cfa", "cbsem")) structural_canvas_mi_refits(snapshot, result, data, analysis_type, estimator, missing, std_lv, mode = mi_mode, ordered = ordered) else NULL
      fit_result(list(
        fit = result$fit, snapshot = snapshot, mi = mi, mi_mode = mi_mode,
        rmsea_ci = rmsea_ci, validity_formula = validity_formula,
        estimator = estimator, missing = missing, std_lv = std_lv, ordered = ordered,
        result_coefficient = result_coefficient
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
    observeEvent(input[[canvas_input]], mark_settings_dirty(), ignoreInit = TRUE)
    observeEvent(input[[confirm_input]], {
      package <- structural_analysis_package(analysis_type)
      if (!requireNamespace(package, quietly = TRUE)) {
        showNotification(sprintf("%s package is required.", package), type = "error")
      } else {
        tryCatch({
          snapshot <- input[[confirm_input]]
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
    lapply(seq_len(100L), function(index) local({
      row_index <- index
      observeEvent(input[[paste0(prefix, "_mi_select_", row_index)]], {
        bundle <- fit_result()
        shiny::req(!is.null(bundle), !is.null(bundle$mi), nrow(bundle$mi) >= row_index)
        tryCatch({
          selected_rows <- if (identical(bundle$mi_mode %||% "theory", "theory")) seq_len(row_index) else row_index
          snapshot <- bundle$snapshot
          for (selected_row in selected_rows) {
            snapshot <- structural_canvas_apply_mi(snapshot, bundle$mi[selected_row, , drop = FALSE])
          }
          result <- execute_analysis(snapshot, bundle)
          showNotification(
            if (identical(statedu_current_language(app_language_fn), "ko")) "선택한 MI 공분산을 추가하여 재분석했습니다." else "The selected MI covariance was added and the model was reanalyzed.",
            type = if (isTRUE(result$converged)) "message" else "warning"
          )
        }, error = function(error) {
          showNotification(conditionMessage(error), type = "error", duration = 8)
        })
      }, ignoreInit = TRUE)
    }))
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
