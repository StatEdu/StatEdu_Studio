source(file.path("R", "utils.R"))
source(file.path("R", "data_io.R"))
source(file.path("R", "server_data_state.R"))
source(file.path("R", "data_editor_wide_long.R"))

wide <- data.frame(
  id = 1:2,
  sex = c("F", "M"),
  qol_1 = c(70, 60),
  qol_2 = c(75, 65),
  dep_1 = c(12, 18),
  dep_2 = c(10, 15),
  check.names = FALSE
)

auto <- wide_long_transform(
  wide,
  repeated_variables = c("qol_1", "qol_2", "dep_1", "dep_2"),
  id_variables = "id",
  time_name = "time",
  output_mode = "auto"
)

stopifnot(nrow(auto) == 4)
stopifnot(all(c("id", "sex", "time", "qol", "dep") %in% names(auto)))
stopifnot(identical(as.character(auto$time), c("1", "2", "1", "2")))
stopifnot(identical(as.numeric(auto$qol), c(70, 75, 60, 65)))
stopifnot(identical(as.numeric(auto$dep), c(12, 10, 18, 15)))

single <- wide_long_transform(
  wide,
  repeated_variables = c("qol_1", "qol_2"),
  id_variables = "id",
  time_name = "visit",
  value_name = "qol",
  output_mode = "single"
)

stopifnot(nrow(single) == 4)
stopifnot(all(c("id", "sex", "visit", "qol") %in% names(single)))
stopifnot(identical(as.character(single$visit), c("1", "2", "1", "2")))
stopifnot(identical(as.numeric(single$qol), c(70, 75, 60, 65)))

single_duplicate_time <- wide_long_transform(
  wide,
  repeated_variables = c("qol_1", "dep_1"),
  id_variables = "id",
  time_name = "visit",
  value_name = "score",
  output_mode = "single"
)

stopifnot(nrow(single_duplicate_time) == 4)
stopifnot(identical(as.character(single_duplicate_time$visit), c("1", "1", "1", "1")))
stopifnot(identical(as.numeric(single_duplicate_time$score), c(70, 12, 60, 18)))

manual <- wide_long_transform(
  wide,
  repeated_variables = c("qol_1", "qol_2"),
  id_variables = "id",
  time_text = "qol_1=baseline\nqol_2=followup"
)

stopifnot(identical(as.character(manual$time), c("baseline", "followup", "baseline", "followup")))

wide_plain <- data.frame(
  id = 1:2,
  drisk = c(1, 0),
  sleepq = c(3, 4),
  walk = c(10, 20),
  check.names = FALSE
)

plain <- wide_long_transform(
  wide_plain,
  repeated_variables = c("drisk", "sleepq", "walk"),
  id_variables = "id"
)

stopifnot(nrow(plain) == 2)
stopifnot(all(c("id", "time", "drisk", "sleepq", "walk") %in% names(plain)))
stopifnot(identical(as.character(plain$time), c("1", "1")))
stopifnot(identical(as.numeric(plain$drisk), c(1, 0)))
stopifnot(identical(as.numeric(plain$sleepq), c(3, 4)))

manual_mapping <- data.frame(
  variable = c("drisk", "sleepq", "walk"),
  measure = c("behavior", "behavior", "activity"),
  time = c("baseline", "followup", "baseline"),
  stringsAsFactors = FALSE
)

mapped <- wide_long_transform(
  wide_plain,
  repeated_variables = c("drisk", "sleepq", "walk"),
  id_variables = "id",
  mapping = manual_mapping
)

stopifnot(nrow(mapped) == 4)
stopifnot(all(c("id", "time", "behavior", "activity") %in% names(mapped)))
stopifnot(identical(as.character(mapped$time), c("baseline", "followup", "baseline", "followup")))
stopifnot(identical(as.numeric(mapped$behavior), c(1, 3, 0, 4)))
stopifnot(identical(as.numeric(mapped$activity), c(10, NA, 20, NA)))

wide_multi <- data.frame(
  id = 1:2,
  sex = c("F", "M"),
  age = c(70, 75),
  x1 = c(10, 20),
  x2 = c(11, 21),
  x3 = c(12, 22),
  x4 = c(13, 23),
  y1 = c(100, 200),
  y2 = c(101, 201),
  y3 = c(102, 202),
  y4 = c(103, 203),
  check.names = FALSE
)

x_spec <- wide_long_make_spec(c("x1", "x2", "x3", "x4"), "x", "different", index_name = "time")
y_spec <- wide_long_make_spec(c("y1", "y2", "y3", "y4"), "y", "different", index_name = "time")
configured <- wide_long_transform_configured(wide_multi, list(x_spec, y_spec), id_variables = "id")

stopifnot(nrow(configured) == 8)
stopifnot(all(c("id", "time", "x", "y") %in% names(configured)))
stopifnot(identical(as.character(configured$time), rep(as.character(1:4), 2)))
stopifnot(identical(as.numeric(configured$x), c(10, 11, 12, 13, 20, 21, 22, 23)))
stopifnot(identical(as.numeric(configured$y), c(100, 101, 102, 103, 200, 201, 202, 203)))

configured_preview <- wide_long_preview_display(configured, list(x_spec, y_spec), id_variables = "id", fixed_mode = "all")
stopifnot(identical(names(configured_preview), c("id", "time", "x", "y")))

configured_fixed <- wide_long_transform_configured(
  wide_multi,
  list(x_spec),
  id_variables = "id",
  keep_other_variables = FALSE,
  fixed_variables = "sex"
)

stopifnot(all(c("id", "sex", "time", "x") %in% names(configured_fixed)))
stopifnot(!"age" %in% names(configured_fixed))
stopifnot(identical(as.character(configured_fixed$sex), rep(c("F", "M"), each = 4)))

configured_fixed_preview <- wide_long_preview_display(
  configured_fixed,
  list(x_spec),
  id_variables = "id",
  fixed_variables = "sex",
  fixed_mode = "selected"
)
stopifnot(identical(names(configured_fixed_preview), c("id", "time", "x", "sex")))

same_spec <- wide_long_make_spec(c("x1", "x2", "x3", "x4"), "x", "same", group_count = 2, time_count = 2)
same_configured <- wide_long_transform_configured(wide_multi, list(same_spec), id_variables = "id", keep_other_variables = FALSE)

stopifnot(nrow(same_configured) == 8)
stopifnot(all(c("id", "group", "time", "x") %in% names(same_configured)))
stopifnot(identical(paste(same_configured$group, same_configured$time), rep(c("1 1", "1 2", "2 1", "2 2"), 2)))

cat("Data Editor wide-to-long validation passed.\n")
