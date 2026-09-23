source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")

if (!requireNamespace("seminr", quietly = TRUE)) stop("seminr is required for MICOM validation.")
set.seed(20260816)
n_group <- 40L
group <- rep(c("A", "B"), each = n_group)
eta1 <- stats::rnorm(n_group)
eta2 <- .55 * eta1 + stats::rnorm(n_group, sd = .8)
base_data <- data.frame(
  x1 = .80 * eta1 + stats::rnorm(n_group, sd = .45), x2 = .75 * eta1 + stats::rnorm(n_group, sd = .50), x3 = .70 * eta1 + stats::rnorm(n_group, sd = .55),
  y1 = .82 * eta2 + stats::rnorm(n_group, sd = .42), y2 = .76 * eta2 + stats::rnorm(n_group, sd = .48), y3 = .71 * eta2 + stats::rnorm(n_group, sd = .54)
)
data <- rbind(base_data, base_data)
data$group <- group
snapshot <- list(nodes = list(
  list(id = "f1", role = "latent", name = "eta1", constructType = "commonFactor", measurementMode = "reflective"),
  list(id = "f2", role = "latent", name = "eta2", constructType = "commonFactor", measurementMode = "reflective"),
  list(id = "x1", role = "indicator", name = "x1", variableId = "x1"), list(id = "x2", role = "indicator", name = "x2", variableId = "x2"), list(id = "x3", role = "indicator", name = "x3", variableId = "x3"),
  list(id = "y1", role = "indicator", name = "y1", variableId = "y1"), list(id = "y2", role = "indicator", name = "y2", variableId = "y2"), list(id = "y3", role = "indicator", name = "y3", variableId = "y3")
), edges = list(
  list(from = "f1", to = "x1"), list(from = "f1", to = "x2"), list(from = "f1", to = "x3"),
  list(from = "f2", to = "y1"), list(from = "f2", to = "y2"), list(from = "f2", to = "y3"),
  list(from = "f1", to = "f2")
))

micom <- structural_canvas_micom(snapshot, data, "group", "PLS", permutations = 19L, seed = 24680L)
stopifnot(
  identical(micom$type, "pls_micom"), nrow(micom$table) == 2L,
  all(c("Observed c", "5% permutation c", "Compositional invariance", "Mean equality", "Variance equality", "Invariance level") %in% names(micom$table)),
  micom$permutations_requested == 19L, micom$permutations_valid >= 19L,
  identical(micom$seed, 24680L), isTRUE(micom$measurement_gate$passed),
  identical(micom$mga_status, "Permutation PLS-MGA completed"),
  nrow(micom$mga_table) == 1L,
  all(c("Path difference", "Permutation p", "BH-adjusted p", "Valid permutations") %in% names(micom$mga_table)),
  micom$mga_table$`Permutation p` >= 1 / 20,
  micom$mga_table$`Permutation p` <= 1
)

message("SEM MICOM validation passed.")
