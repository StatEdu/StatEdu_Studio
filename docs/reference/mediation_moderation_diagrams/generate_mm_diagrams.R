#!/usr/bin/env Rscript
# Generate all mediation/moderation model diagrams for review.
# Run from the project root:  Rscript scripts/generate_mm_diagrams.R
# Output: scripts/mm_diagrams_preview.html

setwd("D:/Program/Studio")

suppressPackageStartupMessages({
  library(shiny)
  library(htmltools)
})

source("R/app_bootstrap.R", local = FALSE)
source_app_modules()

# ── model configs ────────────────────────────────────────────────────────────

MODEL_CONFIGS <- list(
  "1"  = list(structure = "none",     moderated_paths = c("xy")),
  "4"  = list(structure = "parallel", moderated_paths = character(0)),
  "5"  = list(structure = "parallel", moderated_paths = c("xy")),
  "6"  = list(structure = "serial",   moderated_paths = character(0)),
  "7"  = list(structure = "parallel", moderated_paths = c("xm")),
  "8"  = list(structure = "parallel", moderated_paths = c("xm", "xy")),
  "14" = list(structure = "parallel", moderated_paths = c("my")),
  "15" = list(structure = "parallel", moderated_paths = c("my", "xy")),
  "58" = list(structure = "parallel", moderated_paths = c("xm", "my")),
  "59" = list(structure = "parallel", moderated_paths = c("xm", "my", "xy"))
)

x_counts  <- 1:4
m_counts  <- 1:4

# ── helper: build a single diagram ──────────────────────────────────────────

build_diagram <- function(model, x_n, m_n, language = "ko") {
  cfg    <- MODEL_CONFIGS[[model]]
  m_use  <- if (identical(model, "1")) 0L else max(1L, as.integer(m_n))
  # Model 6 (serial) needs at least 2 mediators
  if (identical(model, "6")) m_use <- max(2L, m_use)

  spec <- mediation_moderation_builder_spec(
    cfg$structure,
    cfg$moderated_paths,
    mediator_count = m_use,
    language = language
  )

  x_vars  <- if (x_n == 1L) "X" else paste0("X", seq_len(x_n))
  m_vars  <- if (m_use == 0L) character(0) else if (m_use == 1L) "M" else paste0("M", seq_len(m_use))
  needs_w <- mediation_moderation_model_requires_w(model)
  roles   <- mediation_moderation_role_values(
    y          = "Y",
    x          = x_vars,
    mediators  = m_vars,
    w          = if (needs_w) "W" else character(0),
    covariates = character(0)
  )

  fake_result <- list(diagram_spec = spec, model_number = model, roles = roles)
  dd          <- mediation_moderation_result_diagram_data(fake_result)

  mediation_moderation_diagram(
    dd$spec, dd$roles,
    variable_table = NULL, labels = character(0),
    language = language,
    edge_labels = NULL, edge_significance = NULL,
    variant = "result"
  )
}

# ── build all diagrams ───────────────────────────────────────────────────────

cat("Generating diagrams...\n")

sections <- lapply(names(MODEL_CONFIGS), function(model) {
  m_range <- if (identical(model, "1")) 0L else if (identical(model, "6")) 2:4 else 1:4
  needs_w <- mediation_moderation_model_requires_w(model)
  w_label <- if (needs_w) "W: 1개 고정" else "W: 없음"
  m_label <- if (identical(model, "1")) "M: 없음" else paste0("M: ", paste(m_range, collapse = ","), "개")

  rows <- lapply(m_range, function(m_n) {
    m_disp <- if (identical(model, "1")) 0L else m_n
    cells <- lapply(x_counts, function(x_n) {
      diag <- tryCatch(build_diagram(model, x_n, m_n), error = function(e) {
        tags$div(style = "color:red; font-size:9px;", conditionMessage(e))
      })
      tags$td(
        tags$div(
          style = "font-size:10px; color:#666; margin-bottom:3px; text-align:center;",
          sprintf("X=%d M=%d", x_n, m_disp)
        ),
        diag
      )
    })
    tags$tr(cells)
  })

  tagList(
    tags$h2(sprintf("Model %s  |  %s  |  %s  |  Y: 1개", model, w_label, m_label)),
    tags$table(
      tags$thead(tags$tr(lapply(x_counts, function(x_n)
        tags$th(style = "text-align:center; font-size:11px; color:#4a90d9;",
                sprintf("X=%d개", x_n))))),
      tags$tbody(rows)
    ),
    tags$hr(style = "margin: 28px 0; border-color:#dde6f0;")
  )
})

# ── inline CSS (reuse app styles for nodes/arrows) ───────────────────────────

DIAGRAM_W <- 240
DIAGRAM_H <- 170

inline_css <- sprintf("
body { font-family: system-ui, sans-serif; margin: 32px; background: #f5f8fc; }
h1   { font-size: 20px; color: #1e3a5f; margin-bottom: 28px; }
h2   { font-size: 15px; font-weight: 700; color: #1e3a5f;
       border-bottom: 2px solid #4a90d9; padding-bottom: 6px; margin-bottom: 10px; }
table { border-collapse: collapse; }
th, td { padding: 6px; vertical-align: top; }

/* diagram container */
.mm-diagram-panel {
  position: relative;
  width: %dpx;
  height: %dpx;
  border: 1px solid #b7c3cf;
  border-radius: 6px;
  background: #fbfcfd;
  overflow: hidden;
}
.mm-diagram-title { display: none; }
.mm-diagram-svg {
  position: absolute;
  inset: 0;
  width: 100%%;
  height: 100%%;
}
/* arrows */
.mm-diagram-arrow {
  stroke: #111827; stroke-width: 0.3; fill: none; stroke-linecap: round;
}
.mm-diagram-arrow-nonsignificant { stroke-dasharray: 1.6 1.2; }
.mm-diagram-arrowhead { fill: #111827; }
/* nodes */
.mm-diagram-node {
  position: absolute;
  transform: translate(-50%%, -50%%);
  width: 14%%;
  min-width: 28px;
  height: 10%%;
  min-height: 18px;
  display: flex;
  align-items: center;
  justify-content: center;
  border: 1.5px solid #2563ab;
  border-radius: 4px;
  background: #fff;
  font-size: 9px;
  font-weight: 600;
  color: #1e3a5f;
  text-align: center;
  z-index: 2;
  box-sizing: border-box;
}
.mm-node-assigned { background: #eff6ff; border-color: #2563ab; }
.mm-node-empty    { background: #f8fafc; border-color: #94a3b8; color: #64748b; }
.mm-node-variable-text, .mm-node-role { font-size: 9px; line-height: 1.2; }
", DIAGRAM_W, DIAGRAM_H)

page <- tags$html(
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$title("MM Model Diagrams"),
    tags$style(HTML(inline_css))
  ),
  tags$body(
    tags$h1("매개·조절 모형 다이어그램 전체 보기"),
    tags$p(style = "color:#666; font-size:13px; margin-bottom:28px;",
           "모형 × 독립변수 수(X) × 매개변수 수(M) 전체 조합. 조절변수(W)·종속변수(Y) 1개 고정."),
    sections
  )
)

out_path <- "scripts/mm_diagrams_preview.html"
save_html(page, file = out_path)
cat(sprintf("Done. Open: %s\n", normalizePath(out_path, winslash = "/", mustWork = FALSE)))
