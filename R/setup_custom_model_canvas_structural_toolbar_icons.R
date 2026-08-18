# Structural equation canvas toolbar icons.

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

structural_higher_order_icon <- function() {
  tags$svg(
    class = "structural-higher-order-svg",
    viewBox = "0 0 52 30",
    fill = "none",
    stroke = "currentColor",
    `stroke-width` = "2.25",
    `stroke-linecap` = "round",
    `stroke-linejoin` = "round",
    `aria-hidden` = "true",
    tags$defs(tags$marker(
      id = "sem-higher-order-arrow",
      viewBox = "0 0 6 6",
      refX = "5",
      refY = "3",
      markerWidth = "4.5",
      markerHeight = "4.5",
      orient = "auto",
      tags$path(d = "M0,0 L6,3 L0,6 Z", fill = "currentColor")
    )),
    tags$ellipse(cx = "26", cy = "7.5", rx = "14.5", ry = "5.8"),
    tags$ellipse(cx = "13", cy = "23", rx = "9.5", ry = "4.6"),
    tags$ellipse(cx = "26", cy = "23", rx = "9.5", ry = "4.6"),
    tags$ellipse(cx = "39", cy = "23", rx = "9.5", ry = "4.6"),
    tags$line(x1 = "20", y1 = "12.5", x2 = "15", y2 = "18.3", `marker-end` = "url(#sem-higher-order-arrow)"),
    tags$line(x1 = "26", y1 = "13.2", x2 = "26", y2 = "18.3", `marker-end` = "url(#sem-higher-order-arrow)"),
    tags$line(x1 = "32", y1 = "12.5", x2 = "37", y2 = "18.3", `marker-end` = "url(#sem-higher-order-arrow)"),
    tags$text(
      x = "26",
      y = "10.4",
      `text-anchor` = "middle",
      `font-size` = "9.2",
      `font-weight` = "800",
      `font-family` = "Arial, sans-serif",
      `letter-spacing` = "0",
      stroke = "none",
      fill = "currentColor",
      "2차"
    )
  )
}
