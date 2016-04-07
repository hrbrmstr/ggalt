#' @export
to_basic.GeomXspline <- to_basic.GeomXspline2 <-
  getFromNamespace("to_basic.GeomLine", asNamespace("plotly"))

#' @export
to_basic.GeomBkde2d <-
  getFromNamespace("to_basic.GeomDensity2d", asNamespace("plotly"))

#' @export
to_basic.GeomStateface <- function(data, prestats_data, layout, params, p, ...) {
  prefix_class(data, "GeomText")
}

prefix_class <- function(x, y) {
  structure(x, class = unique(c(y, class(x))))
}
