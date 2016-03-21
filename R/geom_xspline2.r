#' @rdname ggalt-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomXSpline2 <- ggproto("GeomXSpline", Geom,
  required_aes = c("x", "y"),
  default_aes = aes(colour = "black", s_shape=1, s_open=FALSE),
  draw_key = draw_key_point,

  draw_panel = function(data, panel_scales, coord) {
    coords <- coord$transform(data, panel_scales)
    grid::xsplineGrob(
      coords$x, coords$y,
      shape = coords$s_shape-1,  ## kluge!
      open = coords$s_open[1],
      gp = grid::gpar(col = coords$colour)
    )
  }
)

#' Alternative implemenation for connecting  control points/observations
#' with an X-spline
#'
#' @inheritParams geom_xspline
#' @return creates a spline curve
#' @author Ben Bolker
#' @family xspline implementations
geom_xspline2 <- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", na.rm = FALSE, show.legend = NA,
                               inherit.aes = TRUE, ...) {
  layer(
    geom = GeomXSpline2, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
