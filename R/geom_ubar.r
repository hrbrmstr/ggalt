#' Uniform "bar" charts
#'
#' I've been using `geom_segment` more to make "bar" charts, setting
#' `xend` to whatever `x` is and `yend` to `0`. The bar widths remain
#' constant without any tricks and you have granular control over the
#' segment width. I decided it was time to make a `geom`.
#'
#' @md
#' @section Aesthetics:
#' `geom_ubar`` understands the following aesthetics (required aesthetics are in bold):
#'
#' - **`x`**
#' - **`y`**
#' - `alpha`
#' - `colour`
#' - `group`
#' - `linetype`
#' - `size`
#'
#' @inheritParams ggplot2::layer
#' @param na.rm If `FALSE` (the default), removes missing values with
#'    a warning.  If `TRUE` silently removes missing values.
#' @param ... other arguments passed on to `layer`. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   `color = "red"` or `size = 3`. They may also be parameters
#'   to the paired geom/stat.
#' @inheritParams ggplot2::layer
#' @export
#' @examples
# library(ggplot2)
#
# data(economics)
# ggplot(economics, aes(date, uempmed)) +
#   geom_ubar()
geom_ubar <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", ...,
                      na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomUbar,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggalt-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomUbar <- ggproto("GeomUbar", Geom,
  required_aes = c("x", "y"),
  non_missing_aes = c("size"),
  default_aes = aes(
    size = 0.25, colour = "black", alpha = NA
  ),

  setup_data = function(data, params) {
    transform(data, xend = x, yend = 0)
  },

  draw_group = function(data, panel_scales, coord) {

    ggplot2::GeomSegment$draw_panel(data, panel_scales, coord)

  }

)


