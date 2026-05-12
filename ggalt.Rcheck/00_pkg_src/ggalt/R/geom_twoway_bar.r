# #' Two-way bar chart
# #'
# #' @inheritParams ggplot2::geom_bar
# #' @export
# geom_twoway_bar <- function(mapping = NULL, data = NULL,
#                      stat = "identity",
#                      width = NULL,
#                      ...,
#                      na.rm = FALSE,
#                      show.legend = NA,
#                      inherit.aes = TRUE) {
#
#   layer(
#     data = data,
#     mapping = mapping,
#     stat = stat,
#     geom = GeomTwowayBar,
#     position = "stack",
#     show.legend = show.legend,
#     inherit.aes = inherit.aes,
#     params = list(
#       width = width,
#       na.rm = na.rm,
#       ...
#     )
#   )
# }
#
# #' @rdname ggalt-ggproto
# #' @format NULL
# #' @usage NULL
# #' @export
# GeomTwowayBar <- ggproto("GeomTwowayBar", GeomRect,
#   required_aes = c("x", "y"),
#
#   do_setup_data = function(data, params) {
#     data$width <- data$width %||%
#       params$width %||% (resolution(data$x, FALSE) * 0.9)
#
#     d_plus <- subset(data, y>=0, drop=FALSE)
#     d_minus <- subset(data, y<0, drop=FALSE)
#
#     d_plus <- transform(d_plus,
#       ymin = pmin(y, 0), ymax = pmax(y, 0),
#       xmin = x - width / 2, xmax = x + width / 2, width = NULL,
#       is_plus = TRUE
#     )
#
#     d_minus <- transform(d_minus,
#       ymin = pmin(y, 0), ymax = pmax(y, 0),
#       xmin = x - width / 2, xmax = x + width / 2, width = NULL,
#       y = abs(y),
#       is_plus = FALSE
#     )
#
#     cat("setup_data() after _________\n")
#     print(rbind(d_plus, d_minus))
#
#     rbind(d_plus, d_minus)
#
#   },
#
#   draw_panel = function(self, data, panel_scales, coord, width=NULL) {
#
#     cat("draw_panel() _________\n")
#     print(data)
# #
# #     d_plus <- subset(data, is_plus)
# #     d_minus <- subset(data, !is_plus)
# #     d_minus$y <- -d_minus$y
#
#     gList(
#       ggplot2::ggproto_parent(GeomBar, self)$draw_panel(data, panel_scales, coord)
#     )
#   }
# )
#
