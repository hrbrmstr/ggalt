#   #' @rdname ggalt-ggproto
#   #' @format NULL
#   #' @usage NULL
#   #' @export
#   GeomTable <- ggproto(
#     "GeomTable",
#     Geom,
#     required_aes = c("x", "y", "table"),
#     default_aes = aes(
#       widthx = 10,
#       widthy = 10,
#       rownames = NA
#     ),
#     draw_key = draw_key_blank,
#
#     draw_panel = function(data, panel_scales, coord) {
#       if (nrow(data) != 1) {
#         stop(
#           sprintf(
#             "only one table per panel allowed, got %s (%s)",
#             nrow(data),
#             as.character(data)
#           ),
#           call. = FALSE
#         )
#       }
#       wy = data$widthy / 2
#       wx = data$widthx / 2
#
#       corners <-
#         data.frame(x = c(data$x - wx, data$x + wx),
#                    y = c(data$y - wy, data$y + wy))
#       d <- coord$transform(corners, panel_scales)
#
#       # gross hack, but I've found no other way to get a table/matrix/dataframe to this point :-(
#       table = utils::read.csv(text = data$table, header = TRUE)
#       if (!is.na(data$rownames)) {
#         rownames(table) <-
#           unlist(strsplit(data$rownames, "|", fixed = TRUE))
#       }
#
#       x_rng <- range(d$x, na.rm = TRUE)
#       y_rng <- range(d$y, na.rm = TRUE)
#
#       vp <-
#         viewport(
#           x = mean(x_rng),
#           y = mean(y_rng),
#           width = diff(x_rng),
#           height = diff(y_rng),
#           just = c("center", "center")
#         )
#
#       grob <-
#         tableGrob(table, theme = ttheme_minimal())
#       # add a line across the header
#       grob <- gtable_add_grob(
#         grob,
#         grobs = segmentsGrob(y1 = unit(0, "npc"),
#                              gp = gpar(lwd = 2.0)),
#         t = 1,
#         b = 1,
#         l = 1,
#         r = ncol(d) + 1
#       )
#       editGrob(grob, vp = vp, name = paste(grob$name, facet_id()))
#     }
#   )
#
#   facet_id <- local({
#     i <- 1
#     function() {
#       i <<- i + 1
#       i
#     }
#   })
#
#   #' Add a table to a ggplot2 plot
#   #'
#   #' @export
#   #' @author Jan Schulz
#   geom_table <- function(mapping = NULL, data = NULL, stat = "identity",
#                          position = "identity", na.rm = FALSE,
#                          show.legend = NA, inherit.aes = TRUE, ...) {
#       layer(
#         geom = GeomTable,
#         mapping = mapping,
#         data = data,
#         stat = stat,
#         position = position,
#         show.legend = show.legend,
#         inherit.aes = inherit.aes,
#         params = list(na.rm = na.rm, ...)
#       )
#     }
#
